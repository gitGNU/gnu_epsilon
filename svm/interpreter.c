/* SVM interpreter.

   Copyright (C) 2012 Université Paris 13
   Written by Luca Saiu

   This file is part of GNU epsilon.

   GNU epsilon is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   GNU epsilon is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU epsilon.  If not, see <http://www.gnu.org/licenses/>. */


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdbool.h>
#include <assert.h>
#include "svm.h"
#include "interpreter.h"
#include "../runtime/interface-with-c.h"
#include "../runtime/c-wrapper.h" // we need to be able to exit a context

static void svm_dump_general_register_content(const int i, const svm_register_file_t const registers){
  printf("  %s%-3i %s: %-20li u%-20lu 0x%-20lx\n",
         SVM_GENERAL_REGISTER_PREFIX, i, svm_register_special_usage(i),
         (long)registers->generals[i],
         (unsigned long)registers->generals[i],
         (unsigned long)registers->generals[i]);
}

static void svm_dump_fp_register_content(const int i, const svm_register_file_t const registers){
  printf("  %s%-3i     : %-20f\n", SVM_FP_REGISTER_PREFIX, i,
         (double)registers->fps[i]);
}

static void svm_dump_condition_register_content(const int i, const svm_register_file_t const registers){
  printf("  %s%-3i     : %-20u %21s 0x%-20x\n", SVM_CONDITION_REGISTER_PREFIX, i,
         (unsigned)registers->conditions[i], "", (unsigned)registers->conditions[i]);
}

static void svm_dump_state(const svm_register_file_t const registers){
  int i;
  printf("=============================== dump ===============================\n");
  for(i = 0; i < SVM_GENERAL_REGISTER_NO; i ++)
    svm_dump_general_register_content(i, registers);
  for(i = 0; i < SVM_FP_REGISTER_NO; i ++)
    svm_dump_fp_register_content(i, registers);
  for(i = 0; i < SVM_CONDITION_REGISTER_NO; i ++)
    svm_dump_condition_register_content(i, registers);
  printf("\n");
}

/* The following macros are useful to specify instructions in a
   more compact way: */
#define BINARY_OPERATION(result_type, result, \
                         source_type1, source1, \
                         c_infix_operator, \
                         source_type2, source2) \
    result = (svm_word) \
      (result_type)(((source_type1)(source1)) c_infix_operator \
                    ((source_type1)(source2)))
#define GSOURCE1 \
  (registers->generals[instruction->source_register_1])
#define GSOURCE2 \
  (registers->generals[instruction->source_register_2])
#define GTARGET1 \
  (registers->generals[instruction->target_register_1])
/*
#define GTARGET2                                      \
  (registers->generals[instruction->target_register_2])
*/
#define BINARY_FOPERATION(result_type, result, \
                          source_type1, source1, \
                          c_infix_operator, \
                          source_type2, source2) \
    result = (result_type)(((source_type1)(source1)) c_infix_operator \
                           ((source_type1)(source2)))
#define FSOURCE1 \
  (registers->fps[instruction->source_register_1])
#define FSOURCE2 \
  (registers->fps[instruction->source_register_2])
#define FTARGET1 \
  (registers->fps[instruction->target_register_1])
/*
#define FTARGET2                                      \
  (registers->fps[instruction->target_register_2])
*/
#define CSOURCE1 \
  (registers->conditions[instruction->source_register_1])
#define CSOURCE2 \
  (registers->conditions[instruction->source_register_2])
#define CTARGET1 \
  (registers->conditions[instruction->target_register_1])
#define CTARGET2 \
  (registers->conditions[instruction->target_register_2])
#define LINK \
  (registers->generals[SVM_LINK_REGISTER_INDEX])
#define C_PARAMETER(i) \
  (registers->generals[SVM_C_PARAMETER_REGISTER_INDEX + i])
#define C_RESULT \
  (registers->generals[SVM_C_RESULT_REGISTER_INDEX])
#define IMMEDIATE \
  (instruction->immediate.non_float_value)
#define C_FRESULT \
  (registers->fps[SVM_C_FRESULT_REGISTER_INDEX])
#define C_FPARAMETER(i) \
  (registers->fps[SVM_C_FPARAMETER_REGISTER_INDEX + i])
#define FIMMEDIATE \
  (instruction->immediate.float_value)

#ifdef ENABLE_VERBOSE_SVM
static bool svm_log = false;
#endif // #ifdef ENABLE_VERBOSE_SVM

#ifdef ENABLE_VERBOSE_SVM
  #define SVM_LOG_INSTRUCTION(instruction_pointer, instruction_name) \
    do { if(svm_log) \
           printf("%i: %s\n", (int)(instruction_pointer->line_no), instruction_name); fflush(stdout); } while (0)
#else
  #define SVM_LOG_INSTRUCTION(instruction_pointer, instruction_name) \
    /* do nothing */
#endif // #ifdef ENABLE_VERBOSE_SVM

#ifdef ENABLE_SVM_THREADING
  #define JUMPTO(instruction_where_to_jump) \
    do { instruction = (svm_instruction_t)(instruction_where_to_jump); \
         goto* (instruction->label); } while(0)
#else
  #define JUMPTO(instruction_where_to_jump) \
    /* We can't use a break here because of the do..while(0) hack here.  However a */ \
    /* goto jumping before the switch works just fine. */ \
    do { instruction = (svm_instruction_t)(instruction_where_to_jump);  \
         goto before_switch; } while(0)
#endif // #ifdef ENABLE_SVM_THREADING

#define NEXT_INSTRUCTION \
  (instruction + 1)

//#define __asm__(whatever) /* do nothing */

#ifdef ENABLE_SVM_THREADING
  #define INSTRUCTION(INSTRUCTION_NAME, body) \
    EPSILON_IF_UNLIKELY(initializing) { \
      if(instruction->opcode == INSTRUCTION_NAME){ \
        /* __asm__("# initialize instruction " #INSTRUCTION_NAME); */ \
        instruction->label = (&& INSTRUCTION_NAME ## _label); \
        /*printf("Initialized the instruction at %p with the opcode %i=%s\n", instruction, (int)INSTRUCTION_NAME, #INSTRUCTION_NAME);*/ \
        goto initialize_next_instruction_or_return; \
      }; \
      if(false){ \
      INSTRUCTION_NAME ## _label: \
        SVM_LOG_INSTRUCTION(instruction, #INSTRUCTION_NAME); \
        /* __asm__("# instruction " #INSTRUCTION_NAME); */ \
        body; \
        JUMPTO(NEXT_INSTRUCTION); \
      } \
    }
#else
  /* No threading */
  #define INSTRUCTION(INSTRUCTION_NAME, body) \
  case INSTRUCTION_NAME: \
  { SVM_LOG_INSTRUCTION(instruction, #INSTRUCTION_NAME); \
    /* __asm__("# instruction " #INSTRUCTION_NAME); */ \
    body; \
    JUMPTO(NEXT_INSTRUCTION); }
#endif // #ifdef THREADING

static void
svm_initialize_or_run_program(const bool initializing,
                              const svm_program_t program,
                              const svm_register_file_t const registers){
  /* Did the user request this run to be verbose? */
  if(getenv("SVM_VERBOSE") != NULL){
    /* The user requested this run to be verbose.  Set the flag if the
       support was compiled in, fail otherwise: */
#ifdef ENABLE_VERBOSE_SVM
    svm_log = true;
#else
    epsilon_fatal("SVM verbosity was not enabled at configure time");
#endif // #ifdef ENABLE_VERBOSE_SVM
  }

  struct svm_instruction *instructions = program->instructions;
  register svm_instruction_t instruction;
  if(initializing)
    instruction = instructions;
  else
    // maybe we're resuming some suspended computation...
    instruction = registers->instruction_pointer;
  
  /* In threaded mode, the following code works for both writing program
     labels into instructions ("initializing"), and for running.
     This is needed because I have to mention some labels which are only
     visible from the interpreter procedure.
     If threading is not used, this same procedure can also expand to a
     simple switch. */
#ifdef ENABLE_SVM_THREADING
  /* We only need this for initialization, with threading enabled: */
  const int instruction_no = program->instruction_no;
  
  /* If not initializing, skip to the first instruction, which already
     contains a valid address where to jump with a goto*: */
  EPSILON_IF_LIKELY(! initializing)
    JUMPTO(instruction);

  /* Set the label of each instruction: */
 initializing_loop:
  if(instruction >= (instructions + instruction_no)){
    //printf("Initialized %i instructions\n", instruction_no);
    return;
  }
  goto initialize_this_instruction;
 initialize_next_instruction_or_return:
  instruction ++;
  goto initializing_loop;
  
  /* This comment holds for threading mode.
     When initializing, here comes a sequence of if conditionals, checking the
     opcode of an instruction. After an instruction is set, INSTRUCTION generates
     a goto to the initialize_next_instruction_or_return label.
     When initializing the body of each instruction is wrapped into an if(false),
     hence not reachable. */
 initialize_this_instruction:
#else
  /* Not threading: */
  if(initializing) // no need for initialization when not threading
    return;
 before_switch:
  switch(instruction->opcode){
#endif // #ifdef ENABLE_SVM_THREADING
  INSTRUCTION(svm_nop,
              {} );
  INSTRUCTION(svm_copy,
              { GTARGET1 = GSOURCE1; });
  INSTRUCTION(svm_load_immediate,
              { GTARGET1 = IMMEDIATE; });
  INSTRUCTION(svm_load,
              { GTARGET1 = (((epsilon_word_or_float*)GSOURCE1) + ((svm_int)IMMEDIATE))->non_float_value; });
  INSTRUCTION(svm_store,
              { (((epsilon_word_or_float*)GTARGET1) + ((svm_int)IMMEDIATE))->non_float_value = GSOURCE1; });
  INSTRUCTION(svm_add,
              { BINARY_OPERATION(svm_int, GTARGET1, svm_int, GSOURCE1, +, svm_int, GSOURCE2); });
  INSTRUCTION(svm_subtract,
              { BINARY_OPERATION(svm_int, GTARGET1, svm_int, GSOURCE1, -, svm_int, GSOURCE2); });
  INSTRUCTION(svm_multiply,
              { BINARY_OPERATION(svm_int, GTARGET1, svm_int, GSOURCE1, *, svm_int, GSOURCE2); });
  INSTRUCTION(svm_divide,
              { //EPSILON_IF_UNLIKELY(GSOURCE2 == 0)
                //  epsilon_fatal("division by zero");
                BINARY_OPERATION(svm_int, GTARGET1, svm_int, GSOURCE1, /, svm_int, GSOURCE2); });
  INSTRUCTION(svm_modulo,
              { BINARY_OPERATION(svm_int, GTARGET1, svm_int, GSOURCE1, %, svm_int, GSOURCE2); });
  INSTRUCTION(svm_add_immediate,
              { BINARY_OPERATION(svm_int, GTARGET1, svm_int, GSOURCE1, +, svm_int, IMMEDIATE); });
  INSTRUCTION(svm_subtract_immediate,
              { BINARY_OPERATION(svm_int, GTARGET1, svm_int, GSOURCE1, -, svm_int, IMMEDIATE); });
  INSTRUCTION(svm_multiply_immediate,
              { BINARY_OPERATION(svm_int, GTARGET1, svm_int, GSOURCE1, *, svm_int, IMMEDIATE); });
  INSTRUCTION(svm_divide_immediate,
              { //EPSILON_IF_UNLIKELY(IMMEDIATE == 0)
                //  epsilon_fatal("division by zero");
                BINARY_OPERATION(svm_int, GTARGET1, svm_int, GSOURCE1, /, svm_int, IMMEDIATE); });
  INSTRUCTION(svm_modulo_immediate,
              { BINARY_OPERATION(svm_int, GTARGET1, svm_int, GSOURCE1, %, svm_int, IMMEDIATE); });
  INSTRUCTION(svm_left_shift,
              { BINARY_OPERATION(svm_unsigned, GTARGET1, svm_unsigned, GSOURCE1, <<, svm_int, GSOURCE2); });
  INSTRUCTION(svm_right_shift,
              { BINARY_OPERATION(svm_unsigned, GTARGET1, svm_unsigned, GSOURCE1, >>, svm_int, GSOURCE2); });
  INSTRUCTION(svm_left_shift_immediate,
              { BINARY_OPERATION(svm_unsigned, GTARGET1, svm_unsigned, GSOURCE1, <<, svm_int, IMMEDIATE); });
  INSTRUCTION(svm_right_shift_immediate,
              { BINARY_OPERATION(svm_unsigned, GTARGET1, svm_unsigned, GSOURCE1, >>, svm_int, IMMEDIATE); });
  INSTRUCTION(svm_and,
              { BINARY_OPERATION(svm_unsigned, GTARGET1, svm_unsigned, GSOURCE1, &, svm_unsigned, GSOURCE2); });
  INSTRUCTION(svm_or,
              { BINARY_OPERATION(svm_unsigned, GTARGET1, svm_unsigned, GSOURCE1, |, svm_unsigned, GSOURCE2); });
  INSTRUCTION(svm_xor,
              { BINARY_OPERATION(svm_unsigned, GTARGET1, svm_unsigned, GSOURCE1, ^, svm_unsigned, GSOURCE2); });
  INSTRUCTION(svm_not,
              { GTARGET1 = (svm_word)(~(svm_unsigned)(GSOURCE1)); });
  INSTRUCTION(svm_and_immediate,
              { BINARY_OPERATION(svm_unsigned, GTARGET1, svm_unsigned, GSOURCE1, &, svm_unsigned, IMMEDIATE); });
  INSTRUCTION(svm_or_immediate,
              { BINARY_OPERATION(svm_unsigned, GTARGET1, svm_unsigned, GSOURCE1, |, svm_unsigned, IMMEDIATE); });
  INSTRUCTION(svm_xor_immediate,
              { BINARY_OPERATION(svm_unsigned, GTARGET1, svm_unsigned, GSOURCE1, ^, svm_unsigned, IMMEDIATE); });
  INSTRUCTION(svm_is_equal,
              { BINARY_OPERATION(svm_unsigned, GTARGET1, svm_int, GSOURCE1, ==, svm_int, GSOURCE2); });
  INSTRUCTION(svm_is_zero,
              { //printf("Is %i zero? %s\n", GSOURCE1, GSOURCE1 ? "no" : "yes");
                BINARY_OPERATION(svm_unsigned, GTARGET1, svm_int, GSOURCE1, ==, svm_int, 0); });
  INSTRUCTION(svm_is_equal_immediate,
              { BINARY_OPERATION(svm_unsigned, GTARGET1, svm_int, GSOURCE1, ==, svm_int, IMMEDIATE); });
  INSTRUCTION(svm_branch,
              { JUMPTO(GSOURCE1); });
  INSTRUCTION(svm_branch_immediate,
              { JUMPTO(IMMEDIATE); });
  INSTRUCTION(svm_branch_when,
              { if(GSOURCE1)
                  JUMPTO(IMMEDIATE); });
  INSTRUCTION(svm_branch_unless,
              { if(! GSOURCE1)
                  JUMPTO(IMMEDIATE); });

  INSTRUCTION(svm_compare,
              { CTARGET1 = SVM_COMPARE(GSOURCE1, GSOURCE2); });
  INSTRUCTION(svm_compare_immediate,
              { CTARGET1 = SVM_COMPARE(GSOURCE1, IMMEDIATE); });
  INSTRUCTION(svm_branch_on_equal,
              { if(CSOURCE1 & SVM_CONDITION_EQUAL)
                  JUMPTO(IMMEDIATE); });
  INSTRUCTION(svm_branch_on_not_equal,
              { if(CSOURCE1 & SVM_CONDITION_NOT_EQUAL)
                  JUMPTO(IMMEDIATE); });
  INSTRUCTION(svm_branch_on_less,
              { if(CSOURCE1 & SVM_CONDITION_LESS)
                  JUMPTO(IMMEDIATE); });
  INSTRUCTION(svm_branch_on_greater,
              { if(CSOURCE1 & SVM_CONDITION_GREATER)
                  JUMPTO(IMMEDIATE); });
  INSTRUCTION(svm_branch_on_less_or_equal,
              { if(CSOURCE1 & SVM_CONDITION_LESS_OR_EQUAL)
                  JUMPTO(IMMEDIATE); });
  INSTRUCTION(svm_branch_on_greater_or_equal,
              { if(CSOURCE1 & SVM_CONDITION_GREATER_OR_EQUAL)
                  JUMPTO(IMMEDIATE); });

  INSTRUCTION(svm_branch_and_link_immediate,
              { LINK = instruction + 1;
                JUMPTO(IMMEDIATE); });
  INSTRUCTION(svm_branch_and_link,
              { LINK = instruction + 1;
                JUMPTO(GSOURCE1); });
  INSTRUCTION(svm_hcf,
              { epsilon_fatal("The processor is on fire.\n"); });
  INSTRUCTION(svm_exit,
              { epsilon_leave_epsilon_context(); });
  INSTRUCTION(svm_dump_general_register,
              { svm_dump_general_register_content(instruction->source_register_1, registers); });
  INSTRUCTION(svm_dump_fp_register,
              { svm_dump_fp_register_content(instruction->source_register_1, registers); });
  INSTRUCTION(svm_dump_condition_register,
              { svm_dump_condition_register_content(instruction->source_register_1, registers); });
  INSTRUCTION(svm_dump,
              { svm_dump_state(registers); });

  INSTRUCTION(svm_load_condition,
              { CTARGET1 = (epsilon_int)((((epsilon_word_or_float*)GSOURCE1) + ((svm_int)IMMEDIATE))->non_float_value); });
  INSTRUCTION(svm_load_immediate_condition,
              { CTARGET1 = (epsilon_int)IMMEDIATE; });
  INSTRUCTION(svm_store_condition,
              { (((epsilon_word_or_float*)GTARGET1) + ((svm_int)IMMEDIATE))->non_float_value = (svm_word)(svm_int)(CSOURCE1); });


  INSTRUCTION(svm_copy_float,
              { FTARGET1 = FSOURCE1; });
  INSTRUCTION(svm_load_immediate_float,
              { FTARGET1 = FIMMEDIATE; });
  INSTRUCTION(svm_load_float,
              { FTARGET1 = (((epsilon_word_or_float*)GSOURCE1) + ((epsilon_int)FIMMEDIATE))->float_value; });
  INSTRUCTION(svm_store_float,
              { (((epsilon_word_or_float*)GTARGET1) + ((epsilon_int)IMMEDIATE))->float_value = FSOURCE1; });
  INSTRUCTION(svm_add_float,
              { BINARY_FOPERATION(svm_float, FTARGET1, svm_float, FSOURCE1, +, svm_float, FSOURCE2); });
  INSTRUCTION(svm_subtract_float,
              { BINARY_FOPERATION(svm_float, FTARGET1, svm_float, FSOURCE1, -, svm_float, FSOURCE2); });
  INSTRUCTION(svm_multiply_float,
              { BINARY_FOPERATION(svm_float, FTARGET1, svm_float, FSOURCE1, *, svm_float, FSOURCE2); });
  INSTRUCTION(svm_divide_float,
              { BINARY_FOPERATION(svm_float, FTARGET1, svm_float, FSOURCE1, /, svm_float, FSOURCE2); });
  INSTRUCTION(svm_add_float_immediate,
              { BINARY_FOPERATION(svm_float, FTARGET1, svm_float, FSOURCE1, +, svm_float, FIMMEDIATE); });
  INSTRUCTION(svm_subtract_float_immediate,
              { BINARY_FOPERATION(svm_float, FTARGET1, svm_float, FSOURCE1, -, svm_float, FIMMEDIATE); });
  INSTRUCTION(svm_multiply_float_immediate,
              { BINARY_FOPERATION(svm_float, FTARGET1, svm_float, FSOURCE1, *, svm_float, FIMMEDIATE); });
  INSTRUCTION(svm_divide_float_immediate,
              { BINARY_FOPERATION(svm_float, FTARGET1, svm_float, FSOURCE1, /, svm_float, FIMMEDIATE); });
  INSTRUCTION(svm_compare_float,
              { CTARGET1 = SVM_COMPARE(FSOURCE1, FSOURCE2); });
  INSTRUCTION(svm_compare_float_immediate,
              { CTARGET1 = SVM_COMPARE(FSOURCE1, FIMMEDIATE); });

#include "c-instructions.c"

#ifndef ENABLE_SVM_THREADING
  /* Not threading: */
  default:
#endif // #ifndef ENABLE_SVM_THREADING
    epsilon_fatal("unknown opcode %i at threading initialization time", (int)instruction->opcode);
#ifndef ENABLE_SVM_THREADING
  /* Not threading: */
  } // switch
#endif // #ifndef ENABLE_SVM_THREADING
}

void svm_initialize_program(svm_program_t program){
  svm_initialize_or_run_program(true,
                                program,
                                NULL);
}

void svm_run_program(svm_register_file_t register_file){
  svm_initialize_or_run_program(false,
                                register_file->program,
                                register_file);
}
