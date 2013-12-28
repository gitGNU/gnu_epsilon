/* SVM frontend.

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


// To do: epsilon_register_name_hash and epsilon_label_name_hash should become
// part of an unresolved program.  This will make the interface reentrant
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <assert.h>
#include "../utility/utility.h"
#include "svm.h"
#include "parser.h"
#include "frontend.h"
#include "../runtime/interface-with-c.h"

/* We want a nice way of converting register names into indices.  Keys are
   svm_register_names, data are epsilon_ints (register indices): */
static epsilon_string_hash_t epsilon_register_name_hash = NULL;

/* We want a nice way of converting label names into instruction addresses.
   Keys are svm_label_names, data are epsilon_ints (indices in the instruction
   array): */
static epsilon_string_hash_t epsilon_label_name_hash = NULL;

/* We want a nice way of converting global names into addresses.  Keys are
   svm_global_names, data are epsilon_ints (indices in the global word array): */
static epsilon_string_hash_t epsilon_global_name_hash = NULL;


void svm_initialize_frontend(void){
  /* Check that we didn't initialize twice, once and for all: */
  if(epsilon_register_name_hash != NULL)
    epsilon_fatal("svm_initialize_frontend(): initialized twice");

  /* Make the register name conversion hash, and fill it: */
  epsilon_register_name_hash = epsilon_make_string_hash();
  char register_name[100];
  int i;
  for(i = 0; i < SVM_GENERAL_REGISTER_NO; i ++){
    sprintf(register_name, "%s%i", SVM_GENERAL_REGISTER_PREFIX, i);
    epsilon_add_to_string_hash(epsilon_register_name_hash,
                               register_name,
                               EPSILON_INT_TO_EPSILON_WORD(i));
  } // for
  for(i = 0; i < SVM_FP_REGISTER_NO; i ++){
    sprintf(register_name, "%s%i", SVM_FP_REGISTER_PREFIX, i);
    epsilon_add_to_string_hash(epsilon_register_name_hash,
                               register_name,
                               EPSILON_INT_TO_EPSILON_WORD(i));
  } // for
  for(i = 0; i < SVM_CONDITION_REGISTER_NO; i ++){
    sprintf(register_name, "%s%i", SVM_CONDITION_REGISTER_PREFIX, i);
    epsilon_add_to_string_hash(epsilon_register_name_hash,
                               register_name,
                               EPSILON_INT_TO_EPSILON_WORD(i));
  } // for
  /* Also store register aliases: */
  epsilon_add_to_string_hash(epsilon_register_name_hash,
                             svm_stack_pointer_register_name,
                             EPSILON_INT_TO_EPSILON_WORD(SVM_STACK_POINTER_REGISTER_INDEX));
  epsilon_add_to_string_hash(epsilon_register_name_hash,
                             svm_frame_pointer_register_name,
                             EPSILON_INT_TO_EPSILON_WORD(SVM_FRAME_POINTER_REGISTER_INDEX));
  epsilon_add_to_string_hash(epsilon_register_name_hash,
                             svm_link_register_name,
                             EPSILON_INT_TO_EPSILON_WORD(SVM_LINK_REGISTER_INDEX));
  for(i = 0; i < SVM_MAXIMUM_C_PRIMITIVE_ARITY; i ++){
    sprintf(register_name, "%s%i", svm_c_parameter_register_prefix, i);
    epsilon_add_to_string_hash(epsilon_register_name_hash,
                               register_name,
                               EPSILON_INT_TO_EPSILON_WORD(SVM_C_PARAMETER_REGISTER_INDEX + i));
  } // for
  epsilon_add_to_string_hash(epsilon_register_name_hash,
                             svm_c_result_register_name,
                             EPSILON_INT_TO_EPSILON_WORD(SVM_C_RESULT_REGISTER_INDEX));
  for(i = 0; i < SVM_MAXIMUM_C_PRIMITIVE_ARITY; i ++){
    sprintf(register_name, "%s%i", svm_c_fparameter_register_prefix, i);
    epsilon_add_to_string_hash(epsilon_register_name_hash,
                               register_name,
                               EPSILON_INT_TO_EPSILON_WORD(SVM_C_FPARAMETER_REGISTER_INDEX + i));
  } // for
  epsilon_add_to_string_hash(epsilon_register_name_hash,
                             svm_c_fresult_register_name,
                             EPSILON_INT_TO_EPSILON_WORD(SVM_C_FRESULT_REGISTER_INDEX));
  
  /* Make an empty label name hash: */
  epsilon_label_name_hash = epsilon_make_string_hash();

  /* Make an empty global name hash and reset the used words no to zero: */
  epsilon_global_name_hash = epsilon_make_string_hash();
}

void svm_finalize_frontend(void){
  epsilon_destroy_string_hash(epsilon_register_name_hash);
  epsilon_register_name_hash = NULL; // to prevent bugs
  epsilon_destroy_string_hash(epsilon_label_name_hash);
  epsilon_label_name_hash = NULL; // to prevent bugs
  epsilon_destroy_string_hash(epsilon_global_name_hash);
  epsilon_global_name_hash = NULL; // to prevent bugs
}

svm_register_index svm_resolve_register_name(svm_register_name register_name){
  if(! epsilon_is_string_hash_bound_on(epsilon_register_name_hash, register_name))
    epsilon_fatal("unknown register %s", register_name);
  const epsilon_word lookup_result =
    epsilon_lookup_string_hash(epsilon_register_name_hash, register_name);
  return EPSILON_WORD_TO_EPSILON_INT(lookup_result);
}

#ifdef ENABLE_VERBOSE_SVM
/* Defined in the flex-generated scanner: */
extern int svm_lineno;
#endif // #ifdef ENABLE_VERBOSE_SVM

void svm_initialize_unresolved_instruction(svm_unresolved_instruction_t unresolved_instruction){
  unresolved_instruction->opcode = svm_index_less_than_any_instruction; // intentionally invalid
  unresolved_instruction->source_register_1 = 0;
  unresolved_instruction->source_register_2 = 0;
  unresolved_instruction->target_register_1 = 0;
  unresolved_instruction->immediate = NULL;
#ifdef ENABLE_VERBOSE_SVM
  unresolved_instruction->line_no = svm_lineno;
#endif // #ifdef ENABLE_VERBOSE_SVM
}

/* free() the buffers referred by non-NULL pointer fields, without freeing the struct
   ifself: */
void svm_finalize_unresolved_immediate(svm_unresolved_immediate_t immediate){
  switch(immediate->tag){
  case svm_unresolved_immediate_label_tag:
    free(immediate->label);
    break;
  case svm_unresolved_immediate_global_tag:
    free(immediate->global);
    break;
  case svm_unresolved_immediate_c_global_tag:
    free(immediate->c_global);
    break;    
  case svm_unresolved_immediate_float_constant_tag:
  case svm_unresolved_immediate_integer_constant_tag:
    /* Do nothing */
    break;    
  case svm_unresolved_immediate_sum_tag:
  case svm_unresolved_immediate_difference_tag:
  case svm_unresolved_immediate_product_tag:
  case svm_unresolved_immediate_quotient_tag:
  case svm_unresolved_immediate_modulo_tag:
    svm_finalize_unresolved_immediate(immediate->operand1);
    svm_finalize_unresolved_immediate(immediate->operand2);
    break;
  default:
    epsilon_impossible();
  } // switch
}

static svm_unresolved_immediate_t svm_make_unresolved_immediate(svm_unresolved_immediate_tag tag){
  svm_unresolved_immediate_t result = (svm_unresolved_immediate_t)
    epsilon_xmalloc(sizeof(struct svm_unresolved_immediate));
  result->tag = tag;
  return result;
}
svm_unresolved_immediate_t svm_make_unresolved_label_immediate(svm_label_name label){
  svm_unresolved_immediate_t result = svm_make_unresolved_immediate(svm_unresolved_immediate_label_tag);
  result->label = label;
  return result;
}
svm_unresolved_immediate_t svm_make_unresolved_global_immediate(svm_global_name global){
  svm_unresolved_immediate_t result = svm_make_unresolved_immediate(svm_unresolved_immediate_global_tag);
  result->global = global;
  return result;
}
svm_unresolved_immediate_t svm_make_unresolved_c_global_immediate(svm_c_global_name c_global){
  svm_unresolved_immediate_t result = svm_make_unresolved_immediate(svm_unresolved_immediate_c_global_tag);
  result->c_global = c_global;
  return result;
}
svm_unresolved_immediate_t svm_make_unresolved_integer_immediate(epsilon_int value){
  svm_unresolved_immediate_t result = svm_make_unresolved_immediate(svm_unresolved_immediate_integer_constant_tag);
  result->integer_immediate = value;
  return result;
}
svm_unresolved_immediate_t svm_make_unresolved_float_immediate(epsilon_float value){
  svm_unresolved_immediate_t result = svm_make_unresolved_immediate(svm_unresolved_immediate_float_constant_tag);
  result->float_immediate = value;
  return result;
}
svm_unresolved_immediate_t svm_make_unresolved_sum_immediate(svm_unresolved_immediate_t operand1,
                                                             svm_unresolved_immediate_t operand2){
  svm_unresolved_immediate_t result = svm_make_unresolved_immediate(svm_unresolved_immediate_sum_tag);
  result->operand1 = operand1;
  result->operand2 = operand2;
  return result;
}
svm_unresolved_immediate_t svm_make_unresolved_difference_immediate(svm_unresolved_immediate_t operand1,
                                                                    svm_unresolved_immediate_t operand2){
  svm_unresolved_immediate_t result = svm_make_unresolved_immediate(svm_unresolved_immediate_difference_tag);
  result->operand1 = operand1;
  result->operand2 = operand2;
  return result;
}
svm_unresolved_immediate_t svm_make_unresolved_product_immediate(svm_unresolved_immediate_t operand1,
                                                                 svm_unresolved_immediate_t operand2){
  svm_unresolved_immediate_t result = svm_make_unresolved_immediate(svm_unresolved_immediate_product_tag);
  result->operand1 = operand1;
  result->operand2 = operand2;
  return result;
}
svm_unresolved_immediate_t svm_make_unresolved_quotient_immediate(svm_unresolved_immediate_t operand1,
                                                                  svm_unresolved_immediate_t operand2){
  svm_unresolved_immediate_t result = svm_make_unresolved_immediate(svm_unresolved_immediate_quotient_tag);
  result->operand1 = operand1;
  result->operand2 = operand2;
  return result;
}
svm_unresolved_immediate_t svm_make_unresolved_modulo_immediate(svm_unresolved_immediate_t operand1,
                                                                svm_unresolved_immediate_t operand2){
  svm_unresolved_immediate_t result = svm_make_unresolved_immediate(svm_unresolved_immediate_modulo_tag);
  result->operand1 = operand1;
  result->operand2 = operand2;
  return result;
}

void svm_destroy_unresolved_immediate(svm_unresolved_immediate_t unresolved_immediate){
  if(unresolved_immediate == NULL)
    return;
  svm_finalize_unresolved_immediate(unresolved_immediate);
  free(unresolved_immediate);
}

void svm_finalize_unresolved_instruction(svm_unresolved_instruction_t unresolved_instruction){
  if(unresolved_instruction->immediate != NULL)
    svm_destroy_unresolved_immediate(unresolved_instruction->immediate);
}

svm_unresolved_instruction_t svm_make_unresolved_instruction(void){
  svm_unresolved_instruction_t result = (svm_unresolved_instruction_t)
    epsilon_xmalloc(sizeof(struct svm_unresolved_instruction));
  svm_initialize_unresolved_instruction(result);
  return result;
}

void svm_destroy_unresolved_instruction(svm_unresolved_instruction_t unresolved_instruction){
  svm_finalize_unresolved_instruction(unresolved_instruction);
  free(unresolved_instruction);
}

svm_unresolved_instruction_t
svm_make_unresolved_instruction_(enum svm_opcode opcode){
  svm_unresolved_instruction_t result = svm_make_unresolved_instruction();
  result->opcode = opcode;
  return result;
}
svm_unresolved_instruction_t
svm_make_unresolved_instruction_s(enum svm_opcode opcode,
                                  svm_register_index source1){
  svm_unresolved_instruction_t result = svm_make_unresolved_instruction_(opcode);
  result->source_register_1 = source1;
  return result;
}
svm_unresolved_instruction_t
svm_make_unresolved_instruction_t(enum svm_opcode opcode,
                                  svm_register_index target1){
  svm_unresolved_instruction_t result = svm_make_unresolved_instruction_(opcode);
  result->target_register_1 = target1;
  return result;
}
svm_unresolved_instruction_t
svm_make_unresolved_instruction_st(enum svm_opcode opcode,
                                   svm_register_index source1,
                                   svm_register_index target1){
  svm_unresolved_instruction_t result = svm_make_unresolved_instruction_(opcode);
  result->source_register_1 = source1;
  result->target_register_1 = target1;
  return result;
}
svm_unresolved_instruction_t
svm_make_unresolved_instruction_sst(enum svm_opcode opcode,
                                    svm_register_index source1,
                                    svm_register_index source2,
                                    svm_register_index target1){
  svm_unresolved_instruction_t result = svm_make_unresolved_instruction_(opcode);
  result->source_register_1 = source1;
  result->source_register_2 = source2;
  result->target_register_1 = target1;
  return result;
}
svm_unresolved_instruction_t
svm_make_unresolved_instruction_si(enum svm_opcode opcode,
                                   svm_register_index source1,
                                   svm_unresolved_immediate_t immediate){
  svm_unresolved_instruction_t result = svm_make_unresolved_instruction_(opcode);
  result->source_register_1 = source1;
  result->immediate = immediate;
  return result;
}
svm_unresolved_instruction_t
svm_make_unresolved_instruction_sit(enum svm_opcode opcode,
                                    svm_register_index source1,
                                    svm_unresolved_immediate_t immediate,
                                    svm_register_index target1){
  svm_unresolved_instruction_t result = svm_make_unresolved_instruction_(opcode);
  result->source_register_1 = source1;
  result->immediate = immediate;
  result->target_register_1 = target1;
  return result;
}
svm_unresolved_instruction_t
svm_make_unresolved_instruction_it(enum svm_opcode opcode,
                                   svm_unresolved_immediate_t immediate,
                                   svm_register_index target1){
  svm_unresolved_instruction_t result = svm_make_unresolved_instruction_(opcode);
  result->immediate = immediate;
  result->target_register_1 = target1;
  return result;
}
svm_unresolved_instruction_t
svm_make_unresolved_instruction_i(enum svm_opcode opcode,
                                  svm_unresolved_immediate_t immediate){
  svm_unresolved_instruction_t result = svm_make_unresolved_instruction_(opcode);
  result->immediate = immediate;
  return result;
}

void svm_bind_label_name_to_the_next_instruction(const svm_label_name name, svm_unresolved_program_t program){
  if(epsilon_is_string_hash_bound_on(epsilon_label_name_hash, name))
    epsilon_fatal("double definition of the label %s", name);
  const epsilon_int instruction_no =
    epsilon_dynamic_array_used_size(program->instructions) / sizeof(svm_unresolved_instruction_t);
  epsilon_add_to_string_hash(epsilon_label_name_hash,
                             name,
                             EPSILON_EPSILON_INT_TO_EPSILON_WORD(instruction_no));
}

void svm_bind_global_name_to_the_next_global_word(const svm_global_name name, svm_unresolved_program_t program){
  if(epsilon_is_string_hash_bound_on(epsilon_global_name_hash, name))
    epsilon_fatal("double definition of the global %s", name);
  const epsilon_int global_word_no =
    epsilon_dynamic_array_used_size(program->global_words) / sizeof(svm_unresolved_immediate_t);
  /* Bind the global name to the first unused address: */
  epsilon_add_to_string_hash(epsilon_global_name_hash,
                             name,
                             EPSILON_EPSILON_INT_TO_EPSILON_WORD(global_word_no));
}

/* Resolve an immediate (writing into the given svm_word) by reading
   the given svm_unresolved_immediate_t and resolving names with the global bindings
   labels and globals: */
static void svm_resolve_immediate(const svm_unresolved_immediate_t unresolved_immediate,
                                  svm_program_t program,
                                  epsilon_word_or_float *target){
  /* Just resolve the immediate to (integer) zero if no unresolved immediate
     is given; presumably the instruction doesn't need an immediate: */
  if(unresolved_immediate == NULL){
    target->non_float_value = 0;
    return;
  } // if

  switch(unresolved_immediate->tag){
  case svm_unresolved_immediate_label_tag:
    if(! epsilon_is_string_hash_bound_on(epsilon_label_name_hash, unresolved_immediate->label))
      epsilon_fatal("svm_resolve_immediate: unknown label %s", unresolved_immediate->label);
    const epsilon_int label_index =
      EPSILON_WORD_TO_EPSILON_INT(epsilon_lookup_string_hash(epsilon_label_name_hash, unresolved_immediate->label));
    target->non_float_value = (void*)(char*)
      (((struct svm_instruction*)program->instructions) +
       EPSILON_WORD_TO_EPSILON_INT(label_index));
    break;
  case svm_unresolved_immediate_global_tag:
    if(! epsilon_is_string_hash_bound_on(epsilon_global_name_hash, unresolved_immediate->global))
      epsilon_fatal("svm_resolve_immediate: unknown global %s", unresolved_immediate->global);
    const epsilon_int global_word_index =
      EPSILON_WORD_TO_EPSILON_INT(epsilon_lookup_string_hash(epsilon_global_name_hash, unresolved_immediate->global));
    target->non_float_value = (void*)(char*)
      (((epsilon_word*)program->global_words) + global_word_index);
    break;
  case svm_unresolved_immediate_c_global_tag:{
    const epsilon_word c_global =
      epsilon_lookup_c_global(unresolved_immediate->c_global);
    if(c_global == NULL)
      epsilon_fatal("could not load the C global \"%s\"", unresolved_immediate->c_global);
    target->non_float_value = c_global;
    break;
  }
  case svm_unresolved_immediate_integer_constant_tag:
    target->non_float_value = EPSILON_EPSILON_INT_TO_EPSILON_WORD(unresolved_immediate->integer_immediate);
    break;    
  case svm_unresolved_immediate_float_constant_tag:
    target->float_value = unresolved_immediate->float_immediate;
    break;    
  case svm_unresolved_immediate_sum_tag:{
    epsilon_word_or_float resolved_operand1;
    epsilon_word_or_float resolved_operand2;
    svm_resolve_immediate(unresolved_immediate->operand1, program, &resolved_operand1);
    svm_resolve_immediate(unresolved_immediate->operand2, program, &resolved_operand2);
    target->non_float_value =
      EPSILON_EPSILON_INT_TO_EPSILON_WORD(EPSILON_WORD_TO_EPSILON_INT(resolved_operand1.non_float_value) +
                                          EPSILON_WORD_TO_EPSILON_INT(resolved_operand2.non_float_value));
    break;
  }
  case svm_unresolved_immediate_difference_tag:
    break;    
  case svm_unresolved_immediate_product_tag:
    break;    
  case svm_unresolved_immediate_quotient_tag:
    break;    
  case svm_unresolved_immediate_modulo_tag:
    break;    
  default:
    epsilon_impossible();
  }; // switch
}

/* Resolve an instruction (writing into the given svm_instruction_t) by
   copying from the given svm_unresolved_instruction_t and resolving names
   with the global bindings for labels and globals: */
void svm_resolve_instruction(const svm_unresolved_instruction_t unresolved_instruction,
                             svm_program_t program,
                             svm_instruction_t target){
  target->opcode = unresolved_instruction->opcode;
  target->source_register_1 = unresolved_instruction->source_register_1;
  target->source_register_2 = unresolved_instruction->source_register_2;
  target->target_register_1 = unresolved_instruction->target_register_1;
  svm_resolve_immediate(unresolved_instruction->immediate,
                        program,
                        &(target->immediate));
#ifdef ENABLE_VERBOSE_SVM
  target->line_no = unresolved_instruction->line_no;
#endif // #ifdef ENABLE_VERBOSE_SVM
}

svm_unresolved_program_t svm_make_unresolved_program(void){
  svm_unresolved_program_t result = (svm_unresolved_program_t)
    epsilon_xmalloc(sizeof(struct svm_unresolved_program));
  result->instructions = epsilon_make_dynamic_array();
  result->global_words = epsilon_make_dynamic_array();
  return result;
}

void svm_destroy_unresolved_program(svm_unresolved_program_t program){
  const int instruction_no =
    epsilon_dynamic_array_used_size(program->instructions) / sizeof(svm_unresolved_instruction_t);
  const int global_word_no =
    epsilon_dynamic_array_used_size(program->global_words) / sizeof(svm_unresolved_immediate_t);
  svm_unresolved_instruction_t *instructions = (svm_unresolved_instruction_t*)
    epsilon_dynamic_array_to_array(program->instructions);
  svm_unresolved_immediate_t *global_words = (svm_unresolved_immediate_t*)
    epsilon_dynamic_array_to_array(program->global_words);
  int i;
  for(i = 0; i < instruction_no; i ++){
    //printf("Destroying the unresolved instruction with index %i (they are %i)\n", (int)i, (int)instruction_no); fflush(stdout);
    svm_destroy_unresolved_instruction(instructions[i]);
  }
  for(i = 0; i < global_word_no; i ++){
    //printf("Destroying the unresolved immediate with index %i (they are %i)\n", (int)i, (int)global_word_no); fflush(stdout);
    svm_destroy_unresolved_immediate(global_words[i]);
  }
  epsilon_destroy_dynamic_array(program->instructions);
  epsilon_destroy_dynamic_array(program->global_words);
  free(program);
}

svm_program_t svm_resolve_program(svm_unresolved_program_t program){
  /* Arrays will be non-dynamic in the result, so we have to know their size: */
  const int instruction_no =
    epsilon_dynamic_array_used_size(program->instructions) / sizeof(svm_unresolved_instruction_t);
  const int global_word_no =
    epsilon_dynamic_array_used_size(program->global_words) / sizeof(svm_unresolved_immediate_t);
  svm_unresolved_instruction_t *unresolved_instructions = (svm_unresolved_instruction_t*)
    epsilon_dynamic_array_to_array(program->instructions);
  svm_unresolved_immediate_t *unresolved_global_words = (svm_unresolved_immediate_t*)
    epsilon_dynamic_array_to_array(program->global_words);
  
  //printf("svm_resolve_program(): %i instructions and %i global words\n", (int)instruction_no, (int)global_word_no);
  /* Make the resolved program, with its non-dynamic arrays of the correct size: */
  svm_program_t result = svm_make_program(instruction_no, global_word_no);
  
  /* Resolve instructions: */
  printf("The first instruction is at %p\n", &(result->instructions[0]));
  printf("The last one          is at %p\n", &(result->instructions[instruction_no - 1]));
  int i;
  for(i = 0; i < instruction_no; i ++){
    //printf("Resolving the instruction %i\n", i);
    svm_resolve_instruction(unresolved_instructions[i],
                            result,
                            &(result->instructions[i]));
  }

  /* Resolve global words: */
  printf("The first global word is at %p\n", &(result->global_words[0]));
  printf("The last one          is at %p\n", &(result->global_words[global_word_no - 1]));
  for(i = 0; i < global_word_no; i ++)
    svm_resolve_immediate(unresolved_global_words[i],
                          result,
                          &(result->global_words[i]));
  
  return result;
}

void svm_add_unresolved_instruction(svm_unresolved_program_t program,
                                    svm_unresolved_instruction_t instruction){
  epsilon_push_onto_dynamic_array(program->instructions,
                                  &instruction,
                                  sizeof(svm_unresolved_instruction_t));
}

void svm_add_unresolved_global_word(svm_unresolved_program_t program,
                                    svm_unresolved_immediate_t initial_value){
  epsilon_push_onto_dynamic_array(program->global_words,
                                  &initial_value,
                                  sizeof(svm_unresolved_immediate_t));
}

void svm_add_unresolved_global_zero_word(svm_unresolved_program_t program){
  svm_unresolved_immediate_t zero_word = NULL; // this will resolve to zero
  svm_add_unresolved_global_word(program, zero_word);
}

epsilon_int svm_unresolved_program_to_next_instruction_index(svm_unresolved_program_t program){
  return epsilon_dynamic_array_used_size(program->instructions) / sizeof(svm_unresolved_instruction_t);
}

epsilon_int svm_unresolved_program_to_next_global_word_index(svm_unresolved_program_t program){
  return epsilon_dynamic_array_used_size(program->global_words) / sizeof(svm_unresolved_immediate_t);
}
