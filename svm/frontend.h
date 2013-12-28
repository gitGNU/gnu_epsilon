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


#ifndef SLOW_VIRTUAL_MACHINE_FRONTEND_H_
#define SLOW_VIRTUAL_MACHINE_FRONTEND_H_

#include "svm.h"
#include "instructions.h"

/* Initialize or finalize the SVM frontend, needed for parsing from an SVM
   source file: */
void svm_initialize_frontend(void);
void svm_finalize_frontend(void);

/* Here we have the implementation of "ASTs" as built by the parser, with
   references not yet resolved, so that we can parse in one pass, and then
   later backpatch in another pass to resolve references to labels and
   globals.  This backpatching pass is called `resolving'. */

/* Register names can just be resolved at parsing time, as we always know the
   mapping from register name to register index.  Anyway, this is used in the
   parser: */
typedef char* svm_register_name;

svm_register_index svm_resolve_register_name(svm_register_name register_name)
  __attribute__(( hot ));

typedef char* svm_label_name;
typedef char* svm_global_name;
typedef char* svm_c_global_name; // name of a global C variable or C function

/* An immediate is understood to always be one of:
   - the address of a label
   - the address of a global
   - the address of a C global
   - an integer constant
   - the sum, difference, product or quotient of two immediates. */
typedef enum svm_unresolved_immediate_tag{
  svm_unresolved_immediate_invalid_tag = 0, // not used in valid unresolved immediates
  svm_unresolved_immediate_label_tag,
  svm_unresolved_immediate_global_tag,
  svm_unresolved_immediate_c_global_tag,
  svm_unresolved_immediate_float_constant_tag,
  svm_unresolved_immediate_integer_constant_tag,
  svm_unresolved_immediate_sum_tag,
  svm_unresolved_immediate_difference_tag,
  svm_unresolved_immediate_product_tag,
  svm_unresolved_immediate_quotient_tag,
  svm_unresolved_immediate_modulo_tag,
} svm_unresolved_immediate_tag;
typedef struct svm_unresolved_immediate* svm_unresolved_immediate_t;
struct svm_unresolved_immediate{
  svm_unresolved_immediate_tag tag;
  union{
    svm_label_name label;
    svm_global_name global;
    svm_c_global_name c_global;
    svm_int integer_immediate;
    epsilon_float float_immediate;
    struct{
      svm_unresolved_immediate_t operand1;
      svm_unresolved_immediate_t operand2;
    }; // anonymous struct
  }; // anonymous union
}; // struct

struct svm_unresolved_instruction{
  enum svm_opcode opcode;
  svm_register_index source_register_1;
  svm_register_index source_register_2;
  svm_register_index target_register_1;
  /* A NULL value of immediate is admissible, and it means zero. It's the default
     value for a just-initialized unresolved instruction (it's easier to replace
     with a non-NULL pointer if needed, without freeing anything): */
  svm_unresolved_immediate_t immediate;
  
#ifdef ENABLE_VERBOSE_SVM
  /* Source line no: this is useful for debugging: */
  svm_int line_no;
#endif // #ifdef ENABLE_VERBOSE_SVM
}; // struct
typedef struct svm_unresolved_instruction* svm_unresolved_instruction_t;

struct svm_unresolved_program{
  /* A sequence of svm_unresolved_instruction_t (hence a dynamic array of pointers): */
  epsilon_dynamic_array_t instructions; 
  /* A sequence of svm_unresolved_immediate_t (hence a dynamic array of pointers, again): */
  epsilon_dynamic_array_t global_words;
}; // struct
typedef struct svm_unresolved_program* svm_unresolved_program_t;

/* malloc() a new struct and initialize it.  Note that the immediate field is NULL: */
svm_unresolved_instruction_t svm_make_unresolved_instruction(void)
  __attribute__(( malloc ));

/* Finalize and then de-allocate the struct, if it's not NULL: */
void svm_destroy_unresolved_instruction(svm_unresolved_instruction_t unresolved_instruction);

/* malloc() a new struct and initialize it.  Note that the following functions, when taking
   a pointer to a buffer, never make copies of them: they just refer the given objects from
   the returned struct, so that destroying the struct will also destroy the referred
   sub-objects. */
svm_unresolved_immediate_t svm_make_unresolved_label_immediate(svm_label_name label)
  __attribute__(( malloc ));
svm_unresolved_immediate_t svm_make_unresolved_global_immediate(svm_global_name global)
  __attribute__(( malloc ));
svm_unresolved_immediate_t svm_make_unresolved_c_global_immediate(svm_c_global_name c_global)
  __attribute__(( malloc ));
svm_unresolved_immediate_t svm_make_unresolved_integer_immediate(epsilon_int value)
  __attribute__(( malloc ));
svm_unresolved_immediate_t svm_make_unresolved_float_immediate(epsilon_float value)
  __attribute__(( malloc ));
svm_unresolved_immediate_t svm_make_unresolved_sum_immediate(svm_unresolved_immediate_t operand1,
                                                             svm_unresolved_immediate_t operand2)
  __attribute__(( malloc ));
svm_unresolved_immediate_t svm_make_unresolved_difference_immediate(svm_unresolved_immediate_t operand1,
                                                                    svm_unresolved_immediate_t operand2)
  __attribute__(( malloc ));
svm_unresolved_immediate_t svm_make_unresolved_product_immediate(svm_unresolved_immediate_t operand1,
                                                                 svm_unresolved_immediate_t operand2)
  __attribute__(( malloc ));
svm_unresolved_immediate_t svm_make_unresolved_quotient_immediate(svm_unresolved_immediate_t operand1,
                                                                  svm_unresolved_immediate_t operand2)
  __attribute__(( malloc ));
svm_unresolved_immediate_t svm_make_unresolved_modulo_immediate(svm_unresolved_immediate_t operand1,
                                                                svm_unresolved_immediate_t operand2)
  __attribute__(( malloc ));

/* Make an immediate instruction already filled with the given content.  Pointed immediate
   are shared in the result, and not copied: see the previous comment. */
svm_unresolved_instruction_t
svm_make_unresolved_instruction_(enum svm_opcode opcode);
svm_unresolved_instruction_t
svm_make_unresolved_instruction_s(enum svm_opcode opcode,
                                  svm_register_index source1);
svm_unresolved_instruction_t
svm_make_unresolved_instruction_t(enum svm_opcode opcode,
                                  svm_register_index target1);
svm_unresolved_instruction_t
svm_make_unresolved_instruction_st(enum svm_opcode opcode,
                                   svm_register_index source1,
                                   svm_register_index target1);
svm_unresolved_instruction_t
svm_make_unresolved_instruction_sst(enum svm_opcode opcode,
                                    svm_register_index source1,
                                    svm_register_index source2,
                                    svm_register_index target1);
svm_unresolved_instruction_t
svm_make_unresolved_instruction_si(enum svm_opcode opcode,
                                   svm_register_index source1,
                                   svm_unresolved_immediate_t immediate);
svm_unresolved_instruction_t
svm_make_unresolved_instruction_sit(enum svm_opcode opcode,
                                    svm_register_index source1,
                                    svm_unresolved_immediate_t immediate,
                                    svm_register_index target1);
svm_unresolved_instruction_t
svm_make_unresolved_instruction_it(enum svm_opcode opcode,
                                   svm_unresolved_immediate_t immediate,
                                   svm_register_index target1);
svm_unresolved_instruction_t
svm_make_unresolved_instruction_i(enum svm_opcode opcode,
                                  svm_unresolved_immediate_t immediate);

/* Finalize and then de-allocate structs: */
void svm_destroy_unresolved_immediate(svm_unresolved_immediate_t unresolved_immediate);

/* Associate a label name to the index of the next (not yet existing) instruction: */
void svm_bind_label_name_to_the_next_instruction(const svm_label_name name,
                                                 svm_unresolved_program_t program);

/* Associate a global name to the index of the next (not yet existing) global word
   in the global array: */
void svm_bind_global_name_to_the_next_global_word(const svm_global_name name,
                                                  svm_unresolved_program_t program);

/* Create or destroy an unresolved program: */
svm_unresolved_program_t svm_make_unresolved_program(void);
void svm_destroy_unresolved_program(svm_unresolved_program_t program);

/* Resolve a complete unresolved program, and return its resolved version: */
svm_program_t svm_resolve_program(svm_unresolved_program_t unresolved_program);

/* Append an instruction to an unresolved program.  The given instruction will be referred
   by the program dynamic array: */
void svm_add_unresolved_instruction(svm_unresolved_program_t program,
                                    svm_unresolved_instruction_t instruction);

/* Append a global word to an unresolved program.  The given immediate will be referred
   by the program dynamic array:*/
void svm_add_unresolved_global_word(svm_unresolved_program_t program,
                                    svm_unresolved_immediate_t initial_value);
void svm_add_unresolved_global_zero_word(svm_unresolved_program_t program);

/* Return the index of the *next* instruction (which doesn't exist yet) in the
   program dynamic array: */
epsilon_int svm_unresolved_program_to_next_instruction_index(svm_unresolved_program_t program);

/* Return the index of the *next* global word (which doesn't exist yet) in the
   program dynamic array: */
epsilon_int svm_unresolved_program_to_next_global_word_index(svm_unresolved_program_t program);

#endif // #ifndef SLOW_VIRTUAL_MACHINE_FRONTEND_H_
