/* SVM instruction support.

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


#ifndef SLOW_VIRTUAL_MACHINE_INSTRUCTIONS_H_
#define SLOW_VIRTUAL_MACHINE_INSTRUCTIONS_H_

#include "svm.h"
#include "registers.h"

enum svm_opcode{
  svm_index_less_than_any_instruction = -1, // don't move this
  svm_nop = 0,

  svm_copy,
  
  svm_load_immediate,
  svm_load,
  svm_store,

  svm_load_condition,
  svm_load_immediate_condition,
  svm_store_condition,

  svm_copy_float,
  svm_load_immediate_float,
  svm_load_float,
  svm_store_float,
  svm_compare_float,
  svm_compare_float_immediate,
  svm_add_float,
  svm_subtract_float,
  svm_multiply_float,
  svm_divide_float,
  svm_modulo_float,
  svm_add_float_immediate,
  svm_subtract_float_immediate,
  svm_multiply_float_immediate,
  svm_divide_float_immediate,

  svm_add,
  svm_subtract,
  svm_multiply,
  svm_divide,
  svm_modulo,
  svm_add_immediate,
  svm_subtract_immediate,
  svm_multiply_immediate,
  svm_divide_immediate,
  svm_modulo_immediate,

  svm_left_shift,
  svm_right_shift,

  svm_left_shift_immediate,
  svm_right_shift_immediate,

  svm_and,
  svm_or,
  svm_xor,
  svm_not,
  svm_and_immediate,
  svm_or_immediate,
  svm_xor_immediate,
  
  svm_is_equal,
  svm_is_equal_immediate,
  svm_is_zero,
  
  svm_branch,
  svm_branch_immediate,
  svm_branch_when,
  svm_branch_unless,

  svm_compare,
  svm_compare_immediate,
  svm_branch_on_equal,
  svm_branch_on_not_equal,
  svm_branch_on_less,
  svm_branch_on_greater,
  svm_branch_on_less_or_equal,
  svm_branch_on_greater_or_equal,
  
  svm_branch_and_link,
  svm_branch_and_link_immediate,

#include "c-instructions.h"

  svm_hcf,
  svm_dump,
  svm_dump_general_register,
  svm_dump_fp_register,
  svm_dump_condition_register,
  
  svm_exit,
  
  svm_index_greater_than_any_instruction // don't move this
}; // enum

struct svm_instruction{
#ifdef ENABLE_SVM_THREADING
  union{
#endif // #ifdef ENABLE_SVM_THREADING
    enum svm_opcode opcode;
#ifdef ENABLE_SVM_THREADING
    svm_label label;
  }; // anonymous union
#endif // #ifdef ENABLE_SVM_THREADING
  svm_register_index target_register_1;
  //svm_register_index target_register_2;
  svm_register_index source_register_1;
  svm_register_index source_register_2;
  epsilon_word_or_float immediate;
  
#ifdef ENABLE_VERBOSE_SVM
  /* Source line no: this is useful for debugging: */
  int line_no;
#endif // #ifdef ENABLE_VERBOSE_SVM
}; // struct

typedef struct svm_instruction* svm_instruction_t;

struct svm_program{
  struct svm_instruction *instructions;
  epsilon_word_or_float *global_words;
  int instruction_no;
  int global_word_no;
}; // struct

//typedef struct svm_instruction* svm_program_t;
typedef struct svm_program* svm_program_t;

svm_program_t svm_make_program(int instruction_no, int global_word_no)
  __attribute__(( malloc ));
void svm_destroy_program(svm_program_t program);

#endif // #ifndef SLOW_VIRTUAL_MACHINE_INSTRUCTIONS_H_
