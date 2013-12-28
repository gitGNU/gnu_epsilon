/* SVM registers.

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


#ifndef SLOW_VIRTUAL_MACHINE_REGISTERS_H_
#define SLOW_VIRTUAL_MACHINE_REGISTERS_H_

#include "svm.h"
#include "../utility/utility.h"

typedef unsigned char svm_register_index;

//typedef unsigned svm_register_index;
typedef svm_word svm_general_register;
typedef epsilon_float svm_fp_register;
typedef unsigned char svm_condition_register;

#define SVM_GENERAL_REGISTER_PREFIX   "%r"
#define SVM_FP_REGISTER_PREFIX        "%f"
#define SVM_CONDITION_REGISTER_PREFIX "%c"

#define SVM_STACK_POINTER_REGISTER_INDEX 7
#define SVM_FRAME_POINTER_REGISTER_INDEX 6
#define SVM_LINK_REGISTER_INDEX          5
#define SVM_C_PARAMETER_REGISTER_INDEX   2 // first parameter
#define SVM_C_RESULT_REGISTER_INDEX      2

#define SVM_C_FRESULT_REGISTER_INDEX     0
#define SVM_C_FPARAMETER_REGISTER_INDEX  0 // first parameter

#define SVM_MAXIMUM_C_PRIMITIVE_ARITY 4

#define SVM_GENERAL_REGISTER_NO   8
#define SVM_FP_REGISTER_NO        4//8
#define SVM_CONDITION_REGISTER_NO 1//8

struct svm_register_file{
  svm_general_register generals[SVM_GENERAL_REGISTER_NO];
  svm_fp_register fps[SVM_FP_REGISTER_NO];
  svm_condition_register conditions[SVM_CONDITION_REGISTER_NO];
  svm_label instruction_pointer;
  struct svm_program *program;
  bool is_program_initialized;
}; // struct
typedef struct svm_register_file* svm_register_file_t;

svm_register_file_t svm_make_register_file(void) __attribute__((malloc));
void svm_initialize_register_file(svm_register_file_t register_file);
void svm_destroy_register_file(svm_register_file_t register_file);

/* Condition register masks.  A condition register, result of a binary comparison,
   holds the binary or of all the relevant predicates in the following list: */
#define SVM_CONDITION_EQUAL             (1u << 0)
#define SVM_CONDITION_NOT_EQUAL         (1u << 1)
#define SVM_CONDITION_LESS              (1u << 2)
#define SVM_CONDITION_GREATER           (1u << 3)
#define SVM_CONDITION_LESS_OR_EQUAL     (1u << 4)
#define SVM_CONDITION_GREATER_OR_EQUAL  (1u << 5)

/* Compute the content of a condition register after comparing two objects.  This
   works for both integers and floats: */
#define SVM_COMPARE(X, Y) \
  (((X) == (Y)) ? \
   (SVM_CONDITION_EQUAL | SVM_CONDITION_LESS_OR_EQUAL | SVM_CONDITION_GREATER_OR_EQUAL) : \
   (((X) < (Y)) ? \
    (SVM_CONDITION_NOT_EQUAL | SVM_CONDITION_LESS | SVM_CONDITION_LESS_OR_EQUAL) : \
    (SVM_CONDITION_NOT_EQUAL | SVM_CONDITION_GREATER | SVM_CONDITION_GREATER_OR_EQUAL)))

///////// // To do: this shoul be replaced: begin
//typedef svm_word svm_register;
//#define SVM_REGISTER_NO 8
//typedef svm_register* svm_register_file;
///////// // To do: this shoul be replaced: end

/* Aliases for special register names: */
extern char *svm_stack_pointer_register_name;
extern char *svm_frame_pointer_register_name;
extern char *svm_link_register_name;
extern char *svm_c_parameter_register_prefix;
extern char *svm_c_result_register_name;
extern char *svm_c_fparameter_register_prefix;
extern char *svm_c_fresult_register_name;
extern char *svm_general_purpose_register_name;

/* Return an alias for the given general register, or svm_general_purpose_register_name
   if it has no aliases: */
const char* svm_register_special_usage(svm_register_index r);

#endif // #ifndef SLOW_VIRTUAL_MACHINE_REGISTERS_H_
