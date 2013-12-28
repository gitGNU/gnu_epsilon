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


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "registers.h" 
#include "../utility/utility.h" 

char *svm_stack_pointer_register_name = "%rsp";
char *svm_frame_pointer_register_name = "%rfp";
char *svm_link_register_name =          "%rlr";
char *svm_c_parameter_register_prefix = "%rcp";
char *svm_c_result_register_name =      "%rcr";
char *svm_general_purpose_register_name = "    ";
char *svm_c_fparameter_register_prefix = "%fcp";
char *svm_c_fresult_register_name =      "%fcr";

const char* svm_register_special_usage(svm_register_index r){
  if(r == SVM_STACK_POINTER_REGISTER_INDEX)
    return svm_stack_pointer_register_name;
  if(r == SVM_FRAME_POINTER_REGISTER_INDEX)
    return svm_frame_pointer_register_name;
  if(r == SVM_LINK_REGISTER_INDEX)
    return svm_link_register_name;
  /*  else if(r == SVM_C_PARAMETER_REGISTER_INDEX)
      return svm_c_parameter_register_name; */
  else if(r == SVM_C_RESULT_REGISTER_INDEX)
    return svm_c_result_register_name;
  else
    return svm_general_purpose_register_name;
}

void svm_initialize_register_file(svm_register_file_t register_file){
  int i;
  for(i = 0; i < SVM_GENERAL_REGISTER_NO; i ++)
    register_file->generals[i] = 0;
  for(i = 0; i < SVM_FP_REGISTER_NO; i ++)
    register_file->fps[i] = 0.0;
  for(i = 0; i < SVM_CONDITION_REGISTER_NO; i ++)
    register_file->conditions[i] = 0u;
  /* We intentionally set these fields to invalid values just to remember
     that we really have initialize them later... */
  register_file->instruction_pointer = NULL;
  register_file->program = NULL;

  /* The program is not initialized by default: */
  register_file->is_program_initialized = false;
}

svm_register_file_t svm_make_register_file(void){
  svm_register_file_t result =
    epsilon_xmalloc(sizeof(struct svm_register_file));
  svm_initialize_register_file(result);
  return result;
}

void svm_destroy_register_file(svm_register_file_t register_file){
  free(register_file);
}
