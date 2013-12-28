/* [the file purpose in one line ???]

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


/* Backend-specific runtime for the SVM. */

#include "../../backend-specific.h"
#include "../../../svm/registers.h"
#include "../../../svm/instructions.h"
#include "../../../svm/interpreter.h"
#include "../../../utility/utility.h"

void
epsilon_initialize_epsilon_context_register_image(epsilon_epsilon_thread_context_t context,
                                                  epsilon_word svm_program_as_word){ // we cheat!
  svm_register_file_t register_file =
    svm_make_register_file();
  
  /* Actually in the SVM we have to cheat: instead of taking an initial instruction
     pointer we have to use a pointer to the whole SVM program, because we need to
     initialize it when threading is enabled... */
  svm_program_t svm_program = svm_program_as_word;
  register_file->program = svm_program;

  /* Set the instruction pointer so that we start where the caller said: */
  register_file->instruction_pointer =
    & (svm_program->instructions[0]);
  
  /* Set the stack pointer to the last word of the stack.  On the SVM
     the stack grows downwards: */
  register_file->generals[SVM_STACK_POINTER_REGISTER_INDEX] =
    context->epsilon_stack_maximum_word;
  
  /* Make the register file "belong" to the given context: */
  context->register_image = register_file;
}

void epsilon_jump_to_epsilon_context(epsilon_epsilon_thread_context_t context){
  /* Managing registers is particularly easy here with the SVM; we just use the
     pointer in the context, because SVM "registers" are actually in memory... */
  svm_register_file_t register_file =
    context->register_image;
  
  /* Initialize the program, if this is the first time we're seeing it: */
  if(! register_file->is_program_initialized){
    svm_initialize_program(register_file->program);
    register_file->is_program_initialized = true;
  } // if
  
  /* Jump to the next instruction: */
  svm_run_program(register_file);
}
