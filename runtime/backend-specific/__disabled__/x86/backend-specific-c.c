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


/* Backend-specific runtime for the x86 (the part implemented in C). */
#include "../../backend-specific.h"
#include "../../../svm/registers.h"
#include "../../../svm/instructions.h"
#include "../../../svm/interpreter.h"
#include "../../../utility/utility.h"

struct epsilon_x86_register_state{
  epsilon_label instruction_pointer;
  epsilon_word eax, ebx, ecx, edx, esp, ebp, esi, edi;
  epsilon_unsigned eflags; // 32 bit

  /* To do: floats. But I don't care about the stupid x87 FPU. */
}; // struct
typedef struct epsilon_x86_register_state* epsilon_x86_register_state_t;

void
epsilon_initialize_epsilon_context_register_image(epsilon_epsilon_thread_context_t context,
                                                  epsilon_label first_instruction){
  /* Make the register file: */
  epsilon_x86_register_state_t registers =
    epsilon_xmalloc(sizeof(struct epsilon_x86_register_state));

  /* Fill it.  General registers and float registers are set to zero: */
  registers->eax = 0;
  registers->ebx = 0;
  registers->ecx = 0;
  registers->edx = 0;
  registers->esp = 0;
  registers->ebp = 0;
  registers->esi = 0;
  registers->edi = 0; 
  // To do: floats
  registers->eflags = 0;
  registers->instruction_pointer = first_instruction;
  
  /* Make the register file "belong" to the given context: */
  context->register_image = registers;
}
