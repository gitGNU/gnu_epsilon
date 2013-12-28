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


/* Backend-specific runtime for the PowerPC (the part implemented in C). */

#include "../../backend-specific.h"
#include "../../../utility/utility.h"

#define EPSILON_POWERPC_GENERAL_REGISTER_NO 32
#define EPSILON_POWERPC_FP_REGISTER_NO 32

/* Just by convention, this general register is used as the stack pointer
   on the PowerPC: */
#define EPSILON_POWERPC_STACK_POINTER_INDEX 1

struct epsilon_powerpc_register_state{
  epsilon_word instruction_pointer;
  epsilon_word general_registers[EPSILON_POWERPC_GENERAL_REGISTER_NO];
  double fp_registers[EPSILON_POWERPC_GENERAL_REGISTER_NO];
  epsilon_unsigned condition_registers; // 32 bits in 8 4-bit fields
  epsilon_word count_register;
  epsilon_word link_register;
}; // struct
typedef struct epsilon_powerpc_register_state* epsilon_powerpc_register_state_t;

void
epsilon_initialize_epsilon_context_register_image(epsilon_epsilon_thread_context_t context,
                                                  epsilon_label instruction_pointer){
  /* Make the register file: */
  epsilon_powerpc_register_state_t registers =
    epsilon_xmalloc(sizeof(struct epsilon_powerpc_register_state));
  
  /* Fill it.  General registers and float registers are set to zero: */
  int i;
  for(i = 0; i < EPSILON_POWERPC_GENERAL_REGISTER_NO; i ++)
    registers->general_registers[i] = 0;
  for(i = 0; i < EPSILON_POWERPC_FP_REGISTER_NO; i ++)
    registers->fp_registers[i] = 0.0;
  registers->condition_registers = 0u;
  registers->link_register = NULL;
  registers->count_register = NULL;
  
  /* Set the instruction pointer so that we start where the caller said: */
  registers->instruction_pointer = instruction_pointer;
  
  /* Set the stack pointer to the last word of the stack.  On the PowerPC
     the stack grows downwards: */
  registers->general_registers[EPSILON_POWERPC_STACK_POINTER_INDEX] =
    context->epsilon_stack_maximum_word;
  
  /* Make the register file "belong" to the given context: */
  context->register_image = registers;
}

// To do: this isn't use any more as the procedure is implemented in assembly, but
// I want to keep around the inline assembly example.
void epsilon_jump_to_epsilon_context_(epsilon_epsilon_thread_context_t context){
  /* Look at the register image: */
  epsilon_powerpc_register_state_t registers =
    context->register_image;
  
  /* To do: we should load registers... */
  
  /* Jump to the instruction pointer: */
  printf("Jumping to %p...\n", registers->instruction_pointer);
  register epsilon_label instruction_pointer =
    registers->instruction_pointer;
  void *nonexisting_result;
  asm volatile ("mtlr %[instruction_pointer]"
                : "=l" (nonexisting_result) // the result is in the link register
                : [instruction_pointer] "r" (instruction_pointer));
  //asm volatile("mtlr %r6");
  asm volatile("blr"); // maybe blrl
}
