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


#ifndef EPSILON_RUNTIME_BACKEND_SPECIFIC_H_
#define EPSILON_RUNTIME_BACKEND_SPECIFIC_H_

#include "../utility/utility.h"
#include "c-wrapper.h"

/* The implementation of these function is backend-specific, but the
   interface is set here once and for all.
   Implementations are found in a subdirectory of backend-specific, and
   consist in a single assembly file backend-specific-s.s (empty for the
   SVM) and a single C file backend-specific-c.c. */

/* Initialize the register_image field in the given structure, which is
   guaranteed to be otherwise initialized.  The field is malloc()ed. */
void
epsilon_initialize_epsilon_context_register_image(epsilon_epsilon_thread_context_t context,
                                                  epsilon_word initial_instruction_pointer);

/* Jump to the given assembly context -- the code will "return" by jumping,
   from the assembly side, to epsilon_leave_epsilon_context().  This function
   should not be inlined because we *do* want to push a frame onto the C control
   stack, so that later it is safe to call longjmp(). */
void epsilon_jump_to_epsilon_context(epsilon_epsilon_thread_context_t context)
  __attribute__((noinline));

#endif // #ifndef EPSILON_RUNTIME_BACKEND_SPECIFIC_H_
