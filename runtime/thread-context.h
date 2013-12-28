/* The main runtime header #include'ing the others.

   Copyright (C) 2012 Université Paris 13
   Copyright (C) 2012 Luca Saiu [written during his few weeks with no employment]
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

#ifndef EPSILON_THREAD_CONTEXT_H_
#define EPSILON_THREAD_CONTEXT_H_

#include "data.h"

/* These fields are accessed from the assembly side: do not reorder them. */
struct epsilon_thread_context{
  epsilon_value *stack_highest_address;
  epsilon_value *stack_lowest_address;
  epsilon_value *stack;
}; // struct
typedef struct epsilon_thread_context* epsilon_thread_context_t;

epsilon_thread_context_t epsilon_make_thread_context(void);
void epsilon_destroy_thread_context(epsilon_thread_context_t context);

/* This is implemented in assembly, in the backend-specific part: */
void epsilon_run_thread_context(epsilon_thread_context_t thread_context,
                                void *compiled_procedure_address);

#endif // #ifndef EPSILON_THREAD_CONTEXT_H_
