/* This file is part of GNU epsilon.

   Copyright (C) 2012 Université Paris 13
   Written by Luca Saiu

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


#ifndef EPSILONGC_ROOTS_H_
#define EPSILONGC_ROOTS_H_

#include "epsilongc_types.h"
#include "epsilongc_threads.h"
#include <stdio.h>
#include <stdbool.h>
//#include "compile_time_parameters.h"
//#include "../include/malloc.h"
//#include "epsilongc_debug.h"
//#include "../include/time.h"

/* Register a new array of word-aligned, thread-local roots: */
void epsilongc_register_user_defined_thread_local_roots(
        epsilongc_pointer_t buffer,
        epsilongc_unsigned_integer_t length_in_words);

/* Register a new single-word thread-local root, given its pointer (i.e. a
   pointer to the candidate pointer): */
void epsilongc_register_user_defined_thread_local_root(epsilongc_pointer_t *word);

/* Unregister all thread-local roots: */
void epsilongc_unregister_user_defined_thread_local_roots(void);

/* Use the current C stack *top* as an approximation of the stack bottom: */
void set_current_c_stack_top_as_c_stack_bottom(void);

/* Return the current C stack top: */
epsilongc_pointer_t epsilongc_get_c_stack_top(void);

/* Return the safe approximation of the current thread's C stack bottom, as
   it has been saved the last time: */
epsilongc_pointer_t epsilongc_get_c_stack_bottom(void);

/* Fill the given structure with thread-local root information. This *can*
   be safely called from signal handlers, and is O(1). Notice that no
   actual roots other than the register image is stored into the structure;
   the structure only contains enough information for another thread to be
   able to find all the thread's roots: */
void epsilongc_fill_thread_roots(volatile epsilongc_thread_roots_t thread_roots);

void epsilongc_trace_thread_roots(const epsilongc_thread_t mutator_thread);

#endif // #ifndef EPSILONGC_ROOTS_H_
