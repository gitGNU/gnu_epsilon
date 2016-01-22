/* Experimantal driver for code produced by the epsilon0 compiler with the C backend

   Copyright (C) 2013, 2015 Luca Saiu
   Updated in 2014 and 2016 by Luca Saiu

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
#include "runtime/runtime.h"

#ifdef EPSILON_EGC
#include "movinggc/movinggc.h"
#endif // #ifdef EPSILON_EGC

void epsilon_main_entry_point(epsilon_value*);

// FIXME: factorize, replacing old cruft

/* These are defined in the assembly code: */
extern char global_data_beginning;
extern char global_data_end;

int
main (int argc, char **argv){
  epsilon_runtime_initialize (argc, argv);

  /* Add assembly globals as GC roots: */
  char *minimum_global_address = &global_data_beginning;
  char *maximum_global_address = &global_data_end;
  if(minimum_global_address > maximum_global_address){
    char *temporary = minimum_global_address;
    minimum_global_address = maximum_global_address;
    maximum_global_address = temporary;
  }
  //printf("Assembly roots are [%p, %p)\n", minimum_global_address, maximum_global_address);

  epsilon_thread_context_t context = epsilon_make_thread_context();
#ifdef EPSILON_EGC
  egc_register_roots ((void**)minimum_global_address,
                      (void**)maximum_global_address - (void**)minimum_global_address);
#else
  GC_add_roots(minimum_global_address, maximum_global_address);
  GC_add_roots(context, ((char*)context) + sizeof(struct epsilon_thread_context) + 1);
#endif // #ifdef EPSILON_EGC
  /* fprintf (stderr, "Globals: [%p, %p]\n", minimum_global_address, maximum_global_address); */
  /* fprintf (stderr, "Stack:   [%p, %p]\n", context->stack_lowest_address, context->stack_highest_address); */
  //printf("C%p S%p L%p H%p\n", context, context->stack, context->stack_lowest_address, context->stack_highest_address);
  //epsilon_value result =
    epsilon_run_thread_context(context, epsilon_main_entry_point);
  //printf("R:  %li %p\n", epsilon_value_to_epsilon_int(result), result);
  //printf("C%p S%p L%p H%p\n", context, context->stack, context->stack_lowest_address, context->stack_highest_address);

  // Don't bother destroying the main thread context right before exiting.
#ifdef EPSILON_EGC
  egc_dump_generations ();
#endif // #ifdef EPSILON_EGC
  return EXIT_SUCCESS;
}
