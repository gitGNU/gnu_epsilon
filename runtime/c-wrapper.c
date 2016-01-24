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


#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <setjmp.h>

#include "c-wrapper.h"
#include "backend-specific.h"
#include "../utility/utility.h"

epsilon_epsilon_thread_context_t epsilon_make_epsilon_thread_context(epsilon_word initial_instruction_pointer){
  /* Make the struct: */
  epsilon_epsilon_thread_context_t result = (epsilon_epsilon_thread_context_t)
    epsilon_xmalloc(sizeof(struct epsilon_epsilon_thread_context));
  
  /* Make the epsilon control stack: */
  result->epsilon_stack = (epsilon_word*)
    epsilon_xmalloc(sizeof(epsilon_word) * EPSILON_STACK_SIZE_IN_WORDS);
  result->epsilon_stack_minimum_word =
    result->epsilon_stack;
  result->epsilon_stack_maximum_word =
    result->epsilon_stack + EPSILON_STACK_SIZE_IN_WORDS - 1;
  
  /* Initialize parameters and results to NULL, just to prevent bugs: */
  result->parameter_buffer = NULL;
  result->result_buffer = NULL;
    
  /* Make thread-local variables, and initialize them to NULL just to
     make the initial state well-defined: */
  result->thread_local_variables = (epsilon_word*)
    epsilon_xmalloc(sizeof(epsilon_word) * EPSILON_THREAD_LOCAL_WORD_NO);
  result->thread_local_used_word_no =
    EPSILON_THREAD_LOCAL_WORD_NO;
  
  /* Make a register image, after we have initialized all the other fields
     The actual backend-dependent procedure setting registers needs to
     access the other fields, particularly stack pointers: */
  printf("Initializing the register image in a backend-specific way... ");
#if 1 // FIXME: this was actually required, but I have to rethink the system
  epsilon_initialize_epsilon_context_register_image(result,
                                                    initial_instruction_pointer);
#endif // #if 0
  printf("done.\n");

  return result;
}

void epsilon_destroy_epsilon_thread_context(epsilon_epsilon_thread_context_t context){
  /* Notice that the parameters and results may be non-pointers; they are
     freed by the thread creator, if freeing them is needed. */
  
  /* Free the malloc()ed fields: */
  free(context->epsilon_stack);
  free(context->register_image);
  free(context->thread_local_variables);
  
  /* Free the struct itself: */
  free(context);
}


/* This struct represents the C context, as it needs to be saved when
   entering epsilon and restored when going back to C: */
typedef struct epsilon_c_context* epsilon_c_context_t;
struct epsilon_c_context{
  /* The current C state, as saved by setjmp(): */
  jmp_buf state_saved_by_setjmp;
}; // struct

static struct epsilon_dynamic_array epsilon_c_context_stack;

void epsilon_initialize_c_wrapper(void){
  epsilon_initialize_dynamic_array(& epsilon_c_context_stack);
  printf("The C wrapper was initialized\n");
}

void epsilon_finalize_c_wrapper(void){
  epsilon_finalize_dynamic_array(& epsilon_c_context_stack);
  printf("The C wrapper was finalized\n");
}

void epsilon_pop_c_context(void){
  epsilon_pop_from_dynamic_array(& epsilon_c_context_stack,
                                 sizeof(struct epsilon_c_context));
}

epsilon_c_context_t epsilon_top_c_context(void){
  return (epsilon_c_context_t)
    (((char*)
      (epsilon_first_unused_byte_in_dynamic_array(& epsilon_c_context_stack))) -
     sizeof(struct epsilon_c_context));
}

void epsilon_push_c_context(epsilon_c_context_t context_to_copy_and_push){
  /* The structure is safe to simply copy, as it doesn't contain pointers: */
  epsilon_push_onto_dynamic_array(& epsilon_c_context_stack,
                                  context_to_copy_and_push,
                                  sizeof(struct epsilon_c_context));
}

/* The return value is the same as setjmp(): */
static int epsilon_dump_current_c_context(epsilon_c_context_t c_context)
  __attribute__((returns_twice));
static int epsilon_dump_current_c_context(epsilon_c_context_t c_context){
  return setjmp(c_context->state_saved_by_setjmp);
}

/* The return value is the same as setjmp(): */
void epsilon_push_current_c_context(void){
  epsilon_c_context_t reserved_struct = (epsilon_c_context_t)
    epsilon_reserve_from_dynamic_array(& epsilon_c_context_stack,
                                       sizeof(struct epsilon_c_context));
  epsilon_dump_current_c_context(reserved_struct);
}

void epsilon_enter_epsilon_context(epsilon_epsilon_thread_context_t context){
  /* Reserve an element on the stack: */
  epsilon_c_context_t c_context = (epsilon_c_context_t)
    epsilon_reserve_from_dynamic_array(& epsilon_c_context_stack,
                                       sizeof(struct epsilon_c_context));
  /* Call setjmp() so that we can easily exit from the epsilon context
     later. This also fills *c_context: */ 
  if(setjmp(c_context->state_saved_by_setjmp) == 0){
    /* If setjmp() returned zero then we have just saved the context;
       jump to the routine: */
    printf("Pushed a C context...\n");

    /* // To do: replace this part: begin */
    /* printf("To do: now the epsilon context should start, and eventually\n"); */
    /* printf("       call epsilon_leave_epsilon_context from the assembly code\n"); */
    /* epsilon_leave_epsilon_context(); */
    /* // To do: replace this part: end */
#if 1 // FIXME: this was actually required, but I have to rethink the system
    epsilon_jump_to_epsilon_context(context);
#endif // #if 0
  }
  else{
    /* setjmp() returnedd nonzero, so we have just returned from the
       epsilon context.  Restore the C context, but leave the terminated
       epsilon context around so that the caller can inspect its status
       and result values: */
    printf("Just returned from an epsilon context.\n");
    epsilon_pop_from_dynamic_array(& epsilon_c_context_stack,
                                   sizeof(struct epsilon_c_context));
    printf("Popped a C context. Now the control follows the last call of\n");
    printf("epsilon_enter_espilon_context().\n");
  } // else
}

void epsilon_leave_epsilon_context(void){
  /* Jump back: we're gonna finish updating the context stack and free
     data structures in the else branch of epsilon_enter_epsilon_context: */
  epsilon_c_context_t latest_c_context = (epsilon_c_context_t)
    epsilon_top_of_dynamic_array(& epsilon_c_context_stack,
                                 sizeof(struct epsilon_c_context));
  longjmp(latest_c_context->state_saved_by_setjmp, 1);
}
