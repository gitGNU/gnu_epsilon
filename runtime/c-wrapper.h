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


#ifndef EPSILON_C_WRAPPER_H_
#define EPSILON_C_WRAPPER_H_

/* To do: I'm not convinced any longer that we need a global stack of 
   C contexts.  I suspect an automatic C variable is more than enough,
   as the C control stack is more than enough to maintain a LIFO policy;
   using auto variables also avoids any non-reentrancy problems. */

#include "../utility/utility.h"
#include <setjmp.h>

void epsilon_initialize_c_wrapper(void);
void epsilon_finalize_c_wrapper(void);


/* Default epsilon stack size, in words. Stack overflow is *not* checked for: */
#define EPSILON_STACK_SIZE_IN_WORDS 1024

/* How many words to (initially) reserve for thread-local variables, per thread: */
#define EPSILON_THREAD_LOCAL_WORD_NO 32

/* The struct epsilon_epsilon_thread_context holds a snapshot of the assembly
   machine state in a certain thread.  Some fields depend on the backend. */
typedef struct epsilon_epsilon_thread_context* epsilon_epsilon_thread_context_t;
struct epsilon_epsilon_thread_context{
  /* A snapshot of the registers (including the instruction counter) at
     the time of the last context switch.
     The actual buffer size and interpretation depends on the backend,
     but it is guaranteed to be malloc()ed.  Notice that the referred
     structure also contains the instruction pointer, somewhere. */
  void *register_image;

  /* Pointers to the minimum and maximum word within epsilon_stack.  Typically
     only one of those will be used in a given backend, depending on the stack
     growth direction. */
  epsilon_word *epsilon_stack_minimum_word;
  epsilon_word *epsilon_stack_maximum_word;

  /* The C code, or some other thread, can access these fields to pass parameters
     or retrieve results.  parameter_buffer is allocated by the thread creator,
     if it's really a pointer (it could be just contain one word-sized parameter);
     result_buffer may be allocated either by the thread creator or by the thread
     itself. */
  epsilon_word parameter_buffer;
  epsilon_word result_buffer;
  
  /* Pointer to the first element of the stack, which is malloc()ed.  Notice that
     this isn't directly usable in most backends where the stack grows downwards,
     but it's handy for allocating and de-allocating: */
  epsilon_word *epsilon_stack;
  
  /* A malloc()ed buffer containing all thread-locals which are not simply kept
     in registers: */
  void *thread_local_variables;
  
  /* How many words are currently used in the thread-local array: */
  size_t thread_local_used_word_no;
}; // struct

/* Make or destroy a thread context, with the dynamic state in a backend-dependent
   "initialized" state: */
epsilon_epsilon_thread_context_t
epsilon_make_epsilon_thread_context(epsilon_word initial_instruction_pointer)
  __attribute__((malloc));
/* Notice that the parameters and results may be non-pointers; they are
   freed by the thread creator, if freeing them is needed. */
void epsilon_destroy_epsilon_thread_context(epsilon_epsilon_thread_context_t context);

/* Resume a thread from its last saved context, or stop it saving its state.
   These are backend-dependent: */
void epsilon_resume_thread(epsilon_epsilon_thread_context_t context);
void epsilon_suspend_thread(epsilon_epsilon_thread_context_t context,
                            epsilon_word next_instruction_label);

/* Save the current context and jump to the given routine, presumably
   written in assembly, which is free to trash registers.  The
   assembly thunk will be able to return to C by jumping to the 0-argument
   function epsilon_pop_epsilon_context().  epsilon_pop_epsilon_context()
   returns the control to right after the call to this function, and
   the user is finally able to access the epsilon call results from the
   context struct. */
void epsilon_enter_epsilon_context(epsilon_epsilon_thread_context_t context)
  __attribute__((returns_twice));

/* Go back from epsilon to C.  This function should be called from an
   assembly thunk; since it has zero parameters and doesn't ever return
   a single jump instruction should be enough.
   From the point of view of C calling this function returns the control
   to the point *after* the last call to epsilon_enter_epsilon_context(). */
void epsilon_leave_epsilon_context(void)
  __attribute__((noreturn));

#endif // #ifndef EPSILON_C_WRAPPER_H_
