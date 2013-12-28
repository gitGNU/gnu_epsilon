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


#include "epsilongc_types.h"
#include "epsilongc_threads.h"
#include "roots.h"
#include "trace.h"
#include "epsilongc_debug.h"
#include "malloc.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <setjmp.h>
#include <assert.h>
#include "epsilongc_features.h"


static void epsilongc_trace_registers(const epsilongc_thread_t thread){
  //printf("OK-B 1\n"); fflush(stdout);
  /* Unfortunately we can't tell in a portable way where general registers
     reside within the structure, and their alignment: */
  epsilongc_trace_buffer_with_all_alignments((const epsilongc_pointer_t)
                                             &(thread->roots.context_including_registers),
                                             sizeof(thread->roots.context_including_registers));
  //printf("OK-B 2\n"); fflush(stdout);
}

static void epsilongc_trace_c_stack(const epsilongc_thread_t thread){
  //printf("OK-D 1\n"); fflush(stdout);
  const epsilongc_pointer_t stack_top = thread->roots.stack_top;
  const epsilongc_pointer_t stack_bottom = thread->roots.stack_bottom;
  
  //printf("OK-D 2\n"); fflush(stdout);
  /* Trace the C stack (as of now we trace with all alignments, just to be
     safe; on some architectures it could be safe to trace only word-aligned
     potential pointers): */
  const epsilongc_signed_integer_t distance_in_bytes =
    ((char*)stack_top) - ((char*)stack_bottom);
  //printf("OK-D 3\n"); fflush(stdout);
  //#ifdef ENABLE_VERBOSE_DEBUG
  //printf("** The size of the stack is %i bytes + 1 word\n", abs((int)distance_in_bytes));
  //#endif // #ifdef ENABLE_VERBOSE_DEBUG
  if(stack_bottom <= stack_top){
    //printf("OK-D 4a\n"); fflush(stdout);
    epsilongc_assert_on_debug(distance_in_bytes >= 0);
    //printf("OK-D 5a\n"); fflush(stdout);

    epsilongc_trace_buffer_with_all_alignments(stack_bottom,
                                               sizeof(epsilongc_word_t) + distance_in_bytes);
    /* epsilongc_trace_aligned_words_in_buffer(stack_bottom, */
    /*                                         sizeof(epsilongc_word_t) + distance_in_bytes); */


    //printf("OK-D 6a\n"); fflush(stdout);
  }
  else{ 
    //printf("OK-D 4b\n"); fflush(stdout);
    epsilongc_assert_on_debug(distance_in_bytes < 0);
    //printf("OK-D 5b\n"); fflush(stdout);
    epsilongc_trace_buffer_with_all_alignments(stack_top,
                                               sizeof(epsilongc_word_t) - distance_in_bytes);
    //printf("OK-D 6b\n"); fflush(stdout);
  } // else
  //printf("OK-D 7\n"); fflush(stdout);
}

static void epsilongc_trace_user_defined_roots(epsilongc_thread_t thread){
  epsilongc_user_defined_root_t p;
  for(p = thread->roots.user_defined_thread_local_roots; p != NULL; p = p->next){
    #ifdef ENABLE_VERBOSE_DEBUG
    printf("***Tracing a user-defined root: %p, %i bytes\n", p->buffer, (int)p->length_in_bytes);
    #endif // #ifdef ENABLE_VERBOSE_DEBUG
    epsilongc_trace_aligned_words_in_buffer(p->buffer, p->length_in_bytes);
    //epsilongc_trace_buffer_with_all_alignments(p->buffer, p->length_in_bytes);
  } // for
}

void epsilongc_trace_thread_roots(const epsilongc_thread_t mutator_thread){
  //const double time0 = epsilongc_get_current_time();

  //printf("Tracing %s's roots: begin\n", epsilongc_thread_name(mutator_thread));
  //printf("Tracing thread-local roots for %s: begin\n", epsilongc_thread_name(mutator_thread)); fflush(stdout);
#ifdef ENABLE_VERBOSE_DEBUG
  marked_object_no = 0;
#endif // #ifdef ENABLE_VERBOSE_DEBUG
  //printf("Tracing registers for %s: begin\n", epsilongc_thread_name(mutator_thread)); fflush(stdout);
  epsilongc_trace_registers(mutator_thread);

  //const double time1 = epsilongc_get_current_time();
  //printf("  ?1(after registers): %.3f seconds\n", time1 - time0); fflush(stdout);
  //printf("Tracing registers for %s: end\n", epsilongc_thread_name(mutator_thread)); fflush(stdout);
  //printf("Tracing the C stack for %s: begin\n", epsilongc_thread_name(mutator_thread)); fflush(stdout);
  epsilongc_trace_c_stack(mutator_thread);

  //const double time2 = epsilongc_get_current_time();
  //printf("  ?2(after stack): %.3f seconds\n", time2 - time0); fflush(stdout);
  //printf("Tracing the C stack for %s: end\n", epsilongc_thread_name(mutator_thread)); fflush(stdout);
  //printf("Tracing user-defined roots for %s: begin\n", epsilongc_thread_name(mutator_thread)); fflush(stdout);
  epsilongc_trace_user_defined_roots(mutator_thread);

  //const double time3 = epsilongc_get_current_time();
  //printf("  ?3(after user-defined): %.3f seconds\n", time3 - time0); fflush(stdout);
  
  //printf("Tracing user-defined roots for %s: end\n", epsilongc_thread_name(mutator_thread)); fflush(stdout);
#ifdef ENABLE_VERBOSE_DEBUG
  printf("(Marked %i root objects)\n", (int)marked_object_no);
  marked_object_no = 0;
#endif // #ifdef ENABLE_VERBOSE_DEBUG
  //printf("Tracing thread-local roots for %s: end\n", epsilongc_thread_name(mutator_thread)); fflush(stdout);
  //printf("Tracing %s's roots: end\n", epsilongc_thread_name(mutator_thread));
}

/* Let's initialize the stack bottom to an invalid value, to ease debugging: */
static __thread epsilongc_pointer_t epsilongc_stack_bottom = NULL;

epsilongc_pointer_t epsilongc_get_c_stack_top(void){
  /* Approximate the stack top by stack-allocating a useless object and looking
     at a pointer to it: */
  epsilongc_word_t object_on_the_stack_top = 0; // Let's make GCC happy...
  const epsilongc_pointer_t stack_top = (const epsilongc_pointer_t)
    &object_on_the_stack_top;
#ifdef ENABLE_VERBOSE_DEBUG
  printf("The stack top is currently %p\n", stack_top);
#endif // #ifdef ENABLE_VERBOSE_DEBUG
  epsilongc_assert_on_debug(stack_top != NULL);
  return stack_top;
}

epsilongc_pointer_t epsilongc_get_c_stack_bottom(void){
#ifdef ENABLE_VERBOSE_DEBUG
  printf("The stack bottom is currently %p\n", epsilongc_stack_bottom);
#endif // #ifdef ENABLE_VERBOSE_DEBUG
  epsilongc_assert_on_debug(epsilongc_stack_bottom != NULL);
  return epsilongc_stack_bottom;
}

void set_current_c_stack_top_as_c_stack_bottom(void){
  epsilongc_integer_t useless_variable = 0;
  const epsilongc_pointer_t stack_top = &useless_variable;
  epsilongc_stack_bottom = stack_top;
}

/* All the currently-registered user-defined thread-local roots: */
static __thread epsilongc_user_defined_root_t epsilongc_user_defined_roots = NULL;

void epsilongc_register_user_defined_thread_local_roots(epsilongc_pointer_t buffer,
                                           epsilongc_unsigned_integer_t length_in_words){
  /* It makes no sense to have zero-length roots: */
  epsilongc_assert_on_debug(length_in_words > 0);
  
  /* Make a new root, and prepend it to the list: */
  epsilongc_user_defined_root_t new_root = (epsilongc_user_defined_root_t)
    epsilongc_xmalloc(sizeof(struct epsilongc_user_defined_root));
  new_root->buffer = buffer;
  new_root->length_in_bytes = length_in_words * sizeof(epsilongc_word_t);
  new_root->next = epsilongc_user_defined_roots;
  epsilongc_user_defined_roots = new_root;
}

void epsilongc_register_user_defined_thread_local_root(epsilongc_pointer_t *word){
  epsilongc_register_user_defined_thread_local_roots(word, 1);
}

/* Unregister all thread-local roots: */
void epsilongc_unregister_user_defined_thread_local_roots(void){
  /* Scan the list and destroy each element: */
  epsilongc_user_defined_root_t p = epsilongc_user_defined_roots;
  while(p != NULL){
    epsilongc_user_defined_root_t next = p->next;
    free(p);
    p = next;
  } // while
  epsilongc_user_defined_roots = NULL;
}

void epsilongc_fill_thread_roots(volatile epsilongc_thread_roots_t thread_roots){
  //printf("Thread %p, epsilongc_fill_thread_roots(): begin\n", (void*)pthread_self());
  /* Copy the pointer (and only the pointer) to the thread-local root list: */
  thread_roots->user_defined_thread_local_roots = epsilongc_user_defined_roots;
  
  /* Dump registers: getcontext() would be more appropriate than setjmp()
     for getting an image of the registers, but for some reason it was not
     available on GNU/Linux-Sparc the last time I checked; anyhow this
     works; in particular it should also save register-windows where they
     exist: */
  assert(setjmp(thread_roots->context_including_registers) == 0);
  
  /* Copy stack end pointers (and *not* the stack content): */
  thread_roots->stack_bottom = epsilongc_get_c_stack_bottom();
  thread_roots->stack_top = epsilongc_get_c_stack_top();
  //printf("Thread %p, epsilongc_fill_thread_roots(): end\n", (void*)pthread_self());
}
