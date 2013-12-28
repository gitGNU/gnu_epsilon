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


#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <pthread.h>
#include "epsilongc.h"

//#define USE_BOEHM_GC

#ifdef USE_BOEHM_GC
#include <gc/gc.h>
#endif // #ifdef USE_BOEHM_GC

static __thread struct epsilongc_allocator cons_allocator;

void cons_tracer(epsilongc_word_t cons){
  /* Trace the car: */
  //epsilongc_trace_pointer_if_valid(((epsilongc_word_t*)cons)[0]);
  /* Trace the cdr: */
  epsilongc_trace_pointer_if_valid(((epsilongc_word_t*)cons)[1]);
}

//#define USEMYGC

epsilongc_word_t cons(epsilongc_integer_t i,
                      epsilongc_pointer_t list){
#ifndef USE_BOEHM_GC
  epsilongc_word_t result = epsilongc_allocate_from(&cons_allocator);
#else
  epsilongc_word_t result = GC_malloc(sizeof(epsilongc_word_t) * 2);
#endif // #ifdef USE_BOEHM_GC
  ((epsilongc_integer_t*)result)[0] = i;
  ((epsilongc_word_t*)result)[1] = list;
  return result;
}

int actual_main(void){
#ifdef USE_BOEHM_GC
  GC_init();
  //GC_enable_incremental();
#endif // #ifdef USE_BOEHM_GC
  epsilongc_initialize_set_of_pages();

  epsilongc_kind_t cons_kind =
    epsilongc_make_kind(2, 2,
                        1,
                        42,
                        NULL,
                        cons_tracer,
                        NULL);
  epsilongc_pool_t cons_pool = epsilongc_make_pool("cons", cons_kind);
  /* Mutation is sequential in this example: one thread will be enough. */
  epsilongc_register_the_calling_thread_as_a_mutator("the-only-mutator-thread", 0, NULL);
  epsilongc_initialize_allocator(&cons_allocator, cons_pool);
  epsilongc_initialize_tracing_support();
  
  epsilongc_pointer_t x = NULL;
  epsilongc_register_user_defined_thread_local_root(&x);
  
#define N 100000000
  epsilongc_integer_t i;
  for(i = 0; i < N; i++){
    if(rand()%1000 == 0)
      x = NULL;
    x = cons(i, x);
  } // for
  
  return 0;
}

int main(void){
  epsilongc_initialize_garbage_collection();
  set_current_c_stack_top_as_c_stack_bottom(); 
  volatile int result = actual_main();
  return result;
}
