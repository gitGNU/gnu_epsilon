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
#include <epsilongc_types.h>
#include <allocator.h>
#include <kind.h>
#include <set_of_pages.h>
#include <trace.h>
#include <sweep.h>
#include <epsilongc/time.h>
#include <gc/gc.h>

#define USEMYGC

static __thread struct epsilongc_allocator cons_allocator;

void cons_marker(epsilongc_word_t cons){
  /* Trace the car: */
  epsilongc_trace_pointer_if_valid(((epsilongc_word_t*)cons)[0]);
  /* Trace the cdr: */
  epsilongc_trace_pointer_if_valid(((epsilongc_word_t*)cons)[1]);
}

void cons_finalizer(epsilongc_word_t cons){
  //printf("Finalizing %p\n", cons);
}

void print_list(epsilongc_word_t list){
  if(list == NULL)
    printf("\n");
  else{
    printf("%i ", (int)((epsilongc_integer_t*)list)[0]);
    print_list(((epsilongc_word_t*)list)[1]);
  }
}

epsilongc_word_t list;
epsilongc_word_t another_root;

void epsilongc_trace_roots(void){
  epsilongc_trace_pointer_if_valid(list);
  epsilongc_trace_pointer_if_valid(another_root);
}

epsilongc_word_t allocate(void){
#ifdef USEMYGC
  return epsilongc_allocate_from(&cons_allocator);
#else
  return GC_malloc(8);
#endif
}

int main(void){
#ifdef USEMYGC
#else
  GC_init();
#endif 
  epsilongc_initialize_set_of_pages();
  //void *result;
  epsilongc_kind_t cons_kind =
    epsilongc_make_kind(2,
                        1,
                        42,
                        NULL,
                        cons_marker,
                        cons_finalizer);
  epsilongc_pool_t cons_pool = epsilongc_make_pool(cons_kind);
  epsilongc_initialize_allocator(&cons_allocator, cons_pool);
  epsilongc_initialize_tracing();
  
  another_root =
    allocate();

  /* epsilongc_word_t object; */
  /* while(true){ */
  /*   object = epsilongc_allocate_from(&a); */
  /*   printf("%p\n", object); */
  /* } */
#define N 10000000
#define USE_GC
  printf("Allocating %i elements...\n", N);
  int i;
  list = NULL;
  //while(1){
  for(i = 0; i < N; i++){
    epsilongc_word_t new_cons =
      allocate();
    ((epsilongc_integer_t*)new_cons)[0] = (epsilongc_integer_t)i;
    ((epsilongc_word_t*)new_cons)[1] = list;
    list = new_cons;
    if((rand() % 10000) == 0){
      list = NULL; // make the whole list dead
      //printf("*** MAKING THE LIST DEAD ***\n");
    }
    //if((i % 1000000) == 0){
    if((rand() % 1000000) == 0){
#ifdef USEMYGC
      EPSILONGC_BENCHMARK("COLLECTING",
      EPSILONGC_BENCHMARK("tracing roots",
      {
        epsilongc_trace_roots();
      });
      EPSILONGC_BENCHMARK("tracing",
      {
        epsilongc_trace();
      });
      EPSILONGC_BENCHMARK("sweeping the pool",
      {
        epsilongc_sweep_pool(cons_pool);
      });
      );
      printf("\n");
      //printf("i is now %i.\n", (int)i);
      //print_list(list);
#endif // #ifdef USEMYGC
    } // if
  } // for
  // } // while
  printf("Done.\n");
  
  //epsilongc_page_t page = epsilongc_make_page(kind);
  //epsilongc_destroy_page(page);
  //result = epsilongc_allocate_from(&a);
  printf("Still alive: terminating...\n");
  return 0;
}
