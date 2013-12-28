/* This file is part of GNU epsilon.

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
#include <string.h>
#include <malloc.h>
#include <pthread.h>
#include <epsilongc/epsilongc_types.h>
#include <epsilongc/allocator.h>
#include <epsilongc/kind.h>
#include <epsilongc/set_of_pages.h>
#include <epsilongc/trace.h>
#include <epsilongc/sweep.h>
#include <epsilongc/time.h>
#include <gc/gc.h>


static __thread struct epsilongc_allocator cons_allocator;

void cons_marker(epsilongc_word_t cons){
  /* Trace the car: */
  epsilongc_trace_pointer_if_valid(((epsilongc_word_t*)cons)[0]);
  /* Trace the cdr: */
  epsilongc_trace_pointer_if_valid(((epsilongc_word_t*)cons)[1]);
}

void cons_finalizer(epsilongc_word_t cons){
/*   printf("Finalizing %p: (%p . %p)\n", */
/*          cons, */
/*          ((epsilongc_word_t*)cons)[0], */
/*          ((epsilongc_word_t*)cons)[1]); */
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
  return epsilongc_allocate_from(&cons_allocator);
}

int main(void){
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
  double mutation_start_time = epsilongc_get_current_time();
  double mutation_total_time = 0;
  double collection_total_time = 0;
  epsilongc_unsigned_integer_t collection_index = 0;
  while(1){
  for(i = 0; i < N; i++){
    epsilongc_word_t new_cons =
      allocate();
    ((epsilongc_integer_t*)new_cons)[0] = (epsilongc_integer_t)i;
    ((epsilongc_word_t*)new_cons)[1] = list;
    list = new_cons;
    if((rand() % 10000000) == 0){
      list = NULL; // make the whole list dead
      //printf("*** MAKING THE LIST DEAD ***\n");
    }
    //if((i % 1000000) == 0){
    if((rand() % 10000000) == 0){
      const double mutation_end_time = epsilongc_get_current_time();
      const double time_for_this_mutation = mutation_end_time - mutation_start_time;
      mutation_total_time += time_for_this_mutation;
      printf("Mutation: %f seconds\n", time_for_this_mutation);
      const double collection_start_time = epsilongc_get_current_time();
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
      const double collection_end_time = epsilongc_get_current_time();
      const double time_for_this_collection = collection_end_time - collection_start_time;
      collection_total_time += time_for_this_collection;
      printf("Total mutation time: %f seconds\n", mutation_total_time);
      printf("Total collection time: %f seconds (%.2f%%)\n",
             collection_total_time, 100.0 * collection_total_time / (mutation_total_time + collection_total_time));
      printf("\n");
      //printf("i is now %i.\n", (int)i);
      //print_list(list);
      collection_index++;
      /* Now mutation begins again: */
      mutation_start_time = epsilongc_get_current_time();
    } // if
  } // for
  } // while
  printf("Done.\n");
  
  //epsilongc_page_t page = epsilongc_make_page(kind);
  //epsilongc_destroy_page(page);
  //result = epsilongc_allocate_from(&a);
  printf("Still alive: terminating...\n");
  return 0;
}
