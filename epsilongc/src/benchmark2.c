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


#include "epsilongc.h"
#include <stdio.h>
#include <string.h>
#include <malloc.h>

//#define USE_MALLOC

#define THREADS_NO 8//8//8//8//50//8 //50
//#define N 27000000 // this is the maximum I can allocate with malloc() without trashing, on my machine with 8 threads
#define N 500000000

#define POOLS_NO 1//THREADS_NO

static epsilongc_pool_t cons_pools[POOLS_NO];
static __thread struct epsilongc_allocator cons_allocator;
epsilongc_semaphore_t alive_workers_semaphore;

epsilongc_mutex_t mutex;

void make_allocators(void){
  static int i = 0;
  epsilongc_lock_mutex(mutex);
  const int index = i % POOLS_NO;
  epsilongc_initialize_allocator(&cons_allocator, cons_pools[index]);
  i++;
  printf("Using the %i-th pool for the allocator %p\n", index, &cons_allocator);
  epsilongc_unlock_mutex(mutex);
}

void cons_tracer(epsilongc_word_t cons){
  /* Trace the car: */
  //epsilongc_trace_pointer_if_valid(((epsilongc_word_t*)cons)[0]);
  /* Trace the cdr: */
  epsilongc_trace_pointer_if_valid(((epsilongc_word_t*)cons)[1]);
}

inline epsilongc_word_t cons(epsilongc_integer_t i,
                             epsilongc_pointer_t list)
  __attribute__((malloc));
inline epsilongc_word_t cons(epsilongc_integer_t i,
                             epsilongc_pointer_t list){
#ifdef USE_MALLOC
  epsilongc_word_t result = (epsilongc_word_t)malloc(sizeof(epsilongc_word_t) * 2);
#else
  epsilongc_word_t result = epsilongc_allocate_from(&cons_allocator);
#endif // #ifdef USE_MALLOC
  ((epsilongc_integer_t*)result)[0] = i;
  ((epsilongc_word_t*)result)[1] = list;
  return result;
}

void* worker_thread_function(void* useless){
  epsilongc_integer_t i;
  for(i = 0; i < N; i++)
    cons(0, NULL);
  ///
  epsilongc_v_semaphore(alive_workers_semaphore);
  return NULL;
}

int main(void){
  epsilongc_initialize_garbage_collection();

  mutex = epsilongc_make_mutex("mutex", 0);

  epsilongc_kind_t cons_kind =
    epsilongc_make_kind(2, 2,
                        1,
                        42,
                        NULL,
                        cons_tracer,
                        NULL);

  epsilongc_integer_t i;
  for(i = 0; i < POOLS_NO; i++)
    cons_pools[i] = epsilongc_make_pool("cons", cons_kind);
  alive_workers_semaphore = epsilongc_make_semaphore("alive-workers", 0, 0);
  for(i = 0; i < THREADS_NO; i++)
    epsilongc_make_mutator_thread("worker", (epsilongc_integer_t)i,
                                  make_allocators,
                                  worker_thread_function,
                                  NULL);
  for(i = 0; i < THREADS_NO; i++)
    epsilongc_p_semaphore(alive_workers_semaphore);
  //sleep(1);
  //epsilongc_finalize_garbage_collection();
  /* We're done: */
  printf("\n* Main thread. Exiting with SUCCESS.\n\n");
  return 0;
}
