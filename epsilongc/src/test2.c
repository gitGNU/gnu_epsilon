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
#include "myrand.h"
#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <pthread.h>

#define THREAD_NO 8//8//16//8//2//8//8//8//50//8 //50

epsilongc_semaphore_t alive_workers_semaphore;

//#define USE_BOEHM_GC

#ifdef USE_BOEHM_GC
#define GC_THREADS 1
//#define GC_REDIRECT_TO_LOCAL // not needed for newer versions of Boehm GC
#include <gc/gc.h>
#endif // #ifdef USE_BOEHM_GC

static epsilongc_pool_t cons_pool __attribute__((unused));
static epsilongc_pool_t cons_pool2 __attribute__((unused));
static epsilongc_pool_t useless_pool1 __attribute__((unused));
static epsilongc_pool_t useless_pool2 __attribute__((unused));
static __thread struct epsilongc_allocator cons_allocator __attribute__((unused));
static __thread struct epsilongc_allocator cons_allocator2 __attribute__((unused));
static __thread struct epsilongc_allocator useless_allocator1 __attribute__((unused));
static __thread struct epsilongc_allocator useless_allocator2 __attribute__((unused));

void cons_tracer(epsilongc_word_t cons){
  /* Trace the car: */
  //epsilongc_trace_pointer_if_valid(((epsilongc_word_t*)cons)[0]);
  /* Trace the cdr: */
  epsilongc_trace_pointer_if_valid(((epsilongc_word_t*)cons)[1]);
}

void print_list(epsilongc_word_t list){
  if(list == NULL)
    printf("\n");
  else{
    printf("%i ", (int)((epsilongc_integer_t*)list)[0]);
    print_list(((epsilongc_word_t*)list)[1]);
  }
}

inline epsilongc_word_t cons(epsilongc_integer_t i,
                             epsilongc_pointer_t list)
  __attribute__((malloc));
inline epsilongc_word_t cons(epsilongc_integer_t i,
                             epsilongc_pointer_t list){
#ifdef USE_BOEHM_GC
  GC_thr_init();
  epsilongc_word_t result = GC_malloc(sizeof(epsilongc_word_t) * 2);
#else
  //asm("@@@@@@@@@@@@@@@1\n");
  epsilongc_word_t result;
  if(epsilongc_rand() % 100 < 990){
    if(epsilongc_rand() % 100 == -1)
      result = epsilongc_allocate_large_object_conservative(2, 2, 42, NULL);
    else
      result = epsilongc_allocate_from(&cons_allocator);
    //result = epsilongc_allocate_words_conservative(2);
    //result = epsilongc_allocate_bytes_conservative(sizeof(epsilongc_word_t) * 2);
  }
  else
    result = epsilongc_allocate_from(&cons_allocator2);
#endif // #ifdef USE_BOEHM_GC
  ((epsilongc_integer_t*)result)[0] = i;
  ((epsilongc_word_t*)result)[1] = list;
  //asm("@@@@@@@@@@@@@@@2\n");
  return result;
}

void dump_list(epsilongc_word_t list){
  while(list != NULL){
    if(list == (epsilongc_word_t)0xdead){
      printf("0xdead\n");
      return;
    }
    epsilongc_unsigned_integer_t car = ((epsilongc_unsigned_integer_t*)list)[0];
    epsilongc_word_t cdr = ((epsilongc_word_t*)list)[1];  
    epsilongc_page_t page = epsilongc_candidate_pointer_to_candidate_page(list);
    printf("%lu [%lx] (from page %p [%lu, %lu]: (%li . %lu [%lx])\n",
           (long)list,
           (long)list,
           page,
           (unsigned long)((char*)page->first_object),
           (unsigned long)(((char*)page->after_last_object) - 1),
           (long)car, 
           (long)cdr, 
           (long)cdr);
    list = cdr;
  } // while
}

bool check_consistency_starting_from(epsilongc_word_t list,
                                     epsilongc_unsigned_integer_t expected_car){
  while(list != NULL){
    if(list == (epsilongc_word_t)0xdead){
      printf("CONSISTENCY CHECK FAILED!\nThe list is 0xdead\n");
      return false;
    } // if
    epsilongc_unsigned_integer_t car = ((epsilongc_unsigned_integer_t*)list)[0];
    epsilongc_word_t cdr = ((epsilongc_word_t*)list)[1];  
    if(car != expected_car){
      printf("CONSISTENCY CHECK FAILED!\nThe car is %lx [%li] instead of %lx [%li]\n",
             (unsigned long) car,
             (unsigned long) car,
             (long int)expected_car,
             (long int)expected_car);
      return false;
    } // if
    list = cdr;
    expected_car --;
  }
  return true;
}
void check_consistency(epsilongc_word_t list){
  //printf("Making a consistency check...\n");
  if(list == (epsilongc_word_t)0xdead){
    printf("Consistency check FAILED (0xdead at the beginning).\n");
    exit(EXIT_FAILURE);
  }
  if(list == NULL){
    //printf("Consistency check passed.\n");
    return;
  }
  epsilongc_unsigned_integer_t car = ((epsilongc_unsigned_integer_t*)list)[0];
  epsilongc_word_t cdr = ((epsilongc_word_t*)list)[1];
  if(! check_consistency_starting_from(cdr, car - (epsilongc_unsigned_integer_t)1u)){
    dump_list(list);
    printf("CONSISTENCY CHECK FAILED!\n");
    exit(EXIT_FAILURE);
  }
  else{
    //printf("Consistency check passed.\n");
  }
}

void* worker_thread_function(void* useless){
  epsilongc_seed();
  printf("%s: Ok-1\n", epsilongc_calling_thread_name());

#ifdef USE_BOEHM_GC
  struct GC_stack_base stack_base;
  if(GC_get_stack_base(&stack_base) != GC_SUCCESS)
    epsilongc_fatal("GC_get_stack_base() failed");
  if(GC_register_my_thread(&stack_base) != GC_SUCCESS)
    epsilongc_fatal("GC_register_my_thread() failed");
  printf("%s: thread registered to the Boehm collector\n", epsilongc_calling_thread_name());
#endif // #ifdef USE_BOEHM_GC
  
#define LIST_NO 1
  epsilongc_word_t lists[LIST_NO];
  int j;
  for(j = 0; j < LIST_NO; j++)
    lists[j] = NULL;
  printf("%s: Ok-2\n", epsilongc_calling_thread_name());
#ifndef USE_BOEHM_GC
  epsilongc_register_user_defined_thread_local_roots(lists, LIST_NO);
#endif // #ifndef USE_BOEHM_GC

  epsilongc_integer_t i = 0;
  while(true){
#ifndef USE_BOEHM_GC
    /* if((epsilongc_rand() % 100) == -1){ */
    /*   //printf("Making a large object: begin\n"); */
    /*   //printf("L"); */
    /*   epsilongc_allocate_large_object_leaf(rand() % 100000, 1, 42, NULL); */
    /*   //printf("Making a large object: end\n"); */
    /* } // if */
    /* //printf("%s: i is now %li\n", epsilongc_calling_thread_name(), (long)i); fflush(stdout); */
    /* if((epsilongc_rand() % 1000) == -1) */
    /*   epsilongc_release_pages_from_thread_local_allocators(); */
#endif // #ifndef USE_BOEHM_GC
    int k;
    const int garbage_cells_no = (epsilongc_rand() % 10) + 1;
    if((epsilongc_rand() % 100) == -1)
      for(k = 0; k < garbage_cells_no; k++)
        cons(k, NULL);
    int j;
    for(j = 0; j < LIST_NO; j++){
      epsilongc_word_t new_cons = cons(i, lists[j]);
      lists[j] = new_cons;
    } // for
    if((epsilongc_rand() % 100) == 0){
    //if((epsilongc_rand() % 1000000) == 0){
      for(j = 0; j < LIST_NO; j++)
        lists[j] = NULL;
    }
    if((epsilongc_rand() % 100) == 0){
    //if((epsilongc_rand() % 10000) == 0){
      /* Check that the list structure is consistent (i.e., that we
         haven't destroyed alive objects): */
      //printf("%s: Checking consistency\n", epsilongc_calling_thread_name());
      int j;
      for(j = 0; j < LIST_NO; j++)
        check_consistency(lists[j]);
      //printf("Ok.\n");
    } // if
    i++;
    //if(i == (500000000))
    //if(i == (5000000))
    //if(i == (50000000))
    //if(i == (300000000L))
    if(i == (150000000))
      break;
  } // while
  
  printf("%s: Ok-3: SUCCESS\n", epsilongc_calling_thread_name());
  epsilongc_v_semaphore(alive_workers_semaphore);
  return "success";
}

void make_allocators(void){
  epsilongc_initialize_allocator(&cons_allocator, cons_pool);
  //epsilongc_initialize_allocator(&cons_allocator2, cons_pool2);
  //epsilongc_initialize_allocator(&useless_allocator1, useless_pool1);
  //epsilongc_initialize_allocator(&useless_allocator2, useless_pool2);
}

int main(void){
  epsilongc_seed();
#ifdef USE_BOEHM_GC
  printf("Initializing Boehm's GC...\n");
  GC_init();
  //GC_enable_incremental();
  printf("Initialized Boehm's GC.\n");
  epsilongc_initialize_garbage_collection();
#else
  epsilongc_initialize_garbage_collection();
  epsilongc_kind_t cons_kind =
    epsilongc_make_kind(2, 2,
                        1,
                        42,
                        NULL,
                        cons_tracer,
                        NULL);
  cons_pool = epsilongc_make_pool("cons", cons_kind);
  cons_pool2 = epsilongc_make_pool("cons2", cons_kind);

  //useless_pool1 = epsilongc_make_pool("useless1", cons_kind);
  //useless_pool2 = epsilongc_make_pool("useless2", cons_kind);
  //epsilongc_disable_garbage_collection();
  epsilongc_set_verbose_collection(true);
#endif // #ifdef USE_BOEHM_GC
  
  ///* Also do work in the main thread: */
  //epsilongc_register_the_calling_thread_as_a_mutator("main", 42, make_allocators);

  alive_workers_semaphore = epsilongc_make_semaphore("alive-workers", 0, 0);
  epsilongc_integer_t i;
  for(i = 0; i < THREAD_NO; i++)
    epsilongc_make_mutator_thread("worker", (epsilongc_integer_t)i,
                                  make_allocators,
                                  worker_thread_function,
                                  NULL);
  //worker_thread_function(NULL);
  //epsilongc_unregister_the_calling_thread_as_a_mutator();
  
  /* Perform the poor man's join: */
  for(i = 0; i < THREAD_NO; i++)
    epsilongc_p_semaphore(alive_workers_semaphore);

  /* We're done: */
  epsilongc_finalize_garbage_collection();
  printf("\n* Main thread. Exiting with SUCCESS.\n\n");
  return 0;
}
