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


#include "global_structures.h"
#include "epsilongc_threads.h"

#ifdef ENABLE_ASSERTIONS
/* A list containing all the currently existing (and already initialized) pages: */
struct epsilongc_page_list epsilongc_all_pages;
#endif // #ifdef ENABLE_ASSERTIONS

/* The number of processors to use for collection: */
epsilongc_integer_t epsilongc_processors_no = 0; // 0 means "not initialized yet"

epsilongc_mutex_t epsilongc_pages_to_be_swept_mutex = NULL;
epsilongc_mutex_t epsilongc_empty_pages_mutex = NULL;
epsilongc_mutex_t epsilongc_all_allocators_with_a_page_mutex = NULL;

/* All the currently existing pools: */
struct epsilongc_pool_list epsilongc_pools;

/* All the currently existing empty pages, already removed from
   their pools: */
struct epsilongc_page_list epsilongc_empty_pages;

/* Current heap size information: */
epsilongc_integer_t epsilongc_pointers_no_in_the_worst_case;
epsilongc_integer_t epsilongc_objects_no_in_the_worst_case;

/* All the full pages to be swept, and the mutex protecting the list
   from concurrent access: */
volatile struct epsilongc_page_list epsilongc_pages_to_be_swept;

/* All the currently existing thread-local allocators: */
__thread struct epsilongc_allocator_list epsilongc_thread_local_allocators;

/* All the currently existing allocators, in all threads: */
struct epsilongc_allocator_list epsilongc_all_allocators;

/* All the currently existing allocators holding a page, in all threads: */
struct epsilongc_allocator_list epsilongc_all_allocators_with_a_page;

/* All the currently existing allocators holding a page, for the calling thread: */
__thread struct epsilongc_allocator_list epsilongc_thread_local_allocators_with_a_page;

epsilongc_mutex_t epsilongc_within_collection_mutex;
epsilongc_mutex_t epsilongc_global_mutex;
epsilongc_read_write_lock_t epsilongc_global_read_write_lock;

/* Mark stack block lists: */
struct epsilongc_stack_block_list epsilongc_full_stack_blocks;
struct epsilongc_stack_block_list epsilongc_nonempty_and_nonfull_stack_blocks;
struct epsilongc_stack_block_list epsilongc_empty_stack_blocks;

/* Mark stack block synchronization structures: */
epsilongc_mutex_t epsilongc_stack_block_mutex;
epsilongc_semaphore_t epsilongc_nonempty_stack_blocks_no_semaphore;

/* Other collector synchronization structures: */
epsilongc_semaphore_t epsilongc_marking_phase_semaphore;

epsilongc_mutex_t_ epsilongc_begin_to_sweep_mutex;
epsilongc_condition_t epsilongc_begin_to_sweep_condition;
epsilongc_semaphore_t epsilongc_sweeping_phase_semaphore;


/* The heap size in megabytes, or 0 if no fixed size is specified: */
epsilongc_unsigned_integer_t epsilongc_fixed_heap_size_in_bytes = 0;

void epsilongc_set_fixed_heap_size_in_megabytes(const epsilongc_unsigned_integer_t new_size_in_megs){
  epsilongc_fixed_heap_size_in_bytes = new_size_in_megs * 1024 * 1024;
}

/* Fix the heap size to the value of the environment variable, if any: */
static void epsilongc_set_heap_size(void){
  char *value_of_epsilongc_heap_size_as_string = getenv("EPSILONGC_HEAP_SIZE");
  if(value_of_epsilongc_heap_size_as_string != NULL){
    char *rest;
    const long value_of_epsilongc_heap_size =
      strtol(value_of_epsilongc_heap_size_as_string, &rest, 10);
    if(*rest == '\0'){
      epsilongc_set_fixed_heap_size_in_megabytes(value_of_epsilongc_heap_size);
      printf("Setting epsilongc's heap size to %liMb\n", value_of_epsilongc_heap_size);
    }
    else
      printf("WARNING: ignoring $EPSILONGC_HEAP_SIZE, which isn't a number (\"%s\").\n",
             value_of_epsilongc_heap_size_as_string);
  } // if EPSILONGC_HEAP_SIZE is set
}

/* Set the number of processors used by the collector, using autodetection or an
   environment variable, *iff* such number has not been set before; we
   currently don't allow to dynamically change the number of processors, as
   collector threads are created once and for all: */
static void epsilongc_set_processors_no(void){
  /* Do nothing if the number of processors has already been set: */
  if(epsilongc_processors_no != 0)
    return;
  
  /* We support parallel collection: let's first detect the number of processors: */
  // To do: if the processor is hyperthreaded and multicore then use just one thread
  // per core; if it's hyperthreaded and single-core then use just one core.
  const epsilongc_integer_t detected_processors_no =
    (epsilongc_integer_t)sysconf(_SC_NPROCESSORS_CONF);
  
  /* Look at the environment variable, and use it if it's valid: */
  char *value_of_epsilongc_processors_no_as_string = getenv("EPSILONGC_PROCESSORS_NO");
  if(value_of_epsilongc_processors_no_as_string != NULL){
    char *rest;
    const int value_of_epsilongc_processors_no =
      strtol(value_of_epsilongc_processors_no_as_string, &rest, 10);
    if(*rest == '\0'){
      if(value_of_epsilongc_processors_no > detected_processors_no){
        printf("WARNING: $EPSILONGC_PROCESSORS_NO, which is %i, is greater then the detected number of processors (%i).\n",
               (int)value_of_epsilongc_processors_no, (int)detected_processors_no);
      } // inner if
      epsilongc_processors_no = value_of_epsilongc_processors_no;
    }
    else
      printf("WARNING: ignoring $EPSILONGC_PROCESSORS_NO, which isn't a number (\"%s\").\n",
             value_of_epsilongc_processors_no_as_string);
  } // if
  
#ifdef ENABLE_PARALLEL_COLLECTION
  /* Use the autodetected value if we haven't yet set the number of processors: */
  if(epsilongc_processors_no == 0)
    epsilongc_processors_no = detected_processors_no;
  printf("Using %i processors for garbage collection.\n", (int)epsilongc_processors_no);
#else
  /* If we don't support parallel collection then we can't use more than
     one processor for collection: */
  if(epsilongc_processors_no > 1){
    printf("WARNING: the collector was configured for uniprocessors only;\n");
    printf("         using only ONE processor for collection instead of %i.\n",
           (int)epsilongc_processors_no);
  } // if
  printf("Using 1 processor for garbage collection [epsilongc wasn't compiled for parallel collection].\n");
  epsilongc_processors_no = 1;
  return;
#endif // #ifdef ENABLE_PARALLEL_COLLECTION
}

void epsilongc_initialize_global_structures(void){
  epsilongc_pointers_no_in_the_worst_case = 0;
  epsilongc_objects_no_in_the_worst_case = 0;
  
  epsilongc_set_processors_no();
  epsilongc_set_heap_size();
  epsilongc_all_allocators_with_a_page_mutex = epsilongc_make_mutex("all-allocators-with-a-page", 0); 
  epsilongc_within_collection_mutex = epsilongc_make_mutex("within-collection", 0);
  epsilongc_global_mutex = epsilongc_make_mutex("global", 0);
  epsilongc_global_read_write_lock = epsilongc_make_read_write_lock("global", 0);
  epsilongc_empty_pages_mutex = epsilongc_make_mutex("empty-pages", 0);
  assert(epsilongc_pages_to_be_swept_mutex == NULL);
  epsilongc_pages_to_be_swept_mutex = epsilongc_make_mutex("pages-to-be-swept", 0);

  epsilongc_stack_block_mutex = epsilongc_make_mutex("stack-block", 0);
  epsilongc_nonempty_stack_blocks_no_semaphore = epsilongc_make_semaphore("nonempty-stack-blocks-no", 0, 0);
  epsilongc_marking_phase_semaphore = epsilongc_make_semaphore("marking-phase", 0, 0);
  
  EPSILONGC_INITIALIZE_LIST(stack_blocks, &epsilongc_empty_stack_blocks);
  EPSILONGC_INITIALIZE_LIST(stack_blocks, &epsilongc_full_stack_blocks);
  EPSILONGC_INITIALIZE_LIST(stack_blocks, &epsilongc_nonempty_and_nonfull_stack_blocks);

#ifdef ENABLE_DEFERRED_SWEEP
  /* Sweep statistics need to be zeroed even *before* collection if we
     adopt deferred sweeping: */
  epsilongc_initialize_sweep_statistics(&epsilongc_sweep_statistics);
#endif // #ifdef ENABLE_DEFERRED_SWEEP

  /* Make the global list of pools: */
  EPSILONGC_INITIALIZE_LIST(pools, &epsilongc_pools);
  
  /* Make the list of empty pages: */
  EPSILONGC_INITIALIZE_LIST(pages, &epsilongc_empty_pages);
  
  /* Make the list of pages to be swept, and the mutex: */
  EPSILONGC_INITIALIZE_LIST(pages, &epsilongc_pages_to_be_swept);

#ifdef ENABLE_ASSERTIONS
  /* Make the list of all pages: */
  EPSILONGC_INITIALIZE_LIST(pages, &epsilongc_all_pages);
#endif // #ifdef ENABLE_ASSERTIONS

  /* Make the global allocator lists: */
  EPSILONGC_INITIALIZE_LIST(allocators, &epsilongc_all_allocators);
  EPSILONGC_INITIALIZE_LIST(allocators, &epsilongc_all_allocators_with_a_page);

  /* Make sweeping synchronization structures: */
  epsilongc_begin_to_sweep_mutex = epsilongc_make_mutex_("begin-to-sweep", 0);
  epsilongc_begin_to_sweep_condition = epsilongc_make_condition("begin-to-sweep", 0);
  epsilongc_sweeping_phase_semaphore = epsilongc_make_semaphore("sweeping-phase", 0, 0);
}

void epsilongc_finalize_global_structures(void){
  /* Destroy empty pages (not the list, which is not heap-allocated): */
  EPSILONGC_DESTROY_ELEMENTS_OF_LIST(pages, list, epsilongc_page_t,
                                     &epsilongc_empty_pages,
                                     epsilongc_destroy_page_and_remove_it_from_global_structures);
  
  /* Destroy all pools (not the list, which isn't heap-allocated): */
  EPSILONGC_DESTROY_ELEMENTS_OF_LIST(pools, pools, epsilongc_pool_t,
                                     &epsilongc_pools, epsilongc_destroy_pool);

  /* Destroy all pools (not the list, which isn't heap-allocated): */
  EPSILONGC_DESTROY_ELEMENTS_OF_LIST(pages, list, epsilongc_page_t,
                                     &epsilongc_pages_to_be_swept,
                                     epsilongc_destroy_page_and_remove_it_from_global_structures);
  
  /* Finalize stack block lists: */
  const epsilongc_integer_t how_many_stack_blocks_we_are_removing =
    EPSILONGC_LENGTH_OF_LIST(stack_blocks, &epsilongc_empty_stack_blocks) +
    EPSILONGC_LENGTH_OF_LIST(stack_blocks, &epsilongc_full_stack_blocks) +
    EPSILONGC_LENGTH_OF_LIST(stack_blocks, &epsilongc_nonempty_and_nonfull_stack_blocks);
  epsilongc_stack_blocks_no -= how_many_stack_blocks_we_are_removing;
  EPSILONGC_DESTROY_ELEMENTS_OF_LIST(stack_blocks, stack_blocks, epsilongc_stack_block_t,
                                     &epsilongc_empty_stack_blocks,
                                     epsilongc_destroy_stack_block_without_removing_it_from_global_structures);
  EPSILONGC_DESTROY_ELEMENTS_OF_LIST(stack_blocks, stack_blocks, epsilongc_stack_block_t,
                                     &epsilongc_full_stack_blocks,
                                     epsilongc_destroy_stack_block_without_removing_it_from_global_structures);
  EPSILONGC_DESTROY_ELEMENTS_OF_LIST(stack_blocks, stack_blocks, epsilongc_stack_block_t,
                                     &epsilongc_nonempty_and_nonfull_stack_blocks,
                                     epsilongc_destroy_stack_block_without_removing_it_from_global_structures);
  
  /* Destroy the mutex used for the list of pages to be swept (there's no need
     to destroy the list which is not heap-allocated and should be empty now): */
  EPSILONGC_FINALIZE_LIST(pages, &epsilongc_pages_to_be_swept);

  /* Detach the global allocator list elements (don't destroy the allocators themselves;
     they should not be heap-allocated, for access efficiency; and not the
     list itself, which is not heap-allocated). Threre's no need to finalize
     allocators here, as each thread should independently finalze its own
     allocators: */
  EPSILONGC_DETACH_ELEMENTS_OF_LIST(allocators, all_allocators, epsilongc_allocator_t, &epsilongc_all_allocators);
  EPSILONGC_DETACH_ELEMENTS_OF_LIST(allocators, all_allocators_with_a_page, epsilongc_allocator_t, &epsilongc_all_allocators_with_a_page);
  
#ifdef ENABLE_ASSERTIONS
  /* Finalize the list of all pages; in particular, make sure it's empty: */
  EPSILONGC_FINALIZE_LIST(pages, &epsilongc_all_pages);
#endif // #ifdef ENABLE_ASSERTIONS
  epsilongc_pointers_no_in_the_worst_case = 0;
  epsilongc_objects_no_in_the_worst_case = 0;

  /* Destroy mutexes, read/write locks, spinlocks, semaphores and conditions: */
  epsilongc_destroy_mutex(epsilongc_within_collection_mutex);
  epsilongc_destroy_mutex(epsilongc_global_mutex);
  epsilongc_destroy_read_write_lock(epsilongc_global_read_write_lock);
  epsilongc_destroy_mutex(epsilongc_empty_pages_mutex);
  assert(epsilongc_pages_to_be_swept_mutex != NULL);
  epsilongc_destroy_mutex(epsilongc_pages_to_be_swept_mutex);
  epsilongc_pages_to_be_swept_mutex = NULL;
  epsilongc_destroy_mutex(epsilongc_all_allocators_with_a_page_mutex);
  
  epsilongc_destroy_mutex(epsilongc_stack_block_mutex);
  epsilongc_destroy_semaphore(epsilongc_nonempty_stack_blocks_no_semaphore);
  epsilongc_destroy_semaphore(epsilongc_marking_phase_semaphore);

  epsilongc_destroy_mutex_(epsilongc_begin_to_sweep_mutex);
  epsilongc_destroy_condition(epsilongc_begin_to_sweep_condition);
  epsilongc_destroy_semaphore(epsilongc_sweeping_phase_semaphore);
}
