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


#ifndef EPSILONGC_GLOBAL_STRUCTURES_H_
#define EPSILONGC_GLOBAL_STRUCTURES_H_

#include "epsilongc_types.h"
#include "doubly_linked_list_macros.h"
#include "page.h"
#include "pool.h"
#include "allocator.h"
#include "trace.h"
#include "large.h"
#include <stdbool.h>

/* Initialize global structures: */
void epsilongc_initialize_global_structures(void);

/* Finalize global structures: */
void epsilongc_finalize_global_structures(void);

/* The heap size in bytes, or 0 if no fixed size is specified: */
extern epsilongc_unsigned_integer_t epsilongc_fixed_heap_size_in_bytes;

/* Change the heap size: */
void epsilongc_set_fixed_heap_size_in_megabytes(const epsilongc_unsigned_integer_t new_size_in_megs);

/* A type for lists of pages: */
typedef struct epsilongc_page_list* epsilongc_page_list_t;
struct epsilongc_page_list{
  EPSILONGC_LIST_FIELDS(pages, epsilongc_page_t)
}; // struct

/* A type for lists of large objects (we use header pointers, not payload pointers): */
typedef struct epsilongc_large_object_header_list* epsilongc_large_object_header_list_t;
struct epsilongc_large_object_header_list{
  EPSILONGC_LIST_FIELDS(large_object_headers, epsilongc_large_object_header_t)
}; // struct

/* A type for lists of pools: */
typedef struct epsilongc_pool_list* epsilongc_pool_list_t;
struct epsilongc_pool_list{
  EPSILONGC_LIST_FIELDS(pools, epsilongc_pool_t)
}; // struct

/* A type for allocator lists: */
typedef struct epsilongc_allocator_list* epsilongc_allocator_list_t;
struct epsilongc_allocator_list{
  EPSILONGC_LIST_FIELDS(allocators, epsilongc_allocator_t)
}; // struct

/* A type for stack block lists: */
typedef struct epsilongc_stack_block_list* epsilongc_stack_block_list_t;
struct epsilongc_stack_block_list{
  EPSILONGC_LIST_FIELDS(stack_blocks, epsilongc_stack_block_t)
}; // struct

/* Here come several global data structures, with their mutexes: */

/* ====== Pages: ======= */

#ifdef ENABLE_ASSERTIONS
/* The list of all pages. Protected by the *global* mutex. */
extern struct epsilongc_page_list epsilongc_all_pages;
#endif // #ifdef ENABLE_ASSERTIONS

/* All the currently existing empty pages, already removed from the
   lists in their pools: */
extern struct epsilongc_page_list epsilongc_empty_pages;
extern epsilongc_mutex_t epsilongc_empty_pages_mutex;

/* All full pages to be swept: */
extern volatile struct epsilongc_page_list epsilongc_pages_to_be_swept; // To do: does this need to be volatile?
extern epsilongc_mutex_t epsilongc_pages_to_be_swept_mutex;

/* The number of pointers and objects in the whole heap in the worst
   case; these are used for computing the mark stack size. Protected by
   the global mutex: */
extern epsilongc_integer_t epsilongc_pointers_no_in_the_worst_case;
extern epsilongc_integer_t epsilongc_objects_no_in_the_worst_case;

/* ====== Allocators: ======= */

/* All the currently existing allocators, in all threads. Protected by the global
   mutex: */
extern struct epsilongc_allocator_list epsilongc_all_allocators;

/* All the allocators which are currently holding a page, in all threads: */
extern struct epsilongc_allocator_list epsilongc_all_allocators_with_a_page;
extern epsilongc_mutex_t epsilongc_all_allocators_with_a_page_mutex;

/* A list holding the thread-local allocators which are currently holding a page;
   we don't need any mutex for this, as each mutator thread only touches its own
   list: */
extern __thread struct epsilongc_allocator_list epsilongc_thread_local_allocators_with_a_page;

/* All the currently existing thread-local allocators, for this thread. We
   don't need any mutex for this, as each mutator thread only touches its own
   list: */
extern __thread struct epsilongc_allocator_list epsilongc_thread_local_allocators;

/* ====== Pools: ======= */

/* All the currently existing pools. Protected by the global mutex: */
extern struct epsilongc_pool_list epsilongc_pools;

/* ====== Mark-stack blocks: ======= */

/* The global list of full stack blocks which are not currently owned by any
   thread. This is protected by the global mutex: */
extern struct epsilongc_stack_block_list epsilongc_full_stack_blocks;

/* The global list of non-empty and non-full stack blocks which are not currently
   owned by any thread. This is protected by the global mutex: */
extern struct epsilongc_stack_block_list epsilongc_nonempty_and_nonfull_stack_blocks;

/* The global list of empty stack blocks which are not currently owned by
   any thread. This is protected by the global mutex: */
extern struct epsilongc_stack_block_list epsilongc_empty_stack_blocks;

/* ====== Other global structures: ======= */

extern struct epsilongc_large_object_header_list epsilongc_alive_large_objects;
extern struct epsilongc_large_object_header_list epsilongc_condemned_large_objects;

/* How many processors to use *for collection* (it's always possible to use an
   unbounded number of processors for mutation): */
extern epsilongc_integer_t epsilongc_processors_no;

/* Sweep statistics for the last collection (or the current collection, if
   deferred sweep is enabled). This is protected by the global mutex: */
extern struct epsilongc_sweep_statistics epsilongc_sweep_statistics;

/* The number of performed collections till now. Protected by the global
   mutex: */
extern volatile epsilongc_unsigned_integer_t epsilongc_last_collection_index; // To do: I'm not completely sure that we need it to be volatile

/* The global mutex: */
extern epsilongc_mutex_t epsilongc_global_mutex;

/* The mutex used to syncrhonize threads during collection: */
extern epsilongc_mutex_t epsilongc_within_collection_mutex;

/* Synchronization structures for synchronizing mark-stack block access: */
extern epsilongc_mutex_t epsilongc_stack_block_mutex;
extern epsilongc_semaphore_t epsilongc_nonempty_stack_blocks_no_semaphore;

/* We use a read/write lock, acquiring it for writing during collection and for reading
   during other operations which we don't want to happen concurrently with collection: */
extern epsilongc_read_write_lock_t epsilongc_global_read_write_lock;

/* This semaphore is used to synchronize with marking threads, by V'ing it
   at the beginning of the marking phase, and P'ing it when waiting for marker
   threads to finish. When the last marking thread notices that the current
   marking phase is over it simply V's this same semaphore, thus waking up
   the main collector thread: */
extern epsilongc_semaphore_t epsilongc_marking_phase_semaphore;

/* These are synchronization structures for working with sweeper threads: */
extern epsilongc_mutex_t_ epsilongc_begin_to_sweep_mutex;
extern epsilongc_condition_t epsilongc_begin_to_sweep_condition;
extern epsilongc_semaphore_t epsilongc_sweeping_phase_semaphore;

/* ====== Locking convention: ======= */

/* An way to avoid deadlocks is to lock [unlock] mutexes always in the same
   order; it's not required to always lock [unlock] *all* mutexes, but all the
   needed mutexes should always be acquired in the following order:
   
   epsilongc_lock_read_write_lock_for_XXXXXX(global_read_write_lock);
   epsilongc_lock_mutex(epsilongc_all_allocators_with_a_page_mutex);
   epsilongc_lock_mutex(pool->mutex); // for each needed pool, in the order of epsilongc_pools
   epsilongc_lock_mutex(epsilongc_empty_pages_mutex);
   epsilongc_lock_mutex(epsilongc_pages_to_be_swept_mutex);
   epsilongc_lock_mutex(epsilongc_global_mutex);
   epsilongc_lock_mutex(epsilongc_within_collection_mutex);
   
   The proper order for unlocking is exactly the reverse:

   To do: this should always be kept up-to-date, as it's good for cut/paste. */

#endif // #ifndef EPSILONGC_GLOBAL_STRUCTURES_H_
