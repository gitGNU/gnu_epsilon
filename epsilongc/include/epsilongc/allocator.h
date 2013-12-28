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


#ifndef EPSILONGC_ALLOCATOR_H_
#define EPSILONGC_ALLOCATOR_H_

#include "epsilongc_types.h"
#include "kind.h"
#include "page.h"
#include "pool.h"
//#include "../include/doubly_linked_list.h>
#include "doubly_linked_list_macros.h"

/* This struct contains the needed data structures to allocate objects of a
   given kind, from a given pool. A thread-local instance of it should be
   available as a global, for each kind: */
//typedef struct epsilongc_allocator* epsilongc_allocator_t;
struct epsilongc_allocator{
  /* Pointer to the next free word in the current page (or NULL, if the current
     page is full); this is the head of the free list, whose up-to-date version
     is held here instead of in the page itself to reduce the number of
     indirections at allocation time. This part is *really* performance-crictical: */
  epsilongc_word_t *next_free_object;
  
  /* The current page, or NULL if the allocator currently hold no page: */
  epsilongc_page_t current_page;

  /* The pool from which we obtain pages: */
  epsilongc_pool_t pool;

  /* Each allocator belongs to a list of thread-local allocators: */
  EPSILONGC_LIST_ELEMENT_FIELDS(thread_local_allocators, epsilongc_allocator_t)

  /* Each allocator also belongs to the global list of all allocators: */
  EPSILONGC_LIST_ELEMENT_FIELDS(all_allocators, epsilongc_allocator_t)
  
  /* Each allocator belongs to a global list and another thread-local list, when
     it holds a page: */
  EPSILONGC_LIST_ELEMENT_FIELDS(all_allocators_with_a_page, epsilongc_allocator_t)
  EPSILONGC_LIST_ELEMENT_FIELDS(thread_local_allocators_with_a_page, epsilongc_allocator_t)
}; // struct

/* Initialize (global, not thread-local) allocator support. This must be called
   once at initialization time: */
void epsilongc_initialize_allocator_support(void);

/* Finalize (global, not thread-local) allocator support. This must be called
   once at finalization time: */
void epsilongc_finalize_allocator_support(void);

/* Initialize thread-local allocator support. This must be called once per thread
   before initializing the first thread_local_allocator: */
void epsilongc_initialize_thread_local_allocator_support(void);

/* Finalize thread-local allocator support. This must be called before thread
   exit: */
void epsilongc_finalize_thread_local_allocator_support(void);

/* Initialize the given allocator so that it takes pages from the given pool.
   This should be called once per allocator, before using the allocator for
   the first time; each thread has distinct allocators, hence the
   initialization should be performed *once per thread*.
   The allocator itself is (obviously) *NOT* allocated by this function; it
   should be a thread-local global variable, not heap-allocated. */
void epsilongc_initialize_allocator(epsilongc_allocator_t allocator,
                                    epsilongc_pool_t pool);

/* Finalize the given allocator, returning the current page, if any, to a pool.
   The allocator itself is *NOT* de-allocated by this function. */
void epsilongc_finalize_allocator(epsilongc_allocator_t allocator);

/* /\* Move the page from the given allocator to its pool. The allocator is assumed */
/*    to currently *have* a page; the page next_free_object is updated before the */
/*    move, copying the current value from the allocator: *\/ */
/* void epsilongc_move_page_from_allocator(epsilongc_allocator_t allocator); */

/* Return the allocator's page (if any) to its pool: */
void epsilongc_move_page_if_any_from_allocator(epsilongc_allocator_t allocator);

/* Move a non-full page to the given allocator, from its pool. The allocator
   is assumed to have currently *no* page. The allocator next_free_pool is copied
   from the one in the page, so that we can use the allocator's copy, which is
   more efficient, for allocation: */
void epsilongc_move_page_to_allocator(epsilongc_allocator_t allocator);

/* Return the allocators' pages (if any) to their pools, for all existing alloctors
   (not only the ones owned by the calling thread): */
//void epsilongc_move_pages_if_any_from_all_allocators_to_pools_without_unpinning(void);

/* Return the current page (if any) of the given allocator to the appropriate pool;
   then take another page (possibly) with free space from it, assign it to the
   allocator, and update its next_free_object so that we can use it right away: */
void epsilongc_replace_page_with_appropriate_locking(struct epsilongc_allocator *allocator)
  __attribute__((noinline));

/* void epsilongc_sweep_all_allocators(epsilongc_sweep_statistics_t statistics); */

/* Release the pages held by all thread-local allocators (when they hold one).
   The needed synchronization is performed *within* this function: */
void epsilongc_release_pages_from_thread_local_allocators(void);

/* Add declarations for the functions which can be aggressively inlined (and also
   their definitions if aggressive inlining is enabled): */
#include "declarationstoinline.h"

#endif // #ifndef EPSILONGC_ALLOCATOR_H_
