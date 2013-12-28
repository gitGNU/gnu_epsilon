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


#ifndef EPSILONGC_POOL_H_
#define EPSILONGC_POOL_H_

#include "epsilongc_types.h"
#include "kind.h"
#include "page.h"
#include "heuristics.h"
//#include "../include/doubly_linked_list.h>
#include "doubly_linked_list_macros.h"
#include "epsilongc_threads.h"
#include "global_structures.h"

/* A pool can be considered as both a generator of non-full pages and a
   sink of full pages, all of the same given kind. If the pool is requested
   a page when it has no non-full pages, it transparently creates a new one,
   or triggers a collection to free space. */
//typedef struct epsilongc_pool* epsilongc_pool_t;
struct epsilongc_pool{
  /* The kind of all the objects in the pages in this pool: */
  epsilongc_kind_t kind;
  
  /* The pool name: this is only useful for debugging: */
  char *name;
  
  /* A list of pages which may *possibly* have free space: */
  EPSILONGC_LIST_FIELDS(possibly_nonfull_pages, epsilongc_page_t)
  //epsilongc_doubly_linked_list_t possibly_nonfull_pages;
  
#ifdef ENABLE_DEFERRED_SWEEP
  /* A list of pages which are *definitely* full; this is only used
     when deferred sweep is enabled; otherwise we use a global list of
     full pages to be swept: */
  EPSILONGC_LIST_FIELDS(full_pages, epsilongc_page_t)
  //epsilongc_doubly_linked_list_t full_pages;

  /* A list of pages which are *definitely* not completely full and
     can just be reused *without* sweeping them. Pages are added to
     this list by epsilongc_release_pages_from_thread_local_allocators().
     This list is the first one to be checked when a pool needs to
     give a page to an allocator. 
     Pages remaining in this list at collection time can be re-added to
     the list of possibly-non-full pages *after* marking (they should be
     completely unmarked at the time of their insertion into this list). */
  EPSILONGC_LIST_FIELDS(nonfull_not_to_be_swept_pages, epsilongc_page_t)
#endif // #ifdef ENABLE_DEFERRED_SWEEP

  /* Each pool belongs to a global list of pools: */
  EPSILONGC_LIST_ELEMENT_FIELDS(pools, epsilongc_pool_t)

  ///* The element for this pool in the pool list: */
  //epsilongc_doubly_linked_list_element_t element_in_the_pool_list;
  
  /* The mutex protecting this pool's data structures from concurrent access;
     this is used for structures *within this pool*, and not for global
     structures: */
  epsilongc_mutex_t mutex;
}; // struct

/* Initialize pool support. This must be called once at the beginning,
   before initializing the first pool: */
void epsilongc_initialize_pool_support(void);

/* Finalize pool support: */
void epsilongc_finalize_pool_support(void);

/* Return a new pool for objects of the given kind. This should be
   called once per pool, before using the pool for the first time. */
epsilongc_pool_t epsilongc_make_pool(char *kind_name,
                                     epsilongc_kind_t kind)
  __attribute__((malloc));

/* Destroy and deallocate the given pool, and all its pages. */
void epsilongc_destroy_pool(epsilongc_pool_t pool);

/* Return the given page to the given pool, as a full page. The pool is
   destructively updated: */
//void epsilongc_add_full_page_to_pool(epsilongc_page_t page,
//                                     epsilongc_pool_t pool);

/* Take a possibly not full page from the given pool. The pool is
   destructively updated: */
epsilongc_page_t epsilongc_take_page_if_possible(epsilongc_pool_t pool);

/* /\* Sweep all the full pages in the pool, updating the given sweep */
/*    statistics: *\/ */
/* void epsilongc_sweep_pool(epsilongc_pool_t pool, */
/*                           epsilongc_sweep_statistics_t statistics); */

/* /\* Perform a sweep of *all* the pools, updating the given statistics: *\/ */
/* void epsilongc_sweep_all_pools(epsilongc_sweep_statistics_t statistics); */

/* Destroy some unused pages, with the proper synchronization. This assumes that
   the global read-write lock is locked for reading (i.e., we don't want a 
   collection to interrupt this). This does nothing if empty-page destruction
   is disabled: */
void epsilongc_destroy_some_unused_pages(void);

/* Create a new page for the given pool (initializing it out of the critical section) and
   return it. This is conceived to be used only by epsilongc_take_page_if_possible(), as
   the use of synchronization easily shows: */
epsilongc_page_t epsilongc_make_a_new_page_for(epsilongc_pool_t pool);

#endif // #ifndef EPSILONGC_POOL_H_
