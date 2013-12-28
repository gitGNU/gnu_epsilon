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
#include "pool.h"
#include "trace.h"
#include "malloc.h"
//#include "doubly_linked_list.h"
#include "doubly_linked_list_macros.h"
#include "heuristics.h"
#include "run_time_settings.h"
#include "epsilongc_threads.h"
#include "epsilongc.h"
#include "epsilongc_features.h"

// To do: synchronize the pool list (i.e. pool making and destruction), or
// add a comment saying that they must all be created before any allocation.

epsilongc_pool_t epsilongc_make_pool(char *kind_name, epsilongc_kind_t kind){
  epsilongc_pool_t pool =
    (epsilongc_pool_t)epsilongc_xmalloc(sizeof(struct epsilongc_pool));
  pool->kind = kind;
  pool->name = kind_name; // To do: shall I copy it? Probably so...
  
  /* Make the mutex: */
  pool->mutex = epsilongc_make_mutex(pool->name, 0);
  
  /* Make lists: */
  EPSILONGC_INITIALIZE_LIST(possibly_nonfull_pages, pool);
#ifdef ENABLE_DEFERRED_SWEEP
  EPSILONGC_INITIALIZE_LIST(full_pages, pool);
  EPSILONGC_INITIALIZE_LIST(nonfull_not_to_be_swept_pages, pool);
#endif // #ifdef ENABLE_DEFERRED_SWEEP
  
  /* Add this pool to the list of all pools: */
  EPSILONGC_APPEND_OBJECT_TO_LIST(pools, pools, epsilongc_pool_t,
                                  pool, &epsilongc_pools);
  return pool;
}

void epsilongc_destroy_pool(epsilongc_pool_t pool){
  /* Destroy possibly nonfull pages, and their list structure: */
  EPSILONGC_DESTROY_ELEMENTS_OF_LIST(possibly_nonfull_pages, list, epsilongc_page_t,
                                     pool, epsilongc_destroy_page_and_remove_it_from_global_structures);
  
#ifdef ENABLE_DEFERRED_SWEEP
  /* Destroy full pages, and their list structure: */
  EPSILONGC_DESTROY_ELEMENTS_OF_LIST(full_pages, list, epsilongc_page_t,
                                     pool, epsilongc_destroy_page_and_remove_it_from_global_structures);
  /* Destroy non-full not-to-be-swept pages, and their list structure: */
  EPSILONGC_DESTROY_ELEMENTS_OF_LIST(nonfull_not_to_be_swept_pages, list, epsilongc_page_t,
                                     pool, epsilongc_destroy_page_and_remove_it_from_global_structures);
#endif // #ifdef ENABLE_DEFERRED_SWEEP
  
  /* Detach this pool from the list of all pools: */
  EPSILONGC_DETACH_ELEMENT_FROM_LIST(pools, pools, epsilongc_pool_t,
                                     pool, &epsilongc_pools);
  
  /* Destroy the mutex: */
  epsilongc_destroy_mutex(pool->mutex);
  
  /* Free the structure holding everything together. Note that we *don't* free
     the kind: */
  free(pool);
}

epsilongc_page_t epsilongc_make_a_new_page_for(epsilongc_pool_t pool){
#ifdef ENABLE_DUMP_PAGE_INFORMATION
  printf("%s: about to make a new page for the pool %p\n", epsilongc_calling_thread_name(), pool); fflush(stdout);
#endif // #ifdef ENABLE_DUMP_PAGE_INFORMATION
  epsilongc_page_t page = epsilongc_make_page_and_add_it_to_global_structures(pool);
  
#ifdef ENABLE_DUMP_PAGE_INFORMATION
  printf("%s: made a new page for the pool %p: it's %p\n", epsilongc_calling_thread_name(), pool, page); fflush(stdout);
#endif // #ifdef ENABLE_DUMP_PAGE_INFORMATION
  return page;
}

/* We don't want to destroy empty pages too often, as we wish to
   avoid destroying and then allocating again, which requires kernel
   support and is hard to parallelize. Doing this way instead makes
   the heap shrink *slowly* when the working set becomes smaller: */
inline static bool shall_we_destroy_an_empty_page(void){
#ifdef ENABLE_DESTROY_EMPTY_PAGES
  /* Never destroy pages if the heap size is fixed: */
  if(epsilongc_fixed_heap_size_in_bytes != 0)
    return false;
  
  /* Destroy a page only once every EPSILONGC_EMPTY_PAGE_DESTRUCTION_INVERSE_FREQUENCY
     times: */
  static __thread epsilongc_unsigned_integer_t sequential_number = 0;
  sequential_number ++;
  if((sequential_number % EPSILONGC_EMPTY_PAGE_DESTRUCTION_INVERSE_FREQUENCY) != 0)
    return false;
  else
    return true;
#else
  return false; // we never destroy pages
#endif // #ifdef ENABLE_DESTROY_EMPTY_PAGES
}

void epsilongc_destroy_some_unused_pages(void){
#ifdef ENABLE_DESTROY_EMPTY_PAGES
  /* If our heuristics say we should not destroy an empty page now,
     don't do it: */
  if(!shall_we_destroy_an_empty_page())
    return;

  /* Ok, we decided to destroy an empty page (if at least one exists): */
  epsilongc_lock_mutex(epsilongc_empty_pages_mutex);
  if(EPSILONGC_LENGTH_OF_LIST(pages, &epsilongc_empty_pages) > 0){
    epsilongc_page_t empty_page = EPSILONGC_FIRST_ELEMENT_OF_LIST(pages, &epsilongc_empty_pages);
    EPSILONGC_DETACH_ELEMENT_FROM_LIST(pages, list, epsilongc_page_t, empty_page, &epsilongc_empty_pages);
    epsilongc_unlock_mutex(epsilongc_empty_pages_mutex);
    epsilongc_destroy_page_and_remove_it_from_global_structures(empty_page);
#ifdef ENABLE_DUMP_PAGE_INFORMATION
    printf("%s: The empty page %p was destroyed\n", epsilongc_calling_thread_name(), empty_page); fflush(stdout);
#endif // #ifdef ENABLE_DUMP_PAGE_INFORMATION
  } // if
  else
    epsilongc_unlock_mutex(epsilongc_empty_pages_mutex);
#endif // #ifdef ENABLE_DESTROY_EMPTY_PAGES
}

epsilongc_page_t epsilongc_take_page_if_possible(epsilongc_pool_t pool){
  /* With any configuration value, first we must access the pool: */
  epsilongc_lock_mutex(pool->mutex);
  
#ifdef ENABLE_DEFERRED_SWEEP
  /* With deferred sweeping we check for non-full not-to-be-swept pages as the
     very first thing; they're the cheapest pages to reuse and we want as few
     of them as possible to remain in the list at collection time: */
  if(EPSILONGC_LENGTH_OF_LIST(nonfull_not_to_be_swept_pages, pool) > 0){
    /* Ok, we have a page: detach it from the list and return it. There's no
       need to sweep it, as the name says: */
    epsilongc_page_t nonfull_not_to_be_swept_page =
      EPSILONGC_FIRST_ELEMENT_OF_LIST(nonfull_not_to_be_swept_pages, pool);
    EPSILONGC_DETACH_ELEMENT_FROM_LIST(nonfull_not_to_be_swept_pages, list, epsilongc_page_t,
                                       nonfull_not_to_be_swept_page, pool);
    epsilongc_unlock_mutex(pool->mutex);
    
    epsilongc_assert_on_debug(epsilongc_is_page_completely_unmarked(nonfull_not_to_be_swept_page));
    
    /* Ok, return the page: */
    return nonfull_not_to_be_swept_page;
  } // if
#endif // #ifdef ENABLE_DEFERRED_SWEEP
  
  /* Are there any possibly nonfull pages? */
  if(EPSILONGC_LENGTH_OF_LIST(possibly_nonfull_pages, pool) > 0){
    /* Yes, we have at least a possibly not full page. Great, detach it so that we can
       just return it: */
    epsilongc_page_t possibly_nonfull_page =
      EPSILONGC_FIRST_ELEMENT_OF_LIST(possibly_nonfull_pages, pool);
/* #ifdef ENABLE_DUMP_PAGE_INFORMATION */
/*     printf("%s: The pool %p is about to detach the possibly non-full page %p\n", epsilongc_calling_thread_name(), pool, possibly_nonfull_page); fflush(stdout); */
/* #endif // #ifdef ENABLE_DUMP_PAGE_INFORMATION */
    epsilongc_assert_on_debug(possibly_nonfull_page != NULL);
    EPSILONGC_DETACH_ELEMENT_FROM_LIST(possibly_nonfull_pages, list, epsilongc_page_t,
                                       possibly_nonfull_page, pool);
    epsilongc_unlock_mutex(pool->mutex);
#ifdef ENABLE_DUMP_PAGE_INFORMATION
    printf("%s: The pool %p has detached the possibly non-full page %p\n", epsilongc_calling_thread_name(), pool, possibly_nonfull_page); fflush(stdout);
#endif // #ifdef ENABLE_DUMP_PAGE_INFORMATION
        
#ifdef ENABLE_DEFERRED_SWEEP
    /* If we do deferred sweep then we have to sweep the page now and
       update sweep statistics: if we destroy empty pages then, if we
       discovered that the page was empty, we destroy it: */
    struct epsilongc_sweep_statistics sweep_statistics_for_this_page;
    epsilongc_initialize_sweep_statistics(&sweep_statistics_for_this_page);
/* #ifdef ENABLE_DUMP_PAGE_INFORMATION */
/*     printf("%s: The pool %p is about to sweep the page %p (deferred sweep)\n", epsilongc_calling_thread_name(), pool, possibly_nonfull_page); fflush(stdout); */
/* #endif // #ifdef ENABLE_DUMP_PAGE_INFORMATION */
    epsilongc_sweep_page(possibly_nonfull_page, &sweep_statistics_for_this_page);
#ifdef ENABLE_DUMP_PAGE_INFORMATION
    printf("%s: The pool %p has swept the page %p (deferred sweep)\n", epsilongc_calling_thread_name(), pool, possibly_nonfull_page); fflush(stdout);
#endif // #ifdef ENABLE_DUMP_PAGE_INFORMATION
    epsilongc_lock_mutex(epsilongc_global_mutex);
    epsilongc_merge_sweep_statistics(&epsilongc_sweep_statistics,
                                     &epsilongc_sweep_statistics,
                                     &sweep_statistics_for_this_page);
    epsilongc_unlock_mutex(epsilongc_global_mutex);
    
    /* Was this page empty? If so destroy it now (or add it to the list of empty
       pages if we don't destroy them), and try again; we want to reuse
       non-completely empty pages whenever possible, to either save memory or
       allow other pools to reuse; and, finally, we try again by recursively
       calling this same function: */
    if((sweep_statistics_for_this_page.alive_bytes == 0) &&
       shall_we_destroy_an_empty_page()){
#ifdef ENABLE_DESTROY_EMPTY_PAGES
      epsilongc_destroy_page_and_remove_it_from_global_structures(possibly_nonfull_page);
#ifdef ENABLE_DUMP_PAGE_INFORMATION
      printf("%s: The pool %p has destroyed the page %p\n", epsilongc_calling_thread_name(), pool, possibly_nonfull_page); fflush(stdout);
#endif // #ifdef ENABLE_DUMP_PAGE_INFORMATION
#else
      epsilongc_lock_mutex(epsilongc_empty_pages_mutex);
      possibly_nonfull_page->belongs_to_the_empty_pages_list = true;
      EPSILONGC_PREPEND_OBJECT_TO_LIST(pages, list, epsilongc_page_t, possibly_nonfull_page, &epsilongc_empty_pages);
      epsilongc_unlock_mutex(epsilongc_empty_pages_mutex);
#ifdef ENABLE_DUMP_PAGE_INFORMATION
      printf("%s: The pool %p has moved the page %p to the list of empty pages\n", epsilongc_calling_thread_name(), pool, possibly_nonfull_page); fflush(stdout);
#endif // #ifdef ENABLE_DUMP_PAGE_INFORMATION
#endif // #ifdef ENABLE_DESTROY_EMPTY_PAGES
      /* Try again: */
      return epsilongc_take_page_if_possible(pool);
    } // if
#endif // #ifdef ENABLE_DEFERRED_SWEEP
    
    /* Is the page we have detached complately full? If so it's quite useless to return it... */
    if(EPSILONGC_UNCONCEAL_POINTER(possibly_nonfull_page->next_free_object) == NULL){
      /* Return the page to the list of the full pages of this pool (with deferred sweeping) or
         to the global list of pages to be swept (with non-deferred sweeping): */
#ifdef ENABLE_DEFERRED_SWEEP
      epsilongc_lock_mutex(pool->mutex);
      EPSILONGC_APPEND_OBJECT_TO_LIST(full_pages, list, epsilongc_page_t, possibly_nonfull_page, pool);
      epsilongc_unlock_mutex(pool->mutex);
#else
      epsilongc_lock_mutex(epsilongc_pages_to_be_swept_mutex);
      EPSILONGC_APPEND_OBJECT_TO_LIST(pages, list, epsilongc_page_t, possibly_nonfull_page, &epsilongc_pages_to_be_swept);
      epsilongc_unlock_mutex(epsilongc_pages_to_be_swept_mutex);
#endif // #ifdef ENABLE_DEFERRED_SWEEP
      /* Try again: */
      return epsilongc_take_page_if_possible(pool);
    } // if
    
    /* Ok, the page we have detached is not empty, and we alraedy unlocked the
       pool mutex; just return the page: */
    return possibly_nonfull_page;
  } // if we have possibly non-full pages
  else{
    epsilongc_unlock_mutex(pool->mutex); //!!!
    /* No, we don't have any possibly nonfull pages from this pool. Do we
       have an empty page we can recycle? */
    epsilongc_lock_mutex(epsilongc_empty_pages_mutex);
    if(EPSILONGC_LENGTH_OF_LIST(pages, &epsilongc_empty_pages) > 0){
      /* Yes, great: detach it, refurbish it and return it: */
      epsilongc_page_t empty_page = EPSILONGC_FIRST_ELEMENT_OF_LIST(pages, &epsilongc_empty_pages);
      EPSILONGC_DETACH_ELEMENT_FROM_LIST(pages, list, epsilongc_page_t, empty_page, &epsilongc_empty_pages);
      epsilongc_unlock_mutex(epsilongc_empty_pages_mutex);
      empty_page->belongs_to_the_empty_pages_list = false;
#ifdef ENABLE_DUMP_PAGE_INFORMATION
      printf("%s: The pool %p is refurbishing the page %p\n", epsilongc_calling_thread_name(), pool, empty_page); fflush(stdout);
#endif // #ifdef ENABLE_DUMP_PAGE_INFORMATION
      epsilongc_refurbish_page(empty_page, pool);
      return empty_page;
    } // if
    epsilongc_unlock_mutex(epsilongc_empty_pages_mutex);
    
    /* We have no empty pages left; fail: */
    return NULL;
  } // else
}

void epsilongc_initialize_pool_support(void){  
  /////////////
  //#warning This is a kludge
  /* epsilongc_kind_t useless_kind = */
  /*   epsilongc_make_kind(1000, 1, 0, NULL, epsilongc_conservative_tracer, NULL); */
  /* epsilongc_pool_t useless_pool = epsilongc_make_pool("useless", useless_kind); */
  /* while(epsilongc_current_heap_size_in_bytes() < EPSILONGC_MINIMUM_HEAP_SIZE){ */
  /*   epsilongc_page_t page = epsilongc_make_a_new_page_for(useless_pool); */
  /*   epsilongc_lock_mutex(epsilongc_empty_pages_mutex); */
  /*   page->belongs_to_the_empty_pages_list = true; */
  /*   EPSILONGC_APPEND_OBJECT_TO_LIST(pages, list, page_t, page, &epsilongc_empty_pages); */
  /*   epsilongc_unlock_mutex(epsilongc_empty_pages_mutex); */
  /* } // while */
  /////////////
}

void epsilongc_finalize_pool_support(void){
}
