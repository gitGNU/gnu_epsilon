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


#include "allocator.h"
#include "pool.h"
#include "epsilongc_debug.h"
//#include "../include/doubly_linked_list.h>
#include "doubly_linked_list_macros.h"

void epsilongc_initialize_allocator_support(void){
}

static void print_lists(void);

void epsilongc_finalize_allocator_support(void){
}

void epsilongc_initialize_thread_local_allocator_support(void){
  /* Make thread-local lists: */
  EPSILONGC_INITIALIZE_LIST(allocators, &epsilongc_thread_local_allocators);
  EPSILONGC_INITIALIZE_LIST(allocators, &epsilongc_thread_local_allocators_with_a_page);
}

void epsilongc_finalize_thread_local_allocator_support(void){
  /* Finalize the allocators owned by this thread; this detaches them from both the thread-local
     and the global lists: */
  epsilongc_allocator_t allocator =
    EPSILONGC_FIRST_ELEMENT_OF_LIST(allocators, &epsilongc_thread_local_allocators);
  while(allocator != NULL){
    /* We need to keep this, as finalizing an allocator detaches it from lists: */
    epsilongc_allocator_t next_allocator =
      EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(thread_local_allocators, allocator);
    epsilongc_finalize_allocator(allocator);
    allocator = next_allocator;
  } // while
  //printf("Finalized thread-local allocator support:\n");
  
  /* Finalize thread-local lists (i.e. make sure they're empty): */
  EPSILONGC_FINALIZE_LIST(allocators, &epsilongc_thread_local_allocators_with_a_page);
  EPSILONGC_FINALIZE_LIST(allocators, &epsilongc_thread_local_allocators);
  
  print_lists();
}

static void epsilongc_move_page_from_allocator_unlocked(epsilongc_allocator_t allocator){
  const epsilongc_page_t page = allocator->current_page;
  epsilongc_assert_on_debug(page != NULL);
#ifdef ENABLE_DUMP_PAGE_INFORMATION
  printf("%s: The allocator %p is going to release the page %p (%s)\n", epsilongc_calling_thread_name(), allocator, page, epsilongc_is_page_completely_unmarked(page) ? "UNmarked" : "MARKed"); fflush(stdout);
#endif // #ifdef ENABLE_DUMP_PAGE_INFORMATION
#ifndef ENABLE_DEFERRED_SWEEP
  /* The page should be completley unmarked, unless we adopt deferred
     sweeping: */
  epsilongc_assert_on_debug(epsilongc_is_page_completely_unmarked(page));
#endif // #ifndef ENABLE_DEFERRED_SWEEP
  /* Just to play it safe, invalidate the free-list pointer within the page: the page
     will be swept before its next use, so the pointer will be recomputed in any case: */
  page->next_free_object = EPSILONGC_CONCEAL_POINTER(NULL);
  
  /* The page doesn't belongs to the allocator any longer: */
  page->current_allocator = NULL;
  
#ifdef ENABLE_DEFERRED_SWEEP
  epsilongc_pool_t pool = allocator->pool;
  /* Add the page to the pool. If a collection has occurred before we do this,
     the mark array of this page has already been cleared by the collector, hence
     we satisfy the invariant requiring the array of this page be clear at
     page-release time: */
  //printf("%s: The allocator %p is about to append the page %p (%s) to the possibly non-full list of the pool %p]]\n", epsilongc_calling_thread_name(), allocator, page, epsilongc_is_page_completely_unmarked(page) ? "UNmarked" : "MARKed", pool); fflush(stdout);
  epsilongc_assert_on_debug(epsilongc_is_page_completely_unmarked(page));
  epsilongc_lock_mutex(pool->mutex);
  EPSILONGC_APPEND_OBJECT_TO_LIST(full_pages, list, epsilongc_page_t, page, pool);
  epsilongc_unlock_mutex(pool->mutex);
  //printf("%s: The allocator %p has appended the page %p (%s)to the possibly non-full list of the pool %p]]\n", epsilongc_calling_thread_name(), allocator, page, epsilongc_is_page_completely_unmarked(page) ? "UNmarked" : "MARKed", pool); fflush(stdout);
#else
  /* Add the page to the list of pages to be swept: */
  //EPSILONGC_INITIALIZE_LIST_ELEMENT(list, page);// To do: this should not be needed
  epsilongc_lock_mutex(epsilongc_pages_to_be_swept_mutex);
  EPSILONGC_APPEND_OBJECT_TO_LIST(pages, list, epsilongc_page_t, page, &epsilongc_pages_to_be_swept);
  epsilongc_unlock_mutex(epsilongc_pages_to_be_swept_mutex);
  //printf("%s: The allocator %p has appeneded the page %p to the list of pages to be swept]]\n", epsilongc_calling_thread_name(), allocator, page); fflush(stdout);
#endif // #ifdef ENABLE_DEFERRED_SWEEP
  
  /* Detach the allocator from the lists of page-holding allocators: */
  epsilongc_lock_mutex(epsilongc_all_allocators_with_a_page_mutex);
  EPSILONGC_DETACH_ELEMENT_FROM_LIST(allocators, all_allocators_with_a_page, epsilongc_allocator_t,
                                     allocator, &epsilongc_all_allocators_with_a_page);
  epsilongc_unlock_mutex(epsilongc_all_allocators_with_a_page_mutex);
  EPSILONGC_DETACH_ELEMENT_FROM_LIST(allocators, thread_local_allocators_with_a_page, epsilongc_allocator_t,
                                     allocator, &epsilongc_thread_local_allocators_with_a_page);

  /* Remove the page from the allocator: */
  allocator->current_page = NULL;
  allocator->next_free_object = EPSILONGC_CONCEAL_POINTER(NULL);
#ifdef ENABLE_DUMP_PAGE_INFORMATION
  printf("%s: The allocator %p has released the page %p (%s), and now has no page\n", epsilongc_calling_thread_name(), allocator, page, epsilongc_is_page_completely_unmarked(page) ? "UNmarked" : "MARKed"); fflush(stdout);
#endif // #ifdef ENABLE_DUMP_PAGE_INFORMATION
}

static void epsilongc_give_page_to_allocator(epsilongc_page_t page, epsilongc_allocator_t allocator){
  /* Check that this operation is valid if we're debugging: */
  epsilongc_assert_on_debug(allocator->current_page == NULL);
  epsilongc_assert_on_debug(page->current_allocator == NULL);
  epsilongc_assert_on_debug(page->pool == allocator->pool);
  
  /* Update the relevant fields of the allocator *and* the page: */
  allocator->next_free_object = page->next_free_object;
  allocator->current_page = page;
  page->current_allocator = allocator;
  
  /* Attach the allocator to the lists of page-holding allocators: */
  epsilongc_lock_mutex(epsilongc_all_allocators_with_a_page_mutex);
  EPSILONGC_APPEND_OBJECT_TO_LIST(allocators, all_allocators_with_a_page, epsilongc_allocator_t,
                                  allocator, &epsilongc_all_allocators_with_a_page);
  epsilongc_unlock_mutex(epsilongc_all_allocators_with_a_page_mutex);
  EPSILONGC_APPEND_OBJECT_TO_LIST(allocators, thread_local_allocators_with_a_page, epsilongc_allocator_t,
                                  allocator, &epsilongc_thread_local_allocators_with_a_page);

  //printf("%s: The allocator %p now has the page %p (%s)\n", epsilongc_calling_thread_name(), allocator, page, epsilongc_is_page_completely_unmarked(page) ? "UNmarked" : "MARKed"); fflush(stdout);
#ifndef ENABLE_DEFERRED_SWEEP
  /* The page should be completley unmarked, unless we adopt deferred
     sweeping: */
  epsilongc_assert_on_debug(epsilongc_is_page_completely_unmarked(page));
#endif // #ifndef ENABLE_DEFERRED_SWEEP
  
}

void epsilongc_move_page_to_allocator_if_possible_unlocked(epsilongc_allocator_t allocator){
  epsilongc_pool_t pool = allocator->pool;
  epsilongc_assert_on_debug(allocator->current_page == NULL);
  epsilongc_assert_on_debug(allocator->next_free_object == EPSILONGC_CONCEAL_POINTER(NULL));
  
  /* Get a page, if possible; if this fails then just return, and leave the allocator with
     no page: */
  epsilongc_page_t page = epsilongc_take_page_if_possible(pool);
  if(page == NULL)
    return;
  
  /* Ok, we got a page: give it to the allocator, and we're done: */
  epsilongc_give_page_to_allocator(page, allocator);
}

/* void epsilongc_move_page_to_allocator_locked(epsilongc_allocator_t allocator){ */
/*   epsilongc_move_page_to_allocator_possibly_locked(allocator, true); */
/* } */


static void print_lists(void){
  /*
  epsilongc_allocator_t allocator;
  printf("%s: All the allocators are now (they should be %i):\n", epsilongc_calling_thread_name(), (int)EPSILONGC_LENGTH_OF_LIST(allocators, &epsilongc_all_allocators)); fflush(stdout);
  int i = 0;
  for(allocator = EPSILONGC_FIRST_ELEMENT_OF_LIST(allocators, &epsilongc_all_allocators);
      allocator != NULL;
      allocator = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(all_allocators, allocator), i++){
    printf("%i. %p\n", i, allocator); fflush(stdout);
  } // for
  i = 0;
  printf("%s: Thread-local allocators are now  (they should be %i):\n", epsilongc_calling_thread_name(), (int)EPSILONGC_LENGTH_OF_LIST(allocators, &epsilongc_thread_local_allocators)); fflush(stdout);
  for(allocator = EPSILONGC_FIRST_ELEMENT_OF_LIST(allocators, &epsilongc_thread_local_allocators);
      allocator != NULL;
      allocator = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(thread_local_allocators, allocator), i++){
    printf("%i. %p\n", i, allocator); fflush(stdout);
  } // for
  */
}

void epsilongc_initialize_allocator(epsilongc_allocator_t allocator,
                                    epsilongc_pool_t pool){
  //printf("%s: About to initialize the allocator %p...\n", epsilongc_calling_thread_name(), allocator); fflush(stdout);
  /* The allocator does not contain any page at creation time: */
  allocator->next_free_object = EPSILONGC_CONCEAL_POINTER(NULL);
  allocator->current_page = NULL;
  allocator->pool = pool;
  
  /* Add this allocator to the thread-local list of allocators: */
  EPSILONGC_INITIALIZE_LIST_ELEMENT(thread_local_allocators, allocator);
  EPSILONGC_APPEND_OBJECT_TO_LIST(allocators, thread_local_allocators, epsilongc_allocator_t,
                                  allocator, &epsilongc_thread_local_allocators);
  
  /* Add this allocator to the global list of allocators: */
  EPSILONGC_INITIALIZE_LIST_ELEMENT(all_allocators, allocator);
  EPSILONGC_APPEND_OBJECT_TO_LIST(allocators, all_allocators, epsilongc_allocator_t,
                                  allocator, &epsilongc_all_allocators);
  
  /* The allocator currently holds no page, so it's not in the lists of page-holding
     allocators; nonetheless its list element fields should be initialized: */
  EPSILONGC_INITIALIZE_LIST_ELEMENT(all_allocators_with_a_page, allocator);  
  EPSILONGC_INITIALIZE_LIST_ELEMENT(thread_local_allocators_with_a_page, allocator);  
  
  //printf("%s: Initialized the allocator %p.\n", epsilongc_calling_thread_name(), allocator); fflush(stdout);
  print_lists();
}

/* Return the allocator's page (if any) to its pool: */
static void epsilongc_move_page_if_any_from_allocator_unlocked(epsilongc_allocator_t allocator){
  if(allocator->current_page != NULL)
    epsilongc_move_page_from_allocator_unlocked(allocator);
}

void epsilongc_replace_page_with_appropriate_locking(epsilongc_allocator_t allocator){
#ifdef ENABLE_DUMP_PAGE_INFORMATION
  printf("%s: The allocator %p needs a non-full page (its current page is %p)\n", epsilongc_calling_thread_name(), allocator, allocator->current_page); fflush(stdout);
#endif // #ifdef ENABLE_DUMP_PAGE_INFORMATION
  /* We prevent a collection from occurring now, as we will be moving pages around
     and a collection could hit when a page is "in-between"; for example when a page
     is not in a pool any more, but not yet in an allocator; trating all such cases
     one by one and deciding whether, for example, a sweep would be safe, would be
     extremely hard and error-prone: */
  epsilongc_lock_read_write_lock_for_reading(epsilongc_global_read_write_lock);
  
  /* Return the current page to the pool, if any: */
  epsilongc_move_page_if_any_from_allocator_unlocked(allocator);
  
  /* Ok, now we are sure that the allocator has no page. Let's take a new page
     suitable for our pool (if one exists), and make the allocator own it: */
  epsilongc_move_page_to_allocator_if_possible_unlocked(allocator);
  
  /* If we got a page (hence we don't need to collect), then this is a good time
     to destroy some unused pages: */
  if(allocator->current_page != NULL)
    epsilongc_destroy_some_unused_pages();
    
  /* Did we succeed in getting a page? If so we're done. Otherwise... */
  if(allocator->current_page == NULL){
#ifdef ENABLE_DUMP_PAGE_INFORMATION
    printf("%s: The allocator %p couldn't get a page.\n", epsilongc_calling_thread_name(), allocator); fflush(stdout);
#endif // #ifdef ENABLE_DUMP_PAGE_INFORMATION
    /* No, we didn't get any page: garbage collect to free some space, if
       heuristics say it's a good idea: */
    if(epsilongc_should_we_garbage_collect()){
      /* Collect and try again: */
#ifdef ENABLE_DUMP_PAGE_INFORMATION
      printf("%s: I can't get pages from the %s pool: requesting a collection.\n", epsilongc_calling_thread_name(), allocator->pool->name); fflush(stdout);
#endif // #ifdef ENABLE_DUMP_PAGE_INFORMATION
      epsilongc_unlock_read_write_lock(epsilongc_global_read_write_lock);
      epsilongc_request_garbage_collection();
      epsilongc_lock_read_write_lock_for_reading(epsilongc_global_read_write_lock);
      epsilongc_move_page_to_allocator_if_possible_unlocked(allocator);
      if(allocator->current_page != NULL){
#ifdef ENABLE_DUMP_PAGE_INFORMATION
        printf("%s: The allocator %p got the page %p after a collection.\n", epsilongc_calling_thread_name(), allocator, allocator->current_page); fflush(stdout);
#endif // #ifdef ENABLE_DUMP_PAGE_INFORMATION
        epsilongc_unlock_read_write_lock(epsilongc_global_read_write_lock);
        return; // ok, now we have a page
      }
    } // inner if
    
    /* If we arrived here then either heuristics said not to collect, or collecting
       didn't help. Either one is a good reason to make a new page: */
    epsilongc_page_t page = epsilongc_make_a_new_page_for(allocator->pool);
    epsilongc_give_page_to_allocator(page, allocator);        
  } // outer if
  epsilongc_unlock_read_write_lock(epsilongc_global_read_write_lock);
#ifdef ENABLE_DUMP_PAGE_INFORMATION
  printf("%s: at the end of epsilongc_replace_page_with_appropriate_locking() the allocator %p has the page %p (%s)\n", epsilongc_calling_thread_name(), allocator, allocator->current_page, epsilongc_is_page_completely_unmarked(allocator->current_page) ? "UNmarked" : "MARKed"); fflush(stdout);
#endif // #ifdef ENABLE_DUMP_PAGE_INFORMATION
}

void epsilongc_finalize_allocator(epsilongc_allocator_t allocator){
  //printf("%s: About to finalize the allocator %p...\n", epsilongc_calling_thread_name(), allocator); fflush(stdout);

  /* Return the page, if any, to the right pool. This allocator is dying
     and its owner thread will not use the page any more, so it's safe to
     unpin: */
  //printf("%s: finalizing the allocator %p: Ok-1\n", epsilongc_calling_thread_name(), allocator); fflush(stdout);
  epsilongc_move_page_if_any_from_allocator_unlocked(allocator);

  //printf("%s: finalizing the allocator %p: Ok-2\n", epsilongc_calling_thread_name(), allocator); fflush(stdout);
  
  /* Remove this allocator from the thread-local list of allocators: */
  EPSILONGC_DETACH_ELEMENT_FROM_LIST(allocators, thread_local_allocators, epsilongc_allocator_t,
                                     allocator, &epsilongc_thread_local_allocators);

  //printf("%s: finalizing the allocator %p: Ok-3\n", epsilongc_calling_thread_name(), allocator); fflush(stdout);

  //printf("%s: finalizing the allocator %p: Ok-3a: all-allocators has length %i\n", epsilongc_calling_thread_name(), allocator, (int)EPSILONGC_LENGTH_OF_LIST(allocators, &epsilongc_all_allocators)); fflush(stdout);
  /* Remove this allocator from the global list of allocators: */
  EPSILONGC_DETACH_ELEMENT_FROM_LIST(allocators, all_allocators, epsilongc_allocator_t,
                                     allocator, &epsilongc_all_allocators);
  
  /* There should be no need to finalize list element fields for page-holder
     allocator lists, as this allocator has already released its page, if any. */
  
  //printf("%s: Finalized the allocator %p.\n", epsilongc_calling_thread_name(), allocator); fflush(stdout);
  //printf("%s: Finalize the allocator %p.\n", epsilongc_calling_thread_name(), allocator); fflush(stdout);
  print_lists();
}

void epsilongc_release_pages_from_thread_local_allocators(void){
  /* We don't want a collection to happen now: */
  epsilongc_lock_read_write_lock_for_reading(epsilongc_global_read_write_lock);
  
#ifdef ENABLE_ASSERTIONS
  /* Fail if the calling thread is not a mutator: */
  if(! epsilongc_is_the_caller_thread_a_mutator())
    epsilongc_fatal("epsilongc_release_pages_from_thread_local_allocators() called from a non-mutator thread");
#endif // #ifdef ENABLE_ASSERTIONS
  //printf("Ok-Z 1\n"); fflush(stdout);
  /* /\* Enter a critical section: *\/ */
  /* epsilongc_lock_mutex(epsilongc_global_mutex); */
  
  //printf("Ok-Z 2\n"); fflush(stdout);
  /* For each thread-local allocator which has a page... */
  epsilongc_allocator_t allocator =
    EPSILONGC_FIRST_ELEMENT_OF_LIST(allocators, &epsilongc_thread_local_allocators_with_a_page);
  while(allocator != NULL){
    /* We need to take a pointer to the next allocator now, as we're gonna change
       the list: */
    epsilongc_allocator_t next_allocator =
      EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(thread_local_allocators_with_a_page, allocator);
    
    /* Move the page to the appropriate list: */
    epsilongc_page_t page = allocator->current_page;
    epsilongc_assert_on_debug(page != NULL);
    page->next_free_object = allocator->next_free_object;
    page->current_allocator = NULL;
    allocator->current_page = NULL;
    allocator->next_free_object = EPSILONGC_CONCEAL_POINTER(NULL);
    epsilongc_pool_t pool = allocator->pool;
    epsilongc_lock_mutex(pool->mutex);
    if(page->next_free_object != EPSILONGC_CONCEAL_POINTER(NULL)){
      /* The page is not full. If we don't use deferred sweeping it can simply be added
         back to the list of possibly non-full pages; that wouldn't work with deferred
         sweep however, as in that case the page would be swept *again* before reuse.
         So with deferred sweep we use a separate list just for this purpose, containing
         non-full not-to-be-swept pages, which can be just reused: */
      //printf("Allocator %p: releasing the NON-full page %p\n", allocator, page);
#ifdef ENABLE_DEFERRED_SWEEP
      epsilongc_assert_on_debug(epsilongc_is_page_completely_unmarked(page));
      EPSILONGC_PREPEND_OBJECT_TO_LIST(nonfull_not_to_be_swept_pages, list, epsilongc_page_t, page, pool);
#else
#ifdef ENABLE_DUMP_PAGE_INFORMATION
      printf("Allocator %p: voluntary release: prepending the NON-full page %p (%s) to the possibly-not-full pages\n", allocator, page, epsilongc_is_page_completely_unmarked(page) ? "UNmarked" : "MARKed");
#endif // #ifdef ENABLE_DUMP_PAGE_INFORMATION
      epsilongc_assert_on_debug(epsilongc_is_page_completely_unmarked(page));
      EPSILONGC_PREPEND_OBJECT_TO_LIST(possibly_nonfull_pages, list, epsilongc_page_t, page, pool);
#endif // #ifdef ENABLE_DEFERRED_SWEEP
    }
    else{
#ifdef ENABLE_DUMP_PAGE_INFORMATION
      printf("%s: Allocator %p: voluntarily releasing the FULL page %p (%s)\n", epsilongc_calling_thread_name(), allocator, page, epsilongc_is_page_completely_unmarked(page) ? "UNmarked" : "MARKed");
#endif // #ifdef ENABLE_DUMP_PAGE_INFORMATION
      /* The page is full and must be swept before it's reused: */
#ifdef ENABLE_DEFERRED_SWEEP
      EPSILONGC_APPEND_OBJECT_TO_LIST(full_pages, list, epsilongc_page_t, page, pool);
#else
      EPSILONGC_APPEND_OBJECT_TO_LIST(pages, list, epsilongc_page_t, page, &epsilongc_pages_to_be_swept);
#endif // #ifdef ENABLE_DEFERRED_SWEEP
    } // else
    epsilongc_unlock_mutex(pool->mutex);
    
    /* Detach the allocator from the lists of page-holding allocators: */
    epsilongc_lock_mutex(epsilongc_all_allocators_with_a_page_mutex);
    EPSILONGC_DETACH_ELEMENT_FROM_LIST(allocators, all_allocators_with_a_page, epsilongc_allocator_t,
                                       allocator, &epsilongc_all_allocators_with_a_page);
    epsilongc_unlock_mutex(epsilongc_all_allocators_with_a_page_mutex);
    EPSILONGC_DETACH_ELEMENT_FROM_LIST(allocators, thread_local_allocators_with_a_page, epsilongc_allocator_t,
                                       allocator, &epsilongc_thread_local_allocators_with_a_page);
    
    /* Go on with the next allocator: */
    allocator = next_allocator;
  } // while
  
  /* Collections can now happen: */
  epsilongc_unlock_read_write_lock(epsilongc_global_read_write_lock);
}

/* If aggressive inlining is disabled then we need to put definitions here, in a C file to be
   compiled and linked in the library: */
#ifndef ENABLE_AGGRESSIVELY_INLINE
#include "include/epsilongc/definitionstoinline.h"
#endif // #ifndef ENABLE_AGGRESSIVELY_INLINE
