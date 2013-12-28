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


#include "epsilongc_types.h"
#include "epsilongc_features.h"
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include "page.h"
#include "kind.h"
#include "pool.h"
#include "malloc.h"
#include "compile_time_parameters.h"
#include "fatal.h"
#include "set_of_pages.h"
#include "epsilongc_debug.h"
#include "trace.h"
#include "time.h"
#include "epsilongc_debug.h"

/* Initialize page support: */
void epsilongc_initialize_page_support(void){
}

/* Finalize page support: */
void epsilongc_finalize_page_support(void){
}

/* We will only use one of these; which one depends on a configuration option: */
static inline void epsilongc_build_free_list_forward(epsilongc_page_t page) __attribute__((unused));
static inline void epsilongc_build_free_list_backward(epsilongc_page_t page) __attribute__((unused));

/* We will only use one of these; which one depends on a configuration option: */
static inline void epsilongc_sweep_page_forward(epsilongc_page_t page,
                                                epsilongc_sweep_statistics_t statistics) __attribute__((unused));
static inline void epsilongc_sweep_page_backward(epsilongc_page_t page,
                                                 epsilongc_sweep_statistics_t statistics) __attribute__((unused));

static void epsilongc_build_free_list_forward(epsilongc_page_t page){
  const epsilongc_integer_t object_size_in_bytes = page->object_size_in_bytes;
  const epsilongc_integer_t objects_no = page->kind->objects_no_per_page;
  const epsilongc_pointer_t first_object_in_the_page =
    EPSILONGC_PAGE_AND_MARK_ARRAY_INDEX_TO_OBJECT(page, 0);
  epsilongc_integer_t i;
  epsilongc_word_t *ith_object;
  
  /* Build an ordered free list linking dead objects by their first words: */
  epsilongc_word_t **next_object = &(page->next_free_object);
  for(i = 0, ith_object = (epsilongc_word_t*)first_object_in_the_page;
      i < objects_no;
      i++, ith_object = (epsilongc_word_t*)(((char*)ith_object) + object_size_in_bytes)){
    *next_object = EPSILONGC_CONCEAL_POINTER(ith_object);
    next_object = (epsilongc_word_t**)ith_object;
  } // for
  *next_object = EPSILONGC_CONCEAL_POINTER(NULL);
}

static void epsilongc_build_free_list_backward(epsilongc_page_t page){
  const epsilongc_integer_t object_size_in_bytes = page->object_size_in_bytes;
  const epsilongc_integer_t objects_no = page->kind->objects_no_per_page;
  epsilongc_pointer_t* first_object_in_the_page =
    EPSILONGC_PAGE_AND_MARK_ARRAY_INDEX_TO_OBJECT(page, 0);
  epsilongc_pointer_t* last_object_in_the_page =
    EPSILONGC_PAGE_AND_MARK_ARRAY_INDEX_TO_OBJECT(page, objects_no - 1);
  epsilongc_pointer_t* next_to_last_object_in_the_page =
   (epsilongc_pointer_t*)(((char*)last_object_in_the_page) - object_size_in_bytes);
  
  /* Fill the free list: */
  epsilongc_integer_t i;
  epsilongc_word_t *ith_object;
  *last_object_in_the_page = EPSILONGC_CONCEAL_POINTER(NULL);
  //printf("%p: updated the %li-th slot as a particular case, setting it to NULL\n", page,(long)EPSILONGC_PAGE_AND_OBJECT_TO_MARK_ARRAY_INDEX(page, last_object_in_the_page));
  //printf("%p: BEGIN (objects are %li)\n", page, (long)objects_no);
  for(i = objects_no - 2, ith_object = next_to_last_object_in_the_page;
      i >= 0;
      i--, ith_object = (epsilongc_word_t*)(((char*)ith_object) - object_size_in_bytes)){
    epsilongc_word_t * const i_plus_1_th_object =
      (epsilongc_word_t*)(((char*)ith_object) + object_size_in_bytes);
    *ith_object = EPSILONGC_CONCEAL_POINTER(i_plus_1_th_object);
    //printf("%p: updated the %li-th slot\n", page, (long)EPSILONGC_PAGE_AND_OBJECT_TO_MARK_ARRAY_INDEX(page, ith_object)); fflush(stdout);
  } // for
  //printf("%p: END\n", page); fflush(stdout);

  /* Make the page refer the beginning of the free list: */
  page->next_free_object = EPSILONGC_CONCEAL_POINTER(first_object_in_the_page);
}

static void epsilongc_build_free_list(epsilongc_page_t page) EPSILONGC_INLINE_ATTRIBUTE_IFF_AGGRESSIVE_INLINE;
static void epsilongc_build_free_list(epsilongc_page_t page){
#ifdef ENABLE_BACKWARD_SWEEP
  epsilongc_build_free_list_backward(page);
#else
  epsilongc_build_free_list_forward(page);
#endif // #ifdef ENABLE_BACKWARD_SWEEP
}

static void epsilongc_initialize_page(epsilongc_page_t page, epsilongc_pool_t pool) \
  EPSILONGC_INLINE_ATTRIBUTE_IFF_AGGRESSIVE_INLINE;
static void epsilongc_initialize_page(epsilongc_page_t page, epsilongc_pool_t pool){
  epsilongc_kind_t kind = pool->kind;
  /* Fill-in fields: */
  page->tag = kind->tag;
  page->datum = kind->datum;
  page->object_size_in_words =
    kind->object_size_in_words;
  page->object_size_in_bytes =
    sizeof(epsilongc_word_t) * kind->object_size_in_words;
  page->object_alignment_in_bytes =
    sizeof(epsilongc_word_t) * kind->object_alignment_in_words;
  page->tracer = kind->tracer;
  page->kind = kind;
  page->first_object = (epsilongc_word_t*)
    (((char*)page) + kind->first_object_offset_in_bytes);
  page->after_last_object = (epsilongc_word_t*)
    (((char*)page)
     + EPSILONGC_PAGE_SIZE_IN_BYTES
     - kind->unused_space_at_the_end_in_bytes);
  page->pool = pool;
  page->current_allocator = NULL;
  page->belongs_to_the_empty_pages_list  = false;
  
  /* Initialize the list element fields: */
  EPSILONGC_INITIALIZE_LIST_ELEMENT(list, page);
  
  /* We don't initialize the list element field for the all-pages list here;
     we only touch it when adding or removing the page from the global set,
     as that are the only moments when the page becomes globally visible or
     invisible. In particular, refurbishing should not alter the all-pages
     list in any way. */
  
#ifdef ENABLE_OUT_OF_PAGE_MARK_ARRAY
  /* Make the mark array: */
  page->mark_array = (char*)
    epsilongc_xmalloc(epsilongc_mark_array_size_in_bytes(kind));
#endif // #ifdef ENABLE_OUT_OF_PAGE_MARK_ARRAY
  
  /* Unmark every object: */
  epsilongc_completely_unmark_page(page);
  
  /* Make the free list: */
  epsilongc_build_free_list(page);
}

static void epsilongc_finalize_page(epsilongc_page_t page){
#ifdef ENABLE_OUT_OF_PAGE_MARK_ARRAY
  /* Destroy the mark array: */
  free(page->mark_array);
#endif // #ifdef ENABLE_OUT_OF_PAGE_MARK_ARRAY
}

static inline epsilongc_page_t epsilongc_make_page_without_adding_it_to_global_structures(epsilongc_pool_t pool){
  /* Allocate the page, aligned to the right boundary: */
  void *page_as_void_star;
  const int result = posix_memalign(&page_as_void_star,
                                    EPSILONGC_PAGE_SIZE_IN_BYTES,
                                    EPSILONGC_PAGE_SIZE_IN_BYTES);
  if(result != 0){
#define EPSILONGC_MESSAGE_SIZE 10000
    char buffer[EPSILONGC_MESSAGE_SIZE];
    char *message = strerror_r(result, buffer, EPSILONGC_MESSAGE_SIZE);
    epsilongc_fatal("posix_memalign() failed: \"%s\"", message);
  }
  epsilongc_page_t page = page_as_void_star;
  
  //printf("%s: made the new page %p\n", epsilongc_calling_thread_name(), page); fflush(stdout);
  
  /* Initialize the new page and return it: */
  epsilongc_initialize_page(page, pool);
  //printf("+ The page %p was made\n", page); fflush(stdout);
  return page;
}

static inline void epsilongc_add_page_to_global_structures_unlocked(epsilongc_page_t page,
                                                                    const epsilongc_integer_t added_pointers_no,
                                                                    const epsilongc_integer_t added_objects_no){

  /* Make sure the page isn't already in global structures: */
  epsilongc_assert_on_debug(! epsilongc_does_page_exist(page));
  
  /* Add the page to the set of pages: */
  epsilongc_add_page_to_the_set_of_pages(page);
  
  /* /\* Update the number of pointers and objects: *\/  ??? This looks redundant and may be the source of the bug! */
  /* epsilongc_pointers_no_in_the_worst_case += added_pointers_no; */
  /* epsilongc_objects_no_in_the_worst_case += added_objects_no; */
  
#ifdef ENABLE_ASSERTIONS
  /* Add the page to the list of all pages: */
  EPSILONGC_APPEND_OBJECT_TO_LIST(pages, all_pages, epsilongc_page_t, page, &epsilongc_all_pages);
#endif // #ifdef ENABLE_ASSERTIONS
  //printf("%s: the new page %p was added to global structures\n", epsilongc_calling_thread_name(), page); fflush(stdout);
}

epsilongc_page_t epsilongc_make_page_and_add_it_to_global_structures(epsilongc_pool_t pool){
  /* Make a page: */
  epsilongc_page_t page = epsilongc_make_page_without_adding_it_to_global_structures(pool);
  
  /* Compute the number number of added objects and pointers, so that we can correctly
     resize the mark stack: */
  const epsilongc_kind_t const kind = page->kind;
  const epsilongc_integer_t added_pointers_no = kind->pointers_no_per_page_in_the_worst_case;
  const epsilongc_integer_t added_objects_no = kind->objects_no_per_page;
  
  /* Add it to global structures: */  
  epsilongc_lock_mutex(epsilongc_global_mutex);
  epsilongc_add_page_to_global_structures_unlocked(page, added_pointers_no, added_objects_no);
  epsilongc_unlock_mutex(epsilongc_global_mutex);
  
  /* The heap size (in terms of the number of pointers or objects) has changed, so we
     may need to make new stack blocks: */
  epsilongc_make_or_destroy_stack_blocks_as_needed(added_pointers_no,
                                                   added_objects_no);
  
  /* Return the new page: */
  return page;
}

void epsilongc_refurbish_page(epsilongc_page_t page, epsilongc_pool_t pool){
  /* Read some fields releated to the heap size now: we want the critical section
     to be as short as possible: */
  const epsilongc_kind_t const old_kind = page->kind;
  const epsilongc_kind_t const new_kind = pool->kind;
  const epsilongc_integer_t removed_pointers_no = old_kind->pointers_no_per_page_in_the_worst_case;
  const epsilongc_integer_t removed_objects_no = old_kind->objects_no_per_page;
  const epsilongc_integer_t added_pointers_no = new_kind->pointers_no_per_page_in_the_worst_case;
  const epsilongc_integer_t added_objects_no = new_kind->objects_no_per_page;
  const epsilongc_integer_t delta_pointers_no = added_pointers_no - removed_pointers_no;
  const epsilongc_integer_t delta_objects_no = added_objects_no - removed_objects_no;
  
  /* If the kind has changed then finalize the page and initialize it
     again; otherwise the page is ready as it is, we just have to update the
     pool pointer (that may change without need for refurbishing, in the case
     when there are several pools for a single kind). */
  if(old_kind != new_kind){
    /* Re-initialize the page for the new kind; this is the slowest part
       (linear in the page size), but note that it requires absolutely no
       synchronization: */
    epsilongc_assert_on_debug(page->current_allocator == NULL);
    epsilongc_finalize_page(page);
    epsilongc_initialize_page(page, pool);
    
    /* The heap size (in terms of the number of pointers or objects) has
       probably changed: add new stack blocks or destroy existing ones if
       needed: */
    epsilongc_make_or_destroy_stack_blocks_as_needed(delta_pointers_no,
                                                     delta_objects_no);
  } // if
  else{
    /* In this case the heap size hasn't changed and the page internal
       structure can be kept as it is: */
    page->pool = pool;
  } // else
}

static inline void epsilongc_destroy_page_without_removing_it_from_global_structures(epsilongc_page_t page){
  // To do: this is only allowed on GNU systems, because BSD doesn't specify
  // a way to release a memaligned() buffer. Use an autoconf check for
  // this. (To do: can this be done on non-GNU POSIX systems?)
  
  /* Finalize the page: */
  epsilongc_finalize_page(page);
  
  /* Destroy the page buffer: */
  free(page);
  
  //printf("- The page %p was destroyed\n", page); fflush(stdout);
}

static inline void epsilongc_remove_page_from_global_structures_unlocked(epsilongc_page_t page,
                                                                         const epsilongc_integer_t removed_pointers_no,
                                                                         const epsilongc_integer_t removed_objects_no){
  /* /\* Update the number of pointers and objects: *\/ ??? This looks redundant and may be the source of the bug! */
  /* epsilongc_pointers_no_in_the_worst_case -= removed_pointers_no; */
  /* epsilongc_objects_no_in_the_worst_case -= removed_objects_no; */
  
  /* Remove the page from the global set: */
  epsilongc_remove_page_from_the_set_of_pages(page);
  
#ifdef ENABLE_ASSERTIONS
  /* Remove the page from the all-pages list: */
  EPSILONGC_DETACH_ELEMENT_FROM_LIST(pages, all_pages, epsilongc_page_t, page, &epsilongc_all_pages);
#endif // #ifdef ENABLE_ASSERTIONS
}

void epsilongc_destroy_page_and_remove_it_from_global_structures(epsilongc_page_t page){
  // To do: this is only allowed on GNU systems, because BSD doesn't specify
  // a way to release a memaligned() buffer. Use an autoconf check for
  // this. (To do: can this be done on non-GNU POSIX systems?)
  
  /* Read some fields releated to the heap size now: we want the critical section
     to be as short as possible: */
  const epsilongc_kind_t const kind = page->kind;
  const epsilongc_integer_t removed_pointers_no = kind->pointers_no_per_page_in_the_worst_case;
  const epsilongc_integer_t removed_objects_no = kind->objects_no_per_page;
  
  /* The heap size (in terms of the number of pointers or objects) has changed, so we
     may need to destroy some stack blocks: */
  epsilongc_make_or_destroy_stack_blocks_as_needed(- removed_pointers_no,
                                                   - removed_objects_no);
  
  /* In a(-nother) critical section: remove the page from the global list, and update
     the heap size: */
  epsilongc_lock_mutex(epsilongc_global_mutex);
  epsilongc_remove_page_from_global_structures_unlocked(page, removed_pointers_no, removed_objects_no);
  epsilongc_unlock_mutex(epsilongc_global_mutex);
  
  /* Actually destroy the page: */
  epsilongc_destroy_page_without_removing_it_from_global_structures(page);
}

void epsilongc_dump_free_list(epsilongc_page_t page){
  printf("Dumping the free list of the page %p:\n", page);
  const epsilongc_word_t *word;
  for(word = page->next_free_object;
      (char*)word != NULL;
      word = (epsilongc_word_t*)*word){
    printf("%p [%lu]\n", word, (unsigned long)word); fflush(stdout);
  }
  printf("\n");
}

bool epsilongc_is_the_first_object_in(epsilongc_word_t object,
                                     epsilongc_page_t page){
  /* Just when debugging, check whether the pointer points within the page, and
     whether the page exists: */
  char *page_as_char_star = (char*)page;
  char *object_as_char_star = (char*)object;
  epsilongc_assert_on_debug(epsilongc_does_page_exist(page));
  epsilongc_assert_on_debug(object_as_char_star >= page_as_char_star);
  epsilongc_assert_on_debug(object_as_char_star <
                            (page_as_char_star + EPSILONGC_PAGE_SIZE_IN_BYTES));
  
  return (((char*)page->first_object) == object_as_char_star);
}

bool epsilongc_is_the_last_object_in(epsilongc_word_t object,
                                     epsilongc_page_t page){
  /* Just when debugging, check whether the pointer points within the page, and
     whether the page exists: */
  char *page_as_char_star = (char*)page;
  char *object_as_char_star = (char*)object;
  epsilongc_assert_on_debug(epsilongc_does_page_exist(page));
  epsilongc_assert_on_debug(object_as_char_star >= page_as_char_star);
  epsilongc_assert_on_debug(object_as_char_star <
                            (page_as_char_star + EPSILONGC_PAGE_SIZE_IN_BYTES));
  
  return (((char*)page->after_last_object) - page->object_size_in_bytes) == object_as_char_star;
}

/* Sweep the given page, updating the given statistics if any: */
void epsilongc_sweep_page(epsilongc_page_t page,
                          epsilongc_sweep_statistics_t statistics)
  EPSILONGC_INLINE_ATTRIBUTE_IFF_AGGRESSIVE_INLINE;
static void epsilongc_sweep_page_forward(epsilongc_page_t page,
                                         epsilongc_sweep_statistics_t statistics){
  //printf("%s: Sweeping page %p: begin\n", epsilongc_calling_thread_name(), page); fflush(stdout);
  /* Cache some things we're going to repeatedly need. This function is
     performance-crictical. */
#ifdef ENABLE_OVERWRITE_DEAD_OBJECTS
  const epsilongc_integer_t object_size_in_words =
    (const epsilongc_integer_t)page->kind->object_size_in_words;
#endif // #ifdef ENABLE_OVERWRITE_DEAD_OBJECTS
  const epsilongc_integer_t object_size_in_bytes = page->object_size_in_bytes;
  const epsilongc_integer_t objects_no = page->kind->objects_no_per_page;
  const epsilongc_pointer_t first_object_in_the_page =
    EPSILONGC_PAGE_AND_MARK_ARRAY_INDEX_TO_OBJECT(page, 0);
  epsilongc_integer_t i;
  epsilongc_word_t *ith_object;
  const epsilongc_pointer_t mark_array = (const epsilongc_pointer_t)
    EPSILONGC_PAGE_TO_MARK_ARRAY(page);
  //printf("SWEEPING PAGE %p (from the %s pool)\n", page, page->pool->name);

  // Finalization is not implemented yet
/* #ifdef ENABLE_FINALIZATION */
/*   /\* Finalize dead objects, if needed: *\/ */
/*   const epsilongc_finalizer_t finalizer = page->kind->finalizer; */
/*   if(finalizer != NULL) */
/*     for(i = 0, ith_object = (epsilongc_word_t*)first_object_in_the_page; */
/*         i < objects_no; */
/*         i++, ith_object = (epsilongc_word_t*)(((char*)ith_object) + object_size_in_bytes)) */
/*       /\* Most objects should be dead: *\/ */
/*       if(EPSILONGC_LIKELY(EPSILONGC_LOOKUP_MARK_ARRAY(mark_array, i) == 0)){ */
/*         epsilongc_word_t *ith_object = (epsilongc_word_t*) */
/*           (((char*)first_object_in_the_page) + i * object_size_in_bytes); */
/*         finalizer(ith_object); */
/*       } // if */
/* #endif // #ifdef ENABLE_FINALIZATION */
  
  /* We want to remember how many object we find alive: */
  epsilongc_unsigned_integer_t alive_objects_no = 0;
  
  /* Build an ordered free list linking dead objects by their first words: */
  epsilongc_word_t **next_dead_object = &(page->next_free_object);
  for(i = 0, ith_object = (epsilongc_word_t*)first_object_in_the_page;
      i < objects_no;
      i++, ith_object = (epsilongc_word_t*)(((char*)ith_object) + object_size_in_bytes))
    /* Most objects should be dead: */
    if(EPSILONGC_LIKELY(EPSILONGC_LOOKUP_MARK_ARRAY(mark_array, i) == 0)){
      *next_dead_object = EPSILONGC_CONCEAL_POINTER(ith_object);
      next_dead_object = (epsilongc_word_t**)ith_object;
#ifdef ENABLE_OVERWRITE_DEAD_OBJECTS
      /* We start overwriting from the *second* word: the first one has been
         already overwritten with the free list pointer: */
      epsilongc_integer_t j;
      for(j = 1; j < object_size_in_words; j ++)
        ith_object[j] = EPSILONGC_DEAD_WORD;
#endif // #ifdef ENABLE_OVERWRITE_DEAD_OBJECTS
    } // if
    else{
      /* We found an alive object: ok, reset its bit so that it looks potentially
         dead at the next trace, and increment the alive objects' counter: */
      EPSILONGC_UPDATE_MARK_ARRAY(mark_array, i, 0);
      alive_objects_no++;
    } // else
  *next_dead_object = EPSILONGC_CONCEAL_POINTER(NULL);

  //epsilongc_dump_free_list(page);
  
  /* Update statistics, if any: */
  if(statistics != NULL){
    const epsilongc_unsigned_integer_t alive_bytes_in_this_page =
      alive_objects_no * object_size_in_bytes;
    const epsilongc_unsigned_integer_t dead_bytes_in_this_page =
      (objects_no - alive_objects_no) * object_size_in_bytes;
    statistics->swept_pages_no ++;
    statistics->alive_bytes += alive_bytes_in_this_page;
    statistics->dead_bytes += dead_bytes_in_this_page;
  } // if
  //printf("%s: Sweeping page %p: end\n", epsilongc_calling_thread_name(), page); fflush(stdout);
}

static void epsilongc_sweep_page_backward(epsilongc_page_t page,
                                          epsilongc_sweep_statistics_t statistics){
  //printf("SWEEPING PAGE %p (from the %s pool): begin\n", page, page->pool->name);
  const epsilongc_integer_t object_size_in_bytes = page->object_size_in_bytes;
#ifdef ENABLE_OVERWRITE_DEAD_OBJECTS
  const epsilongc_integer_t object_size_in_words =
    (const epsilongc_integer_t)page->kind->object_size_in_words;
#endif // #ifdef ENABLE_OVERWRITE_DEAD_OBJECTS
  const epsilongc_integer_t object_no = page->kind->objects_no_per_page;
  const epsilongc_pointer_t last_object_in_the_page =
    EPSILONGC_PAGE_AND_MARK_ARRAY_INDEX_TO_OBJECT(page, object_no - 1);
  const epsilongc_pointer_t mark_array = (const epsilongc_pointer_t)
    EPSILONGC_PAGE_TO_MARK_ARRAY(page);
  
  /* We want to remember how many alive objects we find: */
  epsilongc_unsigned_integer_t alive_object_no = 0;
  
  /* Build an ordered free list linking dead objects by their first words,
     backwards; the first pointer we add (the last in the free list) will
     be NULL: */
  epsilongc_integer_t i;
  epsilongc_word_t *ith_object;
  epsilongc_pointer_t next_pointer_to_add = NULL;
  for(i = object_no - 1, ith_object = last_object_in_the_page;
      i >= 0;
      i --, ith_object = (epsilongc_word_t*)(((char*)ith_object) - object_size_in_bytes))
    /* Most objects should be dead: */
    if(EPSILONGC_LIKELY(EPSILONGC_LOOKUP_MARK_ARRAY(mark_array, i) == 0)){
      /* The i-th object is dead: we need to add it to the free-list, and
         overwrite it if overwriting dead objects is enabled: */
      *ith_object = EPSILONGC_CONCEAL_POINTER(next_pointer_to_add);
      next_pointer_to_add = ith_object;
#ifdef ENABLE_OVERWRITE_DEAD_OBJECTS
      /* We start overwriting from the *second* word: the first one has been
         already overwritten with the free list pointer: */
      epsilongc_integer_t j;
      for(j = 1; j < object_size_in_words; j ++)
        ith_object[j] = EPSILONGC_DEAD_WORD;
#endif // #ifdef ENABLE_OVERWRITE_DEAD_OBJECTS
    }
    else{
      /* The i-th pointer is alive: reset its bit so that it looks potentially
         dead at the next trace, and increment the alive objects' counter: */
      EPSILONGC_UPDATE_MARK_ARRAY(mark_array, i, 0);
      alive_object_no++;
    } // else

  /* We've built the free list. Let's make the free list beginning pointer
     refer its first element: */
  page->next_free_object = EPSILONGC_CONCEAL_POINTER(next_pointer_to_add);
  
  /* Update statistics, if any: */
  if(statistics != NULL){
    const epsilongc_unsigned_integer_t alive_bytes_in_this_page =
      alive_object_no * object_size_in_bytes;
    const epsilongc_unsigned_integer_t dead_bytes_in_this_page =
      (object_no - alive_object_no) * object_size_in_bytes;
    statistics->swept_pages_no ++;
    statistics->alive_bytes += alive_bytes_in_this_page;
    statistics->dead_bytes += dead_bytes_in_this_page;
  } // if
  //printf("%s: Sweeping page %p: end\n", epsilongc_calling_thread_name(), page); fflush(stdout);
}

void epsilongc_sweep_page(epsilongc_page_t page,
                          epsilongc_sweep_statistics_t statistics){
#ifdef ENABLE_BACKWARD_SWEEP
  epsilongc_sweep_page_backward(page, statistics);
#else
  epsilongc_sweep_page_forward(page, statistics);
#endif // #ifdef ENABLE_BACKWARD_SWEEP
}

bool epsilongc_is_aligned_pointer_within_page_valid_in_page(
        const epsilongc_word_t pointer,
        const epsilongc_page_t page){
  const epsilongc_unsigned_integer_t pointer_as_unsigned_integer =
    (const epsilongc_unsigned_integer_t)pointer;
  const char* pointer_as_char_star =
    (const char*)pointer;
  const char* page_as_char_star =
    (const char*)page;
  /* Just when debugging, check that the pointer actually points within
     the page, that it's word-aligned, and that the page actually exists: */
  epsilongc_assert_on_debug((pointer_as_unsigned_integer % sizeof(epsilongc_word_t)) == 0);
  epsilongc_assert_on_debug(epsilongc_does_page_exist(page));
  epsilongc_assert_on_debug(pointer_as_char_star >= page_as_char_star);
  epsilongc_assert_on_debug(pointer_as_char_star <
                            (page_as_char_star + EPSILONGC_PAGE_SIZE_IN_BYTES));
  
#ifndef ENABLE_INTERIOR_POINTERS
  /* We don't support interior pointers, so the pointed object must exactly
     respect the alignment for objects in this page: */
  if((pointer_as_unsigned_integer % page->object_alignment_in_bytes) != 0)
    return false;
#endif // #ifndef ENABLE_INTERIOR_POINTERS

  /* Does the pointer point to some address within the area where the payload is
     (and not in the header, mark bytes or passing)? */
  if((pointer_as_char_star < (char*)(page->first_object)) ||
     (pointer_as_char_star >= (char*)(page->after_last_object)))
    return false;
  
  /* Ok, the pointer looks valid if we arrived here. Of course, if the page
     exists... */
  return true;
}

epsilongc_unsigned_integer_t epsilongc_current_heap_size_in_bytes(void){
  return epsilongc_pages_no() * EPSILONGC_PAGE_SIZE_IN_BYTES;
}

static bool epsilongc_are_all_mark_bits_in_page(const epsilongc_page_t page, const bool value){
  /* To do: this can be optimized. */
  epsilongc_integer_t i;
  const epsilongc_pointer_t mark_array = (const epsilongc_pointer_t)EPSILONGC_PAGE_TO_MARK_ARRAY(page);
  const epsilongc_integer_t objects_no = page->kind->objects_no_per_page;
  for(i = 0; i < objects_no; i++){
    const bool ith_bit_value = EPSILONGC_LOOKUP_MARK_ARRAY(mark_array, i);
    if(value){ // this is clumsy to write; I'd need a logical nxor operator in C...
      if(! ith_bit_value)
        return false;
    }else{
      if(ith_bit_value)
        return false;
    } // else
  } // for
  return true;
}

/* Return true iff all the objects in the given page are marked: */
bool epsilongc_is_page_completely_marked(const epsilongc_page_t page){
  return epsilongc_are_all_mark_bits_in_page(page, true);
}

/* Return true iff all the objects in the given page are unmarked: */
bool epsilongc_is_page_completely_unmarked(const epsilongc_page_t page){
  return epsilongc_are_all_mark_bits_in_page(page, false);
}

static void epsilongc_set_all_object_mark_bits_in(epsilongc_page_t page, const bool value){
  /* To do: this can be optimized. */
  const epsilongc_pointer_t mark_array = (const epsilongc_pointer_t)EPSILONGC_PAGE_TO_MARK_ARRAY(page);
  const epsilongc_unsigned_integer_t mark_array_size_in_bytes =
    page->kind->mark_array_size_in_bytes;
  if(!value)
    memset(mark_array, (int)0, (size_t)mark_array_size_in_bytes);
  else
    memset(mark_array, (int)255, (size_t)mark_array_size_in_bytes);
}

/* Mark all the objects in the given page: */
void epsilongc_completely_mark_page(epsilongc_page_t page) EPSILONGC_INLINE_ATTRIBUTE_IFF_AGGRESSIVE_INLINE;
void epsilongc_completely_mark_page(epsilongc_page_t page){
  epsilongc_set_all_object_mark_bits_in(page, true);
}

/* Unmark all the objects in the given page: */
void epsilongc_completely_unmark_page(epsilongc_page_t page) EPSILONGC_INLINE_ATTRIBUTE_IFF_AGGRESSIVE_INLINE;
void epsilongc_completely_unmark_page(epsilongc_page_t page){
  epsilongc_set_all_object_mark_bits_in(page, false);
}

static bool epsilongc_does_hold_for_all_pages(bool(*predicate)(const epsilongc_page_t page)){
#ifdef ENABLE_ASSERTIONS
  /* Check the predicate for each page: */
  epsilongc_page_t page;
  bool result = true;
  for(page = EPSILONGC_FIRST_ELEMENT_OF_LIST(pages, &epsilongc_all_pages);
      page != NULL;
      page = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(all_pages, page)){
    if(! predicate(page)){
      //return false; // the predicate is false for this page
      result = false;
      printf("The predicate does not hold for %p, belonging to the %s pool\n", page, page->pool->name);
    } // if
  } // for
  //return true; // the predicate was true for all pages
  return result;
#else
  epsilongc_fatal("epsilongc_does_hold_for_all_pages() requires assertions to be enabled");
#endif // #ifdef ENABLE_ASSERTIONS
}

bool epsilongc_are_all_pages_completely_unmarked(void){
  return epsilongc_does_hold_for_all_pages(epsilongc_is_page_completely_unmarked);
}
