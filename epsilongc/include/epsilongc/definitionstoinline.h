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


/* See the comments in stufftoinline.h. Multiple inclusions of this are
   not prevented, and they are meant not to be. */

#include "declarationstoinline.h"

EPSILONGC_FUNCTION_QUALIFIERS_FOR_AGGRESSIVE_INLINING epsilongc_word_t
epsilongc_allocate_from(struct epsilongc_allocator *allocator){
  /* /\* Is there an free object in the current page? If so, that is the result. */
  /*    This is by far the most frequent case. The GCC optimizer hint is */
  /*    important here, as this code is very performance-crictical. */
  /*    If the current page is full we try again after having obtained a new */
  /*    page from the pool, until we succeed: *\/ */
  //printf("Allocating from the allocator %p\n", allocator);
  /* epsilongc_word_t result_or_NULL; */
  /* while(EPSILONGC_UNLIKELY((result_or_NULL = // this is only true when a page gets completely filled... */
  /*                           EPSILONGC_UNCONCEAL_POINTER(allocator->next_free_object)) == NULL)){ */
  /*   epsilongc_replace_page_with_appropriate_locking(allocator); */
  /* } // while */
  
  /* The result is the next object on the free list, if any. The GCC
     optimizer hint is important here, as this code is performance-critical.
     If the current page is full then we replace it: */
  epsilongc_word_t result_or_NULL =
    EPSILONGC_UNCONCEAL_POINTER(allocator->next_free_object);
  if(EPSILONGC_UNLIKELY(result_or_NULL == NULL)){
    epsilongc_replace_page_with_appropriate_locking(allocator);
    result_or_NULL =
      EPSILONGC_UNCONCEAL_POINTER(allocator->next_free_object);
  } // if
#ifdef ENABLE_ASSERTIONS
  /* Replacing the page *must* yield a page with some empty space: */
  assert(result_or_NULL != NULL);
#endif // #ifdef ENABLE_ASSERTIONS
  
  /* Ok, we have a non-NULL result now; advance the free list pointer,
     and we're done. If allocations from this allocator are frequent it's
     very likely that next_free_object will still be in the primary cache at
     the time of the next call: */
  //printf("Q1: result_or_NULL is %p\n", result_or_NULL);
  //assert(result_or_NULL != NULL);
  allocator->next_free_object = *((epsilongc_word_t*)result_or_NULL);
  
  //printf("Allocated from the allocator %p: the result is %p\n", allocator, result_or_NULL);
  return result_or_NULL;
}

EPSILONGC_FUNCTION_QUALIFIERS_FOR_AGGRESSIVE_INLINING epsilongc_page_t
epsilongc_candidate_pointer_to_candidate_page(const epsilongc_word_t pointer){
  /* Just perform a bitmask operation on the address, clearing the right number
     of least-significant bits; this relies on the alignment of pages: */
  return (epsilongc_page_t)
    (((epsilongc_unsigned_integer_t)pointer) & EPSILONGC_PAGE_BITMASK);
}

EPSILONGC_FUNCTION_QUALIFIERS_FOR_AGGRESSIVE_INLINING epsilongc_kind_tag_t
epsilongc_object_to_tag(const epsilongc_word_t object){
  const epsilongc_page_t page =
    epsilongc_candidate_pointer_to_candidate_page(object);
  return page->tag;
}

EPSILONGC_FUNCTION_QUALIFIERS_FOR_AGGRESSIVE_INLINING epsilongc_kind_datum_t
epsilongc_object_to_datum(const epsilongc_word_t object){
  const epsilongc_page_t page =
    epsilongc_candidate_pointer_to_candidate_page(object);
  return page->datum;
}

EPSILONGC_FUNCTION_QUALIFIERS_FOR_AGGRESSIVE_INLINING epsilongc_integer_t
epsilongc_object_to_size_in_words(const epsilongc_word_t object){
  const epsilongc_page_t page =
    epsilongc_candidate_pointer_to_candidate_page(object);
  return page->object_size_in_words;
}
