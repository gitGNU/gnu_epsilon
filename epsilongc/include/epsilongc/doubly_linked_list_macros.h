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


/* This is an implementation of a doubly-linked list, entirely based on preprocessor
   macros. See test_doubly_linked_list.c for a usage example. */

#ifndef EPSILONGC_DOUBLY_LINKED_LIST_MACROS_H_
#define EPSILONGC_DOUBLY_LINKED_LIST_MACROS_H_

#include "epsilongc_types.h"
#include <stdbool.h>
#include <assert.h>
#include "fatal.h"
#include "malloc.h"

/* This is meant to be called within the definition of a list type: */
#define EPSILONGC_LIST_FIELDS(PREFIX_, ELEMENT_POINTER_TYPE_) \
  ELEMENT_POINTER_TYPE_ PREFIX_ ## _first_element; \
  ELEMENT_POINTER_TYPE_ PREFIX_ ## _last_element; \
  epsilongc_integer_t PREFIX_ ## _length;

/* This is meant to be called within the definition of an element type: */
#define EPSILONGC_LIST_ELEMENT_FIELDS(PREFIX_, ELEMENT_POINTER_TYPE_) \
  ELEMENT_POINTER_TYPE_ PREFIX_ ## _next_element; \
  ELEMENT_POINTER_TYPE_ PREFIX_ ## _previous_element;

/* Access the first element of a list: */
#define EPSILONGC_FIRST_ELEMENT_OF_LIST(PREFIX_, LIST_) \
  ((LIST_)->PREFIX_ ## _first_element)

/* Access the last element of a list: */
#define EPSILONGC_LAST_ELEMENT_OF_LIST(PREFIX_, LIST_) \
  ((LIST_)->PREFIX_ ## _last_element)

/* Length of a list: */
#define EPSILONGC_LENGTH_OF_LIST(PREFIX_, LIST_) \
  ((LIST_)->PREFIX_ ## _length)

/* Is the list empty? */
#define EPSILONGC_IS_LIST_EMPTY(PREFIX_, LIST_) \
  (EPSILONGC_LENGTH_OF_LIST(PREFIX_, LIST_) == 0)

/* The previous element of the given element, or NULL: */
#define EPSILONGC_PREVIOUS_ELEMENT_OF_ELEMENT(PREFIX_, ELEMENT_) \
  ((ELEMENT_)->PREFIX_ ## _previous_element)

/* The next element of the given element, or NULL: */
#define EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(PREFIX_, ELEMENT_) \
  ((ELEMENT_)->PREFIX_ ## _next_element)

/* Does the given element belong to the given list? */
#define EPSILONGC_DOES_ELEMENT_BELONG_TO_LIST(LIST_PREFIX_, ELEMENT_PREFIX_, ELEMENT_POINTER_TYPE_, ELEMENT_, LIST_) \
  ({ ELEMENT_POINTER_TYPE_ p_; \
     bool result_ = false; \
     for(p_ = EPSILONGC_FIRST_ELEMENT_OF_LIST(LIST_PREFIX_, LIST_); \
         p_ != NULL; \
         p_ = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(ELEMENT_PREFIX_, p_)) \
       if(p_ == (ELEMENT_)){ \
         result_ = true; \
         break; \
       } \
     result_; })

/* Initialize an already-allocated list: */
#define EPSILONGC_INITIALIZE_LIST(PREFIX_, LIST_) \
  { EPSILONGC_FIRST_ELEMENT_OF_LIST(PREFIX_, LIST_) = NULL; \
    EPSILONGC_LAST_ELEMENT_OF_LIST(PREFIX_, LIST_) = NULL; \
    EPSILONGC_LENGTH_OF_LIST(PREFIX_, LIST_) = 0; }

/* Initialize an already-allocated list element: */
#define EPSILONGC_INITIALIZE_LIST_ELEMENT(PREFIX_, ELEMENT_) \
  { EPSILONGC_PREVIOUS_ELEMENT_OF_ELEMENT(PREFIX_, ELEMENT_) = NULL; \
    EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(PREFIX_, ELEMENT_) = NULL; }

/* Finalize a list without deallocating it: */
#define EPSILONGC_FINALIZE_LIST(PREFIX_, LIST_) \
  { assert(EPSILONGC_FIRST_ELEMENT_OF_LIST(PREFIX_, LIST_) == NULL); \
    assert(EPSILONGC_LAST_ELEMENT_OF_LIST(PREFIX_, LIST_) == NULL); \
    assert(EPSILONGC_LENGTH_OF_LIST(PREFIX_, LIST_) == 0); }

/* Allocate and initialize a list: */
#define EPSILONGC_MAKE_LIST(PREFIX_, LIST_NONPOINTER_TYPE_) \
  ({ LIST_NONPOINTER_TYPE_ *result_ = (LIST_NONPOINTER_TYPE_*) \
       epsilongc_xmalloc(sizeof(LIST_NONPOINTER_TYPE_)); \
     EPSILONGC_INITIALIZE_LIST(PREFIX_, result_); \
     result_; })

/* Detach all the elements of the given list, but don't deallocate them,
   and make the list empty (but don't deallocate the list): */
#define EPSILONGC_DETACH_ELEMENTS_OF_LIST(LIST_PREFIX_, ELEMENT_PREFIX_, ELEMENT_POINTER_TYPE_, LIST_) \
  { ELEMENT_POINTER_TYPE_ p_ = \
      EPSILONGC_FIRST_ELEMENT_OF_LIST(LIST_PREFIX_, LIST_);  \
    while(p_ != NULL){ \
      ELEMENT_POINTER_TYPE_ next_p_ = \
        EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(ELEMENT_PREFIX_, p_); \
      EPSILONGC_DETACH_ELEMENT_FROM_LIST(LIST_PREFIX_, ELEMENT_PREFIX_, ELEMENT_POINTER_TYPE_, p_, LIST_); \
      p_ = next_p_; \
    } \
    EPSILONGC_INITIALIZE_LIST(LIST_PREFIX_, LIST_); }

/* Detach and deallocate the elements of the given list with the given function,
   and make the list empty (but don't deallocate the list): */
#define EPSILONGC_DESTROY_ELEMENTS_OF_LIST(LIST_PREFIX_, ELEMENT_PREFIX_, ELEMENT_POINTER_TYPE_, LIST_, ELEMENT_DESTROYER_) \
  { ELEMENT_POINTER_TYPE_ p_ = \
      EPSILONGC_FIRST_ELEMENT_OF_LIST(LIST_PREFIX_, LIST_);  \
    while(p_ != NULL){ \
      ELEMENT_POINTER_TYPE_ next_p_ = \
        EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(ELEMENT_PREFIX_, p_); \
      EPSILONGC_DETACH_ELEMENT_FROM_LIST(LIST_PREFIX_, ELEMENT_PREFIX_, ELEMENT_POINTER_TYPE_, p_, LIST_); \
      ELEMENT_DESTROYER_(p_); \
      p_ = next_p_; \
    } \
    EPSILONGC_INITIALIZE_LIST(LIST_PREFIX_, LIST_); }

/* Destroy and deallocate the elements of the given list with the given function,
   and also the list itself: */
#define EPSILONGC_DESTROY_LIST_AND_ELEMENTS(LIST_PREFIX_, ELEMENT_PREFIX_, ELEMENT_POINTER_TYPE_, LIST_, ELEMENT_DESTROYER_) \
  { EPSILONGC_DESTROY_ELEMENTS_OF_LIST(LIST_PREFIX_, ELEMENT_PREFIX_, ELEMENT_POINTER_TYPE_, \
                                       LIST_, ELEMENT_DESTROYER_); \
    free(LIST_); }

/* Prepend the given object to the given list, making it its new first element: */
#define EPSILONGC_PREPEND_OBJECT_TO_LIST(LIST_PREFIX_, ELEMENT_PREFIX_, ELEMENT_POINTER_TYPE_, OBJECT_, LIST_) \
  { if(EPSILONGC_FIRST_ELEMENT_OF_LIST(LIST_PREFIX_, LIST_) != NULL) \
      EPSILONGC_PREVIOUS_ELEMENT_OF_ELEMENT(ELEMENT_PREFIX_, \
                                            EPSILONGC_FIRST_ELEMENT_OF_LIST(LIST_PREFIX_, LIST_)) = \
        (OBJECT_); \
    else \
      EPSILONGC_LAST_ELEMENT_OF_LIST(LIST_PREFIX_, LIST_) = (OBJECT_); \
    EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(ELEMENT_PREFIX_, OBJECT_) = \
      EPSILONGC_FIRST_ELEMENT_OF_LIST(LIST_PREFIX_, LIST_); \
    EPSILONGC_PREVIOUS_ELEMENT_OF_ELEMENT(ELEMENT_PREFIX_, OBJECT_) = NULL; \
    EPSILONGC_FIRST_ELEMENT_OF_LIST(LIST_PREFIX_, LIST_) = (OBJECT_); \
    EPSILONGC_LENGTH_OF_LIST(LIST_PREFIX_, LIST_) ++; }

/* Append the given object to the given list, making it its new last element: */
#define EPSILONGC_APPEND_OBJECT_TO_LIST(LIST_PREFIX_, ELEMENT_PREFIX_, ELEMENT_POINTER_TYPE_, OBJECT_, LIST_) \
  { if(EPSILONGC_LAST_ELEMENT_OF_LIST(LIST_PREFIX_, LIST_) != NULL) \
      EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(ELEMENT_PREFIX_, \
                                        EPSILONGC_LAST_ELEMENT_OF_LIST(LIST_PREFIX_, LIST_)) = \
        (OBJECT_); \
    else \
      EPSILONGC_FIRST_ELEMENT_OF_LIST(LIST_PREFIX_, LIST_) = (OBJECT_); \
    EPSILONGC_PREVIOUS_ELEMENT_OF_ELEMENT(ELEMENT_PREFIX_, OBJECT_) = \
      EPSILONGC_LAST_ELEMENT_OF_LIST(LIST_PREFIX_, LIST_); \
    EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(ELEMENT_PREFIX_, OBJECT_) = NULL; \
    EPSILONGC_LAST_ELEMENT_OF_LIST(LIST_PREFIX_, LIST_) = (OBJECT_); \
    EPSILONGC_LENGTH_OF_LIST(LIST_PREFIX_, LIST_) ++; }

/* Call the given function on each element of the list, starting from the first one: */
#define EPSILONGC_CALL_ON_EACH_ELEMENT_OF_LIST(LIST_PREFIX_, ELEMENT_PREFIX_, ELEMENT_POINTER_TYPE_, FUNCTION_, LIST_) \
  { ELEMENT_POINTER_TYPE_ p_; \
    for(p_ = EPSILONGC_FIRST_ELEMENT_OF_LIST(LIST_PREFIX_, LIST_); \
        p_ != NULL; \
        p_ = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(ELEMENT_PREFIX_, p_)) \
      (FUNCTION_)(p_); }

/* Detach the given element from the list (the element is assumed to belong to the list); don't
   deallocate the element, but reset its previous and next pointers to NULL: */
#define EPSILONGC_DETACH_ELEMENT_FROM_LIST(LIST_PREFIX_, ELEMENT_PREFIX_, ELEMENT_POINTER_TYPE_, ELEMENT_, LIST_) \
  { ELEMENT_POINTER_TYPE_ previous_element_ = \
      EPSILONGC_PREVIOUS_ELEMENT_OF_ELEMENT(ELEMENT_PREFIX_, ELEMENT_); \
    ELEMENT_POINTER_TYPE_ next_element_ = \
      EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(ELEMENT_PREFIX_, ELEMENT_); \
    if(previous_element_ != NULL) \
      EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(ELEMENT_PREFIX_, previous_element_) = next_element_; \
    else \
      EPSILONGC_FIRST_ELEMENT_OF_LIST(LIST_PREFIX_, LIST_) = next_element_; \
    if(next_element_ != NULL) \
      EPSILONGC_PREVIOUS_ELEMENT_OF_ELEMENT(ELEMENT_PREFIX_, next_element_) = previous_element_; \
    else \
      EPSILONGC_LAST_ELEMENT_OF_LIST(LIST_PREFIX_, LIST_) = previous_element_; \
    EPSILONGC_PREVIOUS_ELEMENT_OF_ELEMENT(ELEMENT_PREFIX_, ELEMENT_) = NULL; \
    EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(ELEMENT_PREFIX_, ELEMENT_) = NULL; \
    EPSILONGC_LENGTH_OF_LIST(LIST_PREFIX_, LIST_) --; }

/* Update the given lists by destructively prepending the second list to the first list. The second
   list is made empty but not deallocated: */
#define EPSILONGC_PREPEND_SECOND_LIST_TO_FIRST_LIST(LIST_PREFIX1_, LIST_PREFIX2_, ELEMENT_PREFIX_, ELEMENT_POINTER_TYPE_, LIST1_, LIST2_) \
  { ELEMENT_POINTER_TYPE_ first_element_of_list1_ = \
      EPSILONGC_FIRST_ELEMENT_OF_LIST(LIST_PREFIX1_, LIST1_); \
    ELEMENT_POINTER_TYPE_ last_element_of_list2_ = \
      EPSILONGC_LAST_ELEMENT_OF_LIST(LIST_PREFIX2_, LIST2_); \
    if(first_element_of_list1_ != NULL) \
      EPSILONGC_PREVIOUS_ELEMENT_OF_ELEMENT(ELEMENT_PREFIX_, first_element_of_list1_) = last_element_of_list2_; \
    else \
      EPSILONGC_LAST_ELEMENT_OF_LIST(LIST_PREFIX1_, LIST1_) = last_element_of_list2_; \
    if(last_element_of_list2_ != NULL){ \
      EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(ELEMENT_PREFIX_, last_element_of_list2_) = first_element_of_list1_; \
      EPSILONGC_FIRST_ELEMENT_OF_LIST(LIST_PREFIX1_, LIST1_) = \
        EPSILONGC_FIRST_ELEMENT_OF_LIST(LIST_PREFIX2_, LIST2_); \
    } \
    EPSILONGC_LENGTH_OF_LIST(LIST_PREFIX1_, LIST1_) += EPSILONGC_LENGTH_OF_LIST(LIST_PREFIX2_, LIST2_); \
    EPSILONGC_LENGTH_OF_LIST(LIST_PREFIX2_, LIST2_) = 0; \
    EPSILONGC_FIRST_ELEMENT_OF_LIST(LIST_PREFIX2_, LIST2_) = NULL; \
    EPSILONGC_LAST_ELEMENT_OF_LIST(LIST_PREFIX2_, LIST2_) = NULL; }

/* Update the given lists by destructively appending the second list to the first list. The second
   list is made empty but not deallocated: */
#define EPSILONGC_APPEND_SECOND_LIST_TO_FIRST_LIST(LIST_PREFIX1_, LIST_PREFIX2_, ELEMENT_PREFIX_, ELEMENT_POINTER_TYPE_, LIST1_, LIST2_) \
  { ELEMENT_POINTER_TYPE_ last_element_of_list1_ = \
      EPSILONGC_LAST_ELEMENT_OF_LIST(LIST_PREFIX1_, LIST1_); \
    ELEMENT_POINTER_TYPE_ first_element_of_list2_ = \
      EPSILONGC_FIRST_ELEMENT_OF_LIST(LIST_PREFIX2_, LIST2_); \
    if(last_element_of_list1_ != NULL) \
      EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(ELEMENT_PREFIX_, last_element_of_list1_) = first_element_of_list2_; \
    else \
      EPSILONGC_FIRST_ELEMENT_OF_LIST(LIST_PREFIX1_, LIST1_) = first_element_of_list2_; \
    if(first_element_of_list2_ != NULL){ \
      EPSILONGC_PREVIOUS_ELEMENT_OF_ELEMENT(ELEMENT_PREFIX_, first_element_of_list2_) = last_element_of_list1_; \
      EPSILONGC_LAST_ELEMENT_OF_LIST(LIST_PREFIX1_, LIST1_) = \
        EPSILONGC_LAST_ELEMENT_OF_LIST(LIST_PREFIX2_, LIST2_); \
    } \
    EPSILONGC_LENGTH_OF_LIST(LIST_PREFIX1_, LIST1_) += EPSILONGC_LENGTH_OF_LIST(LIST_PREFIX2_, LIST2_); \
    EPSILONGC_LENGTH_OF_LIST(LIST_PREFIX2_, LIST2_) = 0; \
    EPSILONGC_LAST_ELEMENT_OF_LIST(LIST_PREFIX2_, LIST2_) = NULL; \
    EPSILONGC_FIRST_ELEMENT_OF_LIST(LIST_PREFIX2_, LIST2_) = NULL; }

/* Return the n-th element of the given list. This is 0-based and counts from the first
   element to the last one: */
#define EPSILONGC_NTH_ELEMENT_OF_LIST(LIST_PREFIX_, ELEMENT_PREFIX_, ELEMENT_POINTER_TYPE_, INDEX_, LIST_) \
  ({ epsilongc_integer_t i_; \
     ELEMENT_POINTER_TYPE_ p_; \
     for(p_ = EPSILONGC_FIRST_ELEMENT_OF_LIST(LIST_PREFIX_, LIST_), i_ = 0; \
         i_ != INDEX_; \
         p_ = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(ELEMENT_PREFIX_, p_), i_ ++) \
       if(p_ == NULL) \
         epsilongc_fatal("EPSILONGC_NTH_ELEMENT_OF_LIST: out of range"); \
     p_; })

/* Return the n-th element of the given list. This is 0-based and counts from the last
   element back to the first one: */
#define EPSILONGC_NTH_ELEMENT_OF_LIST_FROM_THE_END(LIST_PREFIX_, ELEMENT_PREFIX_, ELEMENT_POINTER_TYPE_, INDEX_, LIST_) \
  ({ epsilongc_integer_t i_; \
     ELEMENT_POINTER_TYPE_ p_; \
     for(p_ = EPSILONGC_LAST_ELEMENT_OF_LIST(LIST_PREFIX_, LIST_), i_ = 0; \
         i_ != INDEX_; \
         p_ = EPSILONGC_PREVIOUS_ELEMENT_OF_ELEMENT(ELEMENT_PREFIX_, p_), i_ ++) \
       if(p_ == NULL) \
         epsilongc_fatal("EPSILONGC_NTH_ELEMENT_OF_LIST: out of range"); \
     p_; })

#endif // #ifndef EPSILONGC_DOUBLY_LINKED_LIST_MACROS_H_
