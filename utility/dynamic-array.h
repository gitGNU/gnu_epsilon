/* Simple resizable arrays.

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


#ifndef EPSILON_UTILITY_DYNAMIC_ARRAY_H_
#define EPSILON_UTILITY_DYNAMIC_ARRAY_H_

#include <unistd.h>
#include <stdlib.h>

/* This is an array implementation which can be efficiently grown and
   shrunk, and used as a stack.  The array automatically grows when
   elements are added at the end, but it never automatically shrinks.
   Elements may even be non-homogeneous; it's the user's responsibility
   to interpret the buffer content.
   Notice that the exact growth rate is not specified.
   No bound checking is ever performed. */


/* The struct definition is public for performance reasons.  It's useful
   for the user to be able to directly declare variables of this type, to
   save an indirection. */
struct epsilon_dynamic_array{
  char *buffer;
  
  char *first_unused_byte_in_the_buffer;
  char *first_byte_after_the_allocated_buffer;
}; // struct

typedef struct epsilon_dynamic_array* epsilon_dynamic_array_t;

/* Return a pointer to the buffer currently implementing the dynamic array.
   Of course the returned pointer is not guaranteed to be still valid after
   the array is resized. */
void* epsilon_dynamic_array_to_array(epsilon_dynamic_array_t array);

/* Create a new dynamic_array: */
epsilon_dynamic_array_t epsilon_make_dynamic_array(void)
  __attribute__(( malloc ));

/* Finalize and then destroy the given dynamic array, which must have
   been created with epsilon_make_dynamic_array, rather than just
   initialized: */
void epsilon_destroy_dynamic_array(epsilon_dynamic_array_t array);

/* We can also work with auto or static structs by using -initialize and
   -finalize functions, which is potentially faster; the array contents of
   course remain heap-allocated. */

/* Initialize an existing but not yet used struct epsilon_dynamic_array: */
void epsilon_initialize_dynamic_array(epsilon_dynamic_array_t array);

/* Finalize a dynamic array whose struct was *not* created by epsilon_make_dynamic_array().
   This destroys the buffer, but not the struct. */
void epsilon_finalize_dynamic_array(epsilon_dynamic_array_t array);

/* Destructively append an object to a dynamic array.  The object is copied byte-by-byte
   and the array is grown as needed.  The function returns the beginning of the just-copied
   buffer: */
void* epsilon_push_onto_dynamic_array(epsilon_dynamic_array_t array,
                                      void *new_buffer, size_t new_buffer_size_in_bytes);

/* This macro makes easy to push an object of any size into a dynamic array: */
#define EPSILON_PUSH_ONTO_DYNAMIC_ARRAY(ARRAY, OBJECT) \
  do { \
    typeof(OBJECT) wH__atE_VeR = OBJECT; \
    epsilon_push_onto_dynamic_array((ARRAY), &wH__atE_VeR, sizeof(wH__atE_VeR)); \
  } while(0)

/* Reserve the given number of bytes past the end of the used space in the buffer, resizing
   it in the process if needed.  The reserved space in the buffer is not touched by this
   function, but it is marked as "used".  The function returns the beginning of the reserved
   space: */
void* epsilon_reserve_from_dynamic_array(epsilon_dynamic_array_t array,
                                         size_t reserved_bytes);

/* Remove the given number of bytes from the end of the used part of the given dynamic array.
   Return the new top of the stack (the first unused byte): */
void* epsilon_pop_from_dynamic_array(epsilon_dynamic_array_t array,
                                     size_t how_many_bytes_should_be_removed);

/* Return the address of the topmost element of an array (used as a stack),
   given the array and the size of the topmost element: */
void* epsilon_top_of_dynamic_array(epsilon_dynamic_array_t array,
                                   size_t topmost_element_size_in_bytes);

/* Resize the array (if needed) so that it can hold at least the given number of bytes.
   The exact new allocated size of the buffer after a call to this function is not
   specified and may be larger than requested; this function *never* shrinks the buffer.
   This is particularly useful when the caller knows in advance that many small appends
   will take place. One single resize is faster than many incremental increases.*/
void epsilon_resize_dynamic_array(epsilon_dynamic_array_t array,
                                  size_t new_recommended_buffer_size_in_bytes);

/* Resize the array, if needed, so that it can hold at least the given number of bytes
   in addition to its current content.
   Do nothing if the array is already large enough.
   This is particularly useful when the caller knows in advance that many small appends
   will take place. One single resize is faster than many incremental increases. */
void epsilon_make_place_for_in_dynamic_array(epsilon_dynamic_array_t array,
                                             size_t bytes_expected_to_be_added);

/* Resize the array (if needed) so that it can hold exactly the given number of bytes.  If the
   current "used" part is greater than that, then it is automatically shrunk so that all the
   buffer is used.  Note that this is the only way of shrinking the buffer. */
void epsilon_resize_dynamic_array_to_the_exact_size(epsilon_dynamic_array_t array,
                                                    size_t new_buffer_size_in_bytes);

/* Return the current size of the buffer, in bytes: */
size_t epsilon_dynamic_array_allocated_size(const epsilon_dynamic_array_t array);

/* Return the current number of used bytes: */
size_t epsilon_dynamic_array_used_size(const epsilon_dynamic_array_t array);

/* Return the address of the first unused byte, right after the current last piece of
   payload: */
void* epsilon_first_unused_byte_in_dynamic_array(const epsilon_dynamic_array_t array);

#endif // #ifndef EPSILON_UTILITY_DYNAMIC_ARRAY_H_
