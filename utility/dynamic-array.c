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


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "hints.h"
#include "fatal.h"
#include "malloc.h"
#include "dynamic-array.h"

void epsilon_initialize_dynamic_array(epsilon_dynamic_array_t array){
  const size_t size_in_bytes = sizeof(void*); // an arbitrary, small size
  char *buffer = (char*)epsilon_xmalloc(size_in_bytes);
  array->buffer = buffer;
  array->first_unused_byte_in_the_buffer = buffer;
  array->first_byte_after_the_allocated_buffer = buffer + size_in_bytes;
}

void epsilon_finalize_dynamic_array(epsilon_dynamic_array_t array){
  free(array->buffer);
  array->buffer = NULL; /* This is just to prevent bugs, of course.  It would also
                           help when using malloc() from Boehm's GC */
  array->first_unused_byte_in_the_buffer = NULL; // to prevent later use
  array->first_byte_after_the_allocated_buffer = NULL; // to prevent later use
}

epsilon_dynamic_array_t epsilon_make_dynamic_array(void){
  epsilon_dynamic_array_t result = (epsilon_dynamic_array_t)
    epsilon_xmalloc(sizeof(struct epsilon_dynamic_array));
  epsilon_initialize_dynamic_array(result);
  return result;
}

void epsilon_destroy_dynamic_array(epsilon_dynamic_array_t array){
  epsilon_finalize_dynamic_array(array);
  free(array);
}

void* epsilon_dynamic_array_to_array(epsilon_dynamic_array_t array){
  /* This should cost just one load instruction, excluding the call: */
  return (void*)(array->buffer);
}

size_t epsilon_dynamic_array_allocated_size(const epsilon_dynamic_array_t array){
  return array->first_byte_after_the_allocated_buffer - array->buffer;
}

size_t epsilon_dynamic_array_used_size(const epsilon_dynamic_array_t array){
  return array->first_unused_byte_in_the_buffer - array->buffer;
}

void epsilon_resize_dynamic_array_to_the_exact_size(epsilon_dynamic_array_t array,
                                                    size_t new_buffer_size_in_bytes){
  /* Keep track of the old sizes: */
  const size_t old_allocated_size = epsilon_dynamic_array_allocated_size(array);
  const size_t old_used_size = epsilon_dynamic_array_used_size(array);
  
  /* Do nothing if the size is already what the user is asking: */
  if(old_allocated_size == new_buffer_size_in_bytes)
    return;
  
  /* Resize the buffer: */
  char *new_buffer = (char*)epsilon_xrealloc(array->buffer, new_buffer_size_in_bytes);
  array->buffer = new_buffer;
  
  /* Recompute pointers within the new buffer: */
  array->first_byte_after_the_allocated_buffer = new_buffer + new_buffer_size_in_bytes;
  const size_t new_used_size =
    (new_buffer_size_in_bytes < old_used_size) ? // did the resizing truncate used data?
    new_buffer_size_in_bytes : // yes
    old_used_size; // no, we can still hold the old data
  array->first_unused_byte_in_the_buffer = new_buffer + new_used_size;
}

void epsilon_resize_dynamic_array(epsilon_dynamic_array_t array,
                                  size_t new_recommended_buffer_size_in_bytes){
  /* Do nothing if the array is already large enough: */
  const size_t old_allocated_size = epsilon_dynamic_array_allocated_size(array);
  if(old_allocated_size >= new_recommended_buffer_size_in_bytes)
    return;
  
  /* Otherwise, compute a new size. We proceed by (conceptually) doubling the
     current size till we reach or exceed the user recommendation.  Resizing is
     expensive, so we want to do a few big ones, rather than many small ones. */
  size_t new_allocated_size_in_bytes =
    (old_allocated_size > 0) ? old_allocated_size : 1;
  while(new_allocated_size_in_bytes < new_recommended_buffer_size_in_bytes)
    new_allocated_size_in_bytes *= 2;
  
  /* Ok, we have computed the new size we want.  Resize to that exact size now: */
  epsilon_resize_dynamic_array_to_the_exact_size(array, new_allocated_size_in_bytes);
}

void epsilon_make_place_for_in_dynamic_array(epsilon_dynamic_array_t array,
                                             size_t bytes_expected_to_be_added){
  const size_t current_allocated_size = epsilon_dynamic_array_allocated_size(array);
  const size_t current_used_size = epsilon_dynamic_array_used_size(array);
  if(current_used_size + bytes_expected_to_be_added > current_allocated_size)
    epsilon_resize_dynamic_array(array, current_used_size + bytes_expected_to_be_added);
}

void* epsilon_push_onto_dynamic_array(epsilon_dynamic_array_t array,
                                     void *new_buffer, size_t new_buffer_size_in_bytes){
  /* We are gonna add new_buffer_size_in_bytes bytes: resize if neeeded: */
  epsilon_make_place_for_in_dynamic_array(array, new_buffer_size_in_bytes);
  
  /* Hold a pointer to the beginning of the new copy: */
  void *beginning_of_the_copy = (void*)array->first_unused_byte_in_the_buffer;
  
  /* Append the new bytes, and move the used pointer forward: */
  memcpy(array->first_unused_byte_in_the_buffer, new_buffer, new_buffer_size_in_bytes);
  array->first_unused_byte_in_the_buffer += new_buffer_size_in_bytes;

  return beginning_of_the_copy;
}

void* epsilon_reserve_from_dynamic_array(epsilon_dynamic_array_t array,
                                        size_t reserved_bytes){
  /* This is similar to a push, except that we don't write into the buffer: */
  epsilon_make_place_for_in_dynamic_array(array, reserved_bytes);
  void* beginning_of_the_reserved_space = array->first_unused_byte_in_the_buffer;
  array->first_unused_byte_in_the_buffer += reserved_bytes;
  return beginning_of_the_reserved_space;
}

void* epsilon_pop_from_dynamic_array(epsilon_dynamic_array_t array,
                                    size_t how_many_bytes_should_be_removed){
  array->first_unused_byte_in_the_buffer -= how_many_bytes_should_be_removed;
  
  /* Return the new top: */
  return (void*)(array->first_unused_byte_in_the_buffer);
}

void* epsilon_top_of_dynamic_array(epsilon_dynamic_array_t array,
                                   size_t topmost_element_size_in_bytes){
  char *result_as_char_star = 
    ((char*)(epsilon_first_unused_byte_in_dynamic_array(array))) -
    topmost_element_size_in_bytes;
  return (void*)result_as_char_star;
}


void* epsilon_first_unused_byte_in_dynamic_array(const epsilon_dynamic_array_t array){
  return (void*)(array->first_unused_byte_in_the_buffer);
}
