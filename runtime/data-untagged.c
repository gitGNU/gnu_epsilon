/* Data representation: untagged backend, conditionally #include'd by data.c.

   Copyright (C) 2012 Luca Saiu
   Updated in 2014 by Luca Saiu
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


/* The needed headers have already been included in data.c; they are the
   same for data-untagged.h and data-tagged.h. */

/* Representation is completely trivial here: both pointers and
   fixnums are represented as single words with no tagging at all.
   Buffers do not hold their size. */

inline bool epsilon_is_fixnum(epsilon_value value){
  epsilon_fatal("epsilon_is_fixnum() called on an untagged backend");
}
inline bool epsilon_is_pointer(epsilon_value value){
  epsilon_fatal("epsilon_is_pointer() called on an untagged backend");
}
inline size_t epsilon_buffer_size(epsilon_value pointer_value){ // in words
  epsilon_fatal("epsilon_buffer_size() called on an untagged backend");
}

#ifndef EPSILON_RUNTIME_UNTAGGED
inline epsilon_int epsilon_value_to_epsilon_int(epsilon_value value){
  return (epsilon_int)value;
}
inline epsilon_unsigned epsilon_value_to_epsilon_unsigned(epsilon_value value){
  return (epsilon_unsigned)value;
}
inline epsilon_value epsilon_int_to_epsilon_value(epsilon_int i){
  return (epsilon_value)i;
}
#endif //#ifndef EPSILON_RUNTIME_UNTAGGED
/* inline epsilon_value* epsilon_value_to_value_elements(epsilon_value pointer_value){ */
/*   return (epsilon_value*)pointer_value; */
/* } */
epsilon_thread epsilon_value_to_thread(epsilon_value value){
  return value; // FIXME: test and ensure this is correct
}
epsilon_value epsilon_thread_to_epsilon_value(epsilon_thread thread){
  return thread; // FIXME: test and ensure this is correct
}
/* inline epsilon_value epsilon_foreign_pointer_to_epsilon_value(void *p){ */
/*   return epsilon_int_to_epsilon_value((epsilon_int)p); */
/* } */
/* inline void* epsilon_value_to_foreign_pointer(epsilon_value value){ */
/*   return (void*)(epsilon_value_to_epsilon_int(value)); */
/* } */

#ifndef EPSILON_RUNTIME_UNTAGGED
inline bool epsilon_value_eq(epsilon_value value1, epsilon_value value2){
  return value1 == value2;
}
#endif //#ifndef EPSILON_RUNTIME_UNTAGGED

inline epsilon_value epsilon_manually_allocate_with_epsilon_int_length(epsilon_int length_in_words){
  //  return epsilon_xmalloc(length_in_words * sizeof(epsilon_value));
  return epsilon_gc_allocate_with_epsilon_int_length(length_in_words);
}
inline epsilon_value epsilon_gc_allocate_with_epsilon_int_length(epsilon_int length_in_words){
  return GC_MALLOC(length_in_words * sizeof(epsilon_value));
}
inline void epsilon_manually_destroy(epsilon_value pointer_value){
  //free(pointer_value);
  epsilon_gc_destroy(pointer_value);
}
inline void epsilon_gc_destroy(epsilon_value pointer_value){
  // Do nothing.
}
#ifndef EPSILON_RUNTIME_UNTAGGED
inline epsilon_value epsilon_load_with_epsilon_int_offset(epsilon_value pointer_value, epsilon_int offset_in_words){
  return ((epsilon_value*)pointer_value)[offset_in_words];
}
inline void epsilon_store_with_epsilon_int_offset(epsilon_value pointer_value, epsilon_int offset_in_words, epsilon_value datum){
  ((epsilon_value*)pointer_value)[offset_in_words] = datum;
}
#endif //#ifndef EPSILON_RUNTIME_UNTAGGED

inline void epsilon_runtime_appropriate_fail(char *reason){
  epsilon_fatal("%s", reason);
}
