/* Data representation: tagged backend, conditionally #include'd by data.c.

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


/* We reserve the righmost bit to discriminate between fixnums and pointers:
   - fixnum payload is shifted left by one bit
   - pointers refer at least word-aligned objects, so we need no shift: the
     rightmost bit is always 0 in untagged aligned poitners.
   Pointers refer the beginning of arrays whose first word contains the number
   of elements (no bits reserved).  Array elements are ordinary tagged objects. */

/* If EPSILON_1_FOR_POINTERS is #define'd then pointers are tagged with 1 and
   fixnums with 0.  Otherwise, the converse.  I want to measure which solution
   is more efficient, so I will implement both.
   #define'ing EPSILON_1_FOR_POINTERS is more efficient on optimum. */
//#define EPSILON_1_FOR_POINTERS

#define EPSILON_HAS_1_TAG(word) \
  (((epsilon_int)(word)) & 1)
#define EPSILON_HAS_0_TAG(word) \
  (! EPSILON_HAS_1_TAG(word))

/* Check values: */
#ifdef EPSILON_1_FOR_POINTERS
  #define EPSILON_IS_POINTER(word) \
    EPSILON_HAS_1_TAG((epsilon_int)(word))
  #define EPSILON_IS_FIXNUM(word) \
    EPSILON_HAS_0_TAG((epsilon_int)(word))
#else
  #define EPSILON_IS_POINTER(word) \
    EPSILON_HAS_0_TAG((epsilon_int)(word))
  #define EPSILON_IS_FIXNUM(word) \
    EPSILON_HAS_1_TAG((epsilon_int)(word))
#endif // #ifdef EPSILON_1_FOR_POINTERS

/* Tag into values: */
#ifdef EPSILON_1_FOR_POINTERS
  #define EPSILON_TAG_POINTER(word) \
    ((epsilon_word)(((epsilon_int)(word)) | 1))
  #define EPSILON_TAG_FIXNUM(word) \
    ((epsilon_word)(((epsilon_int)(word)) << 1))
#else
  #define EPSILON_TAG_POINTER(word) \
    ((epsilon_word)(word))
  #define EPSILON_TAG_FIXNUM(word) \
    ((epsilon_word)((((epsilon_int)(word)) << 1) | 1))
#endif // #ifdef EPSILON_1_FOR_POINTERS

/* Untag values: */
#ifdef EPSILON_1_FOR_POINTERS
  #define EPSILON_UNTAG_POINTER(word) \
    ((epsilon_word)(((epsilon_int)(word)) - 1)) // Just like ((word) & ~1)
#else
  #define EPSILON_UNTAG_POINTER(word) \
    ((epsilon_word)(word)) // nothing to do, the bit is already 0 anyway
#endif // #ifdef EPSILON_1_FOR_POINTERS
#define EPSILON_UNTAG_FIXNUM(word) \
  (((epsilon_int)(word)) >> 1)
#define EPSILON_UNTAG_UNSIGNED_FIXNUM(word) \
  (((epsilon_unsigned)(word)) >> 1)


inline bool epsilon_is_fixnum(epsilon_value value){
  return EPSILON_IS_FIXNUM(value);
}
inline bool epsilon_is_pointer(epsilon_value value){
  return EPSILON_IS_POINTER(value);
}
inline size_t epsilon_buffer_size(epsilon_value pointer_value){ // in words
  /* This is unsafe: if you call it on a fixnum, too bad.  We care for efficiency here. */
  return *((epsilon_int*)(EPSILON_UNTAG_POINTER(pointer_value)));
}

inline epsilon_int epsilon_value_to_epsilon_int(epsilon_value value){
  return EPSILON_UNTAG_FIXNUM(value);
}
inline epsilon_unsigned epsilon_value_to_epsilon_unsigned(epsilon_value value){
  return EPSILON_UNTAG_UNSIGNED_FIXNUM(value);
}
inline epsilon_value epsilon_int_to_epsilon_value(epsilon_int i){
  return EPSILON_TAG_FIXNUM(i);
}
inline epsilon_value* epsilon_value_to_value_elements(epsilon_value pointer_value){
  return ((epsilon_value*)EPSILON_UNTAG_POINTER(pointer_value)) + 1; // skip the header word
}
epsilon_thread epsilon_value_to_thread(epsilon_value value){
  return EPSILON_UNTAG_POINTER(value); // FIXME: test and ensure this is correct
}
epsilon_value epsilon_thread_to_epsilon_value(epsilon_thread thread){
  return EPSILON_TAG_POINTER(thread); // FIXME: test and ensure this is correct
}
inline epsilon_value epsilon_foreign_pointer_to_epsilon_value(void *p){
  // FIXME: tag without shifhing; make it clear that the pointed value must be aligned
  return epsilon_int_to_epsilon_value((epsilon_int)p);
}
inline void* epsilon_value_to_foreign_pointer(epsilon_value value){
  // FIXME: untag without shifhing; make it clear that the pointed value must be aligned
  return (void*)(epsilon_value_to_epsilon_int(value));
}

inline bool epsilon_value_eq(epsilon_value value1, epsilon_value value2){
  return value1 == value2; // we can safely compare without untagging
}

inline epsilon_value epsilon_manually_allocate_with_epsilon_int_length(epsilon_int length_in_words){
  epsilon_int* address = epsilon_xmalloc((length_in_words + 1) * sizeof(epsilon_value));
  *address = length_in_words;
  return EPSILON_TAG_POINTER(address);
}
inline epsilon_value epsilon_gc_allocate_with_epsilon_int_length(epsilon_int length_in_words){
  epsilon_int* address = GC_MALLOC((length_in_words + 1) * sizeof(epsilon_value));
  *address = length_in_words;
  return EPSILON_TAG_POINTER(address);
}
inline void epsilon_manually_destroy(epsilon_value pointer_value){
  free(EPSILON_UNTAG_POINTER(pointer_value));
}
inline void epsilon_gc_destroy(epsilon_value pointer_value){
  // Do nothing.
}
inline epsilon_value epsilon_load_with_epsilon_int_offset(epsilon_value pointer_value, epsilon_int offset_in_words){
  return ((epsilon_value*)EPSILON_UNTAG_POINTER(pointer_value))[offset_in_words + 1];
}
inline void epsilon_store_with_epsilon_int_offset(epsilon_value pointer_value, epsilon_int offset_in_words, epsilon_value datum){
  ((epsilon_value*)EPSILON_UNTAG_POINTER(pointer_value))[offset_in_words + 1] = datum;
}

inline void epsilon_runtime_appropriate_fail(char *reason){
  epsilon_fatal("%s", reason);
}
