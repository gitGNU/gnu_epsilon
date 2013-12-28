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


#include "kind.h"
#include "page.h"
#include "compile_time_parameters.h"
#include "malloc.h"
#include "fatal.h"
#include <assert.h>
#include <limits.h>
#include <math.h>

/* Return the size of the mark array in bytes, for the given kind.
   Depening on configure-time options the array may be made of words,
   bytes or bits; in any case the correct number of *bytes* is
   returned, already rounded up in the case of bit arrays.
   No other padding is introduced here. */
static epsilongc_integer_t epsilongc_mark_array_size_in_bytes(epsilongc_kind_t kind){
  const epsilongc_unsigned_integer_t objects_no_per_page =
    kind->objects_no_per_page;
#if defined(ENABLE_MARK_BITS)
  /* One bit per object; the size is then rounded up to be
     equal to a whole number of bytes: */
  return ((objects_no_per_page / CHAR_BIT) +
          (((objects_no_per_page % CHAR_BIT) == 0) ? 0 : 1));
#elif defined(ENABLE_MARK_BYTES)
  /* One byte per object: */
  return objects_no_per_page;
#elif defined(ENABLE_MARK_WORDS)
  /* One word per object: */
  return objects_no_per_page * sizeof(epsilongc_word_t);
#else
#error "Either ENABLE_MARK_BITS, ENABLE_MARK_BYTES or ENABLE_MARK_WORDS must be #define'd"
#endif
}

/* Return the number of unused bytes at the end of the payload for pages
   of type kind. This *must* be signed, because it can be used for invalid
   configurations where the result is negative: */
epsilongc_integer_t epsilongc_wasted_bytes_no_at_the_end(epsilongc_kind_t kind){
  const size_t object_size_in_bytes =
    kind->object_size_in_words * sizeof(epsilongc_word_t);
  const epsilongc_integer_t result =
    (EPSILONGC_PAGE_SIZE_IN_BYTES -
     kind->first_object_offset_in_bytes -
     kind->objects_no_per_page * object_size_in_bytes);
  return result;
}

void epsilongc_display_kind_information(epsilongc_kind_t kind){
  const size_t object_size_in_bytes =
    kind->object_size_in_words * sizeof(epsilongc_word_t);
  const size_t object_alignment_in_bytes =
    kind->object_alignment_in_words * sizeof(epsilongc_word_t);
  const epsilongc_signed_integer_t wasted_bytes_at_the_end =
    epsilongc_wasted_bytes_no_at_the_end(kind);
  printf("\n----------- Kind information: begin ----------\n");
  printf("Page size:                  %i bytes\n", (int)EPSILONGC_PAGE_SIZE_IN_BYTES);
  printf("Header size:                %i bytes\n", (int)EPSILONGC_PAGE_HEADER_SIZE_IN_BYTES);
  printf("Object size:                %i bytes\n", (int)object_size_in_bytes);
  printf("Alignment:                  %i bytes\n", (int)object_alignment_in_bytes);
  printf("Objects no. per page:       %i\n", (int)kind->objects_no_per_page);
  printf("Padding before the payload: %i bytes\n", (int)kind->padding_size_in_bytes);
  printf("Offset of the first object: %i bytes\n", (int)kind->first_object_offset_in_bytes);
  printf("Wasted space at the end:    %i bytes\n", (int)wasted_bytes_at_the_end);
  const float overhead_within_the_page =
    (float)(EPSILONGC_PAGE_HEADER_SIZE_IN_BYTES +
#ifndef ENABLE_OUT_OF_PAGE_MARK_ARRAY
            epsilongc_mark_array_size_in_bytes(kind) +
#endif // #ifndef ENABLE_OUT_OF_PAGE_MARK_ARRAY
            kind->padding_size_in_bytes +
            wasted_bytes_at_the_end) /
    (float)EPSILONGC_PAGE_SIZE_IN_BYTES; 
  const float total_overhead =
    (float)(EPSILONGC_PAGE_HEADER_SIZE_IN_BYTES +
#ifdef ENABLE_OUT_OF_PAGE_MARK_ARRAY
            epsilongc_mark_array_size_in_bytes(kind) +
#endif // #ifdef ENABLE_OUT_OF_PAGE_MARK_ARRAY
            kind->padding_size_in_bytes +
            wasted_bytes_at_the_end) /
    (float)EPSILONGC_PAGE_SIZE_IN_BYTES; 

  printf("Overhead (within the page): %.2f%%\n", overhead_within_the_page * 100.0);
  printf("Overhead (total):           %.2f%%\n", total_overhead * 100.0);
  printf("----------- Kind information: end ------------\n\n");
}

/* Compute the padding page parameter for the given kind (destructively
   updating it) based on size, alignment and object number per page;
   return true iff the result is sane: */
static bool compute_padding(epsilongc_kind_t kind){
  const size_t object_alignment_in_bytes =
    kind->object_alignment_in_words * sizeof(epsilongc_word_t);
  
  /* Compute once and for all the mark array size: */
  kind->mark_array_size_in_bytes = (epsilongc_unsigned_integer_t)
    epsilongc_mark_array_size_in_bytes(kind);
  
  /* The disalignment which the first object would have with no padding: */
  const epsilongc_unsigned_integer_t disalignment_with_no_padding =
    (EPSILONGC_PAGE_HEADER_SIZE_IN_BYTES
#ifndef ENABLE_OUT_OF_PAGE_MARK_ARRAY
     + epsilongc_mark_array_size_in_bytes(kind) // mark array size in bytes
#endif // #ifndef ENABLE_OUT_OF_PAGE_MARK_ARRAY
     ) %
    object_alignment_in_bytes;
  
  /* The padding must make the disalignment equal to zero: */
  if(disalignment_with_no_padding == 0)
    kind->padding_size_in_bytes = 0; // we were lucky, no padding is needed
  else
    kind->padding_size_in_bytes =
      object_alignment_in_bytes - disalignment_with_no_padding;
  
  /* Ok, now we can compute the first object offset: */
  kind->first_object_offset_in_bytes =
    EPSILONGC_PAGE_HEADER_SIZE_IN_BYTES +
#ifndef ENABLE_OUT_OF_PAGE_MARK_ARRAY
    epsilongc_mark_array_size_in_bytes(kind) + // mark array size in bytes
#endif // #ifndef ENABLE_OUT_OF_PAGE_MARK_ARRAY
    kind->padding_size_in_bytes;
  assert((kind->first_object_offset_in_bytes % object_alignment_in_bytes) == 0);
  
  /* Compute the number of unused bytes after the payload; it's important
     that this variable is signed (see the comment before the function we
     call here): */
  const epsilongc_integer_t wasted_bytes_at_the_end =
    epsilongc_wasted_bytes_no_at_the_end(kind);
  kind->unused_space_at_the_end_in_bytes =
    (epsilongc_unsigned_integer_t)wasted_bytes_at_the_end;
  
  //printf("\n----------- Tentative kind information: begin ----------\n");
  //epsilongc_display_kind_information(kind);
  //printf("----------- Tentative kind information: end ------------\n\n");

  /* Sanity check: */
  return (wasted_bytes_at_the_end >= 0);
}

void epsilongc_leaf_tracer(epsilongc_word_t object){
  /* Do absolutely nothing. */
}

void epsilongc_conservative_tracer(epsilongc_word_t object){
  /* Use the object as an array of words (this of course
     costs zero machine instructions): */
  const epsilongc_unsigned_integer_t object_size_in_words =
    epsilongc_object_to_size_in_words(object);
  
  /* Obtain the object length: */
  const epsilongc_word_t *object_as_array =
    (const epsilongc_word_t*)object;
  
  /* Trace each word: */
  epsilongc_unsigned_integer_t i;
  for(i = 0; i < object_size_in_words; i++)
    epsilongc_trace_pointer_if_valid(object_as_array[i]);
}

/* Compute the minimum size which is greater than or equal to the given
   minimum size, and a multiple of the given alignment: */
static size_t epsilongc_minimum_size_respecting_the_alignment(const size_t minimum_size,
                                                              const size_t alignment){
  size_t result = minimum_size;
  while((result % alignment) != 0)
    result++;
  return result;
}

epsilongc_kind_t epsilongc_make_kind(const size_t requested_object_size_in_words,
                                     const epsilongc_unsigned_integer_t pointers_no_per_object_in_the_worst_case,
                                     const size_t object_alignment_in_words,
                                     const epsilongc_kind_tag_t tag,
                                     const epsilongc_kind_datum_t datum,
                                     const epsilongc_tracer_t tracer,  
                                     const epsilongc_finalizer_t finalizer){
  /* Perform sanity checks: */
  if(tracer == NULL)
    epsilongc_fatal("The tracer function pointer can never be NULL");
  if(requested_object_size_in_words < 1)
    epsilongc_fatal("you gave %i as size, but the minimum is one word",
                    requested_object_size_in_words);
  if(object_alignment_in_words < 1)
    epsilongc_fatal("you gave %i as alignment, but the minimum is one word",
                    object_alignment_in_words);
  if(pointers_no_per_object_in_the_worst_case > requested_object_size_in_words)
    epsilongc_fatal("an object can't contain more pointers than its size in words"); // To do: reconsider this

  /* If the requested alignment is greater than the requested word size, then
     we have to waste some space per object... */
  size_t object_size_in_words;
  if(object_alignment_in_words > requested_object_size_in_words){
    printf("WARNING: You requested an alignment of %i words for objects of %i words.\n",
           (int)object_alignment_in_words,
           (int)requested_object_size_in_words);
    printf("         Using the *alignment* %i as size.\n", (int)object_alignment_in_words);
    object_size_in_words = object_alignment_in_words;
  } // if
  else if((requested_object_size_in_words % object_alignment_in_words) != 0){
    if(epsilongc_get_verbose_collection())
       printf("WARNING: You requested an alignment of %i words for objects of %i words.\n",
              (int)object_alignment_in_words,
              (int)requested_object_size_in_words);
    const size_t size =
      epsilongc_minimum_size_respecting_the_alignment(requested_object_size_in_words,
                                                      object_alignment_in_words);
    if(epsilongc_get_verbose_collection())
      printf("         Using %i as size.\n", (int)size);
    object_size_in_words = size;
  }
  else
    object_size_in_words = requested_object_size_in_words;
  
  /* We need to do some computations in bytes: */
  const size_t object_size_in_bytes =
    object_size_in_words * sizeof(epsilongc_word_t);
  
  /* Allocate the kind struct: */
  epsilongc_kind_t kind = (epsilongc_kind_t)
    epsilongc_xmalloc(sizeof(struct epsilongc_kind));
  
  /* Copy the already supplied fields: */
  kind->object_size_in_words = object_size_in_words;
  kind->pointers_no_per_object_in_the_worst_case = pointers_no_per_object_in_the_worst_case;
  kind->object_alignment_in_words = object_alignment_in_words;
  kind->tag = tag;
  kind->datum = datum;
  kind->tracer = tracer;
  kind->finalizer = finalizer;
  
  /* Compute the number of objects per page; this is the for the best case,
     but depending on size and alignment the correct number can be one less
     than this: */
#ifdef ENABLE_OUT_OF_PAGE_MARK_ARRAY
  kind->objects_no_per_page =
    (epsilongc_unsigned_integer_t)
    (ceil((float)(EPSILONGC_PAGE_SIZE_IN_BYTES - EPSILONGC_PAGE_HEADER_SIZE_IN_BYTES)
          / (float)(object_size_in_bytes)));
#else
  kind->objects_no_per_page =
    (epsilongc_unsigned_integer_t)
    (ceil((float)(EPSILONGC_PAGE_SIZE_IN_BYTES - EPSILONGC_PAGE_HEADER_SIZE_IN_BYTES)
          / (float)(object_size_in_bytes +
                    /* Overhead in bytes per object, due to the mark array */
#if defined(ENABLE_MARK_BITS)
                    (1.0 / (double)CHAR_BIT) // one bit
#elif defined(ENABLE_MARK_BYTES)
                    1                        // one byte
#elif defined(ENABLE_MARK_WORDS)
                    sizeof(epsilongc_word_t) // one word
#else
#error You have to enable mark bits, mark bytes or mark words
#endif
                    )));
#endif // #ifdef ENABLE_OUT_OF_PAGE_MARK_ARRAY
  
  /* Ok, use the "unsafe" objects_no_per_page for computing the other
     parameters: if this fails, redo it with the safe value: */
  if(! compute_padding(kind)){
#ifdef ENABLE_VERBOSE_DEBUG
    printf("** Computed \"unsafe\" data...\n");
    epsilongc_display_kind_information(kind);
    printf("** No, this was not safe.\n\n** Retrying...\n");
#endif //#ifdef ENABLE_VERBOSE_DEBUG
    kind->objects_no_per_page --;
    assert(compute_padding(kind));
#ifdef ENABLE_VERBOSE_DEBUG 
    printf("** This instead is ok:\n");
    epsilongc_display_kind_information(kind);
    printf("** Good, we're keeping this.\n\n");
#endif //#ifdef ENABLE_VERBOSE_DEBUG
  }else{
#ifdef ENABLE_VERBOSE_DEBUG
    printf("** This is the \"unsafe\" version: we're lucky, one more object fits in the page:\n");
    epsilongc_display_kind_information(kind);
    printf("** The unsafe version was ok; good, one more object fits in the page.\n\n");
#endif //#ifdef ENABLE_VERBOSE_DEBUG
  };
  
  /* Now that we know the number of objects per page we also know the number of
     pointers per page in the worst case: */
  kind->pointers_no_per_page_in_the_worst_case =
    kind->objects_no_per_page * kind->pointers_no_per_object_in_the_worst_case;

  //epsilongc_display_kind_information(kind);

  /* Return the filled struct: */
  return kind;
}
