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


#ifndef EPSILONGC_KIND_H_
#define EPSILONGC_KIND_H_

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdbool.h>
#include "config.h"
#include "epsilongc_types.h"

/* A tracer function is a function which takes an objects and marks (or pushes
   on a mark stack) all its descendants (not the object itself): */
typedef void (*epsilongc_tracer_t)(epsilongc_word_t);

/* This tracer employs conservative pointer finding, scaning all
   the words of an object. It loads the object size at runtime,
   hence it may be slower than a specialized version which only works
   on objects of one fixed size: */
void epsilongc_conservative_tracer(epsilongc_word_t object);

/* This tracer traces absolutely nothing: it's useful for leaf objects: */
void epsilongc_leaf_tracer(epsilongc_word_t object);

/* A finalizer function is a function which destroys an object, also releasing
   all its non-memory resources, if any: */
typedef void (*epsilongc_finalizer_t)(epsilongc_word_t);

/* Kind tags are arbitrary unsigned integers: */
typedef epsilongc_unsigned_integer_t epsilongc_kind_tag_t;

/* Kind data are arbitrary pointers: */
typedef epsilongc_pointer_t epsilongc_kind_datum_t;

/* A description of one kind of objects. For each kind there should be
   only one structure of this type, so that we can avoid bothering with
   deallocating these: */
struct epsilongc_kind{
  /* The tag and datum for this kind: */
  epsilongc_kind_tag_t tag;
  epsilongc_kind_datum_t datum;
  
  /* The size of a single object of this kind. In words: */
  epsilongc_unsigned_integer_t object_size_in_words;

  /* The alignment of objects of this kind. In words: */
  epsilongc_unsigned_integer_t object_alignment_in_words;
  
  /* The maximum number of pointer to objects in the garbage-collected heap
     per object: */
  epsilongc_unsigned_integer_t pointers_no_per_object_in_the_worst_case;
  
  /* The maximum number of pointer to objects in the garbage-collected heap
     per object: */
  epsilongc_unsigned_integer_t pointers_no_per_page_in_the_worst_case;
  
  /* How many objects of this kind fit in a page (this can be computed
     once and for all): */
  epsilongc_unsigned_integer_t objects_no_per_page;
  
  /* Size of the padding before the payload (needed to respect its required
     alignment), computed once and for all: */
  epsilongc_unsigned_integer_t padding_size_in_bytes;

  /* Size of the unused final part of the page, after the payload (which may
     be zero bytes), computed once and for all: */
  epsilongc_unsigned_integer_t unused_space_at_the_end_in_bytes;
  
  /* Offset of the first object relative to the page beginning: */
  epsilongc_unsigned_integer_t first_object_offset_in_bytes;
  
  /* The tracer for objects of this kind; this must *not* be NULL: */
  epsilongc_tracer_t tracer;
  
  /* The finalizer for objects of this kind; this *can* be NULL: */
  epsilongc_finalizer_t finalizer;

  /* Length of the mark array, in bytes, as per the result of 
     epsilongc_mark_array_size_in_bytes(): */
  epsilongc_unsigned_integer_t mark_array_size_in_bytes;
};
typedef struct epsilongc_kind* epsilongc_kind_t;

/* Make a new kind: */
epsilongc_kind_t epsilongc_make_kind(const size_t object_size_in_words,
                                     const epsilongc_unsigned_integer_t pointers_no_per_object_in_the_worst_case,
                                     const size_t object_alignment_in_words,
                                     const epsilongc_kind_tag_t tag,
                                     const epsilongc_kind_datum_t datum,
                                     const epsilongc_tracer_t tracer,  
                                     const epsilongc_finalizer_t finalizer)
  __attribute__((malloc));

#endif // #ifndef EPSILONGC_KIND_H_
