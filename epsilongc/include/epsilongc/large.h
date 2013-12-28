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


#ifndef EPSILONGC_LARGE_H_
#define EPSILONGC_LARGE_H_

#include <stdbool.h>
#include "epsilongc_types.h"
#include "doubly_linked_list_macros.h"
#include "kind.h"
#include "page.h"

/* A large object consists of a header followed by the payload. We use a
   pointer to the header to refer the whole header+payload object: */
typedef struct epsilongc_large_object_header* epsilongc_large_object_header_t;
struct epsilongc_large_object_header{
  /* The object header, exactly the same as for a page; it's important that
     offsets are the same, so that the same fields are accessible for both
     structures without run-time conditionals: */
  PAGE_OR_LARGE_OBJECT_HEADER;
  
  /* Each large object is an element of either the list of alive large
     objects or the list of dead large objects: */
  EPSILONGC_LIST_ELEMENT_FIELDS(large_object_headers, epsilongc_large_object_header_t);
  //EPSILONGC_LIST_ELEMENT_FIELDS(large_objects, epsilongc_large_object_t)
  
  /* The object payload could start right after the header, if there weren't
     alignment requirements: */
  epsilongc_pointer_t payload_beginning;
  
  /* The size of the object, including header, padding and payload, in pages.
     Even when the last page is not completely filled, it is counted: */
  epsilongc_integer_t size_in_pages;
  
  epsilongc_unsigned_integer_t pointers_no_in_the_worst_case;
  
  /* Is this object currently marked? */
  bool is_marked;
}; // struct

/* Return a pointer to the header of the given large object; the parameter
   must be a pointer to the beginning of the payload. The object is assumed
   to exist: */
epsilongc_large_object_header_t
epsilongc_large_object_payload_to_large_object_header(const epsilongc_word_t payload_pointer);

/* Return a pointer to the payload of the large object whose header is given.
   The object is assumed to exist. */
epsilongc_word_t epsilongc_large_object_header_to_large_object_payload(const epsilongc_large_object_header_t header_pointer);

/* Make a new large object and return a pointer to its header (*not* to its
   payload). The payload is *not* initialized or zeroed: */
epsilongc_large_object_header_t
epsilongc_make_large_object_and_return_its_header(const epsilongc_integer_t size_in_words,
                                                  const epsilongc_integer_t alignment_in_words,
                                                  const epsilongc_kind_tag_t tag,
                                                  const epsilongc_kind_datum_t datum,
                                                  const epsilongc_tracer_t tracer,
                                                  const epsilongc_integer_t pointers_no_in_the_worst_case)
  __attribute__((malloc));

/* Condemn (i.e. move to the list of dead objects) all unmarked large objects,
   and unmark the marked ones, while updating sweep statistics: */
void epsilongc_sweep_large_objects_unlocked(epsilongc_sweep_statistics_t sweep_statistics);

/* Initialize large object support: */
void epsilongc_initialize_large_object_support(void);

/* Finalize large object support: */
void epsilongc_finalize_large_object_support(void);

/* This is the procedure to be run in the large object destroyer thread: */
void* epsilongc_destroy_large_objects_thread(void *unused);

/* Request the large object destroyer thread to do its work; this should
   be called at the end of a collection, when some large objects have been
   condemned: */
void epsilongc_request_condemned_large_object_destruction(void);

#endif // #ifndef EPSILONGC_LARGE_H_
