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


#ifndef EPSILONGC_SET_OF_PAGES_H_
#define EPSILONGC_SET_OF_PAGES_H_

#include <stdbool.h>
#include "epsilongc_types.h"
#include "page.h"
#include "large.h"

// To do: change comments: this is now just a hash from non-NULL
// pointers to non-NULL keys.
// Speak about synchronization in this module

/* This is a very fast hash table implementation (with non-NULL pointers
   as keys, and no data), where we expect very few collisions, and take
   advantage of this.
   The idea is to make lookup faster by avoiding one indirection in most
   cases: the first key for each entry is kept out of the bucket.
   The implementation of the remove and insert operations is slightly
   complicated, but lookup is very fast. */

/* A bucket: */
typedef struct epsilongc_pointer_hash_bucket* epsilongc_pointer_hash_bucket_t;
struct epsilongc_pointer_hash_bucket{
  /* The key: */
  epsilongc_pointer_t key;
  
  /* The value: */
  epsilongc_pointer_t datum;
  
  /* The next pointer, or NULL if there is no next: */
  epsilongc_pointer_hash_bucket_t next;
}; // struct

/* A single entry in the table: */
struct epsilongc_pointer_hash_table_entry{
  /* This contains the key, or NULL if there is no key in this position: */
  epsilongc_pointer_t key_or_NULL;
  
  /* The value: */
  epsilongc_pointer_t datum;
  
  /* This bucket is not empty (i.e. not NULL) only when there is a collision
     (i.e. where key_or_NULL is not NULL): */
  epsilongc_pointer_hash_bucket_t bucket;
}; // struct
typedef struct epsilongc_pointer_hash_table_entry* epsilongc_pointer_hash_table_t;

/* The main data structure: */
struct epsilongc_pointer_hash{
  /* The number of entries: */
  epsilongc_unsigned_integer_t allocated_size;
  
  /* The number of full entries (we ignore buckets for this): */
  epsilongc_unsigned_integer_t full_entries_no;
  
  /* The array of entries: */
  epsilongc_pointer_hash_table_t entries;
}; // struct
typedef struct epsilongc_pointer_hash* epsilongc_pointer_hash_t;

/* Initialize a given set of pointers: */
void epsilongc_initialize_pointer_hash(epsilongc_pointer_hash_t set,
                                       const epsilongc_unsigned_integer_t initial_size);
void epsilongc_finalize_pointer_hash(epsilongc_pointer_hash_t set);

/* Return the datum associated to the given key in the given bucket,
   or NULL if the key is not bound. */
epsilongc_pointer_t epsilongc_lookup_pointer_hash_bucket(const epsilongc_pointer_t key,
                                                         const epsilongc_pointer_hash_bucket_t bucket)
  __attribute__((pure/*, always_inline */));
epsilongc_unsigned_integer_t epsilongc_pointer_hash_size(const epsilongc_pointer_hash_t set)
  __attribute__((pure));

void epsilongc_insert_into_pointer_hash(const epsilongc_pointer_t new_key,
                                        const epsilongc_pointer_t new_value,
                                        epsilongc_pointer_hash_t set);
void epsilongc_remove_from_pointer_hash(const epsilongc_pointer_t key_to_remove,
                                        epsilongc_pointer_hash_t set);


/* Initializer and finalizer: */
void epsilongc_initialize_set_of_pages(void);
void epsilongc_finalize_set_of_pages(void);

/* Add a page to the set: */
void epsilongc_add_page_to_the_set_of_pages(const epsilongc_page_t page);

/* Add a large object to the set; notice that the *header* is passed: */
void epsilongc_add_large_object_header_to_the_set_of_pages(const epsilongc_large_object_header_t large_object_header);

/* Remove a page from the set; do nothing if the given page is not in the set: */
void epsilongc_remove_page_from_the_set_of_pages(const epsilongc_page_t page);

/* Remove a large object from the set; notice that the *header* is passed: */
void epsilongc_remove_large_object_header_from_the_set_of_pages(const epsilongc_large_object_header_t large_object_header);

/* If the given pointer is a (possibly) internal pointer to a large object,
   return a pointer to the beginning of the large object payload; otherwise
   return NULL: */
epsilongc_pointer_t epsilongc_candidate_internal_pointer_to_large_object_payload(const epsilongc_pointer_t candidate_internal_pointer)
  __attribute__((pure));

/* If the given pointer is a (possibly) internal pointer to a large object,
   return a pointer to the large object header; otherwise return NULL: */
epsilongc_large_object_header_t epsilongc_candidate_internal_pointer_to_large_object_header(const epsilongc_pointer_t candidate_internal_pointer)
  __attribute__((pure));

/* Return true iff the given candidate page (which is assumed to be a
   page-aligned pointer) is a heap "page": either a proper page, or 
   part of a large object: */
bool epsilongc_is_a_heap_page(const epsilongc_page_t candidate_page)
  __attribute__((pure));

/* Given what could be a pointer to a heap page, return its "sort"; only
   if the returned sort is epsilongc_large_object_page_sort, also store
   the payload beginning into the given location: */
typedef enum epsilongc_sort_of_page{
  epsilongc_not_a_heap_page_sort,   // not a heap page, possibly not even in the address space
  epsilongc_proper_page_sort,  // a page of homogeneous objects
  epsilongc_large_object_page_sort, // part of a large object
} epsilongc_sort_of_page_t;
epsilongc_sort_of_page_t
epsilongc_candidate_page_to_sort_of_page(const epsilongc_page_t candidate_page,
                                         epsilongc_word_t *large_object_payload_beginning);

/* Return the number or currently existing pages: */
epsilongc_unsigned_integer_t epsilongc_pages_no(void)
  __attribute__((pure));

#endif // #ifndef EPSILONGC_SET_OF_PAGES_H_
