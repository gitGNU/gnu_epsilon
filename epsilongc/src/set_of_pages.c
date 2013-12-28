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


#include <stdio.h>
#include <assert.h>
#include "set_of_pages.h"
#include "epsilongc_types.h"
#include "page.h"
#include "epsilongc_debug.h"
#include "compile_time_parameters.h"
#include "malloc.h"

/* The hash function should be as fast as possible: */
static inline epsilongc_unsigned_integer_t epsilongc_pointer_hash_hash_function(epsilongc_pointer_t key){
  return (epsilongc_unsigned_integer_t)key;
}

void epsilongc_initialize_pointer_hash(epsilongc_pointer_hash_t set,
                                       const epsilongc_unsigned_integer_t initial_size){
  /* Let's prevent stupid errors: */
  assert(initial_size > 0);

  /* Just set fields and allocate the big array, already zeroed: */
  set->allocated_size = initial_size;
  set->full_entries_no = 0;
  set->entries = (epsilongc_pointer_hash_table_t)
    epsilongc_xcalloc(initial_size, sizeof(struct epsilongc_pointer_hash_table_entry));
}

void epsilongc_destroy_pointer_hash_bucket(epsilongc_pointer_hash_bucket_t bucket){
  /* The key is not heap-allocated; we just have to destroy all the elements 
     of the bucket. We do nothing if the given bucket is empty; otherwise we
     make a copy of the next pointer, destroy the bucket entry, then recursively
     destroy the rest of the bucket. Recursion is terminal: */
  if(bucket != NULL){
    epsilongc_pointer_hash_bucket_t next = bucket->next;
    free(bucket);
    epsilongc_destroy_pointer_hash_bucket(next);
  } // if
}

epsilongc_pointer_t epsilongc_lookup_pointer_hash_bucket(const epsilongc_pointer_t key,
                                                         const epsilongc_pointer_hash_bucket_t bucket){
  /* Trivial tail-recursive implementation: */
  if(EPSILONGC_LIKELY(bucket == NULL)) // we expect buckets to be empty
    return NULL;
  else if(bucket->key == key)
    return bucket->datum;
  else
    return epsilongc_lookup_pointer_hash_bucket(key, bucket->next);
}

void epsilongc_finalize_pointer_hash(epsilongc_pointer_hash_t set){
  /* Destroy all buckets, the big array, and set the size to zero just to 
     ease debugging: */
  int i;
  const epsilongc_unsigned_integer_t size = set->allocated_size;
  for(i = 0; i < size; i++)
    epsilongc_destroy_pointer_hash_bucket(set->entries[i].bucket);
  free(set->entries);
  set->allocated_size = 0;
  set->full_entries_no = 0;
}

inline epsilongc_pointer_t epsilongc_lookup_pointer_hash(epsilongc_pointer_t key,
                                                         epsilongc_pointer_hash_t set){
  /* The conventions in our data structure require the key *NOT* to be NULL: */
  epsilongc_assert_on_debug(key != NULL);
  
  /* Lookup the current entry: */
  const epsilongc_unsigned_integer_t index =
    epsilongc_pointer_hash_hash_function(key) % set->allocated_size;
  const struct epsilongc_pointer_hash_table_entry *entry =
    &(set->entries[index]);
  const epsilongc_pointer_hash_t key_or_NULL =
    entry->key_or_NULL;
  
  /* Is the key out of the bucket? If not it may be in the bucket: */
  if(key_or_NULL == key)
    return entry->datum;
  else if(EPSILONGC_LIKELY(key_or_NULL == NULL)) // we expect buckets to be empty
    return NULL;
  else
    return epsilongc_lookup_pointer_hash_bucket(key, entry->bucket);
}

/* Return the same bucket received as parameter, destructively modified not to contain
   the given key. Note that the first element may be changed, hence in general the caller
   needs to assign the result of this function to the bucket variable: */
epsilongc_pointer_hash_bucket_t epsilongc_remove_from_pointer_hash_bucket(
                                             epsilongc_pointer_t key_to_remove,
                                             epsilongc_pointer_hash_bucket_t bucket){
  /* If the bucket is empty we have nothing to do: */
  if(bucket == NULL)
    return NULL;
  
  /* If the key to remove is in the first entry then we destroy the first entry, and
     the identity of the bucket needs to change: */
  if(bucket->key == key_to_remove){
    epsilongc_pointer_hash_bucket_t rest_of_the_bucket = bucket->next;
    free(bucket);
    return rest_of_the_bucket;
  } // if
  
  /* The key to remove may be in the rest of the bucket, so we may need to 
     adjust the next pointer of the first element. The fact that this function is
     not tail-recursive should not be a problem, as buckets should typically be
     very short: */
  bucket->next =
    epsilongc_remove_from_pointer_hash_bucket(key_to_remove, bucket->next);
  return bucket;
}

void epsilongc_remove_from_pointer_hash(epsilongc_pointer_t key_to_remove,
                                        epsilongc_pointer_hash_t set){
  /* The conventions in our data structure require the key *NOT* to be NULL: */
  epsilongc_assert_on_debug(key_to_remove != NULL);
  
  /* Lookup the current entry: */
  const epsilongc_unsigned_integer_t index =
    epsilongc_pointer_hash_hash_function(key_to_remove) % set->allocated_size;
  const epsilongc_pointer_hash_t key_or_NULL =
    set->entries[index].key_or_NULL;
  
  /* If the entry is empty we have nothing to do: */
  if(key_or_NULL == NULL)
    return;
  
  /* Is the key we're looking for out of the bucket? If so we have to
     overwrite its place in the entry with the first element of the bucket
     and shorten the bucket; unless the bucket is NULL, in which case we just
     overwrite with NULL: */
  if(key_or_NULL == key_to_remove){
    set->full_entries_no --;
    if(set->entries[index].bucket == NULL)
      set->entries[index].key_or_NULL = NULL;
    else{
      set->entries[index].key_or_NULL = set->entries[index].bucket->key;
      epsilongc_pointer_hash_bucket_t rest_of_the_bucket =
        set->entries[index].bucket->next;
      free(set->entries[index].bucket);
      set->entries[index].bucket = rest_of_the_bucket;
    } // else
  } // outer if
  else{
    /* The key we're looking for may be in the bucket: */
    if(epsilongc_lookup_pointer_hash_bucket(key_to_remove,
                                            set->entries[index].bucket) != NULL){
      /* Update the bucket, as the pointer to its first element may have changed: */
      set->entries[index].bucket =
        epsilongc_remove_from_pointer_hash_bucket(key_to_remove,
                                                  set->entries[index].bucket);
      set->full_entries_no --;
    } // inner if
    /* ...otherwise the key was not in the bucket, and we've nothing to do. */
  } // else
}

static void epsilongc_resize_pointer_hash(epsilongc_pointer_hash_t set){
  printf("Resizing %p (%i -> %i): begin\n",
         set,
         (int)set->allocated_size, (int)set->allocated_size * 2);
  /* Keep a pointer to the old array and remember its size: */
  epsilongc_pointer_hash_table_t old_entries = set->entries;
  epsilongc_unsigned_integer_t old_size = set->allocated_size;
  //printf("Resizing %p: %i -> %i\n", set, (int)old_size, (int)old_size * 2);
  
  /* Re-initialize the structure, doubling its size: */
  epsilongc_initialize_pointer_hash(set, set->allocated_size * 2);
  
  /* Insert all elements into the new set, destroying the buckets of
     the old one: */
  epsilongc_unsigned_integer_t i;
  for(i = 0; i < old_size; i++)
    if(old_entries[i].key_or_NULL != NULL){
      epsilongc_insert_into_pointer_hash(old_entries[i].key_or_NULL, old_entries[i].datum, set);
      while(old_entries[i].bucket != NULL){
        epsilongc_insert_into_pointer_hash(old_entries[i].bucket->key, old_entries[i].bucket->datum, set);
        const epsilongc_pointer_hash_bucket_t rest_of_the_bucket = old_entries[i].bucket->next;
        free(old_entries[i].bucket);
        old_entries[i].bucket = rest_of_the_bucket;
      } // while
    } // if
    else{
      epsilongc_assert_on_debug(old_entries[i].bucket == NULL);
    } // else
  
  /* Now we can destroy the old array: */
  free(old_entries);
  
  printf("Resizing %p: end\n", set);
}

void epsilongc_insert_into_pointer_hash(const epsilongc_pointer_t new_key,
                                        const epsilongc_pointer_t new_datum,
                                        epsilongc_pointer_hash_t set){
  /* The conventions in our data structure require the key and the datum *NOT*
     to be NULL: */
  epsilongc_assert_on_debug(new_key != NULL);
  epsilongc_assert_on_debug(new_datum != NULL);
  
  /* Resize the hash if it has become too full: */
  if(((double)set->full_entries_no / (double)set->allocated_size) >
     EPSILONGC_MAXIMUM_HASH_FILL_RATIO)
    epsilongc_resize_pointer_hash(set);
  
  /* Lookup the current entry: */
  const epsilongc_unsigned_integer_t index =
    epsilongc_pointer_hash_hash_function(new_key) % set->allocated_size;
  const epsilongc_pointer_hash_t key_or_NULL =
    set->entries[index].key_or_NULL;

  /* If the entry is empty then fill it, and we're done: */
  if(key_or_NULL == NULL){
    set->entries[index].key_or_NULL = new_key;
    set->entries[index].datum = new_datum;
    set->full_entries_no ++;
    return;
  } // if
  
  /* Otherwise add it to the bucket, unless it's already there: */
  if(epsilongc_lookup_pointer_hash_bucket(new_key, set->entries[index].bucket) == NULL){
    /* Make a new bucket entry and prepend it to the bucket: */
    epsilongc_pointer_hash_bucket_t new_entry = (epsilongc_pointer_hash_bucket_t)
      epsilongc_xmalloc(sizeof(struct epsilongc_pointer_hash_bucket));
    new_entry->key = new_key;
    new_entry->datum = new_datum;
    new_entry->next = set->entries[index].bucket;
    set->entries[index].bucket = new_entry;
    set->full_entries_no ++;
  } // if  
}

epsilongc_unsigned_integer_t epsilongc_pointer_hash_size(epsilongc_pointer_hash_t set){
  return set->full_entries_no;
}

/* ======== Here begins the implementation of the set of *PAGES* ======== */

static struct epsilongc_pointer_hash epsilongc_set_of_pages;
//static pthread_mutex_t epsilongc_set_of_pages_mutex;

void epsilongc_initialize_set_of_pages(void){
  epsilongc_initialize_pointer_hash(&epsilongc_set_of_pages,
                                       EPSILONGC_INITIAL_SIZE_OF_SET_OF_PAGES);
  //epsilongc_set_of_pages_mutex = epsilongc_make_mutex("set-of-pages", 0);
}

void epsilongc_finalize_set_of_pages(void){
  epsilongc_finalize_pointer_hash(&epsilongc_set_of_pages);
  //epsilongc_destroy_mutex(epsilongc_set_of_pages_mutex);
}

void epsilongc_add_page_to_the_set_of_pages(const epsilongc_page_t page){
  /* By convention, to mean that a page exists, we store its address both
     as a key and as a datum: */
  epsilongc_insert_into_pointer_hash(page, page, &epsilongc_set_of_pages);
}

void epsilongc_add_large_object_header_to_the_set_of_pages(const epsilongc_large_object_header_t large_object_header){
  /* We store an entry for a pointer to the beginning of each "page" occupied
     by the buffer, including the last one which may be only partially used.
     For each entry the datum is the beginning of the payload (in the *first*
     page); notice that for none of this page the key is equal to the datum,
     so it's easy to distinghish pages from large objects: */
  const epsilongc_pointer_t payload_beginning = large_object_header->payload_beginning;
  const epsilongc_integer_t pages_no = large_object_header->size_in_pages;
  epsilongc_pointer_t page = (epsilongc_pointer_t)large_object_header;
  epsilongc_integer_t i;
  for(i = 0; i < pages_no; i++){
    epsilongc_assert_on_debug(epsilongc_lookup_pointer_hash(page, &epsilongc_set_of_pages) == NULL);
    epsilongc_insert_into_pointer_hash(page, payload_beginning, &epsilongc_set_of_pages);
    page = ((char*)page) + EPSILONGC_PAGE_SIZE_IN_BYTES;
  } // for
}

void epsilongc_remove_page_from_the_set_of_pages(const epsilongc_page_t page){
  epsilongc_remove_from_pointer_hash(page, &epsilongc_set_of_pages);
}

void epsilongc_remove_large_object_header_from_the_set_of_pages(const epsilongc_large_object_header_t large_object_header){
  /* We have as many pointers in the table as the large object buffer
     size in pages: */
  const epsilongc_integer_t pages_no = large_object_header->size_in_pages;
  
  /* Remove each table entry; pages are consecutive, of course, as we're dealing
     with a single large buffer: */
  epsilongc_integer_t i;
  epsilongc_pointer_t page = (epsilongc_pointer_t)large_object_header;
  for(i = 0; i < pages_no; i++){
    /* ///////// */
    /* if(epsilongc_lookup_pointer_hash(page, &epsilongc_set_of_pages) != large_object_header->payload_beginning){ */
    /*   printf("datum:   %p\npayload: %p\n", */
    /*          epsilongc_lookup_pointer_hash(page, &epsilongc_set_of_pages), */
    /*          large_object_header->payload_beginning); */
    /*   fflush(stdout); */
    /*   assert(false); */
    /* } */
    /* //////// */
    epsilongc_assert_on_debug(epsilongc_lookup_pointer_hash(page, &epsilongc_set_of_pages)
                              == large_object_header->payload_beginning);
    epsilongc_remove_from_pointer_hash(page, &epsilongc_set_of_pages);
    page = ((char*)page) + EPSILONGC_PAGE_SIZE_IN_BYTES;
  } // for
}

epsilongc_pointer_t epsilongc_candidate_internal_pointer_to_large_object_payload(const epsilongc_pointer_t candidate_internal_pointer){
  /* Extract a pointer to the beginning of the page from a (possibly) internal
     pointer; here of course page_pointer is allowed to refer no existing page,
     an existing page, the first page or any other page of a large object buffer: */
  const epsilongc_pointer_t page_pointer =
    epsilongc_candidate_pointer_to_candidate_page(candidate_internal_pointer);
  
  /* Lookup the pointer: return NULL if we discovered that the given candidate
     internal pointer doesn't refer a large object; otherwise the datum we found
     may be the payload beginning we were looking for: */
  const epsilongc_pointer_t datum =
    epsilongc_lookup_pointer_hash(candidate_internal_pointer, &epsilongc_set_of_pages);
  if((datum == NULL) || (datum == page_pointer))
    return NULL; // the pointer doesn't refer a large object
  else{
    /* Ok, the given candidate internal pointer refers a large object. But to be
       valid it must point some part of the payload, and not a word in the header;
       of course it's allowed to point to a word which would belong to the payload
       in the first page, but *in another page*: */
    const epsilongc_page_t first_page_of_the_object =
      epsilongc_candidate_pointer_to_candidate_page(datum);
    if((page_pointer == first_page_of_the_object) && (candidate_internal_pointer < datum))
      return NULL;
    else
      return datum;
  } // outer else
}

epsilongc_large_object_header_t epsilongc_candidate_internal_pointer_to_large_object_header(const epsilongc_pointer_t candidate_internal_pointer){
  /* Get a pointer to the large object payload and use it to obtain the header,
     if such an object exists; otherwise just return NULL: */
  const epsilongc_pointer_t payload_pointer_or_NULL =
    epsilongc_candidate_internal_pointer_to_large_object_payload(candidate_internal_pointer);
  /* In order to obtain the header pointer from a valid payload pointer we have
     to clear some bits of payload_pointer_or_NULL; but we can also do this
     on NULL without changing the result! In this way we can save a comparison: */
  epsilongc_assert_on_debug(epsilongc_candidate_pointer_to_candidate_page(NULL) == NULL);
  return (epsilongc_large_object_header_t)
    epsilongc_candidate_pointer_to_candidate_page(payload_pointer_or_NULL);
}

bool epsilongc_does_page_exist(const epsilongc_page_t page){
  /* NULL is not an existing page. We have to check this separately as this
     hash table implementation does not support NULL keys: */
  if(page == NULL)
    return false;
  
  /* Ok, the page is not NULL; check the hash table; if the datum is equal to
     the key, by our convention, we mean that the pointer refers a page: */
  return epsilongc_lookup_pointer_hash(page, &epsilongc_set_of_pages) == page;
}

bool epsilongc_is_a_heap_page(const epsilongc_page_t candidate_page){
#ifdef ENABLE_ASSERTIONS
  /* When debugging we check whether the argument is page-aligned: */
  if(EPSILONGC_UNLIKELY(((epsilongc_unsigned_integer_t)candidate_page &
                         (~(epsilongc_unsigned_integer_t)EPSILONGC_PAGE_BITMASK)) != 0u))
    epsilongc_fatal("epsilongc_is_a_heap_page(): the parameter is not page-aligned");
#endif // #ifdef ENABLE_ASSERTIONS
  
  /* Looking up a key which is not in the hash table will yield NULL: we don't
     care, here, if we're referring to a proper page or to a part of a large
     object: */
  return epsilongc_lookup_pointer_hash(candidate_page,
                                       &epsilongc_set_of_pages) != NULL;
}

epsilongc_sort_of_page_t
epsilongc_candidate_page_to_sort_of_page(const epsilongc_page_t candidate_page,
                                         epsilongc_word_t *large_object_payload_beginning){
#ifdef ENABLE_ASSERTIONS
  /* When debugging we check whether the argument is page-aligned and not NULL: */
  if(EPSILONGC_UNLIKELY(((epsilongc_unsigned_integer_t)candidate_page &
                         (~(epsilongc_unsigned_integer_t)EPSILONGC_PAGE_BITMASK)) != 0u))
    epsilongc_fatal("epsilongc_is_a_heap_page(): the parameter is not page-aligned");
  if(EPSILONGC_UNLIKELY(candidate_page == NULL))
    epsilongc_fatal("epsilongc_is_a_heap_page(): the parameter is NULL");
#endif // #ifdef ENABLE_ASSERTIONS
  
  /* Lookup the candidate page in the table; then it's easy to see which sort
     of page it is: */
  const epsilongc_word_t value = epsilongc_lookup_pointer_hash(candidate_page, &epsilongc_set_of_pages);
  if(value == NULL)
    return epsilongc_not_a_heap_page_sort;
  else if(EPSILONGC_LIKELY(value == candidate_page)) // there are typically few large objects
    return epsilongc_proper_page_sort;
  else{
    /* For large object we also communicate to the caller the object
       payload beginning, to save a further hash table lookup: */
    *large_object_payload_beginning = value;
    return epsilongc_large_object_page_sort;
  } // else
}

epsilongc_unsigned_integer_t epsilongc_pages_no(void){
  return epsilongc_pointer_hash_size(&epsilongc_set_of_pages);
}
