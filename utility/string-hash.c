/* Hash tables with string keys.

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
#include "string-hash.h"
#include "malloc.h"

/* Hash a string into an index.  The result must still be cut with %. */
static unsigned int epsilon_string_hash(epsilon_string_hash_key key){
  unsigned int result = 0;
  int i;
  for(i = 0; key[i] != '\0'; i ++)
    result += (i + 1) * key[i];
  return result;
}

/* How many buckets a just-initialized hash has: */
#define EPSILON_INITIAL_STRING_HASH_BUCKET_NO 31 // Good for debugging //64

/* We rehash after the fill factor gets over this: */
#define EPSILON_STRING_HASH_MAXIMUM_FILL_FACTOR 0.70

static void epsilon_initialize_string_hash_of_size(epsilon_string_hash_t hash,
                                                   int bucket_no){
  hash->buckets = (epsilon_string_hash_bucket_t*)
    epsilon_xmalloc(sizeof(epsilon_string_hash_bucket_t) *
                    bucket_no);
  int i;
  for(i = 0; i < bucket_no; i ++)
    hash->buckets[i] = NULL;
  hash->bucket_no = bucket_no;
  hash->binding_no = 0;
}

void epsilon_initialize_string_hash(epsilon_string_hash_t hash){
  epsilon_initialize_string_hash_of_size(hash,
                                         EPSILON_INITIAL_STRING_HASH_BUCKET_NO);
}

static void
epsilon_destroy_string_hash_bucket_item_with_datum_destructor(epsilon_string_hash_bucket_t i,
                                                              void (*destroy_datum)(epsilon_string_hash_datum)){
  free(i->key_copy);
  destroy_datum(i->datum);
  // The datum should *not* be freed.
  free(i);
}

static void
epsilon_destroy_string_hash_bucket_with_datum_destructor(epsilon_string_hash_bucket_t bucket,
                                                         void (*destroy_datum)(epsilon_string_hash_datum)){
  epsilon_string_hash_bucket_t b = bucket;
  while(b != NULL){
    epsilon_string_hash_bucket_t next_bucket = b->next;
    epsilon_destroy_string_hash_bucket_item_with_datum_destructor(b, destroy_datum);
    b = next_bucket;
  } // while
}
void epsilon_finalize_string_hash_with_datum_destructor(epsilon_string_hash_t hash,
                                                        void (*destroy_datum)(epsilon_string_hash_datum)){
  int i;
  const int bucket_no = hash->bucket_no;
  for(i = 0; i < bucket_no; i ++)
    epsilon_destroy_string_hash_bucket_with_datum_destructor(hash->buckets[i], destroy_datum);
  free(hash->buckets);
  hash->buckets = NULL; // to prevent further use
  hash->bucket_no = 0; // to prevent further use
  hash->binding_no = 0;
}
void epsilon_destroy_string_hash_with_datum_destructor(epsilon_string_hash_t hash,
                                                       void (*destroy_datum)(epsilon_string_hash_datum)){
  epsilon_finalize_string_hash_with_datum_destructor(hash, destroy_datum);
  free(hash);
}

/* This is useful to implement finalization functions with no destructor function: */
static void trivial_datum_destructor(epsilon_string_hash_datum useless){
  // Do nothing
}

void epsilon_finalize_string_hash(epsilon_string_hash_t hash){
  epsilon_finalize_string_hash_with_datum_destructor(hash, trivial_datum_destructor);
}

void epsilon_destroy_string_hash(epsilon_string_hash_t hash){
  epsilon_destroy_string_hash_with_datum_destructor(hash, trivial_datum_destructor);
}

epsilon_string_hash_t epsilon_make_string_hash(void){
  epsilon_string_hash_t result = (epsilon_string_hash_t)
    epsilon_xmalloc(sizeof(struct epsilon_string_hash));
  epsilon_initialize_string_hash(result);
  return result;    
}

static void epsilon_rehash_string_hash(epsilon_string_hash_t hash){
  /* Keep a reference to the current buckets: */
  const int old_bucket_no = hash->bucket_no;
  const int old_binding_no = hash->binding_no;
  epsilon_string_hash_bucket_t *old_buckets = hash->buckets;
  
  /* Make a new larger hash: */
  const unsigned int new_bucket_no = old_bucket_no * 2 - 1;
  epsilon_initialize_string_hash_of_size(hash, new_bucket_no);
  
  /* Move each bucket item from the old hash into the new hash.  We
     recycle bucket structs, and keys.  By the way, new buckets will
     be reversed compared to the old ones. */
  int i;
  for(i = 0; i < old_bucket_no; i ++){
    epsilon_string_hash_bucket_t b = old_buckets[i];
    while(b != NULL){
      /* Attach *b at the beginning of the currect bucket in the new
         bucket array: */
      epsilon_string_hash_bucket_t next_item_in_the_old_bucket = b->next;
      const int bucket_index_in_the_new_hash =
        epsilon_string_hash(b->key_copy) % new_bucket_no;
      b->next = hash->buckets[bucket_index_in_the_new_hash];
      hash->buckets[bucket_index_in_the_new_hash] = b;
      
      /* Do the same with the next item in the old bucket */
      b = next_item_in_the_old_bucket;
    } // while
  } // for
  
  /* Store the binding no in the new hash; */
  hash->binding_no = old_binding_no;

  /* Free the old bucket array; all bucket items have been recycled. */
  free(old_buckets);
}

void epsilon_add_to_string_hash(epsilon_string_hash_t hash,
                                epsilon_string_hash_key key,
                                epsilon_string_hash_datum datum){
  /* Rehash if the table is getting too full: */
  const float new_fill_factor =
    (float)(hash->binding_no + 1) / (float)(hash->bucket_no);
  if(new_fill_factor > EPSILON_STRING_HASH_MAXIMUM_FILL_FACTOR)
    epsilon_rehash_string_hash(hash);
  
  /* Update the binding count: */
  hash->binding_no ++;
  
  /* Make a new bucket item: */
  epsilon_string_hash_bucket_t new_bucket_item = (epsilon_string_hash_bucket_t)
    epsilon_xmalloc(sizeof(struct epsilon_string_hash_bucket));
  new_bucket_item->key_copy = (char*)epsilon_xmalloc(1 + strlen(key));
  strcpy(new_bucket_item->key_copy, key);
  new_bucket_item->datum = datum;
  
  /* Attach the new item to the right bucket: */ 
  const int bucket_index = epsilon_string_hash(key) % hash->bucket_no;
  new_bucket_item->next = hash->buckets[bucket_index];
  hash->buckets[bucket_index] = new_bucket_item;
}

epsilon_string_hash_datum epsilon_lookup_string_hash(epsilon_string_hash_t hash,
                                                     epsilon_string_hash_key key){
  /* Locate the bucket: */
  const int bucket_index = epsilon_string_hash(key) % hash->bucket_no;
  epsilon_string_hash_bucket_t b = hash->buckets[bucket_index];
  
  /* Linearly scan the bucket: */
  while(b != NULL){
    if(! strcmp(b->key_copy, key))
      return b->datum;
    b = b->next;
  } // while
  
  /* We didn't find a binding: */
  return NULL;
}

bool epsilon_is_string_hash_bound_on(epsilon_string_hash_t hash,
                                     epsilon_string_hash_key key){
  /* Locate the bucket: */
  const int bucket_index = epsilon_string_hash(key) % hash->bucket_no;
  epsilon_string_hash_bucket_t b = hash->buckets[bucket_index];
  
  /* Linearly scan the bucket: */
  while(b != NULL){
    if(! strcmp(b->key_copy, key))
      return true;
    b = b->next;
  } // while
  
  /* We didn't find a binding: */
  return false;
}

void epsilon_remove_from_string_hash_with_datum_destructor(epsilon_string_hash_t hash,
                                                           epsilon_string_hash_key key,
                                                           void (*destroy_datum)(epsilon_string_hash_datum)){
  const int bucket_index = epsilon_string_hash(key) % hash->bucket_no;
  epsilon_string_hash_bucket_t *b_pointer =
    &(hash->buckets[bucket_index]);
  epsilon_string_hash_bucket_t b = *b_pointer;
  while(b != NULL){
    if(! strcmp(b->key_copy, key)){
      /* We've found the match. */
      *b_pointer = b->next;
      hash->binding_no --;
      epsilon_destroy_string_hash_bucket_item_with_datum_destructor(b, destroy_datum);
      return;
    } // if
    b_pointer = &(b->next);
    b = *b_pointer;
  } // while
}

void epsilon_remove_from_string_hash(epsilon_string_hash_t hash,
                                     epsilon_string_hash_key key){
  epsilon_remove_from_string_hash_with_datum_destructor(hash, key, trivial_datum_destructor);
}
