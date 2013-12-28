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
#include <stdlib.h>
#include <unistd.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>

#include "movinggc.h"
#include "features.h"
#include "tags.h"

#ifdef MOVINGGC_USE_GLOBAL_POINTERS
#ifdef MOVINGGC_USE_REGISTER_POINTERS
// esi edi ebx
//register void **movinggc_fromspace_next_unallocated_object asm ("%esi");
//register void **movinggc_fromspace_after_payload_end asm ("%ebx");
#else // in the #else branch we have !defined(MOVINGGC_USE_REGISTER_POINTERS)
static void **movinggc_fromspace_next_unallocated_object = NULL;;
static void **movinggc_fromspace_after_payload_end = NULL;;
#endif // #ifdef MOVINGGC_USE_REGISTER_POINTERS
#endif // #ifdef MOVINGGC_USE_GLOBAL_POINTERS

#ifdef MOVINGGC_VERBOSE
#define movinggc_log printf
#else
#define movinggc_log(...) /* do nothing */
#endif // #ifdef MOVINGGC_VERBOSE

#ifdef MOVINGGC_VERY_VERBOSE
#define movinggc_verbose_log printf
#else
#define movinggc_verbose_log(...) /* do nothing */
#endif // #ifdef MOVINGGC_VERBOSE

// #define movinggc_objects_no 16
#define MOVINGGC_SEMISPACE_WORDS_NO \
  ((sizeof(void*) == 8) ? \
   ((1 << 16) / sizeof(void*)) /* 64Kib */ \
   : \
   ((1 << 15) / sizeof(void*)) /* 32Kib */)
#define MOVINGGC_INITIAL_ROOTS_ALLOCATED_SIZE 64

/* Enlarge semispaces if fromspace is fuller than this ratio after a
   collection: */
#define MOVINGGC_ENLARGE_THRESHOLD 0.95

#define MOVINGGC_SWAP(A, B) \
  { const __typeof(A) t_e_m_porary__ = A; \
    A = B; \
    B = t_e_m_porary__; }

#define if_likely(CONDITION) \
  if(__builtin_expect(CONDITION, true))
#define if_unlikely(CONDITION) \
  if(__builtin_expect(CONDITION, false))
#define while_likely(CONDITION) \
  while(__builtin_expect(CONDITION, true))
#define while_unlikely(CONDITION) \
  while(__builtin_expect(CONDITION, false))

void movinggc_fatal(const char *message)
  __attribute__((noinline, noreturn));
void movinggc_fatal(const char *message){
  printf("Movinggc_Fatal error: %s\n", message);
  exit(EXIT_FAILURE);
}

#ifdef MOVINGGC_USE_STACK
static void **movinggc_trace_stack = NULL;
static void **movinggc_trace_stack_overtop = NULL;
static size_t movinggc_stack_size_in_words = 0;

static void* movinggc_trace_stack_top(void){
  return movinggc_trace_stack_overtop[-1];
}
static bool movinggc_is_trace_stack_empty(void){
  return movinggc_trace_stack_overtop == movinggc_trace_stack;
}
static void movinggc_trace_stack_push(void *word){
  movinggc_trace_stack_overtop[0] = word;
  movinggc_trace_stack_overtop++;
}
static void* movinggc_trace_stack_pop(){
  void *result = movinggc_trace_stack_top();
  movinggc_trace_stack_overtop--;
  return result;
}
#endif // #ifdef MOVINGGC_USE_STACK

#ifdef MOVINGGC_USE_STACK
/* This assumes the stack to be empty: */
void movinggc_resize_trace_stack(size_t new_size_in_words){
  free(movinggc_trace_stack);
  movinggc_trace_stack = (void**)
    malloc(sizeof(void*) * new_size_in_words);
  if_unlikely(movinggc_trace_stack == NULL)
    movinggc_fatal("could not enlarge the trace stack");
  movinggc_stack_size_in_words = new_size_in_words;
  movinggc_trace_stack_overtop =
    movinggc_trace_stack;
}
#endif // #ifdef MOVINGGC_USE_STACK

const char *a_name = "A-SPACE";
const char *b_name = "B-SPACE";
const char *nonheap_name = "non-heap pointer";

struct movinggc_semispace_header{
  void **next_unallocated_object;
  void **after_payload_end;
  void **payload_beginning;
  size_t payload_size_in_words;
  const char *name;
}; // struct
typedef struct movinggc_semispace_header* movinggc_semispace_header_t;

static struct movinggc_semispace_header fromspace_struct;
static const movinggc_semispace_header_t a_semispace =
  &fromspace_struct;
static const movinggc_semispace_header_t fromspace =
  &fromspace_struct;
static struct movinggc_semispace_header tospace_struct;
static const movinggc_semispace_header_t b_semispace =
  &tospace_struct;
static const movinggc_semispace_header_t tospace =
  &tospace_struct;

const char* movinggc_semispace_name_of(void *untagged_pointer_as_void_star){
  void **untagged_pointer = untagged_pointer;
  if((untagged_pointer >= a_semispace->payload_beginning) &&
     (untagged_pointer < a_semispace->after_payload_end))
    return a_semispace->name;
  else if((untagged_pointer >= b_semispace->payload_beginning) &&
          (untagged_pointer < b_semispace->after_payload_end))
    return b_semispace->name;
  else
    return nonheap_name;
}

void movinggc_destroy_semispace(movinggc_semispace_header_t semispace){
  free(semispace->payload_beginning);
}

void movinggc_initialize_semispace(movinggc_semispace_header_t semispace,
                                   const char *name,
                                   const size_t payload_size_in_words){
  /* Allocate the payload: */
  void **semispace_payload;
  const size_t payload_size_in_bytes =
    sizeof(void*) * payload_size_in_words;
  semispace_payload = (void**)malloc(payload_size_in_bytes);
  if_unlikely(semispace_payload == NULL)
    movinggc_fatal("movinggc_initialize_semispace(): couldn't allocate");
  
  /* Set fields: */
  semispace->name = name;
  semispace->payload_beginning = semispace_payload;
  semispace->after_payload_end = (void**)
    (((char*)semispace_payload) + payload_size_in_bytes);
  semispace->payload_size_in_words = payload_size_in_words;
  semispace->next_unallocated_object = semispace_payload;
  movinggc_verbose_log("New semispace %s: [%p, %p)\n",
         semispace->name,
         semispace->payload_beginning,
         semispace->after_payload_end);

#ifdef MOVINGGC_USE_STACK
  /* The stack must be able to contain at least new_minimum_stack_size
     objects: resize it if it's not large enough: */
  const size_t new_minimum_stack_size_in_words =
    payload_size_in_words / 2 + 1;
  if(movinggc_stack_size_in_words < new_minimum_stack_size_in_words)
    movinggc_resize_trace_stack(new_minimum_stack_size_in_words);
#endif // #ifdef MOVINGGC_USE_STACK
}

void movinggc_destructively_enlarge_semispace(movinggc_semispace_header_t semispace,
                                              size_t new_payload_size_in_words){
  const char *name = semispace->name;
  movinggc_destroy_semispace(semispace);
  movinggc_initialize_semispace(semispace,
                                name,
                                new_payload_size_in_words);
  printf("Enlarged semispace %s: %i words -> %i words\n",
         name,
         (int)semispace->payload_size_in_words,
         (int)new_payload_size_in_words);
}

static void* movinggc_allocate_from(movinggc_semispace_header_t fromspace,
                                    size_t size_in_words)
  __attribute__((always_inline));
static void* movinggc_allocate_from(movinggc_semispace_header_t semispace,
                                    size_t size_in_words){
  void ** const next_unallocated_object =
    semispace->next_unallocated_object;
  void ** const next_unallocated_object_after_the_new_objext =
    next_unallocated_object + size_in_words + 1; // + 1 for the header word
#ifdef MOVINGGC_DEBUG
  if_unlikely(next_unallocated_object_after_this_objext >
              semispace->after_payload_end){
    printf("%p, %p\n", next_unallocated_object_after_the_new_objext, semispace->after_payload_end);
    movinggc_verbose_log("movinggc_allocate_from(): we were trying to allocate from %s\n",
           semispace->name);
    movinggc_fatal("movinggc_allocate_from(): not enough space");
  } // if_unlikely 
#endif // #ifdef MOVINGGC_DEBUG
 
  /* Ok, there is space available; fill the header word, bump the pointer
     and return the next unallocated object: */
  //movinggc_verbose_log("We're allocating from %s\n", semispace->name);
  (*next_unallocated_object) =
    MOVINGGC_TAG_HEADER_AS_NONFORWARDING(size_in_words);
  semispace->next_unallocated_object =
    next_unallocated_object_after_the_new_objext;
  return next_unallocated_object + 1;
}

void movinggc_dump_free_space_statistics(void)
  __attribute__((noinline));

void movinggc_enlarge_semispaces(void)
  __attribute__((noinline));

float movinggc_fill_ratio_of(movinggc_semispace_header_t semispace,
                             size_t words_no_to_be_allocated){
  const size_t free_words_no =
    semispace->after_payload_end -
    semispace->next_unallocated_object -
    words_no_to_be_allocated;
  const size_t semispace_size_in_words = semispace->payload_size_in_words;
  return 1.0 - (float)free_words_no / (float)semispace_size_in_words;
}
float movinggc_fill_ratio(void){
  return movinggc_fill_ratio_of(fromspace, 0);
}

bool movinggc_should_we_enlarge_semispaces(size_t words_no_to_be_allocated)
  __attribute__((noinline));
bool movinggc_should_we_enlarge_semispaces(size_t words_no_to_be_allocated){
  const float current_fill_ratio =
    movinggc_fill_ratio_of(fromspace, words_no_to_be_allocated);
  return current_fill_ratio > MOVINGGC_ENLARGE_THRESHOLD;
}

void movinggc_resize_semispaces(const size_t new_semispace_size_in_words){
  //movinggc_dump_free_space_statistics();
  const size_t old_semispace_size_in_words __attribute__((unused)) =
    fromspace->payload_size_in_words;
  movinggc_log("Enlarging each semispace from %i to %i words...\n", 
               (int)old_semispace_size_in_words,
               (int)new_semispace_size_in_words);
  /* Destructively enlarge tospace: of couse we can afford to lose its
     content, as all the useful data are in fromspace: */
  movinggc_destructively_enlarge_semispace(tospace,
                                           new_semispace_size_in_words);
  /* Collect, so that the enlarged tospace becomes fromspace and vice-versa;
     then we can destructively enlarge also the new tospace: */
  movinggc_gc();
  movinggc_destructively_enlarge_semispace(tospace,
                                           new_semispace_size_in_words);
  movinggc_log("We have now enlarged semispaces to %i words.\n",
               (int)new_semispace_size_in_words);
}

void movinggc_enlarge_semispaces(void){
  const size_t old_semispace_size_in_words =
    fromspace->payload_size_in_words;
  const size_t new_semispace_size_in_words =
    old_semispace_size_in_words * 2;
  movinggc_resize_semispaces(new_semispace_size_in_words);
}

#ifdef MOVINGGC_USE_GLOBAL_POINTERS
static void* movinggc_allocate_from_fromspace(size_t size_in_words){
  /* Remember the current position of the next unallocated pointer: */
  void ** old_next_unallocated_object =
    movinggc_fromspace_next_unallocated_object;
  
  /* Compute the header word: */
  void* const
    header_word = MOVINGGC_TAG_HEADER_AS_NONFORWARDING(size_in_words);
 
  /* Fill the header word, bump the pointer and return a pointer to
     the beginning of the new object payload: */
  (*old_next_unallocated_object) = header_word;
  movinggc_fromspace_next_unallocated_object +=
    size_in_words + 1 /* the "+ 1" is for the header */;
  return old_next_unallocated_object + 1;
}
void* movinggc_allocate(const size_t size_in_words){
  /* Do we have enough space available in fromspace? */
  if_unlikely(movinggc_fromspace_next_unallocated_object + size_in_words + 1 >
              movinggc_fromspace_after_payload_end){
    /* No, we need to GC before allocating... */
    movinggc_gc();
    
    /* And maybe we should also enlarge semispaces, if we're really
       unlucky more than once: */
    while_unlikely(movinggc_should_we_enlarge_semispaces(size_in_words))
      movinggc_enlarge_semispaces();
  } // while_unlikely
  
  /* Ok, now we can allocate: */
  return movinggc_allocate_from_fromspace(size_in_words);
}
#endif // #ifdef MOVINGGC_USE_GLOBAL_POINTERS

#ifndef MOVINGGC_USE_GLOBAL_POINTERS
void* movinggc_allocate(const size_t size_in_words){
  /* Do we have enough space available in fromspace? */
  if_unlikely(fromspace->next_unallocated_object + size_in_words + 1 >
              fromspace->after_payload_end){
    /* No, we need to GC before allocating... */
    movinggc_gc();
    
    /* And maybe we should also enlarge semispaces, if we're really
       unlucky more than once: */
    while_unlikely(movinggc_should_we_enlarge_semispaces(size_in_words))
      movinggc_enlarge_semispaces();
  } // while_unlikely
  
  /* Ok, now we can allocate: */
  return movinggc_allocate_from(fromspace, size_in_words);
}
#endif // #ifndef MOVINGGC_USE_GLOBAL_POINTERS

void movinggc_initialize(void){
  printf("Each semispace is %li words long (%.1fKib)\n",
         (long)MOVINGGC_SEMISPACE_WORDS_NO,
         ((double)MOVINGGC_SEMISPACE_WORDS_NO) * sizeof(void*) / 1024.);
  movinggc_initialize_semispace(fromspace,
                                a_name,
                                MOVINGGC_SEMISPACE_WORDS_NO);
  movinggc_initialize_semispace(tospace,
                                b_name,
                                MOVINGGC_SEMISPACE_WORDS_NO);
#ifdef MOVINGGC_USE_GLOBAL_POINTERS
  movinggc_fromspace_next_unallocated_object =
    fromspace->next_unallocated_object;
  movinggc_fromspace_after_payload_end =
    fromspace->after_payload_end;
#endif // #ifdef MOVINGGC_USE_GLOBAL_POINTERS
}

void movinggc_dump_free_space_statistics_of(movinggc_semispace_header_t semispace){
  const size_t full_space_in_words __attribute__((unused)) =
    (fromspace_struct.next_unallocated_object -
     fromspace_struct.payload_beginning);;
  const size_t full_space_in_bytes __attribute__((unused)) =
    full_space_in_words * sizeof(void*);
  const size_t semispace_size_in_bytes =
    semispace->payload_size_in_words * sizeof(void*);
  printf("%s is %.1f%% full (%.1fKib of %.1fKib)\n",
         fromspace_struct.name,
         movinggc_fill_ratio_of(fromspace, 0) * 100.0,
         full_space_in_bytes / 1024.0,
         semispace_size_in_bytes / 1024.0);
}
void movinggc_dump_free_space_statistics(void){
  movinggc_dump_free_space_statistics_of(fromspace);
}

struct movinggc_root{
  /* The address of the candidate pointer *must* be indirect, as we're
     gonna move it at collection time. */
  void **pointer_to_roots;
  size_t size_in_words;
}; // struct

struct movinggc_root *movinggc_roots = NULL;
size_t movinggc_roots_allocated_size = 0;
size_t movinggc_roots_no = 0;
void register_root(void **pointer_to_roots,
                   size_t size_in_words){
  /* Enlarge the array of roots, if needed: */
  if(movinggc_roots_no == movinggc_roots_allocated_size){
    movinggc_verbose_log("Enlarging the root array from %i ", (int)movinggc_roots_allocated_size);
    if(movinggc_roots_allocated_size == 0)
      movinggc_roots_allocated_size = MOVINGGC_INITIAL_ROOTS_ALLOCATED_SIZE;
    else
      movinggc_roots_allocated_size *= 2;
    movinggc_verbose_log("to %i\n", (int)movinggc_roots_allocated_size);
    movinggc_roots = (struct movinggc_root*)
      realloc(movinggc_roots, sizeof(struct movinggc_root) * movinggc_roots_allocated_size);
    if_unlikely(movinggc_roots == NULL)
      movinggc_fatal("register_root(): couldn't enlerge the array");
  } // if
  
  /* Add the new root: */
  movinggc_roots[movinggc_roots_no].pointer_to_roots = pointer_to_roots;
  movinggc_roots[movinggc_roots_no].size_in_words = size_in_words;
  movinggc_roots_no++;
  /* movinggc_verbose_log("Registered the root %p, whose first word contains %p\n", */
  /*        pointer_to_roots, *pointer_to_roots); */
  /* movinggc_verbose_log("Roots are now %i\n", (int)movinggc_roots_no); */
}

void movinggc_push_root(void **root_pointer){
  register_root(root_pointer, 1);
}

void movinggc_pop_root(void){
  movinggc_roots_no--;
}

static void movinggc_swap_spaces(void){
  /* Swap the space headers; header pointers are const, and they should stay
     like that because of GCC optimization, so we don't touch them. Of course
     the payload is not affected by this: */
  MOVINGGC_SWAP(fromspace_struct, tospace_struct);
  
  /* Reset the next_unallocated_object of what is now tospace, so that the
     next collection will start to fill it from the beginning: */
  tospace_struct.next_unallocated_object =
    tospace_struct.payload_beginning;;

#ifdef MOVINGGC_USE_GLOBAL_POINTERS
  /* Reset global pointers: */
  movinggc_fromspace_next_unallocated_object =
    fromspace_struct.next_unallocated_object;
  movinggc_fromspace_after_payload_end =
    fromspace_struct.after_payload_end;
#endif // #ifdef MOVINGGC_USE_GLOBAL_POINTERS

#ifdef MOVINGGC_DEBUG
  void **p;
  for(p = tospace_struct.payload_beginning;
      p < tospace_struct.after_payload_end;
      p++)
    *p = (void*)0xdead20;
  for(p = fromspace_struct.next_unallocated_object;
      p < fromspace_struct.after_payload_end;
      p++)
    *p = (void*)0xdead30;
#endif // #ifdef MOVINGGC_DEBUG
  /* int i; */
  /* for(i = 0; i < full_space_in_words; i += 3){ */
  /*   movinggc_verbose_log("%p (%s): [%lu] (header)\n", */
  /*          fromspace_struct.payload_beginning + i, */
  /*          movinggc_semispace_name_of(fromspace_struct.payload_beginning + i), */
  /*          (unsigned long)fromspace_struct.payload_beginning[i]); */
  /*   movinggc_verbose_log("  %p: [%lu(tagged) %p(tagged)] (content)\n", */
  /*          fromspace_struct.payload_beginning + i + 1, */
  /*          (unsigned long)MOVINGGC_UNTAG_NONPOINTER(fromspace_struct.payload_beginning[i + 1]), */
  /*          MOVINGGC_UNTAG_POINTER(fromspace_struct.payload_beginning[i + 2])); */
  /* } // for */
  /////
}

/* Return the untagged version if the parameter is a valid tagged pointer,
   otherwise return NULL: */
static const void* movinggc_untag_candidate_pointer(const void *tagged_candidate_pointer){
  if(MOVINGGC_IS_NONPOINTER(tagged_candidate_pointer)){
    movinggc_verbose_log("-  the parameter points to the nonpointer %li; doing nothing\n",
           (long)MOVINGGC_UNTAG_NONPOINTER(tagged_candidate_pointer));
    return NULL;
  } // if
  /* ... otherwise we can assume that the object is a tagged pointer, in production. */
  const void *untagged_candidate_pointer =
    MOVINGGC_UNTAG_POINTER(tagged_candidate_pointer);

#ifdef MOVINGGC_DEBUG
  /* We don't support NULL pointers: */
  if(untagged_candidate_pointer == NULL)
    movinggc_fatal("found a NULL (tagged) pointer in the heap\n");
#endif // #ifdef MOVINGGC_DEBUG

#ifdef MOVINGGC_DEBUG
  /* Is there a pointer tag? */
  if_unlikely(! MOVINGGC_IS_POINTER(tagged_candidate_pointer)){
    movinggc_verbose_log("tagged_candidate_pointer is %p\n", tagged_candidate_pointer);
    movinggc_verbose_log("tagged_candidate_pointer has tag %x\n", MOVINGGC_WORD_TO_TAG(tagged_candidate_pointer));
    movinggc_fatal("tagged_candidate_pointer is neither a pointer nor a non-pointer");
  }
#endif // #ifdef MOVINGGC_DEBUG
  
#ifdef MOVINGGC_DEBUG
  /* Does the parameter refer an already moved pointer? */
  if((untagged_candidate_pointer >= tospace->payload_beginning) &&
     (untagged_candidate_pointer < tospace->after_payload_end)){
    movinggc_verbose_log("-  the pointer %p is already in tospace (%s)\n",
           untagged_candidate_pointer,
           movinggc_semispace_name_of(untagged_candidate_pointer));
    movinggc_fatal("this should never happen");
  }
#endif // #ifdef MOVINGGC_DEBUG

  /* Ok, if we arrived here then the candidate pointer is definitely a pointer: */
  return untagged_candidate_pointer;
}

static void movinggc_scavenge_pointer_to_candidate_pointer(const void **pointer_to_candidate_pointer);

/* Move the given fromspace object and return a tagged pointer to the new tospace
   copy, unless it the parameter points to a forwarding pointer; in that case just
   return a tagged pointer to the tospace copy: */
void* movinggc_scavenge_pointer(const void *untagged_pointer){
  /* If we arrived here then the parameter refers a valid tagged pointer pointing
     within fromspace. */
  movinggc_verbose_log("*  moving %p (%s)\n",
                       untagged_pointer,
                       movinggc_semispace_name_of(untagged_pointer));
    
  /* Check whether the parameter refers a forwarding pointer: */
  const void *tagged_header = ((const void**)untagged_pointer)[-1];
  if_unlikely(MOVINGGC_IS_FORWARDING(tagged_header)){
    void **untagged_forwarding_pointer =
      MOVINGGC_UNTAG_FORWARDING_HEADER(tagged_header);
    movinggc_verbose_log("the thing I am about to move points to a forwarding pointer, %p (already untagged, %s)\n",
           untagged_forwarding_pointer,
           movinggc_semispace_name_of(untagged_forwarding_pointer));
    return MOVINGGC_TAG_POINTER(untagged_forwarding_pointer);
  } // if

#ifdef MOVINGGC_DEBUG
  /* Check that the header has a valid tag: */
  if_unlikely(! MOVINGGC_IS_NONFORWARDING(tagged_header)){
    movinggc_verbose_log("tagged_header is %p\n", tagged_header);
    movinggc_fatal("tagged_header is neither forwarding nor non-forwarding");
  }
#endif // #ifdef MOVINGGC_DEBUG

  /* Ok, the parameter refers a fromspace object which is not a forwarding pointer;
     we have to copy it and install a forwarding pointer in the original pointer
     object: */
  const size_t size_in_words =
    (movinggc_bitmask_t)MOVINGGC_UNTAG_NONFORWARDING_HEADER(tagged_header);
  const void **object_in_tospace =
    movinggc_allocate_from(tospace, size_in_words);
  ((const void**)untagged_pointer)[-1] =
    MOVINGGC_TAG_HEADER_AS_FORWARDING(object_in_tospace);
  movinggc_verbose_log("*  copying %p (%s, %i words) to %p (%s)\n",
         untagged_pointer,
         movinggc_semispace_name_of(untagged_pointer),
         (int)size_in_words,
         object_in_tospace,
         movinggc_semispace_name_of(object_in_tospace));
  movinggc_verbose_log("*  replaced the header at %p (%s) before the pointer with a forwarding pointer to %p\n",
         &untagged_pointer[-1],
         movinggc_semispace_name_of(&untagged_pointer[-1]),
         object_in_tospace);

  /* Now we have to copy object fields into the new copy, and scavenge
     the new copy (or just push the pointers to the words to be changed
     onto the stack, to be scavenged later): */
#ifdef MOVINGGC_USE_MEMCPY
  memcpy(object_in_tospace, untagged_pointer, size_in_words * sizeof(void*));
#endif // #ifdef MOVINGGC_USE_MEMCPY
  int i;
  for(i = 0; i < size_in_words; i++){
#ifndef MOVINGGC_USE_MEMCPY
    object_in_tospace[i] = ((const void**)untagged_pointer)[i];
#endif // #ifndef MOVINGGC_USE_MEMCPY

#ifdef MOVINGGC_USE_STACK
    /* When we use the explicit stack we *only* push pointers to (tagged)
       pointers, because at pop time we won't re-check the tag: */
    if(MOVINGGC_IS_POINTER(object_in_tospace[i]))
      movinggc_trace_stack_push(object_in_tospace + i);
#else
    movinggc_scavenge_pointer_to_candidate_pointer(object_in_tospace + i);
#endif // #ifdef MOVINGGC_USE_STACK
  } // for
  
#ifdef MOVINGGC_DEBUG
  /* Clear the original object, so that we can't use it by mistake: */
  memset(untagged_pointer, 0, size_in_words * sizeof(void*));
#endif // #ifdef MOVINGGC_DEBUG

  /* Return a tagged pointer to the new copy: */
  movinggc_verbose_log("*  done with %p\n", untagged_pointer);
  return MOVINGGC_TAG_POINTER(object_in_tospace);
}

static void movinggc_scavenge_pointer_to_candidate_pointer(const void **pointer_to_candidate_pointer){
  /* Dereference the pointer to the candidate pointer; this is always safe if
     the parameter is, in fact, a pointer to something: */
  const void *tagged_candidate_pointer = *pointer_to_candidate_pointer;
  
  /* Is the candidate pointer really a pointer? Scavenge it if it is, and update the
     pointer-to-pointer; otherwise we have nothing to do: */
  const void *untagged_pointer =
    movinggc_untag_candidate_pointer(tagged_candidate_pointer);
  if(untagged_pointer != NULL)
    *pointer_to_candidate_pointer = movinggc_scavenge_pointer(untagged_pointer);
}

static void movinggc_scavenge_pointer_to_pointer(const void **pointer_to_tagged_pointer)
  __attribute__((unused));
static void movinggc_scavenge_pointer_to_pointer(const void **pointer_to_tagged_pointer){
  /* Dereference the pointer-pointer; this is always safe if
     the parameter is, in fact, a pointer to something: */
  const void *tagged_pointer = *pointer_to_tagged_pointer;
  
  /* Scavenge and update: if the parameter is in fact a pointer to a tagged pointer,
     as it is assumed to be, we don't have to check anything: */
  const void *untagged_pointer = MOVINGGC_UNTAG_POINTER(tagged_pointer);
#ifdef MOVINGGC_DEBUG
  if(!MOVINGGC_IS_POINTER(tagged_pointer))
    fatal("movinggc_scavenge_pointer_to_pointer(): the parameter does not point to a tagged pointer");
#endif // #ifdef MOVINGGC_DEBUG
  *pointer_to_tagged_pointer = movinggc_scavenge_pointer(untagged_pointer);
}

static long movinggc_gc_index = 0;
long movinggc_gc_no(void){
  return movinggc_gc_index;
}

void movinggc_gc(void){
  movinggc_log("GC#%i: copying from %s to %s...\n", movinggc_gc_index,
               fromspace->name, tospace->name);
  int root_index;

  /* ////// */
  /* movinggc_verbose_log("Printing root words before GC:\n"); */
  /* for(root_index = 0; root_index < movinggc_roots_no; root_index++){ */
  /*   const int words_no = movinggc_roots[root_index].size_in_words; */
  /*   int word_index; */
  /*   for(word_index = 0; word_index < words_no; word_index++){ */
  /*     void *word = (movinggc_roots[root_index].pointer_to_roots[word_index]); */
  /*     movinggc_verbose_log("(tagged %p) ", word); */
  /*     if(MOVINGGC_IS_NONPOINTER(word)){ */
  /*       movinggc_verbose_log("non-pointer %li", (long)MOVINGGC_UNTAG_NONPOINTER(word)); */
  /*     } */
  /*     else if(MOVINGGC_IS_POINTER(word)){ */
  /*       movinggc_verbose_log("pointer %p (%s)", */
  /*              MOVINGGC_UNTAG_POINTER(word), */
  /*              movinggc_semispace_name_of(MOVINGGC_UNTAG_POINTER(word))); */
  /*     } */
  /*     movinggc_verbose_log("\n"); */
  /*   } */
  /* } // for */
  /* movinggc_verbose_log("\n"); */
  /* ////// */

  for(root_index = 0; root_index < movinggc_roots_no; root_index++){
    const void **candidate_pointers = (const void **)
      movinggc_roots[root_index].pointer_to_roots;
    const int words_no = movinggc_roots[root_index].size_in_words;
    int word_index;
    for(word_index = 0; word_index < words_no; word_index++){
      movinggc_verbose_log("Scavenging the %i-th word (of %i) of a root, %p (containing the candidate pointer %p)\n",
             word_index, words_no,
             &(movinggc_roots[root_index].pointer_to_roots[word_index]),
             movinggc_roots[root_index].pointer_to_roots[word_index]);
      //movinggc_scavenge_pointer_to_candidate_pointer(&(movinggc_roots[root_index].pointer_to_roots[word_index]));
      movinggc_scavenge_pointer_to_candidate_pointer(candidate_pointers + word_index);
    }
  } // for

#ifdef MOVINGGC_USE_STACK
  while(! movinggc_is_trace_stack_empty())
    movinggc_scavenge_pointer_to_pointer(movinggc_trace_stack_pop());
#endif // #ifdef MOVINGGC_USE_STACK

  /* ///////////////// */
  /* movinggc_verbose_log("Re-printing root words after GC:\n"); */
  /* for(root_index = 0; root_index < movinggc_roots_no; root_index++){ */
  /*   const int words_no = movinggc_roots[root_index].size_in_words; */
  /*   int word_index; */
  /*   for(word_index = 0; word_index < words_no; word_index++){ */
  /*     void *word = (movinggc_roots[root_index].pointer_to_roots[word_index]); */
  /*     movinggc_verbose_log("(tagged %p) ", word); */
  /*     if(MOVINGGC_IS_NONPOINTER(word)){ */
  /*       movinggc_verbose_log("non-pointer %li", (long)MOVINGGC_UNTAG_NONPOINTER(word)); */
  /*     } */
  /*     else if(MOVINGGC_IS_POINTER(word)){ */
  /*       movinggc_verbose_log("pointer %p (%s)", */
  /*              MOVINGGC_UNTAG_POINTER(word), */
  /*              movinggc_semispace_name_of(MOVINGGC_UNTAG_POINTER(word))); */
  /*     } */
  /*     movinggc_verbose_log("\n"); */
  /*   } */
  /* } // for */
  /* movinggc_verbose_log("\n"); */
  /* ////////////////// */

  movinggc_swap_spaces();
  //movinggc_log("GC#%i: done.\n", (int)movinggc_gc_index);
  movinggc_gc_index++;
}
