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


#include "epsilongc_types.h"
#include <stdio.h>
#include <stdbool.h>
#include <limits.h>
#include "trace.h"
#include "epsilongc_features.h"
#include "compile_time_parameters.h"
#include "set_of_pages.h"
#include "malloc.h"
#include "page.h"
#include "epsilongc_debug.h"
#include "time.h"
#include "roots.h"
#include "myrand.h"
#include <sys/mman.h> // we use mmap()

struct epsilongc_mark_stack_element{
  epsilongc_word_t pointer;
  epsilongc_tracer_t tracer;
};
typedef struct epsilongc_mark_stack_element* epsilongc_mark_stack_t;

/* /\* This solution is obviously not parallelizable, hence temporary: *\/ */
/* static epsilongc_mark_stack_t epsilongc_mark_stack = NULL; // 'invalid' initialization, to catch obvious bugs */
/* static epsilongc_integer_t epsilongc_mark_stack_allocated_size = 0; // invalid, again */
/* static epsilongc_integer_t epsilongc_mark_stack_top_index; */

/* The number of currently-existing marking stack blocks: : */
epsilongc_integer_t epsilongc_stack_blocks_no;

/* Over-the-top pointer in the current marking stack block, or NULL: */
static __thread epsilongc_word_t *epsilongc_my_above_the_top_pointer;

/* Pointer to the mark stack block currently owned by this thread, or NULL: */
static __thread epsilongc_stack_block_t epsilongc_my_stack_block;

void epsilongc_initialize_tracing_support(void){
  /* We can make some very cheap (zero instrctions, in fact) sanity checks here: */
  const epsilongc_integer_t stack_block_payload_size = EPSILONGC_STACK_BLOCK_PAYLOAD_SIZE_IN_WORDS;
  if(stack_block_payload_size < 1)
    epsilongc_fatal("the mark-stack block payload size is %i: block size is too small",
                    (int)stack_block_payload_size);
  /* We have no stack blocks yet: */
  epsilongc_stack_blocks_no = 0;
  
/* #if defined(HAVE_MMAP64) */
/* /\* We probably are on a 32-bit system using 64-bit file offsets. *\/ */
/* #define MY_MMAP mmap64 */
/* #define MY_MUNMAP munmap */
/* #elif defined(HAVE_MMAP) */
/* /\* This can be either 32 or 64 bit. *\/ */
/* #define MY_MMAP mmap */
/* #define MY_MUNMAP munmap */
/* #else */
/* /\* What kind of stupid system is this?! *\/ */
/* #warning "No mmap(): reverting to malloc(), but this will be very inefficient." */
/* #define MY_MMAP(UNUSED1, SIZE, UNUSED2, UNUSED3, UNUSED4, UNUSED5) \ */
/*   epsilongc_xmalloc(SIZE) */
/* #define MY_MUNMAP(POINTER, UNUSED_SIZE) \ */
/*   free(POINTER) */
/* #endif */
/*   epsilongc_mark_stack_allocated_size = //EPSILONGC_INITIAL_MARK_STACK_ALLOCATED_SIZE; */
/*     1 * 1024 * 1024 * 1024; */
/*   epsilongc_mark_stack = (epsilongc_mark_stack_t) */
/*     MY_MMAP(NULL, // preferred address: we don't have perferences */
/*             epsilongc_mark_stack_allocated_size, // size */
/*             PROT_READ | PROT_WRITE, // protection: read and write (execution isn't needed) */
/*             MAP_PRIVATE | MAP_ANONYMOUS, // don't write anything back, don't use any file */
/*             0,  // file descriptor: not used for an anonymous mmap() */
/*             0); // offset: not used for an anonymous mmap() */
/*   if(epsilongc_mark_stack == (epsilongc_mark_stack_t)-1) */
/*     epsilongc_fatal("couldn't allocate the mark stack"); */
/*   epsilongc_mark_stack_top_index = -1; */
}
void epsilongc_finalize_tracing_support(void){
  //free(epsilongc_mark_stack);
  epsilongc_fatal("To do: implement epsilongc_finalize_tracing_support()");
  /* if(MY_MUNMAP(epsilongc_mark_stack, epsilongc_mark_stack_allocated_size) != 0) */
  /*   epsilongc_fatal("mmunmap() failed to destroy the mark stack"); */
  /* epsilongc_mark_stack = NULL; // set the variable to an invalid value, for debugging */
}

/* Forward-declare high-level stack primitives: */
// To do: should these have attribute always_inline? They are not very small, but possibly performance-critical.
static void epsilongc_push_pointer(epsilongc_word_t pointer);
static epsilongc_word_t epsilongc_pop_pointer(void);
static bool epsilongc_is_my_stack_block_empty(void);
static bool epsilongc_is_my_stack_block_full(void);

/* void epsilongc_trace_pointer_if_valid(epsilongc_word_t candidate_pointer){ */
/*   /\* Get the candidate page of the candidate pointer: *\/ */
/*   epsilongc_page_t candidate_page = */
/*     epsilongc_candidate_pointer_to_candidate_page(candidate_pointer); */
  
/*   /\* Do nothing if the candidate page isn't a page: *\/ */
/*   if(! epsilongc_does_page_exist(candidate_page)) */
/*     return; */
  
/*   /\* Ok, the page exists, but the pointer might still be out of the payload: *\/ */
/*   if(! epsilongc_is_pointer_within_page_valid_in_page(candidate_pointer, candidate_page)) */
/*     return; */
  
/*   /\* Do nothing if the candidate page belongs to the empty page list: we must */
/*      be tracing a false pointer, and in this case we can recognize this fact: *\/ */
/*   if(candidate_page->belongs_to_the_empty_pages_list){ */
/* #ifdef ENABLE_ASSERTIONS */
/*     printf("Warning: %p is a false pointer referring an empty page\n", candidate_pointer); */
/* #endif // #ifdef ENABLE_ASSERTIONS */
/*     return; */
/*   } // if */
  
/*   /\* Good, the pointer really points to a heap object; get its index: *\/ */
/*   const epsilongc_unsigned_integer_t index = */
/*     EPSILONGC_PAGE_AND_OBJECT_TO_MARK_ARRAY_INDEX(candidate_page, candidate_pointer); */
  
/* //#ifdef ENABLE_INTERIOR_POINTERS */
/*   /\* Now re-compute the pointer from the index, to get the address of the */
/*      FIRST word of the object (passing an interior pointer to a marker would */
/*      make a very hard to debug mess. And don't ask me how I know it). *\/ */
/*   candidate_pointer = */
/*     EPSILONGC_PAGE_AND_MARK_ARRAY_INDEX_TO_OBJECT(candidate_page, index); */
/* //#endif // #ifdef ENABLE_INTERIOR_POINTERS */
  
/*   /\* Do nothing if the object is already marked: *\/ */
/*   const epsilongc_pointer_t mark_array = (const epsilongc_pointer_t) */
/*     EPSILONGC_PAGE_TO_MARK_ARRAY(candidate_page); */
/*   if(EPSILONGC_LOOKUP_MARK_ARRAY(mark_array, index) != 0){ */
/*     //printf("[Doing nothing with %p, which is already marked]\n", candidate_pointer); */
/*     return; */
/*   } // if */

/*   /\* Ok, we have found an unmarked heap object: trace it: *\/ */
/*   //printf("!!! [Marking %p]\n", candidate_pointer); */
/*   EPSILONGC_ATOMICALLY_UPDATE_MARK_ARRAY(mark_array, index, 1); */
/*   epsilongc_push_pointer(candidate_pointer); */
/* } */

void epsilongc_trace_pointer_if_valid(epsilongc_word_t candidate_pointer){
  /* Do nothing if the pointer is not word-aligned: this check is
     performed right at the beginning, as many false pointers should have
     this shape: */
  const epsilongc_unsigned_integer_t candidate_pointer_as_bitmask =
    (const epsilongc_unsigned_integer_t)candidate_pointer;
  if((candidate_pointer_as_bitmask % sizeof(epsilongc_word_t)) != 0)
    return;
  
  /* Get the candidate page of the candidate pointer: */
  const epsilongc_page_t candidate_page =
    epsilongc_candidate_pointer_to_candidate_page(candidate_pointer);
  
  /* Do nothing if the candidate page is NULL; this happens for NULL
     candidate_pointers and for very small integers false pointers (quite
     common in practice): */
  if(candidate_page == NULL)
    return;
  
  /* Look at the page sort, and if it's part of a large object then get its
     payload: */
  epsilongc_word_t large_object_payload;
  const epsilongc_sort_of_page_t sort_of_page =
    epsilongc_candidate_page_to_sort_of_page(candidate_page, &large_object_payload);
  
  /* Do nothing if the candidate page isn't a heap page: */
  if(sort_of_page == epsilongc_not_a_heap_page_sort)
    return;
  
  /* Ok, the page exists, and may either be part of a large object or a
     proper page: */
  if(EPSILONGC_UNLIKELY(sort_of_page == epsilongc_large_object_page_sort)){
    /* For efficiency, we don't check for false pointers referring
       the header of a large object. To do: should this change? */
    const epsilongc_large_object_header_t large_object_header =
      (const epsilongc_large_object_header_t)candidate_page;

    /* printf("* candidate_pointer:    %p\n", candidate_pointer); */
    /* printf("* large_object_header:  %p\n", large_object_header); */
    /* printf("* large_object_payload: %p\n", large_object_payload); */
    /* printf("* candidate_page:       %p\n", candidate_page); */

    /* Do nothing if the object is already marked; otherwise mark it and
       push *the beginning of its payload* onto the stack: */
    if(large_object_header->is_marked)
      return;
    large_object_header->is_marked = true;
    epsilongc_push_pointer(large_object_payload);
    return;
  } // if
  
  /* If we arrived here then we're dealing with a proper page, not
     with a large object: */
  
  /* Ok, the page exists, but the pointer might still be out of the payload: */
  if(! epsilongc_is_aligned_pointer_within_page_valid_in_page(candidate_pointer, candidate_page))
    return;
  
  /* Do nothing if the candidate page belongs to the empty page list: we must
     be tracing a false pointer, and in this case we can recognize this fact: */
  if(candidate_page->belongs_to_the_empty_pages_list){
#ifdef ENABLE_ASSERTIONS
    printf("Warning: %p is a false pointer referring an empty page\n", candidate_pointer);
#endif // #ifdef ENABLE_ASSERTIONS
    return;
  } // if
  
  /* Good, the pointer really points to a heap object; get its index: */
  const epsilongc_unsigned_integer_t index =
    EPSILONGC_PAGE_AND_OBJECT_TO_MARK_ARRAY_INDEX(candidate_page, candidate_pointer);
  
//#ifdef ENABLE_INTERIOR_POINTERS
  /* Now re-compute the pointer from the index, to get the address of the
     FIRST word of the object (passing an interior pointer to a marker would
     make a very hard to debug mess. And don't ask me how I know it). */
  candidate_pointer =
    EPSILONGC_PAGE_AND_MARK_ARRAY_INDEX_TO_OBJECT(candidate_page, index);
//#endif // #ifdef ENABLE_INTERIOR_POINTERS
  
  /* Do nothing if the object is already marked: */
  const epsilongc_pointer_t mark_array = (const epsilongc_pointer_t)
    EPSILONGC_PAGE_TO_MARK_ARRAY(candidate_page);
  if(EPSILONGC_LOOKUP_MARK_ARRAY(mark_array, index) != 0){
    //printf("[Doing nothing with %p, which is already marked]\n", candidate_pointer);
    return;
  } // if

  /* Ok, we have found an unmarked heap object: trace it: */
  //printf("!!! [Marking %p]\n", candidate_pointer);
  EPSILONGC_ATOMICALLY_UPDATE_MARK_ARRAY(mark_array, index, 1);
  epsilongc_push_pointer(candidate_pointer);
}

void epsilongc_trace_aligned_words_in_buffer(const epsilongc_word_t *buffer,
                                             const epsilongc_unsigned_integer_t size_in_bytes){
  /* The buffer should contain whole words: */
  epsilongc_assert_on_debug((size_in_bytes % sizeof(epsilongc_word_t)) == 0);
  const epsilongc_unsigned_integer_t size_in_words =
    size_in_bytes / sizeof(epsilongc_word_t);
  
  /* Trace each candidate pointer, in order: */
  epsilongc_integer_t i;
  for(i = 0; i < size_in_words; i++)
    epsilongc_trace_pointer_if_valid(buffer[i]);
}

void epsilongc_trace_buffer_with_all_alignments(const epsilongc_pointer_t buffer,
                                                const epsilongc_unsigned_integer_t size_in_bytes){
  /* Access each possible candidate pointer in order; this works with 'load byte'
     and 'shift left' instructions, hence it always avoids unaligned word accesses
     in memory: */
  epsilongc_integer_t byte_index;
  const unsigned char *buffer_as_array_of_bytes =
    (const unsigned char*)buffer;
  epsilongc_unsigned_integer_t candidate_pointer_as_unsigned = 0;
  for(byte_index = 0; byte_index < size_in_bytes; byte_index++){
    /* The next pointer can be obtained from the previous one by left-shifting
       it by one byte, and setting its least significative byte to the next
       byte from the buffer: */
#ifdef WORDS_BIGENDIAN
    candidate_pointer_as_unsigned <<= (epsilongc_unsigned_integer_t)CHAR_BIT;
    candidate_pointer_as_unsigned |= buffer_as_array_of_bytes[byte_index];
#else
    /* Idiotic little endians! */
    candidate_pointer_as_unsigned >>= (epsilongc_unsigned_integer_t)CHAR_BIT;
    candidate_pointer_as_unsigned |=
      ((epsilongc_unsigned_integer_t)
       (buffer_as_array_of_bytes[byte_index])) <<
      ((epsilongc_unsigned_integer_t)
       (CHAR_BIT * (sizeof(epsilongc_word_t) - 1)));
#endif // #ifdef WORDS_BIGENDIAN
    /* Trace the candidate pointer, unless we've not filled a word yet (GCC's
       loop optimizer should be able to move this check away from the loop...
       To do: check that it does): */
    //if(byte_index + 1 >= sizeof(epsilongc_word_t))
      {
      const epsilongc_word_t candidate_pointer =
        (const epsilongc_word_t)candidate_pointer_as_unsigned;
#ifdef ENABLE_VERBOSE_DEBUG
      if(byte_index % sizeof(epsilongc_word_t) == 0)
        printf("The aligned word at this position is %lx\n",
               (unsigned long)
               (((epsilongc_unsigned_integer_t*)buffer_as_array_of_bytes)[byte_index / sizeof(epsilongc_word_t)]));
      printf("Tracing %lx\n", (unsigned long)candidate_pointer);
#endif // #ifdef ENABLE_VERBOSE_DEBUG
      epsilongc_trace_pointer_if_valid(candidate_pointer);
    } // if
  } // for
}

/*
  // This is a good test for epsilongc_trace_buffer_with_all_alignments():
  char buffer[] =
    {0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff, 0x00};
  epsilongc_trace_buffer_with_all_alignments(buffer, sizeof(buffer));
*/

/* Make a new stack block, without any synchronization. The content of the payload of the
   returned block is undefined: */
epsilongc_stack_block_t epsilongc_make_stack_block_without_adding_it_to_global_structures(void){
  /* Allocate the block: */
  epsilongc_stack_block_t block =
#ifdef ENABLE_MMAP_MARK_STACK_BLOCKS
    mmap(NULL, // preferred address: we don't have perferences
         EPSILONGC_STACK_BLOCK_SIZE_IN_BYTES, // size
         PROT_READ | PROT_WRITE, // protection: read and write (execution isn't needed)
         MAP_PRIVATE | MAP_ANONYMOUS, // don't write anything back, don't use any file
         0,  // file descriptor: not used for an anonymous mmap()
         0); // offset: not used for an anonymous mmap()
  if(EPSILONGC_UNLIKELY(block == (epsilongc_stack_block_t)-1))
    epsilongc_fatal("couldn't mmap() a mark stack block");
#else
  epsilongc_xmalloc(EPSILONGC_STACK_BLOCK_SIZE_IN_BYTES);
#endif // #ifdef ENABLE_MMAP_MARK_STACK_BLOCKS
  
  /* Set fields and return the block we just made: */
  block->above_the_top_pointer =
    EPSILONGC_STACK_BLOCK_TO_PAYLOAD_BEGINNING(block);
  EPSILONGC_INITIALIZE_LIST_ELEMENT(stack_blocks, block);
  //memset(block->above_the_top_pointer, 0, EPSILONGC_STACK_BLOCK_PAYLOAD_SIZE_IN_WORDS * sizeof(epsilongc_word_t));
  //printf("%s: made the stack block %p\n", epsilongc_calling_thread_name(), block);
  return block;
}

void epsilongc_destroy_stack_block_without_removing_it_from_global_structures(epsilongc_stack_block_t block){
#ifdef ENABLE_MMAP_MARK_STACK_BLOCKS
  munmap(block, EPSILONGC_STACK_BLOCK_SIZE_IN_BYTES);
#else
  free(block);
#endif // #ifdef ENABLE_MMAP_MARK_STACK_BLOCKS
  //printf("%s: destroyed the stack block %p\n", epsilongc_calling_thread_name(), block);
}

/* Return true iff the given stack block is empty; notice that this requires the
   over_the_top_pointer to be up-to-date: */
inline static bool epsilongc_is_stack_block_empty(epsilongc_stack_block_t block){
  return (block->above_the_top_pointer ==
          EPSILONGC_STACK_BLOCK_TO_PAYLOAD_BEGINNING(block));
}

/* Return true iff the given stack block is full; notice that this requires the
   over_the_top_pointer to be up-to-date: */
inline static bool epsilongc_is_stack_block_full(epsilongc_stack_block_t block){
  return (block->above_the_top_pointer ==
          EPSILONGC_STACK_BLOCK_TO_AFTER_PAYLOAD_END(block));
}

/* Destroy an existing stack block, without removing it from the global
   structures and without any synchronization: */
inline static void epsilongc_destroy_stack_blocks_without_removing_them_from_global_structures(epsilongc_stack_block_t *blocks,
                                                                                               const epsilongc_integer_t how_many){
  /* It makes no sense to call this for desoying less than 1 block: */
  epsilongc_assert_on_debug(how_many > 0);
  
  /* Ok, actually destroy blocks: */
  epsilongc_integer_t i;
  for(i = 0; i < how_many; i++)
    epsilongc_destroy_stack_block_without_removing_it_from_global_structures(blocks[i]);
}

/* Make the given number of stack blocks and add them to the global structures, with
   the appropriate synchronization: */
inline static void
epsilongc_make_stack_blocks_and_add_them_to_global_structures(const epsilongc_integer_t how_many,
                                                              const epsilongc_integer_t delta_pointers_no,
                                                              const epsilongc_integer_t delta_objects_no,
                                                              const bool locked){
  /* It makes no sense to call this for making less than 1 block: */
  epsilongc_assert_on_debug(how_many > 0);
  
  /* Make stack blocks and keep pointers to them in a temporary list; this requires
     no synchronization: */
  struct epsilongc_stack_block_list temporary_list;
  EPSILONGC_INITIALIZE_LIST(stack_blocks, &temporary_list);
  epsilongc_integer_t i;
  for(i = 0; i < how_many; i++){
    epsilongc_stack_block_t stack_block =
      epsilongc_make_stack_block_without_adding_it_to_global_structures();
    EPSILONGC_PREPEND_OBJECT_TO_LIST(stack_blocks, stack_blocks,
                                    epsilongc_stack_block_t, stack_block, &temporary_list);
  } // for
  
  /* Add the blocks in our list to the global structures: this requires a critical
     section but is O(1), and very fast: */
  if(locked)
    epsilongc_lock_mutex(epsilongc_stack_block_mutex);
  //printf("MADE %i empty stack blocks\n", (int)how_many); fflush(stdout);
  EPSILONGC_PREPEND_SECOND_LIST_TO_FIRST_LIST(stack_blocks, stack_blocks, stack_blocks,
                                              epsilongc_stack_block_t,
                                              &epsilongc_empty_stack_blocks,
                                              &temporary_list);
  epsilongc_stack_blocks_no += how_many;
  epsilongc_pointers_no_in_the_worst_case += delta_pointers_no;
  epsilongc_objects_no_in_the_worst_case += delta_objects_no;
  //printf("+ Made %i stack blocks: stack blocks are now %i (%.1fMb)\n", (int)how_many, (int)epsilongc_stack_blocks_no, (double)epsilongc_stack_blocks_no * EPSILONGC_STACK_BLOCK_SIZE_IN_BYTES/ 1024.0 / 1024.0); fflush(stdout);
  if(locked)
    epsilongc_unlock_mutex(epsilongc_stack_block_mutex);
}

/* Destroy the given number of stack blocks and remove them from global structures, with
   the appropriate synchronization. This should not be called during garbage collection,
   hence it's assumed that all blocks are empty: */
inline static void epsilongc_destroy_stack_blocks_and_remove_them_from_global_structures(
                      const epsilongc_integer_t how_many,
                      const epsilongc_integer_t delta_pointers_no,
                      const epsilongc_integer_t delta_objects_no,
                      const bool locked){
  /* It makes no sense to call this to destroy less than one stack block: */
  epsilongc_assert_on_debug(how_many > 0);
  
  /* Detach elements from the list and keep pointers in a temporary array; don't destroy
     anything yet; we have to do this in a critical section, so it must be fast: */
  // To do: make a macro to detach n elements from a list, and use it. It would be a little
  // faster than this (but with the same asymptotic complexity, of course)
  epsilongc_stack_block_t *array = alloca(sizeof(epsilongc_stack_block_t) * how_many);
  epsilongc_integer_t i;
  if(locked)
    epsilongc_lock_mutex(epsilongc_stack_block_mutex);
  for(i = 0; i < how_many; i++){
    array[i] = EPSILONGC_FIRST_ELEMENT_OF_LIST(stack_blocks, &epsilongc_empty_stack_blocks);
    epsilongc_assert_on_debug(array[i] != NULL); // we must have enough blocks
    EPSILONGC_DETACH_ELEMENT_FROM_LIST(stack_blocks, stack_blocks, epsilongc_stack_block_t, array[i],
                                       &epsilongc_empty_stack_blocks);
  } // for
  epsilongc_stack_blocks_no -= how_many;
  epsilongc_pointers_no_in_the_worst_case += delta_pointers_no;
  epsilongc_objects_no_in_the_worst_case += delta_objects_no;
  //printf("- About to destroy %i stack blocks (out of the critical section): visible stack blocks are already only %i (%.1fMb)\n", (int)how_many, (int)epsilongc_stack_blocks_no, (double)epsilongc_stack_blocks_no * EPSILONGC_STACK_BLOCK_SIZE_IN_BYTES/ 1024.0 / 1024.0); fflush(stdout);
  if(locked)
    epsilongc_unlock_mutex(epsilongc_stack_block_mutex);
  
  /* Destroy the elements in the array, out of the critical section: */
  epsilongc_destroy_stack_blocks_without_removing_them_from_global_structures(array, how_many);
  //printf("DESTROYED %i stack blocks\n", (int)how_many); fflush(stdout);
}

/* Release the stack block owned by the calling thread to the appropriate list, with
   the appropriate synchronization: */
void epsilongc_release_stack_block(void){
  epsilongc_assert_on_debug(epsilongc_my_stack_block != NULL);
  //printf("%p: releasing stack block %p\n", (void*)pthread_self(), epsilongc_my_stack_block); fflush(stdout);
  epsilongc_my_stack_block->above_the_top_pointer =
    epsilongc_my_above_the_top_pointer;
  epsilongc_integer_t empty_stack_blocks_no_after_release;
  if(epsilongc_is_stack_block_full(epsilongc_my_stack_block)){ // the block is completely full
    epsilongc_lock_mutex(epsilongc_stack_block_mutex);
    EPSILONGC_PREPEND_OBJECT_TO_LIST(stack_blocks, stack_blocks, epsilongc_stack_block_t, epsilongc_my_stack_block, &epsilongc_full_stack_blocks);
    empty_stack_blocks_no_after_release = EPSILONGC_LENGTH_OF_LIST(stack_blocks, &epsilongc_empty_stack_blocks);
    epsilongc_unlock_mutex(epsilongc_stack_block_mutex);
    epsilongc_v_semaphore(epsilongc_nonempty_stack_blocks_no_semaphore); // a non-empty block has become avaible
  }
  else if(epsilongc_is_stack_block_empty(epsilongc_my_stack_block)){ // the block is completely empty
    epsilongc_lock_mutex(epsilongc_stack_block_mutex);
    EPSILONGC_PREPEND_OBJECT_TO_LIST(stack_blocks, stack_blocks, epsilongc_stack_block_t, epsilongc_my_stack_block, &epsilongc_empty_stack_blocks);
    empty_stack_blocks_no_after_release = EPSILONGC_LENGTH_OF_LIST(stack_blocks, &epsilongc_empty_stack_blocks);
    epsilongc_unlock_mutex(epsilongc_stack_block_mutex);
    /* We don't V the semaphore in this case: no thread ever waits for *empty* blocks
       to become available.  */
  }
  else{ // the block is neither empty nor full
    epsilongc_lock_mutex(epsilongc_stack_block_mutex);
    EPSILONGC_PREPEND_OBJECT_TO_LIST(stack_blocks, stack_blocks, epsilongc_stack_block_t, epsilongc_my_stack_block, &epsilongc_nonempty_and_nonfull_stack_blocks);
    empty_stack_blocks_no_after_release = EPSILONGC_LENGTH_OF_LIST(stack_blocks, &epsilongc_empty_stack_blocks);
    epsilongc_unlock_mutex(epsilongc_stack_block_mutex);
    epsilongc_v_semaphore(epsilongc_nonempty_stack_blocks_no_semaphore); // a non-empty block has become avaible
  } // else
  epsilongc_my_stack_block = NULL;
  epsilongc_my_above_the_top_pointer = NULL;
  
  /* If all blocks are now in the empty list then we have completed this marking
     phase, and we can wake up the thread which was waiting for this: */
  if(empty_stack_blocks_no_after_release == epsilongc_stack_blocks_no){
    //if(epsilongc_get_verbose_collection())
    //  printf("This marking phase is over\n"); fflush(stdout);
    epsilongc_v_semaphore(epsilongc_marking_phase_semaphore);
  } // if
}

/* If there is no available non-empty block for other marker threads and we
   have enough stack elements for ourselves then "donate" part of the content
   of our stack block to the others: */
void epsilongc_rebalance_if_needed(void){
  /* Do nothing if we don't have a stack block: */
  if(epsilongc_my_stack_block == NULL)
    return;
  
  /* Do nothing if we don't have enough elements: */
  const epsilongc_integer_t stack_elements_in_our_block =
    EPSILONGC_STACK_ELEMENTS_NO_IN_OUR_BLOCK;
  if(stack_elements_in_our_block < 2)
    return;
  
  /* If there are already some non-empty blocks available then return, doing
     nothing else; otherwise take an empty block (if any!), which will be our
     new block: */
  epsilongc_lock_mutex(epsilongc_stack_block_mutex);
  epsilongc_stack_block_t our_new_stack_block;
  const epsilongc_stack_block_t our_old_stack_block = epsilongc_my_stack_block;
  const bool are_there_nonfull_blocks =
    (EPSILONGC_LENGTH_OF_LIST(stack_blocks, &epsilongc_nonempty_and_nonfull_stack_blocks) > 0) ||
    (EPSILONGC_LENGTH_OF_LIST(stack_blocks, &epsilongc_full_stack_blocks) > 0);
  our_new_stack_block = EPSILONGC_FIRST_ELEMENT_OF_LIST(stack_blocks, &epsilongc_empty_stack_blocks);
  if(are_there_nonfull_blocks ||
     (our_new_stack_block == NULL)){ // if we don't have space for rebalancing, we just don't do it
    epsilongc_unlock_mutex(epsilongc_stack_block_mutex);
    return;
  } // if
  
  /* If we arrived here then we want to do rebalancing, and we can successfully
     acquire a new empty stack block: */
  EPSILONGC_DETACH_ELEMENT_FROM_LIST(stack_blocks, stack_blocks, epsilongc_stack_block_t,
                                     our_new_stack_block, &epsilongc_empty_stack_blocks);
  epsilongc_unlock_mutex(epsilongc_stack_block_mutex);

  /* Ok, if we have arrived here then we want to donate part of the content
     of our stack block. Copy the part that we want to keep (the top part is
     better for locality reasons) into the new block, adjust the stack pointer
     for the old block, release it, and finally acquire the new one. Notice that
     acquiring the new block requires no synchronization in this case, as we have
     already correctly detached the element from the list, and now we only work
     on thread-local variables: */
  const epsilongc_integer_t stack_elements_to_donate =
    stack_elements_in_our_block / 2;
  const epsilongc_integer_t stack_elements_to_keep =
    stack_elements_in_our_block - stack_elements_to_donate;
  memcpy(EPSILONGC_STACK_BLOCK_TO_PAYLOAD_BEGINNING(our_new_stack_block),
         EPSILONGC_STACK_BLOCK_TO_PAYLOAD_BEGINNING(our_old_stack_block) + stack_elements_to_donate,
         sizeof(epsilongc_word_t) * stack_elements_to_keep);
  /* epsilongc_integer_t i; */
  /* for(i = 0; i < stack_elements_to_keep; i++) */
  /*   *(EPSILONGC_STACK_BLOCK_TO_PAYLOAD_BEGINNING(our_new_stack_block) + i) = */
  /*     *(EPSILONGC_STACK_BLOCK_TO_PAYLOAD_BEGINNING(our_old_stack_block) + stack_elements_to_donate + i); */
  epsilongc_my_above_the_top_pointer -= stack_elements_to_donate;
  epsilongc_release_stack_block(); // this V's the semaphore
  epsilongc_my_above_the_top_pointer =
    EPSILONGC_STACK_BLOCK_TO_PAYLOAD_BEGINNING(our_new_stack_block) +
    stack_elements_to_keep;
  epsilongc_my_stack_block = our_new_stack_block;
  //printf("%p: rebalanced: donated %i elements (kept %i)\n", (void*)pthread_self(), (int)stack_elements_to_donate, (int)stack_elements_to_keep); fflush(stdout);
  //printf("%p: our old stack block was     %p\n", (void*)pthread_self(), our_old_stack_block); fflush(stdout);
  //printf("%p: our new stack block will be %p\n", (void*)pthread_self(), our_new_stack_block); fflush(stdout);
  //printf("%p: our stack block is now      %p\n", (void*)pthread_self(), epsilongc_my_stack_block); fflush(stdout);
}

/* It's possible to request blocks of the following "types": */
typedef enum epsilongc_stack_block_type{
  /* Not completely full blocks: either partially full or empty: */
  epsilongc_nonfull_stack_block_type,
  
  /* Not completely empty blocks: either partially full or full: */
  epsilongc_nonempty_stack_block_type,
} epsilongc_stack_block_type_t; // enum

/* Remove a block with the given fullness from the appropriate list, and return
   it; if a non-full block is requested this can not fail; otherwise, when there
   are no blocks of the required type, we just return NULL: */
static epsilongc_stack_block_t
epsilongc_get_stack_block_if_possible(const epsilongc_stack_block_type_t required_type){
  epsilongc_stack_block_t new_stack_block;
  switch(required_type){
  case epsilongc_nonfull_stack_block_type:
    /* Detach a non-full block from a list; we prefer to get an empty block when
       possible, and leave non-empty blocks to other threads which may need to
       pop: */
    epsilongc_lock_mutex(epsilongc_stack_block_mutex);
    new_stack_block = EPSILONGC_FIRST_ELEMENT_OF_LIST(stack_blocks, &epsilongc_empty_stack_blocks);
    if(new_stack_block != NULL){
      /* Ok, we found an empty stack block; take it: */
      EPSILONGC_DETACH_ELEMENT_FROM_LIST(stack_blocks, stack_blocks, epsilongc_stack_block_t,
                                         new_stack_block, &epsilongc_empty_stack_blocks);
      epsilongc_unlock_mutex(epsilongc_stack_block_mutex);
    } // if
    else{
      /* We didn't find an empty stack block; let's take a non-full and non-empty
         one; we may need to block for this: */
      epsilongc_unlock_mutex(epsilongc_stack_block_mutex);
      printf("DEADLOCKED?\n");
      epsilongc_p_semaphore(epsilongc_nonempty_stack_blocks_no_semaphore);
      printf("NO!\n");
      epsilongc_lock_mutex(epsilongc_stack_block_mutex);
      /* We could't find a non-empty non-full block: let's take an empty one: */
      new_stack_block = EPSILONGC_FIRST_ELEMENT_OF_LIST(stack_blocks, 
                                                        &epsilongc_nonempty_and_nonfull_stack_blocks);
      if(new_stack_block != NULL){
        EPSILONGC_DETACH_ELEMENT_FROM_LIST(stack_blocks, stack_blocks, epsilongc_stack_block_t,
                                           new_stack_block,
                                           &epsilongc_nonempty_and_nonfull_stack_blocks);
        epsilongc_unlock_mutex(epsilongc_stack_block_mutex);
      } // inner if
      else
        assert(false); // we got out from the P(), so there must be a block
    }
    break;
  case epsilongc_nonempty_stack_block_type:
    /* Detach a non-empty block from a list; we prefer to reuse non-empty non-full
       blocks when possible; such a block may not be immediately available, so we
       must passively wait when there are zero non-empty stack blocks: */
    epsilongc_p_semaphore(epsilongc_nonempty_stack_blocks_no_semaphore);
    epsilongc_lock_mutex(epsilongc_stack_block_mutex);
    new_stack_block = EPSILONGC_FIRST_ELEMENT_OF_LIST(stack_blocks, &epsilongc_nonempty_and_nonfull_stack_blocks);
    if(new_stack_block != NULL){
      /* Ok, we found a non-empty non-full block; take it: */
      EPSILONGC_DETACH_ELEMENT_FROM_LIST(stack_blocks, stack_blocks, epsilongc_stack_block_t, new_stack_block,
                                         &epsilongc_nonempty_and_nonfull_stack_blocks);
    } // if
    else{
      /* We could't find a non-empty non-full block: let's take a full one: */
      new_stack_block = EPSILONGC_FIRST_ELEMENT_OF_LIST(stack_blocks, &epsilongc_full_stack_blocks);
      if(new_stack_block != NULL){
        EPSILONGC_DETACH_ELEMENT_FROM_LIST(stack_blocks, stack_blocks, epsilongc_stack_block_t, new_stack_block,
                                           &epsilongc_full_stack_blocks);
      } // inner if
    } // else
    epsilongc_unlock_mutex(epsilongc_stack_block_mutex);
    break;
  default:
    /* Unreachable: */
    assert(false);
  } // switch
  
  /* Return the stack block we took (or NULL): */
  return new_stack_block;
}

/* Acquire a block with the given fullness from the appropriate list; if a
   non-full block is requested this can not fail; otherwise, when there are no
   full blocks, the thread-local block will remain NULL after having called
   this: */
static void epsilongc_acquire_stack_block_if_possible(const epsilongc_stack_block_type_t required_type){
  /* We must have no stack block when we call this: */
  epsilongc_assert_on_debug(epsilongc_my_stack_block == NULL);
  
  /* Get the stack block, if possible: */
  epsilongc_stack_block_t new_stack_block =
    epsilongc_get_stack_block_if_possible(required_type);
  
  /* Update our thread-local information about the new block (if any), out of
     the critical section: */
  if(new_stack_block != NULL){
    epsilongc_my_stack_block = new_stack_block;
    epsilongc_my_above_the_top_pointer =
      new_stack_block->above_the_top_pointer;
    //printf("%p:  acquired stack block %p\n", (void*)pthread_self(), epsilongc_my_stack_block); fflush(stdout);
  } // if
}

/* Acquire a stack block of the given type, assuming that
   acquisition succeeds. */
static void epsilongc_acquire_stack_block(const epsilongc_stack_block_type_t required_type){
  epsilongc_acquire_stack_block_if_possible(required_type);
  epsilongc_assert_on_debug(epsilongc_my_stack_block != NULL);
}

static void
epsilongc_make_or_destroy_stack_blocks_as_needed_possibly_locked(const epsilongc_integer_t delta_pointers_no,
                                                                 const epsilongc_integer_t delta_objects_no,
                                                                 const bool locked){
  /* How many blocks do exist now, and how large is the heap? We need a (very
     short) critical section for this: */
  if(locked)
    epsilongc_lock_mutex(epsilongc_stack_block_mutex);
  const epsilongc_integer_t current_pointers_no_in_the_worst_case =
    epsilongc_pointers_no_in_the_worst_case;
  const epsilongc_integer_t current_objects_no_in_the_worst_case =
    epsilongc_objects_no_in_the_worst_case;
  const epsilongc_integer_t current_stack_blocks_no =
    epsilongc_stack_blocks_no;
  if(locked)
    epsilongc_unlock_mutex(epsilongc_stack_block_mutex);
  
  /* How many blocks do we need to make or destroy? In the worst case the
     needed stack size in words is equal to the minimum between the number
     of pointers and the number of objects; and just to play it safe we add
     to this one block per marker thread plus one block, so that we can
     safely use integer division: */
  const epsilongc_integer_t new_pointers_no_in_the_worst_case =
    current_pointers_no_in_the_worst_case + delta_pointers_no;
  const epsilongc_integer_t new_objects_no_in_the_worst_case =
    current_objects_no_in_the_worst_case + delta_objects_no;
  const epsilongc_integer_t new_stack_size_in_words =
    (new_pointers_no_in_the_worst_case > new_objects_no_in_the_worst_case) ?
    new_pointers_no_in_the_worst_case :
    new_objects_no_in_the_worst_case;
  const epsilongc_integer_t new_stack_blocks_no =
    new_stack_size_in_words / EPSILONGC_STACK_BLOCK_PAYLOAD_SIZE_IN_WORDS +
    epsilongc_processors_no + 1;
  const epsilongc_integer_t blocks_no_delta =
    new_stack_blocks_no - current_stack_blocks_no;
  
  /* If the number of block hasn't changed we don't need to do anything more
     than updating the heap size; otherwise we need to create or destroy some
     blocks: */
  if(blocks_no_delta == 0){
    if(locked)
      epsilongc_lock_mutex(epsilongc_stack_block_mutex);
    epsilongc_pointers_no_in_the_worst_case += delta_pointers_no;
    epsilongc_objects_no_in_the_worst_case += delta_objects_no;
    if(locked)
      epsilongc_unlock_mutex(epsilongc_stack_block_mutex);
    //printf("%s: no need to make or destroy stack blocks.\n", epsilongc_calling_thread_name());
  }
  else if(blocks_no_delta < 0){
    //printf("%s: removing %i stack blocks: begin\n", epsilongc_calling_thread_name(), (int)-blocks_no_delta);
    epsilongc_destroy_stack_blocks_and_remove_them_from_global_structures(- blocks_no_delta,
                                                                          delta_pointers_no,
                                                                          delta_objects_no,
                                                                          locked);
    //printf("%s: removing %i stack blocks: end\n", epsilongc_calling_thread_name(), (int)-blocks_no_delta);
  }
  else if(blocks_no_delta > 0){
    //printf("%s: making %i stack blocks: begin\n", epsilongc_calling_thread_name(), (int)blocks_no_delta);
    epsilongc_make_stack_blocks_and_add_them_to_global_structures(blocks_no_delta,
                                                                  delta_pointers_no,
                                                                  delta_objects_no,
                                                                  locked);
    //printf("%s: making %i stack blocks: end\n", epsilongc_calling_thread_name(), (int)blocks_no_delta);
  } // if
  //printf("%s: objects are now %i and pointers are now %i\n", epsilongc_calling_thread_name(), (int)epsilongc_objects_no_in_the_worst_case, (int)epsilongc_pointers_no_in_the_worst_case); fflush(stdout);
}

void epsilongc_make_or_destroy_stack_blocks_as_needed(const epsilongc_integer_t delta_pointers_no,
                                                      const epsilongc_integer_t delta_objects_no){
  epsilongc_make_or_destroy_stack_blocks_as_needed_possibly_locked(delta_pointers_no,
                                                                   delta_objects_no,
                                                                   true);
}
void epsilongc_make_or_destroy_stack_blocks_as_needed_unlocked(const epsilongc_integer_t delta_pointers_no,
                                                               const epsilongc_integer_t delta_objects_no){
  epsilongc_make_or_destroy_stack_blocks_as_needed_possibly_locked(delta_pointers_no,
                                                                   delta_objects_no,
                                                                   false);
}
/* Push the given pointer into the current stack block; this function automatically
   changes the current stack block if needed: */
static void epsilongc_push_pointer(epsilongc_word_t pointer){
  //printf("Pushing %p\n", pointer);
  epsilongc_assert_on_debug(pointer != NULL); 
  
  /* We need to push: take another non-full block if the current one is full: */
  if(epsilongc_is_my_stack_block_full()){
    //printf("+ Push: need to switch stack block: begin: %20p -> ", epsilongc_my_stack_block); fflush(stdout);
    /* Release the current block and get another non-full one: */
    epsilongc_release_stack_block();
    epsilongc_acquire_stack_block(epsilongc_nonfull_stack_block_type);
    //printf("%-20p\n", epsilongc_my_stack_block); fflush(stdout);
    
    /* Now we *must* have succeeded in acquiring a stack block, since we create
       blocks in a sufficient number. */
  } // if
  
  /* Ok, now we have a non-full block: */
  epsilongc_assert_on_debug(epsilongc_my_stack_block != NULL);
  epsilongc_assert_on_debug(! epsilongc_is_my_stack_block_full());
  
  /* Actually push, i.e. write the pointer into the appropriate place and advance
     the pointer: */
  *epsilongc_my_above_the_top_pointer = pointer;
  epsilongc_my_above_the_top_pointer ++;
}

/* Pop a pointer from the current stack block; this function automatically
   changes the current stack block if needed. This function returns NULL if
   the stack is empty; of course this interface is built on the assumption
   that NULL is not a valid value for a pointer in the stack: */
static epsilongc_word_t epsilongc_pop_pointer(void){
  /* We need to pop: if the current block if empty we need to change it with
     another non-empty one: */
  if(epsilongc_is_my_stack_block_empty()){
    //printf("- Pop:  need to switch stack block: begin: %20p -> ", epsilongc_my_stack_block); fflush(stdout);
    /* Release the current block and get another non-empty one: */
    epsilongc_release_stack_block();
    epsilongc_acquire_stack_block_if_possible(epsilongc_nonempty_stack_block_type);
    //printf("%-20p\n", epsilongc_my_stack_block); fflush(stdout);
    
    /* Did we succeed in acquiring another non-empty block? If no such block
       exists then we can simply return NULL to convey the fact that the stack
       is now empty, at least for what concerns the calling thread: */
    if(epsilongc_my_stack_block == NULL){
      printf("Returning NULL as we have no stack block\n");
      return NULL;
    }
  } // if
  
  /* Ok, now we have a non-empty block: */
  epsilongc_assert_on_debug(epsilongc_my_stack_block != NULL);
  epsilongc_assert_on_debug(! epsilongc_is_my_stack_block_empty());
  
  /* Actually pop, i.e. decrement the pointer, and load the result, which is
     pointed by the new above-the-top pointer: */
  epsilongc_my_above_the_top_pointer --;
  const epsilongc_word_t result = *epsilongc_my_above_the_top_pointer;
  //printf("Popped  %p\n", result);
  return result;
}

/* Return true iff the current thread-local stack block, which is assumed to exist,
   is empty: */
static bool epsilongc_is_my_stack_block_empty(void){
  /* We must have a stack block when we call this: */
  epsilongc_assert_on_debug(epsilongc_my_stack_block != NULL);
  
  /* The stack is empty iff the over-the-top pointer refers the payload beginning: */
  const epsilongc_pointer_t payload_beginning =
    EPSILONGC_STACK_BLOCK_TO_PAYLOAD_BEGINNING(epsilongc_my_stack_block);
  
  return epsilongc_my_above_the_top_pointer == payload_beginning;
}

/* Return true iff the current thread-local stack block, which is assumed to exist,
   is full: */
static bool epsilongc_is_my_stack_block_full(void){
  /* We must have a stack block when we call this: */
  epsilongc_assert_on_debug(epsilongc_my_stack_block != NULL);
  
  /* The stack is full iff the over-the-top pointer refers the word right after the
     payload end: */
  const epsilongc_pointer_t payload_end =
    EPSILONGC_STACK_BLOCK_TO_AFTER_PAYLOAD_END(epsilongc_my_stack_block);
  
  return epsilongc_my_above_the_top_pointer == payload_end;
}

void epsilongc_acquire_a_nonfull_block_if_needed(void){
  if(epsilongc_my_stack_block == NULL)
    epsilongc_acquire_stack_block(epsilongc_nonfull_stack_block_type);
  
  /* Now we must have a block; if not we made a mistake when computing the
     number of blocks: */
  epsilongc_assert_on_debug(epsilongc_my_stack_block != NULL);
}

void epsilongc_trace(void){
  /* Acquire a non-empty block to start popping from, if we don't already
     have one: */
  if(epsilongc_my_stack_block == NULL)
    epsilongc_acquire_stack_block(epsilongc_nonempty_stack_block_type);
  
  /* Keep popping a pointer, and if it's unmarked pushing all pointers
     reachable from that. Stack blocks are automatically acquired and
     released as needed, blocking when there is nothing to pop: */
#if defined(ENABLE_PARALLEL_COLLECTION) && defined(ENABLE_BALANCE_MARKING_LOAD)
  unsigned int i = 0; // we want to immediately get a chance of rebalancing
#endif // #if defined(ENABLE_PARALLEL_COLLECTION) && defined(ENABLE_BALANCE_MARKING_LOAD)
  while(true){
#if defined(ENABLE_PARALLEL_COLLECTION) && defined(ENABLE_BALANCE_MARKING_LOAD)
    /* If we use more than one processor for collecting... */
    if(epsilongc_processors_no > 1){
      /* Rebalance if needed, once in a while: */
      if(i == 0){
        epsilongc_rebalance_if_needed();
        i = (unsigned int)epsilongc_rand(); // avoid unwanted phasing among threads...
      } // inner if
      i = (i + 1) % EPSILONGC_POLLING_INTERVAL_CYCLE_LENGTH;
    } // outer if
#endif // #if if defined(ENABLE_PARALLEL_COLLECTION) && defined(ENABLE_BALANCE_MARKING_LOAD)
    
    const epsilongc_pointer_t pointer = epsilongc_pop_pointer();
#ifdef ENABLE_PREFETCH_OBJECTS_TO_MARK
    /* Prefetch (the first part of) the object we're tracing: */
    const epsilongc_word_t __attribute__((unused)) content =
      *(epsilongc_word_t*)pointer;
#endif // #ifdef ENABLE_PREFETCH_OBJECTS_TO_MARK
    
    /* Take the object tracer from the page; this is based on the
       assumption above: */
    const epsilongc_page_t page_or_large_object_header =
      epsilongc_candidate_pointer_to_candidate_page(pointer);
    const epsilongc_tracer_t tracer = page_or_large_object_header->tracer;
    
    /* Ok, now we have everything we need; trace the pointer: */
    tracer(pointer);
  } // while
}

void epsilongc_initialize_thread_local_tracing_support(void){
  /* We don't have any current stack block: */
  epsilongc_my_above_the_top_pointer = NULL;
  epsilongc_my_stack_block = NULL;
}

void epsilongc_finalize_thread_local_tracing_support(void){
  /* Release the current stack block, if any: */
  if(epsilongc_my_stack_block != NULL)
    epsilongc_release_stack_block();
}
