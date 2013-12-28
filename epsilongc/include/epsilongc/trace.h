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


#ifndef EPSILONGC_TRACE_H_
#define EPSILONGC_TRACE_H_

#include <stdio.h>
#include <stdbool.h>
#include "epsilongc_types.h"
#include "epsilongc_threads.h"
#include "doubly_linked_list_macros.h"

/* Just as we do for pages, we use block header pointers to refer
   to whole block headers: */
typedef struct epsilongc_stack_block_header* epsilongc_stack_block_t;

/* The header of a mark stack block. Payload comes immediately after the header,
   whose size is a whole multiple of the word size: */
struct epsilongc_stack_block_header{
  /* Above-the-top pointer for this block; this is equal to the payload
     start address if this block is empty: */
  epsilongc_word_t *above_the_top_pointer;
  
  /* Each block belongs to a list of blocks: */
  EPSILONGC_LIST_ELEMENT_FIELDS(stack_blocks, epsilongc_stack_block_t)
}; // struct

/* Get a pointer to the first payload word in a mark stack block from a
   pointer to its header: */
#define EPSILONGC_STACK_BLOCK_TO_PAYLOAD_BEGINNING(BLOCK_) \
  (epsilongc_word_t*)(((char*)(BLOCK_)) + sizeof(struct epsilongc_stack_block_header))

/* Get a pointer to the word coming right after the last payload word, from a
   header pointer: */
#define EPSILONGC_STACK_BLOCK_TO_AFTER_PAYLOAD_END(BLOCK_) \
  (epsilongc_word_t*)(((char*)(BLOCK_)) + EPSILONGC_STACK_BLOCK_SIZE_IN_BYTES)

/* The number of words in a stack block payload (i.e. the number of pointers
   which a block can hold): */
#define EPSILONGC_STACK_BLOCK_PAYLOAD_SIZE_IN_WORDS \
  (EPSILONGC_STACK_BLOCK_TO_AFTER_PAYLOAD_END(NULL) - EPSILONGC_STACK_BLOCK_TO_PAYLOAD_BEGINNING(NULL))

/* The number of words currently present in the caller thread's stack block, which
   is assumed to exist: */
#define EPSILONGC_STACK_ELEMENTS_NO_IN_OUR_BLOCK \
  (((epsilongc_word_t*)epsilongc_my_above_the_top_pointer) - \
   (epsilongc_word_t*)EPSILONGC_STACK_BLOCK_TO_PAYLOAD_BEGINNING(epsilongc_my_stack_block))

/* Initialize tracing support. This should be called once [To do:
   once per marking thread, I guess] before allocating: */
void epsilongc_initialize_tracing_support(void);
void epsilongc_finalize_tracing_support(void);

/* This should be called when marking roots and from tracers: */
void epsilongc_trace_pointer_if_valid(epsilongc_word_t candidate_pointer);

/* Trace the roots of the given mutator thread and push them into the
   thread-local stack: */
void epsilongc_trace_roots(const epsilongc_thread_t mutator_thread);

/* If we currently have no stack block get a non-full one, so that we
   can begin to trace: */
void epsilongc_acquire_a_nonfull_block_if_needed(void);

/* Continue to trace starting from the roots which have already been pushed
   on the thread-local stack: */
void epsilongc_trace(void);

/* Trace all words in the given buffer, in sequence. Only word-aligned
   potential pointers are considered: */
void epsilongc_trace_aligned_words_in_buffer(const epsilongc_word_t *buffer,
                                             const epsilongc_unsigned_integer_t size_in_bytes);

/* Trace all words in the given buffer, in sequence, with all possible
   alignments (1 byte, 2 bytes, 3 bytes ... word): */
void epsilongc_trace_buffer_with_all_alignments(const epsilongc_pointer_t buffer,
                                                const epsilongc_unsigned_integer_t size_in_bytes);

/* Initialize thread-local tracing support. This is for collector threads,
   not for mutators: */
void epsilongc_initialize_thread_local_tracing_support(void);

/* Finalize thread-local tracing support. This is for collector threads,
   not for mutators: */
void epsilongc_finalize_thread_local_tracing_support(void);

/* Destroy an existing stack block, without removing it from the global
   structures and without any synchronization. This is only needed for finalizing: */
void epsilongc_destroy_stack_block_without_removing_it_from_global_structures(epsilongc_stack_block_t block);

/* This function should be added whenever the heap size changes, on page making,
   page destruction, and non-trivial page refurbishing; its effect is making new
   stack blocks or destroying existing ones, according to how many of them we may
   need now *in the worst case*; this function performs the needed syncrhonization
   internally: */
void epsilongc_make_or_destroy_stack_blocks_as_needed(const epsilongc_integer_t delta_pointers_no,
                                                      const epsilongc_integer_t delta_objects_no);
void epsilongc_make_or_destroy_stack_blocks_as_needed_unlocked(const epsilongc_integer_t delta_pointers_no,
                                                               const epsilongc_integer_t delta_objects_no);

void epsilongc_release_stack_block(void);

/* How many stack blocks currently exist. Protected by the global mutex: */
extern epsilongc_integer_t epsilongc_stack_blocks_no;

#endif // #ifndef EPSILONGC_TRACE_H_
