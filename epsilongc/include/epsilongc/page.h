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


#ifndef EPSILONGC_PAGE_H_
#define EPSILONGC_PAGE_H_

#include <stdbool.h>
#include "epsilongc_types.h"
#include "kind.h"
#include "compile_time_parameters.h"
#include "epsilongc_debug.h"
#include "heuristics.h"
#include "doubly_linked_list_macros.h"

/* A page is a memalign()ed memory buffer containing:
   - the header
     - the header contains, only if ENABLE_OUT_OF_PAGE_MARK_ARRAY is #define'd,
       a pointer to the mark array
   - only if ENABLE_OUT_OF_PAGE_MARK_ARRAY is *not* defined: the mark array, word-aligned
     (this comes for free: sizeof(epsilongc_page_header) is always multiple of the word
     size)
   - padding, in order to respect the requested alignment for objects
   - The payload (word-aligned [To do: shall I align it to a multiple of the object size?]).
   It's important that the page is memalign()ed, because this allows us to perform
   the following operations very efficiently:
   - Accessing the tag, datum, kind or any field in the header given a pointer to an
     object in the page
   - checking whether a given object is within a given page.
*/

/* We slightly abuse the standard by using a pointer to a page header to access
   an entire page, including mark bytes and payload: */
typedef struct epsilongc_page_header* epsilongc_page_header_t;
typedef epsilongc_page_header_t epsilongc_page_t;

/* Let's avoid circular #include's mess...: */
typedef struct epsilongc_allocator* epsilongc_allocator_t;
typedef struct epsilongc_pool* epsilongc_pool_t;

/* The first fields of the header used both for pages and for single large objects: */
#define PAGE_OR_LARGE_OBJECT_HEADER \
  /* The tag is the first field of the kind. This is good for performance,
     as we can access the tag for a page with a 0-offset load instruction
     and no indirection when we have a page pointer in a register. For the
     same reason we also cache the datum here: */ \
  epsilongc_kind_tag_t tag; \
  epsilongc_kind_datum_t datum; \
  \
  /* Tracer (this must be fast to access, so we copy here in each page,
     instead of requiring two load instructions to get it from the
     kind pointer): */ \
  epsilongc_tracer_t tracer; \
  /* Object size in words, again copied from the kind information: */ \
  epsilongc_unsigned_integer_t object_size_in_words;

/* The header of a page holding objects of a given kind: */
struct epsilongc_page_header{
  /* The header: */
  PAGE_OR_LARGE_OBJECT_HEADER
  
  /* Size and alignment of each object in this page, in bytes: */
  epsilongc_unsigned_integer_t object_size_in_bytes;
  epsilongc_unsigned_integer_t object_alignment_in_bytes;
  
  /* All the information about the kind of objects in this page is
     available from this pointer; most performance-critical information
     is accessible from PAGE_OR_LARGE_OBJECT_HEADER; looking at this costs
     one more load instruction: */
  epsilongc_kind_t kind;
  
  /* Head of the free list of this page. Note that this is *not* kept
     up-to-date when the page is used by an allocator; in that case the
     allocator field with the same name holds the authorative value. 
     This field is only updated when a page is moved from an allocator
     to a pool, and is just used to initialize the allocator free-list. */
  epsilongc_word_t *next_free_object;
  
  /* Pointer to the first object in the payload (it's within the same
     memalign()ed page, after the GC bits and the padding): */
  epsilongc_word_t* first_object;
  
  /* Pointer to the beginning of the space after the last object in the
     payload (which may be out of the page): */
  epsilongc_word_t* after_last_object;
  
  /* This page belongs to one of the two lists of its pool, or to the
     global list of empty pages, or to the global list of full pages: */
  EPSILONGC_LIST_ELEMENT_FIELDS(list, epsilongc_page_t)

#ifdef ENABLE_ASSERTIONS
  /* When assertions are enabled we also support a list containing *all*
     existing (and already initialized) pages: */
  EPSILONGC_LIST_ELEMENT_FIELDS(all_pages, epsilongc_page_t)
#endif // #ifdef ENABLE_ASSERTIONS

  /* The pool to which this page belongs: */
  epsilongc_pool_t pool;

  /* The allocator currently holding this page, or NULL: */
  epsilongc_allocator_t current_allocator;
  
#ifdef ENABLE_OUT_OF_PAGE_MARK_ARRAY
  /* Pointer to the mark array, which is *not* in the page: */
  char *mark_array;
#endif // #ifdef ENABLE_OUT_OF_PAGE_MARK_ARRAY
  
  /* A boolean (as large as a word because we want to keep the header size
     a multiple of the word size) recording whether the page belongs to the
     empty page list: */
  epsilongc_integer_t belongs_to_the_empty_pages_list;
}; // struct

/* These are often useful: */
#define EPSILONGC_PAGE_HEADER_SIZE_IN_BYTES \
  (sizeof(struct epsilongc_page_header))
#define EPSILONGC_PAGE_HEADER_SIZE_IN_WORDS \
  (EPSILONGC_PAGE_HEADER_SIZE_IN_BYTES / sizeof(word_t))

/* Lookup the mark byte array. Note how the macro returning a mark
   array index also works correctly in the case of interior pointers
   (but it performs absolutely *no* range checking): */
#ifdef ENABLE_OUT_OF_PAGE_MARK_ARRAY
#define EPSILONGC_PAGE_TO_MARK_ARRAY(PAGE) \
  (((const epsilongc_page_t)(PAGE))->mark_array)
#else
#define EPSILONGC_PAGE_TO_MARK_ARRAY(PAGE) \
  (((const char*)(PAGE)) + EPSILONGC_PAGE_HEADER_SIZE_IN_BYTES)
#endif // #ifdef ENABLE_OUT_OF_PAGE_MARK_ARRAY (else branch)

/* Lookup a mark array element: */
#if defined(ENABLE_MARK_BITS)
#define EPSILONGC_LOOKUP_MARK_ARRAY(ARRAY, INDEX) \
  ((const bool) \
   ((((const epsilongc_unsigned_integer_t*)(ARRAY))[(INDEX) / EPSILONGC_BITS_PER_WORD]) \
    & ((epsilongc_unsigned_integer_t)1 << ((INDEX) % EPSILONGC_BITS_PER_WORD))))
#elif defined(ENABLE_MARK_BYTES)
#define EPSILONGC_LOOKUP_MARK_ARRAY(ARRAY, INDEX) \
  ((const char)(((const char*)(ARRAY))[INDEX]))
#elif defined(ENABLE_MARK_WORDS)
#define EPSILONGC_LOOKUP_MARK_ARRAY(ARRAY, INDEX) \
  ((const epsilongc_unsigned_integer_t) \
   (((const epsilongc_unsigned_integer_t*)(ARRAY))[INDEX]))
#else
#error You have to enable mark bits, mark bytes or mark words
#endif

/* Set a mark array element: */
#if defined(ENABLE_MARK_BITS)
#define EPSILONGC_UPDATE_MARK_ARRAY(ARRAY, INDEX, VALUE) \
{if(VALUE) \
  ((((epsilongc_unsigned_integer_t*)(ARRAY))[(INDEX) / EPSILONGC_BITS_PER_WORD]) \
   |= ((epsilongc_unsigned_integer_t)1 << \
       (epsilongc_unsigned_integer_t)((INDEX) % EPSILONGC_BITS_PER_WORD))); \
 else \
  ((((epsilongc_unsigned_integer_t*)(ARRAY))[(INDEX) / EPSILONGC_BITS_PER_WORD]) \
   &= ~((epsilongc_unsigned_integer_t)1 << \
        (epsilongc_unsigned_integer_t)((INDEX) % EPSILONGC_BITS_PER_WORD)));}
#ifdef ENABLE_PARALLEL_COLLECTION
#define EPSILONGC_ATOMICALLY_UPDATE_MARK_ARRAY(ARRAY, INDEX, VALUE) \
{if(VALUE) \
   __sync_fetch_and_or(&(((epsilongc_unsigned_integer_t*)(ARRAY))[(INDEX) / EPSILONGC_BITS_PER_WORD]), \
                       ((epsilongc_unsigned_integer_t)1 << \
                       (epsilongc_unsigned_integer_t)((INDEX) % EPSILONGC_BITS_PER_WORD))); \
 else \
   __sync_fetch_and_and(&(((epsilongc_unsigned_integer_t*)(ARRAY))[(INDEX) / EPSILONGC_BITS_PER_WORD]), \
                        ~((epsilongc_unsigned_integer_t)1 << \
                          (epsilongc_unsigned_integer_t)((INDEX) % EPSILONGC_BITS_PER_WORD)));}
#else // mark bits, no parallel collection
#define EPSILONGC_ATOMICALLY_UPDATE_MARK_ARRAY \
  EPSILONGC_UPDATE_MARK_ARRAY
#endif // #ifdef ENABLE_PARALLEL_COLLECTION
#elif defined(ENABLE_MARK_BYTES) // mark bytes, either parallel or sequential collection
#define EPSILONGC_ATOMICALLY_UPDATE_MARK_ARRAY(ARRAY, INDEX, VALUE) \
  ((unsigned char*)(ARRAY))[INDEX] = (unsigned char)(VALUE)
#define EPSILONGC_UPDATE_MARK_ARRAY \
  EPSILONGC_ATOMICALLY_UPDATE_MARK_ARRAY
#elif defined(ENABLE_MARK_WORDS)
#define EPSILONGC_UPDATE_MARK_ARRAY(ARRAY, INDEX, VALUE) \
  ((epsilongc_unsigned_integer_t*)(ARRAY))[INDEX] = \
    (epsilongc_unsigned_integer_t)(VALUE)
#define EPSILONGC_ATOMICALLY_UPDATE_MARK_ARRAY \
  EPSILONGC_UPDATE_MARK_ARRAY
#else
#error You have to enable mark bits, mark bytes or mark words
#endif

/* Return the mark array index for a given page and a given
   object contained in it: */
#define EPSILONGC_PAGE_AND_OBJECT_TO_MARK_ARRAY_INDEX(PAGE, OBJECT) \
  ((((char*)(OBJECT)) - ((char*)((PAGE)->first_object))) / \
   (PAGE)->object_size_in_bytes)

/* Return the object identified by the given page and mark array
   index: */
#define EPSILONGC_PAGE_AND_MARK_ARRAY_INDEX_TO_OBJECT(PAGE, INDEX) \
  ((epsilongc_word_t*) \
   (((const char*)((PAGE)->first_object)) + ((INDEX)*((PAGE)->object_size_in_bytes))))

/* Check whether a page is in the set: */
bool epsilongc_does_page_exist(const epsilongc_page_t page)
  __attribute__((pure/*, always_inline */));

/* Bitwise-and'ing this mask to an address which may be a pointer returns the
   (hypothetical) page the pointer belongs to: */
#define EPSILONGC_PAGE_BITMASK \
  (~(epsilongc_unsigned_integer_t)((1u << EPSILONGC_PAGE_OFFSET_WIDTH) - 1u))

/* Initialize page support. This should be called once (not once per thread)
   at initialization time: */
void epsilongc_initialize_page_support(void);

/* Finalize page support. This should be called once (not once per thread)
   at finalization time: */
void epsilongc_finalize_page_support(void);

/* Return the current size of the heap, in bytes: */
epsilongc_unsigned_integer_t epsilongc_current_heap_size_in_bytes(void)
  __attribute__((pure));

/* Check whether a pointer refers to an object in this page; the pointer
   is *assumed* to point to an address within the page (this is not checked),
   is *assumed* to be word-aligned (not checked), and the page is *assumed*
   to exist (this is also not checked): */
bool epsilongc_is_aligned_pointer_within_page_valid_in_page(const epsilongc_word_t pointer, 
                                                            const epsilongc_page_t page);

/* Make a new page for the given pool and add it to global structures. This internally
   synchronizes on the global mutex for a short time. The new page, which is returned,
   is *not* added to any list of the pool: */
epsilongc_page_t epsilongc_make_page_and_add_it_to_global_structures(epsilongc_pool_t pool)
  __attribute__((malloc));

/* Destroy the given page and remove it from the global set of pages; this internally
   synchronizes on global_mutex: */
void epsilongc_destroy_page_and_remove_it_from_global_structures(epsilongc_page_t page);

/* Change the kind of an empty page (if needed), and make it ready to be used again
   on a possibly different pool. This synchronizes for a short time: */
void epsilongc_refurbish_page(epsilongc_page_t page, epsilongc_pool_t pool);

/* Sweep the given page, updating the given statistics if not NULL: */
void epsilongc_sweep_page(epsilongc_page_t page,
                          epsilongc_sweep_statistics_t statistics);

/* Dump the free list. This is useful for debugging: */
void epsilongc_dump_free_list(epsilongc_page_t page);

/* This is useful for debugging: */
bool epsilongc_is_the_first_object_in(epsilongc_word_t object,
                                      epsilongc_page_t page);

/* This is useful for debugging: */
bool epsilongc_is_the_last_object_in(epsilongc_word_t object,
                                     epsilongc_page_t page);

/* Return true iff all the objects in the given page are marked: */
bool epsilongc_is_page_completely_marked(const epsilongc_page_t page);

/* Return true iff all the objects in the given page are unmarked: */
bool epsilongc_is_page_completely_unmarked(const epsilongc_page_t page);

/* Mark all the objects in the given page: */
void epsilongc_completely_mark_page(epsilongc_page_t page);

/* Unmark all the objects in the given page: */
void epsilongc_completely_unmark_page(epsilongc_page_t page);


#ifdef ENABLE_CONCEAL_FREE_LIST
/* A cheap way of (reversibly) concealing pointers is representing them with
   their least-significant bit on. In this case concealing or unconcealing a
   pointer takes one assembler instruction in the worst case (the C compiler
   might be able to combine concealing/unconcealing into a load/store with
   non-zero offset in some cases): */
#define EPSILONGC_CONCEAL_POINTER(POINTER) \
  ((epsilongc_pointer_t)((epsilongc_unsigned_integer_t)(POINTER) + 1))
#define EPSILONGC_UNCONCEAL_POINTER(POINTER) \
  ((epsilongc_pointer_t)((epsilongc_unsigned_integer_t)(POINTER) - 1))
#else
/* The identity function is clearly a reversible "concealing" function,
   which costs zero assembler instructions: */
#define EPSILONGC_CONCEAL_POINTER(POINTER) \
  (POINTER)
#define EPSILONGC_UNCONCEAL_POINTER(POINTER) \
  (POINTER)
#endif // #ifdef ENABLE_CONCEAL_FREE_LIST

/* Return true iff all the currently existing pages are completely
   unmarked. This always fails with a fatal error when assertions are
   not enabled. */
bool epsilongc_are_all_pages_completely_unmarked(void);

#endif // #ifndef EPSILONGC_PAGE_H_
