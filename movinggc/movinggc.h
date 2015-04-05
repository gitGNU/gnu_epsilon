/* This file is part of GNU epsilon.

   Copyright (C) 2012 Université Paris 13
   Copyright (C) 2015 Luca Saiu
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


#ifndef EGC_H_
#define EGC_H_

#include <stdint.h>

/* It's important to include features.h because of global register
   variables, which have to be visible in all compilation units. */
#include "features.h"

/* Initialize and finalize: */
void egc_initialize (void);

/* Generation initialization and finalization. */
struct egc_generation;
typedef struct egc_generation* egc_generation_t;
/* Allocate and return a generation array of generation_no elements. */
egc_generation_t
egc_make_generations (size_t generation_no)
  __attribute__ ((cold, malloc, returns_nonnull));
/* Each generation (as returned by egc_make_generations) must be
   initialized exactly once. */
void
egc_initialize_semispace_generation (egc_generation_t generation,
                                     int semispace_no, /* 1 or 2 */
                                     size_t word_no)
  __attribute__ ((cold));
void
egc_initialize_marksweep_generation (egc_generation_t generation,
                                     size_t word_no)
  __attribute__ ((cold));
/* Generation data structures have to refer one another.  This has to
   be called after initializing them all. */
void egc_link_generations (egc_generation_t generations, size_t generation_no)
  __attribute__ ((cold));

/* Register an array of permanent roots: */
void egc_register_roots (void **pointer_to_roots, size_t size_in_words);

/* Handle temporary roots.  It is not allowed to register permanent
   roots when any temporary root is active.  Attempting to do so will
   likely have disastrous effects, and is not checked for. */
void egc_push_dynamic_root (void **pointer_to_root);
void egc_pop_dynamic_root (void);
void egc_pop_dynamic_roots (size_t how_many);

/* Pushing and popping temporary roots all the time is very expensive.
   When possible it is much better to scan for roots asynchronously,
   right before a collection, for example by examining an execution
   stack with a known structure.  This is the intended use case for
   the following hooks, to be run before and after root scavenging:
   the pre-root hook should push temporary roots, and the post-root
   hook should pop them.  The number of active temporary roots must be
   the same at pre- entry and at post- exit time.  Hooks may not
   allocate from the GC'd heap.

   Hook-setting functions can be given a NULL parameter to remove the
   previous hook.  It's not possible to chain hooks: at most one per
   type can be active at each time. */
typedef void (*egc_hook_t) (void *argument);
void egc_set_pre_hook (egc_hook_t hook);
void egc_set_post_hook (egc_hook_t hook);
void egc_set_hook_argument (void *argument);

/* Allocate a new heap object and return an UNtagged pointer to it.
   The user has to initialize the returned object in *every* field
   before the next allocation; behavior is undefined if she doesn't.

   Before allocation a collection may be triggered.  In that case
   the allocation function executes, in this order:
   1) the pre-GC hook, if any, passing the GC hook argument (whose content
      may be modified by either hook);
   2) a garbage collection;
   3) the post-GC hook, if any, passing the same GC hook argument;
   4) a heap resize, if needed.

   The char versions are faster, but it still assumes objects to have
   a size which is a multiple of the word size.  The initializing
   versions fill the new buffer with (non-pointer) zeroes.  Notice
   that initialization takes time, and it is better avoided; the
   function are provided for convenience, to be used in cases where it
   is complicated to completely initialize a buffer before allocating
   another. */
void *egc_allocate_chars (size_t size_in_chars)
  __attribute__ ((hot, malloc, returns_nonnull));
void *egc_allocate_words (size_t size_in_words)
  __attribute__ ((hot, malloc, flatten, returns_nonnull));
void *egc_allocate_chars_inizializing (size_t size_in_chars)
  __attribute__ ((hot, malloc, flatten, returns_nonnull));
void *egc_allocate_words_inizializing (size_t size_in_chars)
  __attribute__ ((hot, malloc, flatten, returns_nonnull));

// FIXME: remove, or add more cases.
void *
egc_allocate_cons (void)
  __attribute__ ((hot, malloc, flatten, returns_nonnull));

/* This has to be called on a slot before it's overwritten with a
   (tagged) pointer.

   It is *not* necessary to call it in the following two cases:
   a) when the write operation is an initialization;
   b) when the new value being written is a non-pointer. */
void egc_write_barrier (void **untagged_initial_pointer,
                        long offset_in_words);


/* Explicit full GC.  Also execute the pre- and post-GC hooks, if any. */
void egc_full_gc (void) __attribute__ ((noinline, cold));

/* Statistics and debugging: */
float egc_fill_ratio (void);
const char *egc_heap_name_of (const void *untagged_pointer);
long egc_gc_no (void); // how many times did we GC?
double egc_allocated_bytes (void); // how many times bytes did we allocate since the beginning?
void egc_dump_generations (void);
void egc_dump_generation_contents (void);
void
egc_print_marks (egc_generation_t g)
  __attribute__ ((cold));
void
egc_dump_times (void);

/* Generation index.  Generation 0 is the youngest one. */
typedef int
egc_generation_index_t;

/* Tagging. */

typedef uintptr_t egc_bitmask_t;
typedef void *egc_pointer_t;

#define EGC_TAG_SIZE_IN_BITS 1

#define EGC_POINTER_TAG    ((egc_bitmask_t)1u)
#define EGC_NONPOINTER_TAG ((egc_bitmask_t)0u)

#define EGC_GENERATION_BIT_NO 2 /* FIXME: change to 1 if I only have two generations */

#define EGC_WORD_TO_BITMASK(X) \
  (egc_bitmask_t)((egc_pointer_t)(X))
#define EGC_BITMASK_TO_POINTER(X) \
  (egc_pointer_t)((egc_bitmask_t)(X))

#define EGC_WORD_TO_TAG(X) \
  ((EGC_WORD_TO_BITMASK(X)) & \
   ((egc_bitmask_t)((1 << EGC_TAG_SIZE_IN_BITS) - 1)))

#define EGC_IS_POINTER(X) \
  (EGC_WORD_TO_TAG(X) == EGC_POINTER_TAG)
#define EGC_IS_NONPOINTER(X) \
  (EGC_WORD_TO_TAG(X) == EGC_NONPOINTER_TAG)

#define EGC_TAG_POINTER(X) \
  (EGC_POINTER_TAG ? \
    (EGC_BITMASK_TO_POINTER(EGC_WORD_TO_BITMASK(X) | \
     EGC_POINTER_TAG)) \
   : \
   (X))
#define EGC_UNTAG_POINTER(X) \
  (EGC_POINTER_TAG ? \
    (EGC_BITMASK_TO_POINTER(EGC_WORD_TO_BITMASK(X) & \
     ~(egc_bitmask_t)((1 << EGC_TAG_SIZE_IN_BITS) - 1))) \
   : \
   EGC_BITMASK_TO_POINTER(EGC_WORD_TO_BITMASK(X)))
#define EGC_TAG_NONPOINTER(X) \
  (EGC_BITMASK_TO_POINTER(((EGC_WORD_TO_BITMASK((egc_bitmask_t)X) << \
                                 EGC_TAG_SIZE_IN_BITS) | \
                                EGC_NONPOINTER_TAG)))
#define EGC_UNTAG_NONPOINTER(X) \
  (EGC_BITMASK_TO_POINTER(EGC_WORD_TO_BITMASK(X) >> EGC_TAG_SIZE_IN_BITS))

/* The non-forwarding case should be the more efficient, since it is
   used in the fast path of allocation.  We store the object sizes in
   bytes, and since the size is always even we can store the tag in
   the least significant bit without shifting.  The same schema works
   for forwarding pointers, since the referred pointer has also the
   least significant bit set to zero.

   FIXME: change the comment above. */
#define EGC_NONFORWARDING_HEADER(SIZE, GENERATION)                    \
  (EGC_BITMASK_TO_POINTER((((egc_bitmask_t)(SIZE)                \
                                << EGC_GENERATION_BIT_NO)             \
                                | (egc_bitmask_t)(GENERATION)) << 1))
#define EGC_NONFORWARDING_HEADER_TO_SIZE(H)                        \
  ((size_t)(EGC_WORD_TO_BITMASK(H) >> 1) >> EGC_GENERATION_BIT_NO)
#define EGC_NONFORWARDING_HEADER_TO_GENERATION(H)                      \
  ((size_t)((EGC_WORD_TO_BITMASK(H) >> 1)                              \
            & (egc_bitmask_t)((1 << EGC_GENERATION_BIT_NO) - 1)))

#define EGC_FORWARDING_HEADER(UNTAGGED_DESTINATION)                      \
  (EGC_BITMASK_TO_POINTER(EGC_WORD_TO_BITMASK(UNTAGGED_DESTINATION) \
                               | (egc_bitmask_t)1))
#define EGC_FORWARDING_HEADER_TO_DESTINATION(H)       \
  (EGC_BITMASK_TO_POINTER(EGC_WORD_TO_BITMASK(H) \
                               & (egc_bitmask_t)~1))

#define EGC_IS_FORWARDING(H) \
  (EGC_WORD_TO_BITMASK(H) & 1)
#define EGC_IS_NONFORWARDING(H) \
  (! EGC_IS_FORWARDING(H))

#endif // #ifndef EGC_H_
