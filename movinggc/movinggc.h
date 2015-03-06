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


#ifndef MOVINGGC_H_
#define MOVINGGC_H_

#include <stdint.h>

/* It's important to include features.h because of global register
   variables, which have to be visible in all compilation units. */
#include "features.h"

/* Initialize and finalize: */
void movinggc_initialize (void);

/* Register an array of permanent roots: */
void register_roots (void **pointer_to_roots, size_t size_in_words);

/* Handle temporary roots.  It is not allowed to register permanent
   roots when any temporary root is active.  Attempting to do so will
   likely have disastrous effects, and is not checked for. */
void movinggc_push_dynamic_root (void **pointer_to_root);
void movinggc_pop_dynamic_root (void);
void movinggc_pop_dynamic_roots (size_t how_many);

/* Pushing and popping temporary roots all the time is very expensive.
   When possible it is much better to scan for roots asynchronously,
   right before a collection, for example by examining an execution
   stack with a known structure.  This is the intended use case for
   the following hooks, to be run before and after collection: the
   pre-GC hook should push temporary roots, and the post-GC hook
   should pop them.  The number of active temporary roots must be the
   same at pre-GC entry and at post-GC exit time.  Hooks may not
   allocate from the GC'd heap.

   Hook-setting functions can be given a NULL parameter to remove the
   previous hook.  It's not possible to chain hooks: at most one per
   type can be active at each time. */
typedef void (*movinggc_hook_t) (void *argument);
void movinggc_set_pre_hook (movinggc_hook_t hook);
void movinggc_set_post_hook (movinggc_hook_t hook);
void movinggc_set_hook_argument (void *argument);

/* Allocate a new heap object and return an UNtagged pointer to it.

   Before allocation a collection may be triggered.  In that case
   the allocation function executes, in this order:
   1) the pre-GC hook, if any, passing the GC hook argument (whose content
      may be modified by either hook);
   2) a garbage collection;
   3) the post-GC hook, if any, passing the same GC hook argument;
   4) a heap resize, if needed.

   The char version is faster, but it still assumes objects to have a
   size which is a multiple of the word size. */
void *movinggc_allocate_chars (size_t size_in_chars)
  __attribute__ ((hot, malloc));
void *movinggc_allocate_words (size_t size_in_words)
  __attribute__ ((hot, malloc, flatten));

/* Explicit GC.  Also executes the pre- and post-GC hooks, if any. */
void movinggc_gc (void) __attribute__ ((noinline, cold));

/* Statistics and debugging: */
float movinggc_fill_ratio (void);
const char *movinggc_semispace_name_of (const void *untagged_pointer);
long movinggc_gc_no (void); // how many times did we GC?
double movinggc_allocated_bytes (void); // how many times bytes did we allocate since the beginning?
void movinggc_dump_semispaces (void);
void movinggc_dump_semispace_contents (void);

/* Generation index.  Generation 0 is the youngest one. */
typedef int
movinggc_generation_index_t;

/* Tagging. */

typedef uintptr_t movinggc_bitmask_t;
typedef void *movinggc_pointer_t;

#define MOVINGGC_TAG_SIZE_IN_BITS 1

#define MOVINGGC_POINTER_TAG    ((movinggc_bitmask_t)1u)
#define MOVINGGC_NONPOINTER_TAG ((movinggc_bitmask_t)0u)

#define MOVINGGC_GENERATION_BIT_NO 2 /* FIXME: change to 1 if I only have two generations */

#define MOVINGGC_WORD_TO_BITMASK(X) \
  (movinggc_bitmask_t)((movinggc_pointer_t)(X))
#define MOVINGGC_BITMASK_TO_POINTER(X) \
  (movinggc_pointer_t)((movinggc_bitmask_t)(X))

#define MOVINGGC_WORD_TO_TAG(X) \
  ((MOVINGGC_WORD_TO_BITMASK(X)) & \
   ((movinggc_bitmask_t)((1 << MOVINGGC_TAG_SIZE_IN_BITS) - 1)))

#define MOVINGGC_IS_POINTER(X) \
  (MOVINGGC_WORD_TO_TAG(X) == MOVINGGC_POINTER_TAG)
#define MOVINGGC_IS_NONPOINTER(X) \
  (MOVINGGC_WORD_TO_TAG(X) == MOVINGGC_NONPOINTER_TAG)

#define MOVINGGC_TAG_POINTER(X) \
  (MOVINGGC_POINTER_TAG ? \
    (MOVINGGC_BITMASK_TO_POINTER(MOVINGGC_WORD_TO_BITMASK(X) | \
     MOVINGGC_POINTER_TAG)) \
   : \
   (X))
#define MOVINGGC_UNTAG_POINTER(X) \
  (MOVINGGC_POINTER_TAG ? \
    (MOVINGGC_BITMASK_TO_POINTER(MOVINGGC_WORD_TO_BITMASK(X) & \
     ~(movinggc_bitmask_t)((1 << MOVINGGC_TAG_SIZE_IN_BITS) - 1))) \
   : \
   MOVINGGC_BITMASK_TO_POINTER(MOVINGGC_WORD_TO_BITMASK(X)))
#define MOVINGGC_TAG_NONPOINTER(X) \
  (MOVINGGC_BITMASK_TO_POINTER(((MOVINGGC_WORD_TO_BITMASK((movinggc_bitmask_t)X) << \
                                 MOVINGGC_TAG_SIZE_IN_BITS) | \
                                MOVINGGC_NONPOINTER_TAG)))
#define MOVINGGC_UNTAG_NONPOINTER(X) \
  (MOVINGGC_BITMASK_TO_POINTER(MOVINGGC_WORD_TO_BITMASK(X) >> MOVINGGC_TAG_SIZE_IN_BITS))

/* The non-forwarding case should be the more efficient, since it is
   used in the fast path of allocation.  We store the object sizes in
   bytes, and since the size is always even we can store the tag in
   the least significant bit without shifting.  The same schema works
   for forwarding pointers, since the referred pointer has also the
   least significant bit set to zero.

   FIXME: change the comment above. */
#define MOVINGGC_NONFORWARDING_HEADER(SIZE, GENERATION)             \
  (MOVINGGC_BITMASK_TO_POINTER(((movinggc_bitmask_t)(SIZE)          \
                               << MOVINGGC_GENERATION_BIT_NO)       \
                               | (movinggc_bitmask_t)(GENERATION)))
#define MOVINGGC_NONFORWARDING_HEADER_TO_SIZE(H)                        \
  ((size_t)(MOVINGGC_WORD_TO_BITMASK(H) >> MOVINGGC_GENERATION_BIT_NO))
#define MOVINGGC_NONFORWARDING_HEADER_TO_GENERATION(H)                      \
  ((size_t)(MOVINGGC_WORD_TO_BITMASK(H)                                     \
            & (movinggc_bitmask_t)((1 << MOVINGGC_GENERATION_BIT_NO) - 1)))

#define MOVINGGC_FORWARDING_HEADER(UNTAGGED_DESTINATION)                      \
  (MOVINGGC_BITMASK_TO_POINTER(MOVINGGC_WORD_TO_BITMASK(UNTAGGED_DESTINATION) \
                               | (movinggc_bitmask_t)1))
#define MOVINGGC_FORWARDING_HEADER_TO_DESTINATION(H)       \
  (MOVINGGC_BITMASK_TO_POINTER(MOVINGGC_WORD_TO_BITMASK(H) \
                               & (movinggc_bitmask_t)~1))

#define MOVINGGC_IS_FORWARDING(H) \
  (MOVINGGC_WORD_TO_BITMASK(H) & 1)
#define MOVINGGC_IS_NONFORWARDING(H) \
  (! MOVINGGC_IS_FORWARDING(H))

#endif // #ifndef MOVINGGC_H_
