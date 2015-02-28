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


#ifndef MOVINGGC_TAGS_H_
#define MOVINGGC_TAGS_H_

#include <stdint.h>

typedef uintptr_t movinggc_bitmask_t;
typedef void *movinggc_pointer_t;

#define MOVINGGC_TAG_SIZE_IN_BITS 1

#define MOVINGGC_POINTER_TAG    ((movinggc_bitmask_t)1u)
#define MOVINGGC_NONPOINTER_TAG ((movinggc_bitmask_t)0u)

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
   least significant bit set to zero. */
#define MOVINGGC_NONFORWARDING_HEADER(SIZE) \
  (MOVINGGC_BITMASK_TO_POINTER(SIZE))
#define MOVINGGC_NONFORWARDING_HEADER_TO_SIZE(H) \
  ((size_t)(MOVINGGC_WORD_TO_BITMASK(H)))

#define MOVINGGC_FORWARDING_HEADER(UNTAGGED_DESTINATION) \
  (MOVINGGC_BITMASK_TO_POINTER(MOVINGGC_WORD_TO_BITMASK(UNTAGGED_DESTINATION) | 1))
#define MOVINGGC_FORWARDING_HEADER_TO_DESTINATION(H) \
  (MOVINGGC_BITMASK_TO_POINTER(MOVINGGC_WORD_TO_BITMASK(H) & ~1L))

#define MOVINGGC_IS_FORWARDING(H) \
  (MOVINGGC_WORD_TO_BITMASK(H) & 1)
#define MOVINGGC_IS_NONFORWARDING(H) \
  (! MOVINGGC_IS_FORWARDING(H))

#endif // #ifndef MOVINGGC_TAGS_H_
