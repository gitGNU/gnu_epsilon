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


#ifndef EPSILONGC_FEATURES_H_
#define EPSILONGC_FEATURES_H_

#include <stdio.h>
#include <stdbool.h>
#include "epsilongc_types.h"
#include "config.h"

/* Check that exactly one way of marking is enabled: */
#if defined(ENABLE_MARK_BITS) && defined(ENABLE_MARK_BYTES)
  #error "You can't enable mark bits and mark bytes together"
#elif defined(ENABLE_MARK_BITS) && defined(ENABLE_MARK_WORDS)
  #error "You can't enable mark bits and mark words together"
#elif defined(ENABLE_MARK_BYTES) && defined(ENABLE_MARK_WORDS)
  #error "You can't enable mark bytes and mark words together"
#elif (!defined(ENABLE_MARK_BITS)) && (!defined(ENABLE_MARK_BYTES)) && (!defined(ENABLE_MARK_WORDS))
  #error "You must enable EXACTLY ONE of 'mark bits', 'mark bytes' and 'mark words'"
#else
  /* Ok, exactly one sort of marking structure was enabled. */
#endif

/* Check whether some currently unimplemented feature was enabled: */
#if defined(ENABLE_LARGE_OBJECTS)
#error Sorry, support for large objects is not implemented yet
#endif
#if defined(ENABLE_FINALIZATION)
#error Sorry, finalization is not implemented yet
#endif
#if defined(ENABLE_INTERIOR_POINTERS)
#error Sorry, support for interior pointers is not implemented yet
#endif

/* If we're dubugging we overwrite each word of each dead object with 0xdead;
   if we're not debugging we overwrite it with zero, which may be cheaper (and
   of course if ENABLE_OVERWRITE_DEAD_OBJECTS is not #defined we don't overwrite
   at all): */
#ifdef ENABLE_ASSERTIONS
#define EPSILONGC_DEAD_WORD ((epsilongc_word_t)0xdead)
#else
#define EPSILONGC_DEAD_WORD ((epsilongc_word_t)0)
#endif // #ifdef ENABLE_ASSERTIONS

/* Check some features: */
#ifndef HAVE_STRINGIZE
#error 'Your compiler lacks the ## preprocessor operator. Sorry, you need it; get GCC.'
#endif // #ifndef HAVE_STRINGIZE

#endif // #ifndef EPSILONGC_FEATURES_H_
