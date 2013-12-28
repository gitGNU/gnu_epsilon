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


#ifndef EPSILONGC_DEBUG_H_
#define EPSILONGC_DEBUG_H_

#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <assert.h>
#include "config.h"

/* Initialize support for debugging the garbage collector: */
void epsilongc_initialize_debugging(void);

/* Initialize support for debugging the garbage collector: */
void epsilongc_finalize_debugging(void);

/* Conditionally enable debugging assertions: */
#ifdef ENABLE_ASSERTIONS
#define epsilongc_assert_on_debug(EXPRESSION) \
  { fflush(stdout); fflush(stderr); assert(EXPRESSION); }
#define epsilongc_weakly_assert_on_debug(EXPRESSION_) \
  { if(EPSILONGC_UNLIKELY(! (EXPRESSION_))){ \
      printf("WARNING: weak assertion violated at %s:%i in %s(): %s is false\n", __FILE__, __LINE__, __FUNCTION__, #EXPRESSION_); \
      fflush(stdout); }}
#else
#define epsilongc_assert_on_debug(EXPRESSION) \
  { if(false) if(EXPRESSION) /* nothing */; } // always do semantic checks for EXPRESSION
#define epsilongc_weakly_assert_on_debug(EXPRESSION) \
  epsilongc_assert_on_debug(EXPRESSION)
#endif // #ifdef ENABLE_ASSERTIONS

/* Conditionally enable some inlining directives: */
#ifdef ENABLE_AGGRESSIVELY_INLINE
#define EPSILONGC_INLINE_ATTRIBUTE_IFF_AGGRESSIVE_INLINE \
  /* nothing */ //__attribute__((inline))
#else
#define EPSILONGC_INLINE_ATTRIBUTE_IFF_AGGRESSIVE_INLINE \
  __attribute__((noinline))
#endif // #ifdef ENABLE_AGGRESSIVELY_INLINE


/* See the comment in epsilongc_types.h */
int epsilongc_signal_safe_printf(const char *format, ...);
int epsilongc_signal_safe_fprintf(FILE *stream, const char *format, ...);
int epsilongc_signal_safe_fflush(FILE *stream);

#endif // #ifndef EPSILONGC_DEBUG_H_
