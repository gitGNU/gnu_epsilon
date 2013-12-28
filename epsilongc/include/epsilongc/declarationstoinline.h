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


/* According to configuration options these definitions can be compiled in
   the library, or simply #include'd in a public header, so that the caller
   can aggressively inline them in its code. This is typically worth the
   code size increase, as these functions are very performance-critical. */

#ifndef EPSILONGC_DECLARATIONS_TO_AGGRESSIVELY_INLINE
#define EPSILONGC_DECLARATIONS_TO_AGGRESSIVELY_INLINE

//#include "epsilongc_types.h" // this also #include's config.h

/* Conditionally #define qualifiers and attributes used to force inlining: */
#ifdef ENABLE_AGGRESSIVELY_INLINE
/* Aggressively inline, and define functions in public headers: */
#define EPSILONGC_FUNCTION_QUALIFIERS_FOR_AGGRESSIVE_INLINING static inline
#define EPSILONGC_FUNCTION_ATTRIBUTES_FOR_AGGRESSIVE_INLINING __attribute__((always_inline))
#else
/* Don't inline, and define functions in C files: */
#define EPSILONGC_FUNCTION_QUALIFIERS_FOR_AGGRESSIVE_INLINING /* no qualifiers */
#define EPSILONGC_FUNCTION_ATTRIBUTES_FOR_AGGRESSIVE_INLINING /* no attributes */
#endif // #ifdef ENABLE_AGGRESSIVELY_INLINE


/* The following declarations should be visible from headers in any case (we need
   separate prototype even if we immediately supply the body, just to be able to
   specify function attributes): */

/* Return a new object from the given allocator: */
EPSILONGC_FUNCTION_QUALIFIERS_FOR_AGGRESSIVE_INLINING
epsilongc_word_t epsilongc_allocate_from(struct epsilongc_allocator *allocator)
  EPSILONGC_FUNCTION_ATTRIBUTES_FOR_AGGRESSIVE_INLINING
  __attribute__((malloc));

/* Given a potential heap pointer return its page. This is only a bitmask operation,
   and the actual existence of the returned page is not checked:  */
EPSILONGC_FUNCTION_QUALIFIERS_FOR_AGGRESSIVE_INLINING epsilongc_page_t
epsilongc_candidate_pointer_to_candidate_page(const epsilongc_word_t pointer)
  EPSILONGC_FUNCTION_ATTRIBUTES_FOR_AGGRESSIVE_INLINING
  __attribute__((pure));

EPSILONGC_FUNCTION_QUALIFIERS_FOR_AGGRESSIVE_INLINING epsilongc_kind_tag_t
epsilongc_object_to_tag(const epsilongc_word_t object)
  EPSILONGC_FUNCTION_ATTRIBUTES_FOR_AGGRESSIVE_INLINING
  __attribute__((pure));

EPSILONGC_FUNCTION_QUALIFIERS_FOR_AGGRESSIVE_INLINING epsilongc_kind_datum_t
epsilongc_object_to_datum(const epsilongc_word_t object)
  EPSILONGC_FUNCTION_ATTRIBUTES_FOR_AGGRESSIVE_INLINING
  __attribute__((pure));

EPSILONGC_FUNCTION_QUALIFIERS_FOR_AGGRESSIVE_INLINING epsilongc_integer_t
epsilongc_object_to_size_in_words(const epsilongc_word_t object)
  EPSILONGC_FUNCTION_ATTRIBUTES_FOR_AGGRESSIVE_INLINING
  __attribute__((pure));

/* Conditionally #include the whole definitions: */
#ifdef ENABLE_AGGRESSIVELY_INLINE
#include "definitionstoinline.h"
#endif // #ifdef ENABLE_AGGRESSIVELY_INLINE

#endif // #ifndef EPSILONGC_DECLARATIONS_TO_AGGRESSIVELY_INLINE
