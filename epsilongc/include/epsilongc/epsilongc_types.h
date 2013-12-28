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


#ifndef EPSILONGC_TYPES_H_
#define EPSILONGC_TYPES_H_

#include "epsilongc_macros.h"

#include <stdio.h>
#include <stdbool.h>
#include <limits.h>


/* Find an integer type with the same size as void*: */
#if SIZEOF_INT == SIZEOF_VOID_P
#define EPSILONGC_INTEGER_T int
#elif SIZEOF_LONG == SIZEOF_VOID_P
#define EPSILONGC_INTEGER_T long
#elif SIZEOF_LONG_LONG == SIZEOF_VOID_P
#define EPSILONGC_INTEGER_T long long
#elif SIZEOF_SHORT == SIZEOF_VOID_P
#warning Very strange: is this a 16bit architecture?
#define EPSILONGC_INTEGER_T short
#else
#error Sorry, I can not find a C integer type with the same size as void*
#endif

/* Define some type aliases: */
typedef signed EPSILONGC_INTEGER_T epsilongc_integer_t;
typedef signed EPSILONGC_INTEGER_T epsilongc_signed_integer_t;
typedef unsigned EPSILONGC_INTEGER_T epsilongc_unsigned_integer_t;
typedef void* epsilongc_pointer_t;
typedef epsilongc_pointer_t epsilongc_word_t;
typedef epsilongc_unsigned_integer_t epsilongc_bitmask_t;

/* The number of bits and bytes in a word: */
#define EPSILONGC_BITS_PER_WORD (CHAR_BIT * sizeof(epsilongc_word_t))
#define EPSILONGC_BYTES_PER_WORD (sizeof(epsilongc_word_t))

/* This may be useful for debugging: */
#define EPSILONGC_INVALID_POINTER ((epsilongc_pointer_t)0xdead0000)

/* See the comment in the #include'd header: */
#include "epsilongc_debugging_hack.h"

#endif // #ifndef EPSILONGC_TYPES_H_
