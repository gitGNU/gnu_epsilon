/* Machine-dependant types and convertion operators.

   Copyright (C) 2012 Université Paris 13
   Written by Luca Saiu

   This file is part of GNU epsilon.

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


#ifndef EPSILON_UTILITY_TYPES_H_
#define EPSILON_UTILITY_TYPES_H_

#include "config.h"

/* We consider the width of a pointer to be the width of a machine word;
   that is the case in all the machines I know. */
typedef void* epsilon_word;

/* An instruction label is just a pointer, in all architectures included
   the SVM, with or without threading (without threading it's a pointer to
   an SVM instruction struct, with threading it's a void*): */
typedef void* epsilon_label;

/* We need an interger type which is as large as a word, in both
   signed and unsigned versions.  I currently don't use intptr_t
   just to attempt (not too seriously) C89 compatibility.
   It's useful to also have a float type as large as a word: */
#if (SIZEOF_VOID_P == 1)
  #error VERY unusual: a char has the same size as a pointer.
  #error               Please report information about your
  #error               machine to the author. The thing will
  #error               probably NOT work as it is.
#elif (SIZEOF_VOID_P == SIZEOF_SHORT)
  #error Strange: the pointer-sized C integer type is short
  #error          please report information about your machine
  #error          to the author.
#elif (SIZEOF_VOID_P == SIZEOF_INT)
  typedef signed   int epsilon_int;
  typedef unsigned int epsilon_unsigned;
#elif (SIZEOF_VOID_P == SIZEOF_LONG)
  typedef signed   long epsilon_int;
  typedef unsigned long epsilon_unsigned;
#elif (SIZEOF_VOID_P == SIZEOF_LONG_LONG)
  typedef signed   long long epsilon_int;
  typedef unsigned long long epsilon_unsigned;
#else
  #error Could not find a pointer-sized C integer type
#endif
#if (SIZEOF_FLOAT == SIZEOF_VOID_P)
  typedef float epsilon_float;
#elif (SIZEOF_DOUBLE == SIZEOF_VOID_P)
  typedef double epsilon_float;
#elif (SIZEOF_LONG_DOUBLE == SIZEOF_VOID_P)
  typedef long double epsilon_float;
#else
  #error Could not find a pointer-sized C float type
#endif
//typedef float epsilon_float; // just to stress-test on x86_64: let's make epsilon_float smaller than a word. To do: remove this

/* Sometimes we need to encode an integer within a word or vice-versa.  Using
   multiple casts directly is not very readable, so I provide these macros: */
#define EPSILON_EPSILON_INT_TO_EPSILON_WORD(X)      ((epsilon_word)(epsilon_int)(X))
#define EPSILON_WORD_TO_EPSILON_INT(X)              ((epsilon_int)(epsilon_word)(X))
#define EPSILON_EPSILON_UNSIGNED_TO_EPSILON_WORD(X) ((epsilon_word)(epsilon_unsigned)(X))
#define EPSILON_WORD_TO_EPSILON_UNSIGNED(X)         ((epsilon_unsigned)(epsilon_word)(X))

#define EPSILON_CHAR_TO_EPSILON_WORD(X)             ((epsilon_word)(epsilon_int)(char)(X))
#define EPSILON_SHORT_TO_EPSILON_WORD(X)            ((epsilon_word)(epsilon_int)(short)(X))
#define EPSILON_INT_TO_EPSILON_WORD(X)              ((epsilon_word)(epsilon_int)(int)(X))
#define EPSILON_LONG_TO_EPSILON_WORD(X)             ((epsilon_word)(epsilon_int)(long)(X))

#define EPSILON_WORD_TO_CHAR(X)                     ((char)(epsilon_int)(epsilon_word)(X))
#define EPSILON_WORD_TO_SHORT(X)                    ((short)(epsilon_int)(epsilon_word)(X))
#define EPSILON_WORD_TO_INT(X)                      ((int)(epsilon_int)(epsilon_word)(X))
#define EPSILON_WORD_TO_LONG(X)                     ((long)(epsilon_int)(epsilon_word)(X))

#endif // #ifndef EPSILON_UTILITY_TYPES_H_
