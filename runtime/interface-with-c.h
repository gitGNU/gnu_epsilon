/* [the file purpose in one line ???]

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


#ifndef EPSILON_INTERFACE_WITH_C_H_
#define EPSILON_INTERFACE_WITH_C_H_

#include "../utility/types.h"

/* This needs to be cast to the correct type, but it's useful to make
   prototypes simpler: */
typedef void*(*epsilon_some_c_function)();

/* Type abbreviations:
   - `c' char
   - `s' short
   - `i' int
   - `l' long
   - `f' float
   - `d' double
   - `w' epsilon word, epsilon int, or any C pointer
   - (nothing) void, for parameters
   - `v' void, for results */

/* The abbreviations above are used to name a set of C function types,
   returning an object of some type and taking paramters of some
   types.  For example:
     int f(long x, char y) // f has type epsilon_c_function_i_lc
     void g(int x, char y) // g has type epsilon_c_function__ic */

/* Convert a one-letter abbreviation into a C type: */
#define EPSILON_SHORT_TYPE_TO_TYPE(short_name) \
  epsilon_ ## short_name ## _type
typedef char         epsilon_c_type;
typedef short        epsilon_s_type;
typedef int          epsilon_i_type;
typedef long         epsilon_l_type;
typedef float        epsilon_f_type;
typedef double       epsilon_d_type;
typedef epsilon_word epsilon_w_type;
typedef void         epsilon_v_type; // only for EPSILON_C_FUNCTION_TYPEn
                                     // and EPSILON_CAST_TO_C_FUNCTION_TYPEn

// To do: the typedef generation machinery is probably not useful any more.

/* Generate a `typedef' type definition for a function taking 
   parameters of the given type and returning the given result.
   type.  All types should be passed in their abbreviated form. 
   Example:
     EPSILON_TYPEDEF_FUNCTION3(i, f, c, w) // no semicolon
   generates a type epsilon_c_function_i_fcw; it's a function taking
   three parameters (a float, and char and a word), and returning
   an int. */
#define EPSILON_TYPEDEF_VOID_FUNCTION1(P1) \
  typedef void(*epsilon_c_function__ ## P1) \
    (EPSILON_SHORT_TYPE_TO_TYPE(P1));
#define EPSILON_TYPEDEF_VOID_FUNCTION2(P1, P2) \
  typedef void(*epsilon_c_function__ ## P1 ## P2) \
    (EPSILON_SHORT_TYPE_TO_TYPE(P1), \
     EPSILON_SHORT_TYPE_TO_TYPE(P2));
#define EPSILON_TYPEDEF_VOID_FUNCTION3(P1, P2, P3) \
  typedef void(*epsilon_c_function__ ## P1 ## P2 ## P3) \
    (EPSILON_SHORT_TYPE_TO_TYPE(P1), \
     EPSILON_SHORT_TYPE_TO_TYPE(P2), \
     EPSILON_SHORT_TYPE_TO_TYPE(P3));
#define EPSILON_TYPEDEF_VOID_FUNCTION4(P1, P2, P3, P4) \
  typedef void(*epsilon_c_function__ ## P1 ## P2 ## P3 ## P4) \
    (EPSILON_SHORT_TYPE_TO_TYPE(P1), \
     EPSILON_SHORT_TYPE_TO_TYPE(P2), \
     EPSILON_SHORT_TYPE_TO_TYPE(P3), \
     EPSILON_SHORT_TYPE_TO_TYPE(P4));
#define EPSILON_TYPEDEF_FUNCTION0(R) \
  typedef EPSILON_SHORT_TYPE_TO_TYPE(R)(*epsilon_c_function_ ## R ## _) \
    (void);
#define EPSILON_TYPEDEF_FUNCTION1(R, P1) \
  typedef EPSILON_SHORT_TYPE_TO_TYPE(R)(*epsilon_c_function_ ## R ## _ ## P1) \
    (EPSILON_SHORT_TYPE_TO_TYPE(P1));
#define EPSILON_TYPEDEF_FUNCTION2(R, P1, P2) \
  typedef EPSILON_SHORT_TYPE_TO_TYPE(R)(*epsilon_c_function_ ## R ## _ ## P1 ## P2) \
    (EPSILON_SHORT_TYPE_TO_TYPE(P1), \
     EPSILON_SHORT_TYPE_TO_TYPE(P2));
#define EPSILON_TYPEDEF_FUNCTION3(R, P1, P2, P3) \
  typedef EPSILON_SHORT_TYPE_TO_TYPE(R)(*epsilon_c_function_ ## R ## _ ## P1 ## P2 ## P3) \
    (EPSILON_SHORT_TYPE_TO_TYPE(P1), \
     EPSILON_SHORT_TYPE_TO_TYPE(P2), \
     EPSILON_SHORT_TYPE_TO_TYPE(P3));
#define EPSILON_TYPEDEF_FUNCTION4(R, P1, P2, P3, P4) \
  typedef EPSILON_SHORT_TYPE_TO_TYPE(R)(*epsilon_c_function_ ## R ## _ ## P1 ## P2 ## P3 ## P4) \
    (EPSILON_SHORT_TYPE_TO_TYPE(P1), \
     EPSILON_SHORT_TYPE_TO_TYPE(P2), \
     EPSILON_SHORT_TYPE_TO_TYPE(P3), \
     EPSILON_SHORT_TYPE_TO_TYPE(P4));

/* We don't need a macro in the case of void -> void.  There is only
   one possible type of this shape. */
typedef void(*epsilon_c_function__)(void);

/* These are useful to cast a function pointer into a C function
   pointer type having the given result type and parameter types.
   Types are expressed with one-letter abbreviations, except for
   `v' when void is the result type.
   Notice that C function-pointer syntax limits the number of parentheses
   around a cast type. */
#define EPSILON_C_FUNCTION_TYPE0(R) \
  EPSILON_SHORT_TYPE_TO_TYPE(R)(*)(void)
#define EPSILON_C_FUNCTION_TYPE1(R, P1) \
  EPSILON_SHORT_TYPE_TO_TYPE(R)(*)(EPSILON_SHORT_TYPE_TO_TYPE(P1))
#define EPSILON_C_FUNCTION_TYPE2(R, P1, P2) \
  EPSILON_SHORT_TYPE_TO_TYPE(R)(*)(EPSILON_SHORT_TYPE_TO_TYPE(P1), \
                                   EPSILON_SHORT_TYPE_TO_TYPE(P2))
#define EPSILON_C_FUNCTION_TYPE3(R, P1, P2, P3) \
  EPSILON_SHORT_TYPE_TO_TYPE(R)(*)(EPSILON_SHORT_TYPE_TO_TYPE(P1), \
                                   EPSILON_SHORT_TYPE_TO_TYPE(P2), \
                                   EPSILON_SHORT_TYPE_TO_TYPE(P3))
#define EPSILON_C_FUNCTION_TYPE4(R, P1, P2, P3, P4) \
  EPSILON_SHORT_TYPE_TO_TYPE(R)(*)(EPSILON_SHORT_TYPE_TO_TYPE(P1), \
                                   EPSILON_SHORT_TYPE_TO_TYPE(P2), \
                                   EPSILON_SHORT_TYPE_TO_TYPE(P3), \
                                   EPSILON_SHORT_TYPE_TO_TYPE(P4))

/* Cast the given object to a function-pointer type described by
   one-letter abbreviations.  A void return type must be indicated
   as `v'. */
#define EPSILON_CAST_TO_C_FUNCTION_TYPE0(R, f) \
  ((EPSILON_C_FUNCTION_TYPE0(R))f)
#define EPSILON_CAST_TO_C_FUNCTION_TYPE1(R, P1, f) \
  ((EPSILON_C_FUNCTION_TYPE1(R, P1))f)
#define EPSILON_CAST_TO_C_FUNCTION_TYPE2(R, P1, P2, f) \
  ((EPSILON_C_FUNCTION_TYPE2(R, P1, P2))f)
#define EPSILON_CAST_TO_C_FUNCTION_TYPE3(R, P1, P2, P3, f) \
  ((EPSILON_C_FUNCTION_TYPE3(R, P1, P2, P3))f)
#define EPSILON_CAST_TO_C_FUNCTION_TYPE4(R, P1, P2, P3, P4, f) \
  ((EPSILON_C_FUNCTION_TYPE4(R, P1, P2, P3, P4))f)

/* Cast the given function f to the given type, expressed with one-letter
   abbreviations (`v' for a void return type), then call it passing it the
   given parameters x1..xn. */
#define EPSILON_CALL_C0(R, f) \
  (EPSILON_CAST_TO_C_FUNCTION_TYPE0(R, f)())
#define EPSILON_CALL_C1(R, P1, f, x1) \
  (EPSILON_CAST_TO_C_FUNCTION_TYPE1(R, P1, f)(x1))
#define EPSILON_CALL_C2(R, P1, P2, f, x1, x2) \
  (EPSILON_CAST_TO_C_FUNCTION_TYPE2(R, P1, P2, f)(x1, x2))
#define EPSILON_CALL_C3(R, P1, P2, P3, f, x1, x2, x3) \
  (EPSILON_CAST_TO_C_FUNCTION_TYPE3(R, P1, P2, P3, f)(x1, x2, x3))
#define EPSILON_CALL_C4(R, P1, P2, P3, P4, f, x1, x2, x3, x4) \
  (EPSILON_CAST_TO_C_FUNCTION_TYPE4(R, P1, P2, P3, P4, f)(x1, x2, x3, x4))


/* Return the address of the given C global.  This is a wrapper around
   dlopen() and dlsym(), with caching.  An empty MODUlE means that the
   symbol should be looked for in the program itself: */
epsilon_word epsilon_lookup_c_global_from(char *symbol, char *module);

/* Same as above, but the name is encoded as MODULE$SYMBOL, for examples
   "m$cos".  An empty MODUlE means that the symbol should be looked for
   in the program itself: */
epsilon_word epsilon_lookup_c_global(char *module_dollar_symbol);

#endif // #ifndef EPSILON_INTERFACE_WITH_C_H_
