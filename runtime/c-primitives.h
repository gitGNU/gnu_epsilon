/* epsilon primitives implemented in C.

   Copyright (C) 2012 Luca Saiu
   Written by Luca Saiu
   Updated in 2013 and 2016 by Luca Saiu

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


/* C primitives work with a stack logic: they take their parameters
   from a memory array of epsilon_value objects, and return their
   results on the same array, overwriting the first parameters.  Both
   parameters and results are stored in order, starting from index 0.
   The caller is supposed to know both in- and out-dimension: no such
   information is stored.  Of course, for efficiency reason, the stack
   primitives work on is not a stack as implemented in
   utility/stack.[ch], but a simple C buffer.

   Backends implementing (some) primitives in assembly will use more
   efficient calling conventions; this solution has the advantage of
   being simple and general. */

#ifndef __EPSILON_C_PRIMITIVES__
#define __EPSILON_C_PRIMITIVES__

#include "data.h"

/* Primitives have names of a bounded size, and exist in a bounded
   quantity: */
#define EPSILON_MAXIMUM_PRIMITIVE_NAME_LENGTH 100
#define EPSILON_MAXIMUM_IN_DIMENSION          4
#define EPSILON_MAXIMUM_OUT_DIMENSION         4
#define EPSILON_MAXIMUM_INOUT_DIMENSION \
  ((EPSILON_MAXIMUM_IN_DIMENSION < EPSILON_MAXIMUM_OUT_DIMENSION) ? \
   EPSILON_MAXIMUM_IN_DIMENSION : \
   EPSILON_MAXIMUM_OUT_DIMENSION)
#define EPSILON_MAXIMUM_C_PRIMITIVE_NO 200

/* A primitive only needs the buffer beginning to find its parameters
   and to know where to write its results.  Notice that the C result
   type is void. */
typedef void(*epsilon_c_primitive_function)(epsilon_value*);

/* Each primitive has a fixed name, in- and out-dimension, and in the
   case of primitives implemented in C, a function pointer. */
struct epsilon_c_primitive_descriptor{
  char name[EPSILON_MAXIMUM_PRIMITIVE_NAME_LENGTH];
  epsilon_int in_dimension;
  epsilon_int out_dimension;
  epsilon_c_primitive_function function_pointer;
}; // struct

/* Initialize primitive information, so that primitives may be
   accessed by name or by index.  It would be most efficient to simply
   call primitives as functions, or via function pointers; the problem
   is that function address may change, in general, whenever the C
   runtime is recompiled.  From the point of view of epsilon such
   addresses are foreign pointers: they can be saved when unexec'ing,
   and some past version did that, but the solution was very fragile
   with respect to C runtime modifications and was architecture-,
   system- and compiler-dependent.  This new solution simply
   associates an index to each primitive: the index for each primitive
   is fixed at initialization time, and only depends on the order in
   which is primitive is added within epsilon_initialize_c_primitives,
   which we will normally keep constant.  Access by index is quite
   efficient, based as it is on a global function pointer array. */
void
epsilon_c_primitives_initialize (int argc, char **argv);

/* Given a C primitive name, return its index or descriptor.  Lookup
   by name is relatively expensive. */
epsilon_int epsilon_lookup_c_primitive_index(char *name);
struct epsilon_c_primitive_descriptor* epsilon_lookup_c_primitive_descriptor(char *name);

/* Call a C primitive whose index is known: */
void epsilon_call_c_primitive_by_index(epsilon_int index, epsilon_value *stack);

/* This is a way of avoiding the overhead of epsilon_call_c_primitive_by_index,
   particularly useful from generated assembly code: */
extern epsilon_c_primitive_function epsilon_c_primitive_functions[];
extern struct epsilon_c_primitive_descriptor epsilon_c_primitive_descriptors[];

#endif // #ifndef __EPSILON_C_PRIMITIVES__
