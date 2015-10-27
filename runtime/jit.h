/* JIT engine for epsilon0, currently based on threaded code.

   Copyright (C) 2015  Luca Saiu
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


#ifndef EPSILON_JIT_H_
#define EPSILON_JIT_H_

#include "runtime.h"

/* The state of a thread running JITted code.  The structure is intentionally
   opaque and its inner content is subject to change in the future, possibly
   according to the configuration.

   FIXME: should this be heap-allocated, or be a manually managed root poiting
   to heap objects? */
typedef struct ejit_thread_state* ejit_thread_state_t;

ejit_thread_state_t
ejit_make_thread_state (void)
  __attribute__ ((malloc));

void
ejit_destroy_thread_state (const ejit_thread_state_t s);

/* void */
/* ejit_push_on_thread_state (const ejit_thread_state_t s, epsilon_value v); */

/* The generated code, which is allocated on the C heap and manually freed.
   Again, the structure is intentionally opaque. */
typedef struct ejit_code* ejit_code_t;

/* If literals_slot_pointer is non-NULL generate a final end instruction and
   place the literals slot index into the given location, so that the code can
   be easily executed even if there is no associated symbol.

   FIXME: say that the literals part which is generated is not a GC root and should
   be used immediately (by being put on a stack), before GC-allocating. */
ejit_code_t
ejit_compile (epsilon_value expression, epsilon_value formal_list,
              long *literals_slot_pointer)
  __attribute__(( malloc ));

void
ejit_destroy_code (const ejit_code_t c);

void
ejit_run_code (ejit_code_t code, ejit_thread_state_t state);

// FIXME: explain how this is useful for the main expression.
void
ejit_evaluate_expression (epsilon_value expression);


#endif // #ifndef EPSILON_JIT_H_
