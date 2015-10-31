/* Data representation, tagging and memory operations.

   Copyright (C) 2012, 2014 Luca Saiu
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


#ifndef __EPSILON_DATA_H_
#define __EPSILON_DATA_H_

#include <unistd.h>
#include <stdbool.h>
#include "../utility/types.h"

#ifdef EPSILON_EGC
#include "movinggc/movinggc.h"
#endif // #ifdef EPSILON_EGC

/* This should be treated as an abstract data type.  According to
   which runtime is specified via a preprocessor macro, there might be
   or not be boxedness tags, and the representation might or might not
   be always boxed. */
typedef epsilon_word epsilon_value;

/* What a thread exactly is also depends on the runtime, but it always
   boils down to a pointer to something.  Notice that the runtime does
   not necessarily tags threads differently from fixnums. */
typedef void* epsilon_thread;

/* Initialize global data structures, if any: */
void epsilon_data_initialize(void);

/* These always fail on runtimes with no boxedness tags: */
bool epsilon_is_fixnum(epsilon_value value);
bool epsilon_is_pointer(epsilon_value value);
size_t epsilon_buffer_size(epsilon_value pointer_value); // in words; may crash on non-pointers

#ifndef EPSILON_RUNTIME_UNTAGGED
/* Tag and untag.  Notice that foreign pointers, not managed by us,
   are tagged as fixnums, but not shifted: this assumes that even
   foreign pointers are aligned to at least 16 bits. */
epsilon_int epsilon_value_to_epsilon_int(epsilon_value value); // always succeed, even if value is a pointer
epsilon_unsigned epsilon_value_to_epsilon_unsigned(epsilon_value value); // always succeed, even if value is a pointer
epsilon_value epsilon_int_to_epsilon_value(epsilon_int i);
epsilon_value* epsilon_value_to_value_elements(epsilon_value pointer_value); // fail if non-pointer with untagged runtimes
epsilon_value epsilon_foreign_pointer_to_epsilon_value(void *p);
void* epsilon_value_to_foreign_pointer(epsilon_value value);
bool epsilon_value_to_bool(epsilon_value value);
epsilon_value epsilon_bool_to_epsilon_value(bool b);
epsilon_thread epsilon_value_to_thread(epsilon_value value);
epsilon_value epsilon_thread_to_epsilon_value(epsilon_thread thread);
#endif // #ifndef EPSILON_RUNTIME_UNTAGGED

epsilon_thread epsilon_current_thread(void);

/* Make a new (joinable) thread executing the given function, to be
   called with the given parameters: */
typedef epsilon_value(*epsilon_thread_function)(epsilon_value);
epsilon_thread epsilon_make_thread(epsilon_thread_function function, epsilon_value datum);

/* Join the given thread, and return its result: */
epsilon_value epsilon_join_thread(epsilon_thread thread);

/* FIXME: comment */
epsilon_value epsilon_call_with_initialized_gc(epsilon_thread_function function,
                                               epsilon_value datum)
  __attribute__((noinline,noclone));

/* Comparisons (by identity only): */
bool epsilon_value_is_zero(epsilon_value value);
bool epsilon_value_eq(epsilon_value value1, epsilon_value value2);

/* Memory operations */
epsilon_value epsilon_manually_allocate_with_value_length(epsilon_value length_in_words);
epsilon_value epsilon_manually_allocate_with_epsilon_int_length(epsilon_int length_in_words);
epsilon_value epsilon_gc_allocate_with_value_length(epsilon_value length_in_words)
  __attribute__((malloc));
epsilon_value epsilon_gc_allocate_with_epsilon_int_length(epsilon_int length_in_words)
  __attribute__((malloc));
void epsilon_manually_destroy(epsilon_value pointer_value);
void epsilon_gc_destroy(epsilon_value pointer_value);
epsilon_value epsilon_load_with_epsilon_int_offset(epsilon_value pointer_value, epsilon_int offset_in_words);
void epsilon_store_with_epsilon_int_offset(epsilon_value pointer_value, epsilon_int offset_in_words, epsilon_value datum);
epsilon_value epsilon_load_with_value_offset(epsilon_value pointer_value, epsilon_value offset_in_words);
void epsilon_store_with_value_offset(epsilon_value pointer_value, epsilon_value offset_in_words, epsilon_value datum);

/* Utility string and symbol operations, using epsilon1's representation: */
char* epsilon_string_to_malloced_char_star(epsilon_value epsilon_string);
char* epsilon_symbol_to_malloced_char_star(epsilon_value epsilon_symbol);

/* Utility list operations: */
bool epsilon_value_is_null(epsilon_value list_value);
epsilon_value epsilon_value_car(epsilon_value list_value);
epsilon_value epsilon_value_cdr(epsilon_value list_value);
epsilon_value epsilon_value_cons(epsilon_value element_value, epsilon_value list_value);
epsilon_int epsilon_value_length(epsilon_value x);

/* Not technically part of data representation, but definitely
   backend-dependant: */
void epsilon_runtime_appropriate_fail(char *reason) // Fail in a runtime-appropriate way
  __attribute__((noreturn));

// FIXME: use macros when possible instead of this ugly kludge
#ifdef EPSILON_RUNTIME_UNTAGGED
#define epsilon_value_to_epsilon_int(X) ((epsilon_int)(long)(X))
#define epsilon_value_to_epsilon_unsigned(X) ((epsilon_unsigned)(long)(X))
#define epsilon_value_to_foreign_pointer(X) (X)
#define epsilon_foreign_pointer_to_epsilon_value(X) (X)
#define epsilon_int_to_epsilon_value(X) ((epsilon_value)(long)(X))
#define epsilon_value_to_bool(X) ((long)(X))
#define epsilon_bool_to_epsilon_value(X) ((epsilon_value)(long)(X))
#define epsilon_value_is_zero(X) ((long)(X) == 0)
#define epsilon_value_eq(X, Y) ((epsilon_int)((epsilon_value)(long)(X) == (epsilon_value)(long)(Y)))
#define epsilon_load_with_epsilon_int_offset(P, O) (((epsilon_value*)(long)(P))[(long)(O)])
#define epsilon_store_with_epsilon_int_offset(P, O, D) ((epsilon_value*)(long)(P))[(long)(O)]=((epsilon_value)(long)(D))
#define epsilon_load_with_value_offset(P, O) (epsilon_load_with_epsilon_int_offset(P, epsilon_value_to_epsilon_int(O)))
#define epsilon_store_with_value_offset(P, O, D) epsilon_store_with_epsilon_int_offset(P, epsilon_value_to_epsilon_int(O), D)
#define epsilon_value_to_value_elements(P) (P)
#endif //#ifdef EPSILON_RUNTIME_UNTAGGED
#endif // #ifndef __EPSILON_DATA_H_
