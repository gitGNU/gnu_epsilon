/* Data representation, tagging and memory operations.

   Copyright (C) 2012 Luca Saiu [written during his few weeks with no employment]
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


#include "data.h"
#include "../utility/utility.h"

/* The following few functions don't depend on the particular backend: */
#ifndef EPSILON_RUNTIME_UNTAGGED
inline bool epsilon_value_to_bool(epsilon_value value){
  return (bool)(epsilon_value_to_epsilon_int(value));
}
inline epsilon_value epsilon_bool_to_epsilon_value(bool b){
  return epsilon_int_to_epsilon_value((epsilon_int)b);
}
inline bool epsilon_value_is_zero(epsilon_value value){
  return epsilon_value_eq(value, epsilon_int_to_epsilon_value(0));
}
#endif //#ifndef EPSILON_RUNTIME_UNTAGGED
inline epsilon_value epsilon_manually_allocate_with_value_length(epsilon_value length_in_words){
  return epsilon_manually_allocate_with_epsilon_int_length(epsilon_value_to_epsilon_int(length_in_words));
}
inline epsilon_value epsilon_gc_allocate_with_value_length(epsilon_value length_in_words){
  return epsilon_gc_allocate_with_epsilon_int_length(epsilon_value_to_epsilon_int(length_in_words));
}
#ifndef EPSILON_RUNTIME_UNTAGGED
inline epsilon_value epsilon_load_with_value_offset(epsilon_value pointer_value, epsilon_value offset_in_words){
  return epsilon_load_with_epsilon_int_offset(pointer_value, epsilon_value_to_epsilon_int(offset_in_words));
}
inline void epsilon_store_with_value_offset(epsilon_value pointer_value, epsilon_value offset_in_words, epsilon_value datum){
  epsilon_store_with_epsilon_int_offset(pointer_value, epsilon_value_to_epsilon_int(offset_in_words), datum);
}
#endif //#ifndef EPSILON_RUNTIME_UNTAGGED

char* epsilon_string_to_malloced_char_star(epsilon_value epsilon_string){
  epsilon_int length =
    epsilon_value_to_epsilon_int(epsilon_load_with_epsilon_int_offset(epsilon_string, 0));
  char *result = epsilon_xmalloc(length + 1);
  int i;
  for(i = 0; i < length; i ++)
    result[i] = epsilon_value_to_epsilon_int(epsilon_load_with_epsilon_int_offset(epsilon_string, i + 1));
  result[length] = '\0';
  return result;
}
char* epsilon_symbol_to_malloced_char_star(epsilon_value epsilon_symbol){
  return epsilon_string_to_malloced_char_star(epsilon_load_with_epsilon_int_offset(epsilon_symbol, 0));
}

inline bool epsilon_value_is_null(epsilon_value list_value){
  return epsilon_value_is_zero(list_value);
}
inline epsilon_value epsilon_value_car(epsilon_value list_value){
  return epsilon_load_with_epsilon_int_offset(list_value, 0);
}
inline epsilon_value epsilon_value_cdr(epsilon_value list_value){
  return epsilon_load_with_epsilon_int_offset(list_value, 1);
}
inline epsilon_value epsilon_value_cons(epsilon_value element_value, epsilon_value list_value){
  epsilon_value result = epsilon_gc_allocate_with_epsilon_int_length(2);
  epsilon_store_with_epsilon_int_offset(result, 0, element_value);
  epsilon_store_with_epsilon_int_offset(result, 1, list_value);
  return result;
}
inline epsilon_int epsilon_value_length(epsilon_value x){
  epsilon_int result = 0;
  epsilon_value p;
  for(p = x; ! epsilon_value_is_null(p); p = epsilon_value_cdr(p))
    result ++;
  return result;
}

#ifndef EPSILON_RUNTIME_SMOB
#include <pthread.h>
//#define GC_DEBUG
//#define GC_THREADS
#include <gc/gc.h> // FIXME: does this need to be here?
#ifndef GC_API
#define GC_API
#endif

void epsilon_data_initialize(void){
  GC_INIT();
#if HAVE_GC_ALLOW_REGISTER_THREADS
  GC_allow_register_threads();
#else
  #warning GC_allow_register_threads is not available.  Trying to go on without calling it.
#endif
  //printf("The main thread is %p\n", (void*)pthread_self());
}

epsilon_thread epsilon_current_thread(void){
  return (epsilon_thread)pthread_self();
}
epsilon_thread epsilon_make_thread(epsilon_thread_function function, epsilon_value datum){
  pthread_t thread;
  int pthread_create_result = pthread_create(&thread, NULL, function, datum);
  EPSILON_IF_UNLIKELY(pthread_create_result != 0)
    epsilon_runtime_appropriate_fail("epsilon_make_thread [pthread_create failed]");
  return (epsilon_thread)thread;
}
epsilon_value epsilon_join_thread(epsilon_thread thread){
  epsilon_value thread_result;
  int pthread_join_result = pthread_join((pthread_t)thread, &thread_result);
  EPSILON_IF_UNLIKELY(pthread_join_result != 0)
    epsilon_runtime_appropriate_fail("epsilon_join_thread [pthread_join failed]");
  return thread_result;
}

epsilon_value epsilon_call_with_initialized_gc(epsilon_thread_function function,
                                               epsilon_value datum){
  //pthread_t thread = pthread_self();
  //printf("epsilon_call_with_initialized_gc: registering %p\n", (void*)thread);
  struct GC_stack_base stack_base;
  EPSILON_IF_UNLIKELY(GC_get_stack_base(&stack_base) == GC_UNIMPLEMENTED)
    epsilon_runtime_appropriate_fail("epsilon_call_with_initialized_gc [GC_get_stack_base failed]");
  int register_my_thread_result = GC_register_my_thread(&stack_base);
  EPSILON_IF_UNLIKELY((register_my_thread_result == GC_NO_THREADS) ||
                      (register_my_thread_result == GC_UNIMPLEMENTED))
    epsilon_runtime_appropriate_fail("epsilon_call_with_initialized_gc [GC_register_my_thread failed]");
//GC_thr_init();
//printf("epsilon_call_with_initialized_gc: about to work from %p\n", (void*)thread);
  epsilon_value result = function(datum);
  GC_unregister_my_thread();
//printf("epsilon_call_with_initialized_gc: unregistered %p\n", (void*)thread);
  return result;
}
#endif // #ifndef EPSILON_RUNTIME_SMOB

//#define EPSILON_RUNTIME_UNTAGGED
//#define EPSILON_RUNTIME_TAGGED
//#define EPSILON_RUNTIME_SMOB

#if   defined(EPSILON_RUNTIME_UNTAGGED)
  #include "data-untagged.c"
#elif defined(EPSILON_RUNTIME_TAGGED)
  #include "data-tagged.c"
#elif defined(EPSILON_RUNTIME_SMOB)
  #include "data-smob.c"
#else
  #error "data.c: no runtime macro was specified"
#endif
