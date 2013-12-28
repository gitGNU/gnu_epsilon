/* Data representation: SMOB backend, conditionally #include'd by data.c.

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


/* With this runtime, epsilon values are SCM objects belonging to our
   SMOB type. */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <libguile.h>
#include "marshal.h"

/* The SMOB tag of our "whatever" objects: */
scm_t_bits epsilon_whatever_tag; // non-static: this is currently declared as extern in guile+whatever.c

/* Commonly-used values which we don't want to allocate over and over again: */
static SCM epsilon_one_0_smob_instance;
static SCM epsilon_one_1_smob_instance;
static SCM epsilon_one_127_smob_instance; // an easy-to-recognize value

// Our "whatever" SMOB
//////////////////////////////////////////////////////

/* In order not to confuse Guile's GC, we have to distinguish pointers
   and non-pointers at runtime.  SMOBs are boxed, so the garbage
   collector must be able to find other SMOBs referred by a buffer
   SMOB. */
enum epsilon_whatever_case { epsilon_whatever_atom, epsilon_whatever_buffer, epsilon_whatever_thread };
typedef enum epsilon_whatever_case epsilon_whatever_case_t;
struct epsilon_whatever{
  epsilon_whatever_case_t whatever_case;
  union {
    epsilon_int atom;
    struct{
      SCM *SCM_buffer;
      size_t element_no;
    }; // anonymous struct
    SCM thread;
  }; // anonymous union
}; // struct

/* A pointer-sized object, containing the same datum in a struct
   epsilon_whatever, without tags. */
typedef epsilon_word epsilon_whatever_t;

inline static struct epsilon_whatever* epsilon_smob_to_struct_pointer_unsafe(SCM scm){
  //scm_assert_smob_type(whatever_tag, scm);
  return (struct epsilon_whatever*)SCM_SMOB_DATA(scm);
}
inline static struct epsilon_whatever* epsilon_smob_to_struct_pointer_safe(SCM scm){
  /* Ensure the parameter is acutally a whatever SMOB: */
  scm_assert_smob_type(epsilon_whatever_tag, scm);

  /* Ok, if we didn't fail already it's actually safe to use the "unsafe" version: */
  return epsilon_smob_to_struct_pointer_unsafe(scm);
}

inline static epsilon_whatever_t epsilon_smob_to_whatever_t(SCM scm){
  struct epsilon_whatever *s = epsilon_smob_to_struct_pointer_safe(scm);
  switch(s->whatever_case){
  case epsilon_whatever_atom:
    return (epsilon_whatever_t)(s->atom);
  case epsilon_whatever_buffer:
    return (epsilon_whatever_t)(s->SCM_buffer);
  case epsilon_whatever_thread:
    return (epsilon_whatever_t)(s->thread);
  default:
    //assert(false);
    scm_wrong_type_arg("epsilon_smob_to_whatever_t", 1, scm);
  } // switch
}

inline static SCM epsilon_make_smob(epsilon_whatever_case_t whatever_case){
  SCM result;
  struct epsilon_whatever *struct_pointer =
    scm_gc_malloc(sizeof(struct epsilon_whatever), "whatever struct");
  struct_pointer->element_no = 0;
  struct_pointer->SCM_buffer = NULL;
  struct_pointer->whatever_case = whatever_case;
  SCM_NEWSMOB(result, epsilon_whatever_tag, struct_pointer);
  return result;
}

static void epsilon_print_whatever_to_a_file_port(SCM whatever_smob, SCM port, scm_print_state *pstate){
  /* Get a file descriptor from the port, convert it to a FILE*, and
     do our thing.  Such conversions, according to the GNU libc
     manual, may require "cleaning" the stream with a fflush.  Notice
     that we have to duplicate the file descriptor so that fclose does
     not close the original file. */
  SCM original_file_descriptor_as_SCM = scm_fileno(port);
  int original_file_descriptor = scm_to_long(original_file_descriptor_as_SCM);
  int file_descriptor = dup(original_file_descriptor);
  FILE *file_star = fdopen(file_descriptor, "w");
  if(file_star == NULL)
    epsilon_fatal("print_whatever: fdopen");
  fflush(file_star); // also required after opening
  epsilon_dump_value(whatever_smob, file_star);
  fclose(file_star); // FIXME: if fclose is wrong here, do a fclean
}
static void epsilon_print_whatever_to_a_nonfile_port(SCM whatever_smob, SCM port, scm_print_state *pstate){
  /* Ugly kludge: write to a temporary file, and copy from the file to the requested port. */
  FILE *temporary_file = tmpfile();
  epsilon_dump_value(whatever_smob, temporary_file);
  long end_position = ftell(temporary_file);
  fseek(temporary_file, 0, SEEK_SET);
  long i;
  for(i = 0; i < end_position; i ++)
    scm_write_char(SCM_MAKE_CHAR(fgetc(temporary_file)), port);
  fclose(temporary_file); // this automatically deletes the file created by tmpfile
}
static int epsilon_print_whatever(SCM whatever_smob, SCM port, scm_print_state *pstate){
  /* Our dumping facility is based on FILE*, so it doesn't work
     directly with string ports. */
  if(scm_to_bool(scm_file_port_p(port)))
    epsilon_print_whatever_to_a_file_port(whatever_smob, port, pstate);
  else
    epsilon_print_whatever_to_a_nonfile_port(whatever_smob, port, pstate);
  return 1; // success
}

static SCM epsilon_whatever_smob_equalp(SCM a, SCM b){
  return (epsilon_smob_to_whatever_t(a) == epsilon_smob_to_whatever_t(b)) ?
    SCM_BOOL_T :
    SCM_BOOL_F;
}
static SCM epsilon_mark_whatever (SCM whatever_smob) __attribute__((unused));
static SCM epsilon_mark_whatever (SCM whatever_smob){
  //printf("Marking something: BEGIN\n"); fflush(stdout);
  //printf("Marking %p: begin\n", epsilon_whatever_smob); fflush(stdout);
  struct epsilon_whatever *whatever =
    (struct epsilon_whatever*)SCM_SMOB_DATA(whatever_smob);
  //printf("  The struct is at %p\n", whatever); fflush(stdout);
  
  switch(whatever->whatever_case){
  case epsilon_whatever_atom:
    // do nothing
    break;
  case epsilon_whatever_buffer:{
    SCM *buffer = whatever->SCM_buffer;
    size_t element_no = whatever->element_no;
    //printf("  (it's a %li-element buffer)\n", (long)element_no); fflush(stdout);
    long i;
    //scm_gc_mark(buffer);
    for(i = 0; i < element_no; i ++)
      scm_gc_mark(buffer[i]);
    break;
  }
  case epsilon_whatever_thread:
    scm_gc_mark(whatever->thread);
    break;
  default:
    assert(false);
  } // switch
  //printf("Marking %li: end\n", smob_to_long(whatever_smob)); fflush(stdout);
  //printf("Marking something: END\n"); fflush(stdout);
  return SCM_BOOL_F;
}

static size_t epsilon_free_whatever (SCM whatever_smob){
  //long smob_as_long = smob_to_long(whatever_smob);
  //printf("Freeing %li: begin\n", smob_as_long); fflush(stdout);
  struct epsilon_whatever *whatever =
    (struct epsilon_whatever*)SCM_SMOB_DATA(whatever_smob);
  switch(whatever->whatever_case){
  case epsilon_whatever_buffer:
    //printf("- Freeing the buffer at %p\n", whatever->SCM_buffer); fflush(stdout);
    if(whatever->SCM_buffer != NULL)
      scm_gc_free(whatever->SCM_buffer,
                  sizeof(SCM) * whatever->element_no,
                  "SCM buffer in a whatever struct");
    break;
  case epsilon_whatever_atom:
    // do nothing
    break;
  case epsilon_whatever_thread:
    /*
    scm_gc_free(whatever->thread,
                sizeof(SCM),
                "thread in a whatever object");
    */
    // Do nothing
    break;
  default:
    assert(false);
  } // switch
  //printf("- Freeing the struct at %p\n", whatever); fflush(stdout);
  scm_gc_free(whatever,
              sizeof(struct epsilon_whatever),
              "whatever struct");
  //printf("Freeing %li: end\n", smob_as_long); fflush(stdout);
  return 0;
}

// Declared functions
//////////////////////////////////////////////////////

inline bool epsilon_is_fixnum(epsilon_value value){
  struct epsilon_whatever *s = epsilon_smob_to_struct_pointer_safe(value);
  return ((s->whatever_case == epsilon_whatever_atom)
          || (s->whatever_case == epsilon_whatever_thread));
}
inline bool epsilon_is_pointer(epsilon_value value){
  struct epsilon_whatever *s = epsilon_smob_to_struct_pointer_safe(value);
  return (s->whatever_case == epsilon_whatever_buffer);
}
inline size_t epsilon_buffer_size(epsilon_value value){ // in words
  struct epsilon_whatever *s = epsilon_smob_to_struct_pointer_safe(value);
  EPSILON_IF_LIKELY(s->whatever_case == epsilon_whatever_buffer)
    return s->element_no;
  else
    epsilon_runtime_appropriate_fail("epsilon_buffer_size [non-pointer]");
}

inline epsilon_value epsilon_foreign_pointer_to_epsilon_value(void *p){
  return epsilon_int_to_epsilon_value((epsilon_int)p);
}
inline void* epsilon_value_to_foreign_pointer(epsilon_value value){
  return (void*)(epsilon_value_to_epsilon_int(value));
}

/* Here we also succeed if value is a pointer: */
inline epsilon_int epsilon_value_to_epsilon_int(epsilon_value value){
  return (epsilon_int)(epsilon_smob_to_whatever_t(value));
}
inline static epsilon_value epsilon_int_to_epsilon_value_unoptimized(epsilon_int i){
  SCM result = epsilon_make_smob(epsilon_whatever_atom);
  struct epsilon_whatever *s = epsilon_smob_to_struct_pointer_unsafe(result);
  s->atom = i;
  return result;
}
inline epsilon_value epsilon_int_to_epsilon_value(epsilon_int i){
  if(i == 0)
    return epsilon_one_0_smob_instance;
  else if(i == 1)
    return epsilon_one_1_smob_instance;
  else
    return epsilon_int_to_epsilon_value_unoptimized(i);
}
inline epsilon_value* epsilon_value_to_value_elements(epsilon_value pointer_value){
  struct epsilon_whatever *s = epsilon_smob_to_struct_pointer_safe(pointer_value);
  EPSILON_IF_LIKELY(s->whatever_case == epsilon_whatever_buffer)
    return (epsilon_value*)(s->SCM_buffer);
  else
    epsilon_runtime_appropriate_fail("epsilon_value_to_value_elements [non-pointer]");
}
inline epsilon_value epsilon_thread_to_epsilon_value(epsilon_thread guile_thread){
  SCM result = epsilon_make_smob(epsilon_whatever_thread);
  struct epsilon_whatever *s = epsilon_smob_to_struct_pointer_unsafe(result);
  s->thread = (SCM)guile_thread;
  return result;
}
inline epsilon_thread epsilon_value_to_thread(epsilon_value value){
  struct epsilon_whatever *s = epsilon_smob_to_struct_pointer_safe(value);
  EPSILON_IF_UNLIKELY(s->whatever_case != epsilon_whatever_thread)
    epsilon_runtime_appropriate_fail("epsilon_value_to_guile_thread [non-thread whatever]");
  return (epsilon_thread)(SCM)(epsilon_smob_to_whatever_t(value));
}

inline bool epsilon_value_eq(epsilon_value value1, epsilon_value value2){
  return epsilon_smob_to_whatever_t(value1) == epsilon_smob_to_whatever_t(value2);
}

inline epsilon_value epsilon_manually_allocate_with_epsilon_int_length(epsilon_int length_in_words){
  // No manual allocation for this runtime
  return epsilon_gc_allocate_with_epsilon_int_length(length_in_words);
}
inline epsilon_value epsilon_gc_allocate_with_epsilon_int_length(epsilon_int element_no){
  SCM result = epsilon_make_smob(epsilon_whatever_buffer);

  SCM *buffer = scm_gc_malloc(sizeof(SCM) * element_no, "whatever buffer");
  int i;
  for(i = 0; i < element_no; i ++)
    buffer[i] = epsilon_one_127_smob_instance; // make it recognizable

  struct epsilon_whatever *s = epsilon_smob_to_struct_pointer_unsafe(result);
  s->element_no = element_no;
  s->SCM_buffer = buffer;

  return result;
}
inline void epsilon_manually_destroy(epsilon_value pointer_value){
  // Do nothing
}
inline void epsilon_gc_destroy(epsilon_value pointer_value){
  // Do nothing
}
inline epsilon_value epsilon_load_with_epsilon_int_offset(epsilon_value pointer_value, epsilon_int offset_in_words){
  struct epsilon_whatever *s = epsilon_smob_to_struct_pointer_safe(pointer_value);
  EPSILON_IF_UNLIKELY(s->whatever_case != epsilon_whatever_buffer)
    epsilon_runtime_appropriate_fail("epsilon_load_with_epsilon_int_offset [non-pointer]");
  EPSILON_IF_UNLIKELY(offset_in_words < 0)
    epsilon_runtime_appropriate_fail("epsilon_load_with_epsilon_int_offset [out-of-bound index: negative]");
  epsilon_int element_no = s->element_no;
  EPSILON_IF_UNLIKELY(offset_in_words >= element_no)
    epsilon_runtime_appropriate_fail("epsilon_load_with_epsilon_int_offset [out-of-bound index: too large]");
  else
    return s->SCM_buffer[offset_in_words];
}
inline void epsilon_store_with_epsilon_int_offset(epsilon_value pointer_value, epsilon_int offset_in_words, epsilon_value datum){
  struct epsilon_whatever *s = epsilon_smob_to_struct_pointer_safe(pointer_value);
  EPSILON_IF_UNLIKELY(s->whatever_case != epsilon_whatever_buffer)
    epsilon_runtime_appropriate_fail("epsilon_load_with_epsilon_int_offset [non-pointer]");
  EPSILON_IF_UNLIKELY(offset_in_words < 0)
    epsilon_runtime_appropriate_fail("epsilon_load_with_epsilon_int_offset [out-of-bound index: negative]");
  epsilon_int element_no = s->element_no;
  EPSILON_IF_UNLIKELY(offset_in_words >= element_no)
    epsilon_runtime_appropriate_fail("epsilon_load_with_epsilon_int_offset [out-of-bound index: too large]");
  else
    s->SCM_buffer[offset_in_words] = datum;
}

void epsilon_runtime_appropriate_fail(char *reason){
  scm_error(scm_from_locale_symbol("misc-error"),
            "epsilon_runtime_appropriate_fail",
            reason,
            SCM_EOL,
            SCM_EOL);
}

epsilon_thread epsilon_current_thread(void){
  return (epsilon_thread)scm_current_thread();
}

epsilon_thread epsilon_make_thread(epsilon_thread_function function, epsilon_value datum){
  //printf("OK epsilon_make_thread\n");
  SCM SCM_thread = scm_spawn_thread((scm_t_catch_body)function, datum, NULL, NULL);
  return (epsilon_thread)SCM_thread;
}

epsilon_value epsilon_join_thread(epsilon_thread SCM_thread){ 
  //printf("OK epsilon_join_thread\n");
  epsilon_value result = scm_join_thread((SCM)SCM_thread);
  return result;
}

epsilon_value epsilon_call_with_initialized_gc(epsilon_thread_function function,
                                               epsilon_value datum){
  //printf("epsilon_call_with_initialized_gc: the thread is %p\n", (void*)pthread_self());
  return scm_with_guile(function, datum);
  //return function(datum);
  //scm_init_guile(); return function(datum);
}

void epsilon_data_initialize(void){
  /* scm_init_guile is less portable than the alternative, but I don't
     really care about portability to very exotic systems here: Guile
     is for bootstrapping only, and the dependency will go away soon.  */
  scm_init_guile();

  /* Initialize our SMOB type: */
  epsilon_whatever_tag = scm_make_smob_type("whatever", sizeof(struct epsilon_whatever));
  scm_set_smob_print(epsilon_whatever_tag, epsilon_print_whatever);
  scm_set_smob_equalp(epsilon_whatever_tag, epsilon_whatever_smob_equalp);
#if ( SCM_MAJOR_VERSION == 1 )
  scm_set_smob_mark(epsilon_whatever_tag, epsilon_mark_whatever);
#endif
  scm_set_smob_free(epsilon_whatever_tag, epsilon_free_whatever);
  
  /* Make common data, to be reused: */
  epsilon_one_0_smob_instance = scm_permanent_object(epsilon_int_to_epsilon_value_unoptimized(0));
  epsilon_one_1_smob_instance = scm_permanent_object(epsilon_int_to_epsilon_value_unoptimized(1));
  epsilon_one_127_smob_instance = scm_permanent_object(epsilon_int_to_epsilon_value_unoptimized(127));
}
