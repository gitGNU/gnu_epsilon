/* epsilon primitives implemented in C.

   Copyright (C) 2012, 2013, 2014 Luca Saiu
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


#include "c-primitives.h"
#include "data.h"
#include "../utility/utility.h"
#include "marshal.h"
#include "epsilon0-interpreter.h"

#include <errno.h>
#include <iconv.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>

#ifdef HAVE_LIBUNISTRING
#include <locale.h>
#include <unistdio.h>
#include <uniconv.h>
#endif // #ifdef HAVE_LIBUNISTRING

#include <readline/readline.h>
#include <readline/history.h>

#ifdef HAVE_LIBUNISTRING
// The current locale, as identified by iconv.
static const char* epsilon_locale_charset;
#endif // #ifdef HAVE_LIBUNISTRING

/* #ifndef EPSILON_RUNTIME_SMOB */
/* #define GC_THREADS */
/* #include <gc/gc.h> */
/* #endif // #ifndef EPSILON_RUNTIME_SMOB */

// FIXME: remove if not used in the end

/* The primitive function arrays.  They refer the same primitives in the same order. */
static epsilon_int epsilon_c_primitive_no = 0;
epsilon_c_primitive_function epsilon_c_primitive_functions[EPSILON_MAXIMUM_C_PRIMITIVE_NO];
struct epsilon_c_primitive_descriptor epsilon_c_primitive_descriptors[EPSILON_MAXIMUM_C_PRIMITIVE_NO];

inline void epsilon_call_c_primitive_by_index(epsilon_int index, epsilon_value *stack){
  epsilon_c_primitive_functions[index](stack);
}

/* The primitive hash maps each primitive name into a pointer within
   epsilon_c_primitive_functions: */
static struct epsilon_string_hash epsilon_c_primitive_hash;

epsilon_int epsilon_lookup_c_primitive_index(char *name){
  epsilon_c_primitive_function *pointer =
    epsilon_lookup_string_hash(& epsilon_c_primitive_hash, name);
  /* Return the distance, in elements, between the pointer and the
     array beginning: */
  EPSILON_IF_LIKELY(pointer != NULL)
    return pointer - epsilon_c_primitive_functions;
  else
    epsilon_fatal("epsilon_lookup_c_primitive_index: unknown primitive `%s'", name);
}

struct epsilon_c_primitive_descriptor* epsilon_lookup_c_primitive_descriptor(char *name){
  epsilon_int index = epsilon_lookup_c_primitive_index(name);
  return &(epsilon_c_primitive_descriptors[index]);
}

static void epsilon_initialize_c_primitive(char *name,
                                           epsilon_c_primitive_function function_pointer,
                                           epsilon_int in_dimension, 
                                           epsilon_int out_dimension){
  //printf("Initializing the primitive %s...  ", name);
  epsilon_int new_primitive_index = epsilon_c_primitive_no;
  epsilon_c_primitive_function *pointer =
    ((epsilon_c_primitive_function*)epsilon_c_primitive_functions) + new_primitive_index;
  *pointer = function_pointer;
  strcpy(epsilon_c_primitive_descriptors[new_primitive_index].name, name);
  epsilon_c_primitive_descriptors[new_primitive_index].in_dimension = in_dimension;
  epsilon_c_primitive_descriptors[new_primitive_index].out_dimension = out_dimension;
  epsilon_c_primitive_descriptors[new_primitive_index].function_pointer = function_pointer;
  epsilon_add_to_string_hash(&epsilon_c_primitive_hash, name, (epsilon_word)pointer);
  epsilon_c_primitive_no ++;
  //printf("Done.\n");
}

// Primitive definitions: begin
///////////////////////////////////////////////////////////////////

static void epsilon_primitive_duplicate(epsilon_value *stack){
  stack[1] = stack[0];
}
static void epsilon_primitive_swap(epsilon_value *stack){
  epsilon_value x = stack[0];
  epsilon_value y = stack[1];
  stack[0] = y;
  stack[1] = x;
}
static void epsilon_primitive_zero_p(epsilon_value *stack){*stack = epsilon_bool_to_epsilon_value(epsilon_value_to_epsilon_int(*stack) == 0);}
static void epsilon_primitive_make_zero(epsilon_value *stack){*stack = epsilon_int_to_epsilon_value(0);}
static void epsilon_primitive_successor(epsilon_value *stack){*stack = epsilon_int_to_epsilon_value(epsilon_value_to_epsilon_int(*stack) + 1);}
static void epsilon_primitive_predecessor(epsilon_value *stack){*stack = epsilon_int_to_epsilon_value(epsilon_value_to_epsilon_int(*stack) - 1);}
static void epsilon_primitive_whatever_eq_p(epsilon_value *stack){
  epsilon_value x = stack[0];
  epsilon_value y = stack[1];

  *stack = epsilon_int_to_epsilon_value(epsilon_value_eq(x, y));
}
static void epsilon_primitive_less_than_p(epsilon_value *stack){*stack = epsilon_bool_to_epsilon_value(epsilon_value_to_epsilon_int(stack[0]) < epsilon_value_to_epsilon_int(stack[1]));}
static void epsilon_primitive_less_than_or_equal_p(epsilon_value *stack){*stack = epsilon_bool_to_epsilon_value(epsilon_value_to_epsilon_int(stack[0]) <= epsilon_value_to_epsilon_int(stack[1]));}
static void epsilon_primitive_negate(epsilon_value *stack){*stack = epsilon_int_to_epsilon_value(- epsilon_value_to_epsilon_int(stack[0]));}
static void epsilon_primitive_plus(epsilon_value *stack){*stack = epsilon_int_to_epsilon_value(epsilon_value_to_epsilon_int(stack[0]) + epsilon_value_to_epsilon_int(stack[1]));}
static void epsilon_primitive_minus(epsilon_value *stack){*stack = epsilon_int_to_epsilon_value(epsilon_value_to_epsilon_int(stack[0]) - epsilon_value_to_epsilon_int(stack[1]));}
static void epsilon_primitive_times(epsilon_value *stack){*stack = epsilon_int_to_epsilon_value(epsilon_value_to_epsilon_int(stack[0]) * epsilon_value_to_epsilon_int(stack[1]));}
static void epsilon_primitive_divided(epsilon_value *stack){
  epsilon_int y_as_epsilon_int = epsilon_value_to_epsilon_int(stack[1]);
  EPSILON_IF_UNLIKELY(y_as_epsilon_int == 0)
    //epsilon_value_out_of_range("fixnum:/", stack[1]);
    epsilon_runtime_appropriate_fail("fixnum:/");
  *stack = epsilon_int_to_epsilon_value(epsilon_value_to_epsilon_int(stack[0]) / y_as_epsilon_int);
}
static void epsilon_primitive_modulo(epsilon_value *stack){
  epsilon_int y_as_epsilon_int = epsilon_value_to_epsilon_int(stack[1]);
  EPSILON_IF_UNLIKELY(y_as_epsilon_int == 0)
    //epsilon_value_out_of_range("fixnum:%", stack[1]);
    epsilon_runtime_appropriate_fail("fixnum:%");
  *stack = epsilon_int_to_epsilon_value(epsilon_value_to_epsilon_int(stack[0]) % y_as_epsilon_int);
}
static void epsilon_primitive_divided_and_modulo(epsilon_value *stack){
  epsilon_int x_as_epsilon_int = epsilon_value_to_epsilon_int(stack[0]);
  epsilon_int y_as_epsilon_int = epsilon_value_to_epsilon_int(stack[1]);
  EPSILON_IF_UNLIKELY(y_as_epsilon_int == 0)
    //epsilon_value_out_of_range("fixnum:%", stack[1]);
    epsilon_runtime_appropriate_fail("fixnum:%/");
  stack[0] = epsilon_int_to_epsilon_value(x_as_epsilon_int / y_as_epsilon_int);
  stack[1] = epsilon_int_to_epsilon_value(x_as_epsilon_int % y_as_epsilon_int);
}
static void epsilon_primitive_bitwise_not(epsilon_value *stack){*stack = epsilon_int_to_epsilon_value(~ epsilon_value_to_epsilon_int(stack[0]));}
static void epsilon_primitive_bitwise_and(epsilon_value *stack){*stack = epsilon_int_to_epsilon_value(epsilon_value_to_epsilon_int(stack[0]) & epsilon_value_to_epsilon_int(stack[1]));}
static void epsilon_primitive_bitwise_or(epsilon_value *stack){*stack = epsilon_int_to_epsilon_value(epsilon_value_to_epsilon_int(stack[0]) | epsilon_value_to_epsilon_int(stack[1]));}
static void epsilon_primitive_bitwise_xor(epsilon_value *stack){*stack = epsilon_int_to_epsilon_value(epsilon_value_to_epsilon_int(stack[0]) ^ epsilon_value_to_epsilon_int(stack[1]));}
static void epsilon_primitive_left_shift(epsilon_value *stack){*stack = epsilon_int_to_epsilon_value(epsilon_value_to_epsilon_int(stack[0]) << epsilon_value_to_epsilon_int(stack[1]));}
static void epsilon_primitive_arithmetic_right_shift(epsilon_value *stack){*stack = epsilon_int_to_epsilon_value(epsilon_value_to_epsilon_int(stack[0]) >> epsilon_value_to_epsilon_int(stack[1]));}
static void epsilon_primitive_logic_right_shift(epsilon_value *stack){*stack = epsilon_int_to_epsilon_value(epsilon_value_to_epsilon_unsigned(stack[0]) >> epsilon_value_to_epsilon_int(stack[1]));}
static void epsilon_primitive_left_shift_1_bit(epsilon_value *stack){*stack = epsilon_int_to_epsilon_value(epsilon_value_to_epsilon_int(stack[0]) << 1);}
static void epsilon_primitive_arithmetic_right_shift_1_bit(epsilon_value *stack){*stack = epsilon_int_to_epsilon_value(epsilon_value_to_epsilon_int(stack[0]) >> 1);}
static void epsilon_primitive_logic_right_shift_1_bit(epsilon_value *stack){*stack = epsilon_int_to_epsilon_value(epsilon_value_to_epsilon_unsigned(stack[0]) >> 1);}

static void epsilon_primitive_gc_disable(epsilon_value *stack){
/* #ifndef EPSILON_RUNTIME_SMOB */
/*   GC_disable(); */
/* #endif // #ifndef EPSILON_RUNTIME_SMOB */
}
static void epsilon_primitive_gc_reenable(epsilon_value *stack){
/* #ifndef EPSILON_RUNTIME_SMOB */
/*   GC_enable(); */
/* #endif // #ifndef EPSILON_RUNTIME_SMOB */
}
static void epsilon_primitive_buffer_make_uninitialized(epsilon_value *stack){
  /* For this implementation in Scheme where efficiency isn't the main
     concern, we want to explicitly initialize "uninitalized" buffers
     so as to simulate "random" data: */
  epsilon_value word_no_epsilon_value = *stack;
  epsilon_int word_no = epsilon_value_to_epsilon_int(word_no_epsilon_value);

  epsilon_value result = epsilon_gc_allocate_with_epsilon_int_length(word_no);
  epsilon_value initial_value = epsilon_int_to_epsilon_value(127);
  int i;
  for(i = 0; i < word_no; i ++)
    epsilon_store_with_epsilon_int_offset(result, i, initial_value);
  *stack = result;
}
// At least for the time being, always initialize buffers.  This makes
// debugging easier, as we can print values without crashing.
static void epsilon_primitive_buffer_make(epsilon_value *stack){
  epsilon_primitive_buffer_make_uninitialized(stack);
}
static void epsilon_primitive_buffer_destroy(epsilon_value* stack){
  epsilon_gc_destroy(*stack);
}
static void epsilon_primitive_buffer_get(epsilon_value *stack){
  stack[0] = epsilon_load_with_value_offset(stack[0], stack[1]);
}
static void epsilon_primitive_buffer_set(epsilon_value *stack){
  epsilon_store_with_value_offset(stack[0], stack[1], stack[2]);
}
static void epsilon_primitive_fixnum_write(epsilon_value* stack) __attribute__((unused));
static void epsilon_primitive_fixnum_write(epsilon_value* stack){
  epsilon_int fixnum = epsilon_value_to_epsilon_int(stack[0]);
  printf("%li", (long)fixnum);
}

static void epsilon_primitive_debug_dump(epsilon_value *stack){
  fprintf(stdout, "DUMP: ");
  epsilon_dump_value(stack[0], stdout);
  fprintf(stdout, "\n");
}
static void epsilon_primitive_debug_fail(epsilon_value *stack){
  epsilon_runtime_appropriate_fail("debug:fail");
}

static void epsilon_primitive_atom_p(epsilon_value *stack){
  stack[0] = epsilon_bool_to_epsilon_value(epsilon_is_fixnum(stack[0]));
}
static void epsilon_primitive_buffer_p(epsilon_value *stack){
  stack[0] = epsilon_bool_to_epsilon_value(epsilon_is_pointer(stack[0]));
}
static void epsilon_primitive_thread_p(epsilon_value *stack){
  //epsilon_runtime_appropriate_fail("primitive:thread? [unimplemented]");
  //printf("epsilon_primitive_thread_p(): WARNING: unimplemented: this always returns false\n");
  stack[0] = epsilon_bool_to_epsilon_value(false); // FIXME: unimplemented: this always returns false
}
static void epsilon_primitive_buffer_length(epsilon_value *stack){
  stack[0] = epsilon_int_to_epsilon_value(epsilon_buffer_size(stack[0]));
}

static void epsilon_primitive_io_open_file(epsilon_value *stack){
  epsilon_value file_name = stack[0];
  epsilon_value mode = stack[1];
  char *file_name_as_char_star = epsilon_string_to_malloced_char_star(file_name);
  epsilon_int mode_as_epsilon_int = epsilon_value_to_epsilon_int(mode);
  char *mode_as_char_star;
  switch(mode_as_epsilon_int){
  case 0:
    mode_as_char_star = "r"; break;
  case 1:
    mode_as_char_star = "w"; break;
  default:
    //value_out_of_range("io:open-file", mode);
    epsilon_runtime_appropriate_fail("io:open-file");
  } // switch
  FILE* result_as_file_star = fopen(file_name_as_char_star, mode_as_char_star);
  free(file_name_as_char_star); // free in any case, success or error
  epsilon_value result = epsilon_foreign_pointer_to_epsilon_value(result_as_file_star);
  /* FILE *reread_file_star = epsilon_value_to_foreign_pointer(result); */
  /* printf("#### %p should be equal to %p\n", result_as_file_star, reread_file_star); */
  stack[0] = result;
}
static void epsilon_primitive_io_close_file(epsilon_value *stack){
  FILE *file_star = epsilon_value_to_foreign_pointer(stack[0]);
  fclose(file_star);
}
static void epsilon_primitive_io_eof_p(epsilon_value *stack){
  FILE *file_star = EPSILON_EPSILON_INT_TO_EPSILON_WORD(epsilon_value_to_epsilon_int(stack[0]));
  /* Read a character (and then unread it), just to have feof return
     the result we want: */
  //int c = getc(file_star);
  int result_as_int = feof(file_star);
  //ungetc(c, file_star);
  stack[0] = epsilon_bool_to_epsilon_value(result_as_int);
}
static void epsilon_primitive_io_read_character(epsilon_value *stack){
  FILE *file_descriptor_as_file_star = epsilon_value_to_foreign_pointer(stack[0]);
  /* FIXME: I can't see how to use libunistring for reading Unicode
     characters from input, or even for how to specify an encoding.
     For the time being, I'll assume a char is a character, in the
     input direction. */
  epsilon_int result_as_epsilon_int = getc(file_descriptor_as_file_star);
  //  EPSILON_IF_UNLIKELY(result_as_epsilon_int == EOF)
  //    result_as_epsilon_int = 0; // we return 0 as the EOF marker
  stack[0] = epsilon_int_to_epsilon_value(result_as_epsilon_int);
}
static void epsilon_primitive_io_readline(epsilon_value *stack){
  FILE* old_readline_input_stream = rl_instream;
  FILE* old_readline_output_stream = rl_outstream;
  rl_instream = stdin;
  rl_outstream = stdout;
  char* c_string = readline("> ");
  rl_instream = old_readline_input_stream;
  rl_outstream = old_readline_output_stream;
  if (c_string == NULL){
    stack[0] = epsilon_int_to_epsilon_value(0);
    return;
  }
  add_history(c_string);
  int result = write_history(NULL);
  if (result != 0)
    fprintf(stderr, "Warning: writing history failed\n");
  size_t nul_offset = strlen(c_string), length;
  if (nul_offset > 0)
    add_history(c_string);
  uint32_t *wide_string;
#ifdef HAVE_LIBUNISTRING
  if (nul_offset == 0)
    {
      wide_string = NULL;
      length = 0;
    }
  else
    {
      if (NULL == (wide_string
                   = u32_conv_from_encoding (epsilon_locale_charset,
                                             iconveh_error,
                                             c_string,
                                             nul_offset,
                                             NULL,
                                             NULL,
                                             &length)))
        switch (errno)
          {
          case EILSEQ:
            epsilon_runtime_appropriate_fail("converstion to Unicode string failed");
          case EINVAL:
            epsilon_runtime_appropriate_fail("invalid value during Unicode conversion");
          case ENOMEM:
            epsilon_runtime_appropriate_fail("out-of-memory during Unicode conversion");
          default:
            epsilon_runtime_appropriate_fail("unknown failure during Unicode conversion");
          }
    }
#else
  length = nul_offset;
  wide_string = epsilon_xmalloc(sizeof(uint32_t) * (nul_offset + 1));
  int j;
  for (j = 0; j < nul_offset; j ++)
    wide_string[j] = c_string[j];
#endif // #ifdef HAVE_LIBUNISTRING
  free(c_string);
  epsilon_word buffer =
    epsilon_gc_allocate_with_epsilon_int_length(length + 2);
  int i;
  epsilon_store_with_epsilon_int_offset(buffer, 0,
                                        epsilon_int_to_epsilon_value(length + 1));
  for (i = 0; i < length; i ++)
    epsilon_store_with_epsilon_int_offset(buffer,
                                          i + 1,
                                          epsilon_int_to_epsilon_value(wide_string[i]));
  epsilon_store_with_epsilon_int_offset(buffer, length + 1,
                                        epsilon_int_to_epsilon_value('\n'));
  free(wide_string);
  stack[0] = buffer;
}
static void epsilon_primitive_io_write_character(epsilon_value *stack){
  FILE *file_star = epsilon_value_to_foreign_pointer(stack[0]);
  epsilon_int character_as_epsilon_int = epsilon_value_to_epsilon_int(stack[1]);

//#undef HAVE_LIBUNISTRING
#ifdef HAVE_LIBUNISTRING
  uint32_t unicode_one_character_string[2];
  unicode_one_character_string[0] = (uint32_t)character_as_epsilon_int;
  unicode_one_character_string[1] = '\0';
  ulc_fprintf(file_star, "%llU", unicode_one_character_string);
#else
  /* Naively use one byte per character: */
  if(putc(character_as_epsilon_int, file_star) == EOF)
    epsilon_runtime_appropriate_fail("io:write-character");
#endif // #ifdef HAVE_LIBUNISTRING
}
static void epsilon_primitive_io_write_32bit_bigendian(epsilon_value *stack){
  FILE *file_star = epsilon_value_to_foreign_pointer(stack[0]);
  int32_t x = epsilon_value_to_epsilon_int(stack[1]);
  write_32bit_bigendian(file_star, x);
}
static void epsilon_primitive_io_read_32bit_bigendian(epsilon_value *stack){
  FILE *file_star = epsilon_value_to_foreign_pointer(stack[0]);
  stack[0] = epsilon_int_to_epsilon_value(read_32bit_bigendian(file_star));
}
static void epsilon_primitive_io_load_byte(epsilon_value *stack){
  epsilon_runtime_appropriate_fail("io:load-byte [unimplemented]");
}
static void epsilon_primitive_io_store_byte(epsilon_value *stack){
  epsilon_runtime_appropriate_fail("io:store-byte! [unimplemented]");
}
static void epsilon_primitive_io_standard_input(epsilon_value *stack){
  stack[0] = epsilon_foreign_pointer_to_epsilon_value(stdin);
}
static void epsilon_primitive_io_standard_output(epsilon_value *stack){
  /* printf("stdout, untagged: %p\n", stdout); */
  /* printf("stdout, tagged:   %p\n", epsilon_int_to_epsilon_value(EPSILON_WORD_TO_EPSILON_INT(stdout))); */
  /* printf("stdout, detagged: %p\n", (void*)epsilon_value_to_epsilon_int(epsilon_int_to_epsilon_value(EPSILON_WORD_TO_EPSILON_INT(stdout)))); */
  stack[0] = epsilon_foreign_pointer_to_epsilon_value(stdout);
}
static void epsilon_primitive_io_standard_error(epsilon_value *stack){
  stack[0] = epsilon_foreign_pointer_to_epsilon_value(stderr);
}

static void epsilon_primitive_io_read_sexpression(epsilon_value *stack){
  /* epsilon_value guile_sexpression = value_read(value_current_input_port()); */
  /* epsilon_value result_value = value_call_1(value_c_eval_string("guile-sexpression->sexpression"), */
  /*                              guile_sexpression); */
  /* stack[0] = result_value; */
  epsilon_runtime_appropriate_fail("io:read-sexpression [unimplemented]");
}

static void epsilon_primitive_primitive_get_index(epsilon_value *stack){
  epsilon_value name_as_value_string = stack[0];
  char *c_string = epsilon_string_to_malloced_char_star(name_as_value_string);
  epsilon_int index = epsilon_lookup_c_primitive_index(c_string);
  free(c_string);
  *stack = epsilon_int_to_epsilon_value(index);
}

static void epsilon_primitive_primitive_call_in_c(epsilon_value *stack){
  epsilon_value primitive_name_as_value_string = stack[0];
  epsilon_value primitive_actuals_as_value_list = stack[1];
  
  /* Lookup the primitive: */
  char* primitive_name = epsilon_string_to_malloced_char_star(primitive_name_as_value_string);
  struct epsilon_c_primitive_descriptor *descriptor = epsilon_lookup_c_primitive_descriptor(primitive_name);
  free(primitive_name);
  epsilon_int in_dimension = descriptor->in_dimension;

  //printf("Calling >%s<: copying the %i parameters\n", primitive_name, (int)in_dimension);
  
  /* Copy parameters onto the stack: */
  epsilon_value primitive_parameter_result_stack[EPSILON_MAXIMUM_INOUT_DIMENSION];
  epsilon_int actual_no = 0;
  epsilon_value p;
  for(p = primitive_actuals_as_value_list; ! epsilon_value_is_null(p); p = epsilon_value_cdr(p))
    primitive_parameter_result_stack[actual_no ++] = epsilon_value_car(p);
  if(actual_no != in_dimension)
    epsilon_runtime_appropriate_fail("primitive:call-in-c [in-dimension mismatch]");
  
  /* Call the primitive: */
  epsilon_c_primitive_function function_pointer = descriptor->function_pointer;
  function_pointer(primitive_parameter_result_stack);
  
  /* Make the result list: */
  epsilon_int out_dimension = descriptor->out_dimension;
  epsilon_value result = epsilon_int_to_epsilon_value(0); // empty list
  epsilon_int i = 0;
  for(i = out_dimension - 1; i >= 0; i --)
    result = epsilon_value_cons(primitive_parameter_result_stack[i], result);
  //printf("There are >%i< results\n", (int)out_dimension);
  
  /* Return the list as the only result, via the stack: */
  stack[0] = result;
}

static void epsilon_primitive_e0_eval(epsilon_value *stack){
  epsilon_value expression = stack[0];
  epsilon_value local_environment = stack[1];

  stack[0] = epsilon_e0_eval_making_stacks(expression, local_environment);
}

static void epsilon_primitive_marshal_marshal_to_open_file(epsilon_value *stack){
  /* We have an open file we want to write to, and the object to dump: */
  FILE *file_star = epsilon_value_to_foreign_pointer(stack[0]);
  epsilon_value object = stack[1];
  epsilon_marshal_value(object, file_star);
}
static void epsilon_primitive_marshal_unmarshal_from_open_file(epsilon_value *stack){
  FILE *file_star = epsilon_value_to_foreign_pointer(stack[0]);
  stack[0] = epsilon_unmarshal_value(file_star);
}
static void epsilon_update_all_globals(epsilon_value global_name_value_list){
  while(epsilon_value_to_epsilon_int(global_name_value_list) != 0){
    /* Extract data for the first global: */
    epsilon_value first_cons = epsilon_load_with_epsilon_int_offset(global_name_value_list, 0);
    epsilon_value global_name = epsilon_load_with_epsilon_int_offset(first_cons, 0);
    epsilon_value global = epsilon_load_with_epsilon_int_offset(first_cons, 1);
    
    /* Update the relevant symbol field: */
    epsilon_store_with_epsilon_int_offset(global_name, 2, global);
    epsilon_store_with_epsilon_int_offset(global_name, 1, epsilon_int_to_epsilon_value(1)); // globally bound
    
    /* Go on with the next element: */
    global_name_value_list = epsilon_value_cdr(global_name_value_list);
  } // while
}
static void epsilon_update_all_procedures(epsilon_value name_formals_body_list){
  while(epsilon_value_to_epsilon_int(name_formals_body_list) != 0){
    /* Extract data for the first procedure: */
    epsilon_value first_triple = epsilon_load_with_epsilon_int_offset(name_formals_body_list, 0);
    epsilon_value procedure_name = epsilon_load_with_epsilon_int_offset(first_triple, 0);
    epsilon_value formals = epsilon_load_with_epsilon_int_offset(first_triple, 1);
    epsilon_value body = epsilon_load_with_epsilon_int_offset(first_triple, 2);
    
    /* Update the relevant symbol fields: */
    epsilon_store_with_epsilon_int_offset(procedure_name, 3, formals);
    epsilon_store_with_epsilon_int_offset(procedure_name, 4, body);
    
    /* Go on with the next element: */
    name_formals_body_list = epsilon_value_cdr(name_formals_body_list);
  } // while
}
static void epsilon_primitive_state_update_globals_and_procedures(epsilon_value *stack){
  /* Notice that we don't ever use eval here; it would be dangerous,
     because we would be in the middle of a global transformation. */
  
  epsilon_value global_name_value_list = stack[0];
  epsilon_update_all_globals(global_name_value_list);
  
  epsilon_value procedure_name_formals_body_list = stack[1];
  epsilon_update_all_procedures(procedure_name_formals_body_list);
}
static void epsilon_primitive_unix_system(epsilon_value *stack){
  epsilon_value epsilon_string = stack[0];
  char *char_star = epsilon_string_to_malloced_char_star(epsilon_string);
  int result = system(char_star);
  free(char_star);
  stack[0] = epsilon_int_to_epsilon_value(result);
}
static void epsilon_primitive_unix_unlink(epsilon_value *stack){
  epsilon_value epsilon_string = stack[0];
  char *char_star = epsilon_string_to_malloced_char_star(epsilon_string);
  int result = unlink(char_star);
  free(char_star);
  stack[0] = epsilon_int_to_epsilon_value(result);
}

static void epsilon_primitive_io_write_value(epsilon_value *stack){
  epsilon_value file = epsilon_value_to_foreign_pointer(stack[0]);
  epsilon_value value = stack[1];
#ifdef EPSILON_RUNTIME_UNTAGGED
  fprintf(file, "%li", (long)epsilon_value_to_epsilon_int(value));
#else
  epsilon_dump_value(value, file);
#endif // #ifdef EPSILON_RUNTIME_UNTAGGED
}

///////////////////////////////////////////////////////////////////
// Primitive definitions: end

void epsilon_c_primitives_initialize(void){
// FIXME: move away into a separate initialization function.
#ifdef HAVE_LIBUNISTRING
  /* Set the current locale according to environment variables.  This
     is apparently required to have locale_charset return the right
     charset. */
  setlocale(LC_ALL, "");
  epsilon_locale_charset = locale_charset();
#ifdef ENABLE_VERBOSE_DEBUG
  fprintf(stderr, "[The locale charset as a pointer is %p]\n", epsilon_locale_charset);
  fprintf(stderr, "[its string length is %i]\n", (int)strlen(epsilon_locale_charset));
  fprintf(stderr, "[The locale charset is %s]\n", epsilon_locale_charset);
#endif // #ifdef ENABLE_VERBOSE_DEBUG
#endif // #ifdef HAVE_LIBUNISTRING

  int result = read_history(NULL);
  if (result != 0)
    fprintf(stderr, "Warning: reading history failed\n");

  epsilon_initialize_string_hash(& epsilon_c_primitive_hash);

  /* Now initialize primitives one by one: */
  epsilon_initialize_c_primitive("whatever:zero?", epsilon_primitive_zero_p, 1, 1);
  epsilon_initialize_c_primitive("whatever:eq?", epsilon_primitive_whatever_eq_p, 2, 1);
  epsilon_initialize_c_primitive("fixnum:1-", epsilon_primitive_predecessor, 1, 1);
  epsilon_initialize_c_primitive("fixnum:1+", epsilon_primitive_successor, 1, 1);

  epsilon_initialize_c_primitive("buffer:make", epsilon_primitive_buffer_make, 1, 1);
  epsilon_initialize_c_primitive("buffer:make-uninitialized", epsilon_primitive_buffer_make_uninitialized, 1, 1);
  epsilon_initialize_c_primitive("buffer:destroy", epsilon_primitive_buffer_destroy, 1, 0);
  epsilon_initialize_c_primitive("buffer:get", epsilon_primitive_buffer_get, 2, 1);
  epsilon_initialize_c_primitive("buffer:set!", epsilon_primitive_buffer_set, 3, 0);
  epsilon_initialize_c_primitive("buffer:initialize!", epsilon_primitive_buffer_set, 3, 0);

  epsilon_initialize_c_primitive("fixnum:<", epsilon_primitive_less_than_p, 2, 1);
  epsilon_initialize_c_primitive("fixnum:<=", epsilon_primitive_less_than_or_equal_p, 2, 1);
  epsilon_initialize_c_primitive("fixnum:negate", epsilon_primitive_negate, 1, 1);
  epsilon_initialize_c_primitive("fixnum:+", epsilon_primitive_plus, 2, 1);
  epsilon_initialize_c_primitive("fixnum:-", epsilon_primitive_minus, 2, 1);
  epsilon_initialize_c_primitive("fixnum:*", epsilon_primitive_times, 2, 1);
  epsilon_initialize_c_primitive("fixnum:/", epsilon_primitive_divided, 2, 1);
  epsilon_initialize_c_primitive("fixnum:%", epsilon_primitive_modulo, 2, 1);
  epsilon_initialize_c_primitive("fixnum:/%", epsilon_primitive_divided_and_modulo, 2, 2);

  epsilon_initialize_c_primitive("fixnum:bitwise-not", epsilon_primitive_bitwise_not, 1, 1);
  epsilon_initialize_c_primitive("fixnum:bitwise-and", epsilon_primitive_bitwise_and, 2, 1);
  epsilon_initialize_c_primitive("fixnum:bitwise-or", epsilon_primitive_bitwise_or, 2, 1);
  epsilon_initialize_c_primitive("fixnum:bitwise-xor", epsilon_primitive_bitwise_xor, 2, 1);
  epsilon_initialize_c_primitive("fixnum:left-shift", epsilon_primitive_left_shift, 2, 1);
  epsilon_initialize_c_primitive("fixnum:arithmetic-right-shift", epsilon_primitive_arithmetic_right_shift, 2, 1);
  epsilon_initialize_c_primitive("fixnum:logic-right-shift", epsilon_primitive_logic_right_shift, 2, 1);
  epsilon_initialize_c_primitive("fixnum:left-shift-1-bit", epsilon_primitive_left_shift_1_bit, 1, 1);
  epsilon_initialize_c_primitive("fixnum:arithmetic-right-shift-1-bit", epsilon_primitive_arithmetic_right_shift_1_bit, 1, 1);
  epsilon_initialize_c_primitive("fixnum:logic-right-shift-1-bit", epsilon_primitive_logic_right_shift_1_bit, 1, 1);

  epsilon_initialize_c_primitive("gc:disable!", epsilon_primitive_gc_disable, 0, 0);
  epsilon_initialize_c_primitive("gc:reenable!", epsilon_primitive_gc_reenable, 0, 0);

  epsilon_initialize_c_primitive("whatever:atom?", epsilon_primitive_atom_p, 1, 1);
  epsilon_initialize_c_primitive("whatever:buffer?", epsilon_primitive_buffer_p, 1, 1);
  epsilon_initialize_c_primitive("whatever:thread?", epsilon_primitive_thread_p, 1, 1);
  epsilon_initialize_c_primitive("buffer:length", epsilon_primitive_buffer_length, 1, 1);

  epsilon_initialize_c_primitive("io:load-byte", epsilon_primitive_io_load_byte, 1, 1);
  epsilon_initialize_c_primitive("io:store-byte!", epsilon_primitive_io_store_byte, 2, 0);

  epsilon_initialize_c_primitive("io:standard-input", epsilon_primitive_io_standard_input, 0, 1);
  epsilon_initialize_c_primitive("io:standard-output", epsilon_primitive_io_standard_output, 0, 1);
  epsilon_initialize_c_primitive("io:standard-error", epsilon_primitive_io_standard_error, 0, 1);
  epsilon_initialize_c_primitive("io:open-file", epsilon_primitive_io_open_file, 2, 1);
  epsilon_initialize_c_primitive("io:close-file", epsilon_primitive_io_close_file, 1, 0);
  epsilon_initialize_c_primitive("io:eof?", epsilon_primitive_io_eof_p, 1, 0);
  epsilon_initialize_c_primitive("io:read-character", epsilon_primitive_io_read_character, 1, 1);
  epsilon_initialize_c_primitive("io:readline", epsilon_primitive_io_readline, 0, 1);
  epsilon_initialize_c_primitive("io:write-character", epsilon_primitive_io_write_character, 2, 0);
  epsilon_initialize_c_primitive("io:read-32-bit-big-endian", epsilon_primitive_io_read_32bit_bigendian, 1, 1);
  epsilon_initialize_c_primitive("io:write-32-bit-big-endian", epsilon_primitive_io_write_32bit_bigendian, 2, 0);

  epsilon_initialize_c_primitive("io:read-sexpression", epsilon_primitive_io_read_sexpression, 1, 1);

  epsilon_initialize_c_primitive("primitive:get-index", epsilon_primitive_primitive_get_index, 1, 1);
  epsilon_initialize_c_primitive("primitive:call-in-c", epsilon_primitive_primitive_call_in_c, 2, 1);

  epsilon_initialize_c_primitive("debug:dump", epsilon_primitive_debug_dump, 1, 0);
  epsilon_initialize_c_primitive("debug:fail", epsilon_primitive_debug_fail, 0, 0);

  epsilon_initialize_c_primitive("whatever:duplicate", epsilon_primitive_duplicate, 1, 2);
  epsilon_initialize_c_primitive("whatever:swap", epsilon_primitive_swap, 2, 2);

  epsilon_initialize_c_primitive("whatever:make-zero", epsilon_primitive_make_zero, 0, 1);

  epsilon_initialize_c_primitive("marshal:marshal-to-open-file", epsilon_primitive_marshal_marshal_to_open_file, 2, 0);
  epsilon_initialize_c_primitive("marshal:unmarshal-from-open-file", epsilon_primitive_marshal_unmarshal_from_open_file, 1, 1);
  epsilon_initialize_c_primitive("state:update-globals-and-procedures!", epsilon_primitive_state_update_globals_and_procedures, 2, 0);
  epsilon_initialize_c_primitive("e0:eval-in-c", epsilon_primitive_e0_eval, 2, 1);
  epsilon_initialize_c_primitive("unix:system", epsilon_primitive_unix_system, 1, 1);
  epsilon_initialize_c_primitive("unix:unlink", epsilon_primitive_unix_unlink, 1, 1);

  epsilon_initialize_c_primitive("io:write-value", epsilon_primitive_io_write_value, 2, 0); // FIXME: remove after bootstrapping from Guile
}
