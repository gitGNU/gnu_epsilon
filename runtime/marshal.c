/* Textual dumping and binary marshalling and unmarshalling.

   Copyright (C) 2012 Université Paris 13
   Copyright (C) 2012 Luca Saiu
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


#include <stdio.h>
#include <stdbool.h>
#include <netinet/in.h> // for endianness conversion
#include "marshal.h"
#include "../utility/utility.h"

#define GREEN   "\033[0m\033[32m"
#define RED     "\033[0m\033[31m"
//#define YELLOW  "\033[1m\033[33m"
#define BROWN   "\033[0m\033[33m"
//#define PURPLE  "\033[0m\033[35m"
#define NOTHING "\033[0m"

epsilon_int epsilon_dump_maximum_depth = 2;

/* We use a stack to check whether we already saw some pointer: every
   new pointer is pushed to the stack once. */
void epsilon_dump_value_recursive(epsilon_value value, FILE *file,
                                   epsilon_stack_t stack, long nesting_level){
  if(epsilon_is_fixnum(value)){
    fprintf(file, "%s%li%s", GREEN, (long)epsilon_value_to_epsilon_int(value), NOTHING);
  }
  else EPSILON_IF_LIKELY(epsilon_is_pointer(value)){
    epsilon_value *elements = epsilon_value_to_value_elements(value);
    bool is_pointer_known = epsilon_stack_has(stack, elements);
    if(is_pointer_known)
      fprintf(file, "%s", BROWN);
    else
      fprintf(file, "%s", RED);
    fprintf(file, "0x%lx", (long)(elements));
    if(! is_pointer_known){
      epsilon_stack_push(stack, elements); // from now on we consider it known
      fprintf(file, "[");
      if((epsilon_dump_maximum_depth == -1) || // unlimited depth, by convention
         ((nesting_level + 1) <= epsilon_dump_maximum_depth)){
        epsilon_int element_no = epsilon_buffer_size(value);
        int i;
        for(i = 0; i < element_no; i ++){
          epsilon_dump_value_recursive(elements[i], file, stack, nesting_level + 1);
          if(i != (element_no - 1))
            fprintf(file, " ");
        }
        fprintf(file, "%s]", RED);
      }
      else // pointer not seen before, but too deep to show
        fprintf(file, "...]");
    } // outer if
    fprintf(file, "%s", NOTHING);
  }
  else
    epsilon_fatal("epsilon_dump_value_recursive: non-fixnum non-pointer?");
  /* /\* Old thread case *\/ */
  /* char string[50]; */
  /* sprintf(string, "%s%p%s", PURPLE, s->thread, NOTHING); */
  /* scm_puts(string, port); */
}

void epsilon_dump_value(epsilon_value value, FILE *file){
  epsilon_stack_t stack = epsilon_stack_make();
  epsilon_dump_value_recursive(value, file, stack, 0);
  epsilon_stack_destroy(stack);
}


void epsilon_marshal_value(epsilon_value object, FILE *file_star){
  /* As an auxiliary data structure, we need a table mapping pointers
     (wrapped into epsilon_value smobs) into unique identifiers, and a stack of
     pointers to be treated.  In this case the pointers in the stack
     don't need to be GC-protected, as they are all already reachable
     from the object we received as well. */
  struct epsilon_unboxed_hash pointer_to_identifier;
  epsilon_initialize_unboxed_hash(&pointer_to_identifier);
  epsilon_stack_t pointers_to_be_dumped = epsilon_stack_make();
  if(epsilon_is_pointer(object)){
    epsilon_stack_push(pointers_to_be_dumped, object);
    //printf("PUSHING THE FIRST POINTER: %p\n", smob_to_whatever(object));
  }
  
  /* Fill the table with all reachable pointers, allocating
     identifiers sequentially.  At the same time, also build an array
     with a single instance of all reachable pointers, which we can
     sequentially scan, in the same order as the identifiers. */
  epsilon_stack_t reachable_pointers = epsilon_stack_make();
  while(! epsilon_stack_empty(pointers_to_be_dumped)){
    epsilon_value pointer = (epsilon_value)epsilon_stack_pop(pointers_to_be_dumped);
    if(epsilon_lookup_unboxed_hash(&pointer_to_identifier,
                                   pointer) == NULL){
      //printf("POPPING %p\n", smob_to_whatever(pointer)); //
      /* The pointer is new: add it to the array and to the table; its
         identifier will be the number of current bindings, which is a
         way of having sequentially-allocated identifiers starting
         from zero. */
      epsilon_stack_push(reachable_pointers, pointer);
      epsilon_add_to_unboxed_hash(&pointer_to_identifier,
                                  pointer,
                                  (epsilon_word)(epsilon_int)(pointer_to_identifier.binding_no));
      /* All the buffer elements are also reachable; as an
         optimization, ignore non-pointers and pointers which we have
         already added to the table. */
      size_t buffer_length = epsilon_buffer_size(pointer);
      epsilon_value *elements = epsilon_value_to_value_elements(pointer);
      int i;
      for(i = 0; i < buffer_length; i ++){
        if((epsilon_is_pointer(elements[i])) &&
           (epsilon_lookup_unboxed_hash(&pointer_to_identifier, elements[i]) == NULL)){
          //printf("PUSHING THE REACHED POINTER %p\n", smob_to_whatever(elements[i]));
          epsilon_stack_push(pointers_to_be_dumped, elements[i]);
        } // inner if
      } // for
    } // if
  } // while
  size_t buffer_no = pointer_to_identifier.binding_no;
  
  /* Dump buffer definitions to the file, preceded by their number: */
  write_32bit_bigendian(file_star, buffer_no);
  int buffer_index;
  for(buffer_index = 0; buffer_index <= buffer_no; buffer_index ++){
    /* Dump the i-th buffer definition: first the element no, then
       each element preceded by a one-word tag: 0 for an atom, 1 for a
       pointer.  Just to factorize a little, we also consider the main
       element as the sole element of a dummy buffer, but in that case
       we do not write the dummy buffer dimension. */
    size_t element_no;
    epsilon_value *elements;
    if(buffer_index != buffer_no){
      epsilon_value buffer = reachable_pointers->buffer[buffer_index];
      //struct whatever *struct_pointer = smob_to_struct_pointer(buffer);
      element_no = epsilon_buffer_size(buffer);
      elements = epsilon_value_to_value_elements(buffer);
      write_32bit_bigendian(file_star, element_no);
    }
    else{
      element_no = 1;
      elements = &object;
    } // else
    
    int element_index;
    for(element_index = 0; element_index < element_no; element_index ++){
      if(epsilon_is_fixnum(elements[element_index])){
        write_32bit_bigendian(file_star, 0);
        write_32bit_bigendian(file_star,
                              epsilon_value_to_epsilon_int(elements[element_index]));
      }
      else EPSILON_IF_LIKELY(epsilon_is_pointer(elements[element_index])){
        write_32bit_bigendian(file_star, 1);
        write_32bit_bigendian(file_star,
                              (epsilon_int)epsilon_lookup_unboxed_hash(&pointer_to_identifier,
                                                                elements[element_index]));
      }
      else
        /* value_error(value_from_locale_symbol("misc-error"), */
        /*           "unexec:dump-to-open-file", */
        /*           "not a pointer nor an atom", */
        /*           epsilon_int_to_epsilon_value(0), */
        /*           epsilon_int_to_epsilon_value(0)); */
        epsilon_runtime_appropriate_fail("epsilon_marshal_value [non-pointer non-fixnum]");
    } // inner for
  } // outer for
  
  /* Destroy temporary structures and we're done.  There are no
     resuts. */
  epsilon_finalize_unboxed_hash(&pointer_to_identifier);
  epsilon_stack_destroy(reachable_pointers);
  epsilon_stack_destroy(pointers_to_be_dumped);
}

epsilon_value epsilon_unmarshal_value(FILE *file_star){
  /* Read the buffer element no; this is enough to dimension several
     structures: */
  size_t nontrivial_buffer_no =
    (size_t)read_32bit_bigendian(file_star);
  
  /* We reserve a dummy one-element buffer for the main element.  This
     saves some code. */
  size_t buffer_no = nontrivial_buffer_no + 1;
  size_t *size_array = epsilon_xmalloc(sizeof(size_t) * buffer_no);;
  bool **tag_array_array = epsilon_xmalloc(sizeof(bool*) * buffer_no);;
  int32_t **content_array_array = epsilon_xmalloc(sizeof(int32_t*) * buffer_no);;
  
  /* Read buffers from file: */
  int buffer_index;
  for(buffer_index = 0; buffer_index < nontrivial_buffer_no; buffer_index ++){
    size_t element_no = (size_t)read_32bit_bigendian(file_star);
    size_array[buffer_index] = element_no;
    bool *tag_array = (bool*)epsilon_xmalloc(sizeof(bool) * element_no);
    int32_t *content_array = (int32_t*)epsilon_xmalloc(sizeof(int32_t) * element_no);
    tag_array_array[buffer_index] = tag_array;
    content_array_array[buffer_index] = content_array;
    int element_index;
    for(element_index = 0; element_index < element_no; element_index ++){
      tag_array[element_index] = (bool)read_32bit_bigendian(file_star);
      content_array[element_index] = read_32bit_bigendian(file_star);
    } // inner for
  } // outer for
  
  /* Read the last element: */
  size_array[buffer_no - 1] = 1;
  tag_array_array[buffer_no - 1] = epsilon_xmalloc(sizeof(bool));
  content_array_array[buffer_no - 1] = epsilon_xmalloc(sizeof(int32_t));
  tag_array_array[buffer_no - 1][0] = (bool)
    read_32bit_bigendian(file_star);
  content_array_array[buffer_no - 1][0] =
    read_32bit_bigendian(file_star);
  
  /* Make smob buffers, and fill them.  Notice that buffers is not a
     manually freeable C array; this is for GC-protecting its
     elements. */
  epsilon_value buffers = epsilon_gc_allocate_with_epsilon_int_length(buffer_no);
  for(buffer_index = 0; buffer_index < buffer_no; buffer_index ++)
    epsilon_store_with_epsilon_int_offset(buffers,
                                          buffer_index,
                                          epsilon_gc_allocate_with_epsilon_int_length(size_array[buffer_index]));
  for(buffer_index = 0; buffer_index < buffer_no; buffer_index ++){
    size_t element_no = size_array[buffer_index];
    bool *tag_array = tag_array_array[buffer_index];
    int32_t *content_array = content_array_array[buffer_index];
    epsilon_value buffer = epsilon_load_with_epsilon_int_offset(buffers, buffer_index);
    int element_index;
    for(element_index = 0; element_index < element_no; element_index ++)
      if(tag_array[element_index] == 0) // fixnum
        epsilon_store_with_epsilon_int_offset(buffer,
                                              element_index,
                                              epsilon_int_to_epsilon_value((epsilon_int)(content_array[element_index])));
      else // pointer
        epsilon_store_with_epsilon_int_offset(buffer,
                                              element_index,
                                              epsilon_load_with_epsilon_int_offset(buffers, content_array[element_index]));
  } // outer for
  
  /* Destroy manually-allocated arrays: */
  for(buffer_index = 0; buffer_index < buffer_no; buffer_index ++){
    free(tag_array_array[buffer_index]);
    free(content_array_array[buffer_index]);
  } // for
  free(tag_array_array);
  free(content_array_array);
  free(size_array);

  /* The result is the only element of the last buffer: */
  epsilon_value result = epsilon_load_with_epsilon_int_offset(epsilon_load_with_epsilon_int_offset(buffers, buffer_no - 1), 0);
  epsilon_gc_destroy(buffers);
  return result;
}

void write_32bit_bigendian(FILE *f, int32_t value){
  value = (int32_t)htonl(value);
  int fwrite_return_value = fwrite(&value, sizeof(int32_t), 1, f);
  if(fwrite_return_value != 1)
    /* value_error(value_from_locale_symbol("misc-error"), */
    /*           scheme_procedure_name, */
    /*           "fwrite didn't return 1", */
    /*           epsilon_int_to_epsilon_value(0), */
    /*           epsilon_int_to_epsilon_value(0)); */
    epsilon_runtime_appropriate_fail("io:write-32-bit-big-endian");
}

int32_t read_32bit_bigendian(FILE *f){
  int32_t result_as_int32_t;
  int fread_return_value = fread(&result_as_int32_t, sizeof(int32_t), 1, f);
  if(fread_return_value == 1)
    return (int32_t)ntohl(result_as_int32_t);
  else
    /* value_error(value_from_locale_symbol("misc-error"), */
    /*           scheme_procedure_name, */
    /*           "fread didn't return 1", */
    /*           epsilon_int_to_epsilon_value(0), */
    /*           epsilon_int_to_epsilon_value(0)); */
    epsilon_runtime_appropriate_fail("io:read-32-bit-big-endian");
}
