/* SVM primitive implementation.

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


#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <string.h>
#include "svm.h"
#include "frontend.h"
#include "parser.h"
#include "registers.h"
#include "interpreter.h"
#include "../utility/utility.h"
#include "../runtime/runtime.h" // we need to enter/exit epsilon contexts

epsilon_word epsilon_primitive_zero_p(epsilon_word i_){
  epsilon_int i = EPSILON_WORD_TO_EPSILON_INT(i_);
  return EPSILON_EPSILON_INT_TO_EPSILON_WORD(i == 0);
}
epsilon_word epsilon_primitive_eq_p(epsilon_word a, epsilon_word b){
  return EPSILON_EPSILON_INT_TO_EPSILON_WORD(a == b);
}
epsilon_word epsilon_primitive_gt_p(epsilon_word a_, epsilon_word b_){
  epsilon_int a = EPSILON_WORD_TO_EPSILON_INT(a_);
  epsilon_int b = EPSILON_WORD_TO_EPSILON_INT(b_);
  return EPSILON_EPSILON_INT_TO_EPSILON_WORD(a > b);
}
epsilon_word epsilon_primitive_lt_p(epsilon_word a_, epsilon_word b_){
  epsilon_int a = EPSILON_WORD_TO_EPSILON_INT(a_);
  epsilon_int b = EPSILON_WORD_TO_EPSILON_INT(b_);
  return EPSILON_EPSILON_INT_TO_EPSILON_WORD(a < b);
}
epsilon_word epsilon_primitive_gte_p(epsilon_word a_, epsilon_word b_){
  epsilon_int a = EPSILON_WORD_TO_EPSILON_INT(a_);
  epsilon_int b = EPSILON_WORD_TO_EPSILON_INT(b_);
  return EPSILON_EPSILON_INT_TO_EPSILON_WORD(a >= b);
}
epsilon_word epsilon_primitive_lte_p(epsilon_word a_, epsilon_word b_){
  epsilon_int a = EPSILON_WORD_TO_EPSILON_INT(a_);
  epsilon_int b = EPSILON_WORD_TO_EPSILON_INT(b_);
  return EPSILON_EPSILON_INT_TO_EPSILON_WORD(a <= b);
}

epsilon_word epsilon_primitive_plus_1(epsilon_word a_){
  epsilon_int a = EPSILON_WORD_TO_EPSILON_INT(a_);
  return EPSILON_EPSILON_INT_TO_EPSILON_WORD(a + 1);
}
epsilon_word epsilon_primitive_minus_1(epsilon_word a_){
  epsilon_int a = EPSILON_WORD_TO_EPSILON_INT(a_);
  return EPSILON_EPSILON_INT_TO_EPSILON_WORD(a - 1);
}

epsilon_word epsilon_primitive_bitwise_not(epsilon_word bitmask_){
  epsilon_unsigned bitmask = EPSILON_WORD_TO_EPSILON_UNSIGNED(bitmask_);
  return EPSILON_EPSILON_UNSIGNED_TO_EPSILON_WORD(~bitmask);
}
epsilon_word epsilon_primitive_bitwise_and(epsilon_word bitmask_1_, epsilon_word bitmask_2_){
  epsilon_unsigned bitmask_1 = EPSILON_WORD_TO_EPSILON_UNSIGNED(bitmask_1_);
  epsilon_unsigned bitmask_2 = EPSILON_WORD_TO_EPSILON_UNSIGNED(bitmask_2_);
  return EPSILON_EPSILON_UNSIGNED_TO_EPSILON_WORD(bitmask_1 & bitmask_2);
}
epsilon_word epsilon_primitive_bitwise_or(epsilon_word bitmask_1_, epsilon_word bitmask_2_){
  epsilon_unsigned bitmask_1 = EPSILON_WORD_TO_EPSILON_UNSIGNED(bitmask_1_);
  epsilon_unsigned bitmask_2 = EPSILON_WORD_TO_EPSILON_UNSIGNED(bitmask_2_);
  return EPSILON_EPSILON_UNSIGNED_TO_EPSILON_WORD(bitmask_1 | bitmask_2);
}
epsilon_word epsilon_primitive_bitwise_xor(epsilon_word bitmask_1_, epsilon_word bitmask_2_){
  epsilon_unsigned bitmask_1 = EPSILON_WORD_TO_EPSILON_UNSIGNED(bitmask_1_);
  epsilon_unsigned bitmask_2 = EPSILON_WORD_TO_EPSILON_UNSIGNED(bitmask_2_);
  return EPSILON_EPSILON_UNSIGNED_TO_EPSILON_WORD(bitmask_1 ^ bitmask_2);
}
epsilon_word epsilon_primitive_logical_left_shift(epsilon_word bitmask_, epsilon_word position_no_){
  epsilon_unsigned bitmask = EPSILON_WORD_TO_EPSILON_UNSIGNED(bitmask_);
  epsilon_unsigned position_no = EPSILON_WORD_TO_EPSILON_UNSIGNED(position_no_);
  return EPSILON_EPSILON_UNSIGNED_TO_EPSILON_WORD(bitmask << position_no);
}
epsilon_word epsilon_primitive_logical_right_shift(epsilon_word bitmask_, epsilon_word position_no_){
  epsilon_unsigned bitmask = EPSILON_WORD_TO_EPSILON_UNSIGNED(bitmask_);
  epsilon_unsigned position_no = EPSILON_WORD_TO_EPSILON_UNSIGNED(position_no_);
  return EPSILON_EPSILON_UNSIGNED_TO_EPSILON_WORD(bitmask >> position_no);
}
epsilon_word epsilon_primitive_arithmetic_left_shift(epsilon_word bitmask_, epsilon_word position_no_){
  epsilon_int bitmask = EPSILON_WORD_TO_EPSILON_UNSIGNED(bitmask_);
  epsilon_unsigned position_no = EPSILON_WORD_TO_EPSILON_UNSIGNED(position_no_);
  return EPSILON_EPSILON_UNSIGNED_TO_EPSILON_WORD(bitmask << position_no);
}
epsilon_word epsilon_primitive_arithmetic_right_shift(epsilon_word bitmask_, epsilon_word position_no_){
  epsilon_int bitmask = EPSILON_WORD_TO_EPSILON_UNSIGNED(bitmask_);
  epsilon_unsigned position_no = EPSILON_WORD_TO_EPSILON_UNSIGNED(position_no_);
  epsilon_int result = bitmask;
  /* C99 doesn't define whether >> sign-extends when the left
     parameter is signed negative, so we have to play it safe.  GCC
     currently specifies a behavior, but its documentation explicitly
     says that the choice is subject to change. */
  int i;
  for(i = position_no; i >= 0; i --)
    result /= 2;
  return EPSILON_EPSILON_UNSIGNED_TO_EPSILON_WORD(result);
}

epsilon_word epsilon_primitive_plus(epsilon_word a_, epsilon_word b_){
  epsilon_int a = EPSILON_WORD_TO_EPSILON_INT(a_);
  epsilon_int b = EPSILON_WORD_TO_EPSILON_INT(b_);
  return EPSILON_EPSILON_INT_TO_EPSILON_WORD(a + b);
}
epsilon_word epsilon_primitive_minus(epsilon_word a_, epsilon_word b_){
  epsilon_int a = EPSILON_WORD_TO_EPSILON_INT(a_);
  epsilon_int b = EPSILON_WORD_TO_EPSILON_INT(b_);
  return EPSILON_EPSILON_INT_TO_EPSILON_WORD(a - b);
}
epsilon_word epsilon_primitive_times(epsilon_word a_, epsilon_word b_){
  epsilon_int a = EPSILON_WORD_TO_EPSILON_INT(a_);
  epsilon_int b = EPSILON_WORD_TO_EPSILON_INT(b_);
  return EPSILON_EPSILON_INT_TO_EPSILON_WORD(a * b);
}
epsilon_word epsilon_primitive_divided(epsilon_word a_, epsilon_word b_){
  epsilon_int a = EPSILON_WORD_TO_EPSILON_INT(a_);
  epsilon_int b = EPSILON_WORD_TO_EPSILON_INT(b_);
  return EPSILON_EPSILON_INT_TO_EPSILON_WORD(a / b);
}
epsilon_word epsilon_primitive_modulo(epsilon_word a_, epsilon_word b_){
  epsilon_int a = EPSILON_WORD_TO_EPSILON_INT(a_);
  epsilon_int b = EPSILON_WORD_TO_EPSILON_INT(b_);
  return EPSILON_EPSILON_INT_TO_EPSILON_WORD(a % b);
}

void epsilon_primitive_print_char(epsilon_word a_){
  epsilon_int a = EPSILON_WORD_TO_EPSILON_INT(a_);
  //printf("%c", (char)a);
  putchar((int)a);
}
void epsilon_primitive_print_int(epsilon_word a_){
  epsilon_int a = EPSILON_WORD_TO_EPSILON_INT(a_);
  printf("%li", (long)a);
}
void epsilon_primitive_print_bool(epsilon_word a_){
  epsilon_int a = EPSILON_WORD_TO_EPSILON_INT(a_);
  printf("#%c", a?'t':'f');
}
void epsilon_primitive_print_space(void){
  printf(" ");
}
void epsilon_primitive_print_newline(void){
  printf("\n");
}

epsilon_word epsilon_primitive_read_char(void){
  int a = getchar();
  if (a == -1)
    epsilon_fatal("epsilon_primitive_read_char(): got EOF. You should have called eof? before...");
  return EPSILON_INT_TO_EPSILON_WORD(a);
}

epsilon_word epsilon_primitive_eof_p(void){
  return EPSILON_INT_TO_EPSILON_WORD((int)feof(stdin));
}

void epsilon_primitive_dump_int(epsilon_word a_){
  epsilon_int a = EPSILON_WORD_TO_EPSILON_INT(a_);
  printf("Dump: %li [%lu or 0x%lx as unsigned]\n", (long)a, (unsigned long)a, (unsigned long)a); fflush(stdout);
}

epsilon_word epsilon_primitive_heap_allocate_buffer(epsilon_word word_no_){
  epsilon_int word_no = EPSILON_WORD_TO_EPSILON_INT(word_no_);
  epsilon_word result = epsilon_xmalloc(sizeof(epsilon_word) * word_no);
  /* printf("epsilon_primitive_heap_allocate_buffer(): made a %li-word buffer at %p\n", (long)word_no, result); fflush(stdout); */
  return result;
}

epsilon_word epsilon_primitive_ref(epsilon_word pointer_){
  epsilon_word *pointer = pointer_;
  /* printf("epsilon_primitive_ref(): dereferencing %p\n", pointer); fflush(stdout); */
  return *pointer;
}

/* initialize! is different from set! in the case of a generational GC, because it doesn't
   execute a write barrier. */
void epsilon_primitive_initialize_x(epsilon_word pointer_, epsilon_word new_content){
  epsilon_word *pointer = pointer_;
  /* printf("epsilon_primitive_initialize_x(): setting %p to %p\n", pointer, new_content); fflush(stdout); */
  *pointer = new_content;
}

void epsilon_primitive_set_x(epsilon_word pointer_, epsilon_word new_content){
  epsilon_word *pointer = pointer_;
  /* printf("epsilon_primitive_set_x(): setting %p to %p\n", pointer, new_content); fflush(stdout); */
  *pointer = new_content;
}

epsilon_word epsilon_primitive_call_primitive(epsilon_word primitive_name,
                                              epsilon_word parameter_value_list,
                                              epsilon_word local_environment,
                                              epsilon_word context){
  epsilon_word *result = epsilon_primitive_heap_allocate_buffer(EPSILON_INT_TO_EPSILON_WORD(2));
  result[0] = EPSILON_INT_TO_EPSILON_WORD(42); // To do: really implement
  result[1] = context;
  return result;
}

void epsilon_primitive_garbage_collect(void){
  printf("Garbage-collecting [To do: do it for real]\n"); fflush(stdout);
}

void epsilon_primitive_halt_and_catch_fire(void){
  printf("ERROR: The computer is on fire\n"); fflush(stdout);
  exit(EXIT_FAILURE);
}
