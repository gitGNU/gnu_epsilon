/* A simple dynamically-growing stack implementation.

   Copyright (C) 2012 Université Paris 13
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


#include <assert.h>
#include "stack.h"
#include "malloc.h"

#define EPSILON_STACK_INITIAL_SIZE 64

epsilon_stack_t epsilon_stack_make(void){
  epsilon_stack_t result = (epsilon_stack_t)epsilon_xmalloc(sizeof(struct epsilon_stack));
  result->buffer = (epsilon_word*)epsilon_xmalloc(sizeof(epsilon_word) * EPSILON_STACK_INITIAL_SIZE);
  result->element_no = 0;
  result->allocated_element_no = EPSILON_STACK_INITIAL_SIZE;
  return result;
}

void epsilon_stack_destroy(epsilon_stack_t stack){
  free(stack->buffer);
  free(stack);
}

bool epsilon_stack_has(epsilon_stack_t stack, epsilon_word element){
  int i;
  for (i = 0; i < stack->element_no; i ++)
    if(stack->buffer[i] == element)
      return true;
  return false;
}

void epsilon_stack_push(epsilon_stack_t stack, epsilon_word element){
  if(stack->element_no >= stack->allocated_element_no){
    const size_t old_size = stack->allocated_element_no;
    const size_t new_size = old_size * 2;
    stack->buffer = (epsilon_word*)epsilon_xrealloc(stack->buffer,
                                                    sizeof(epsilon_word)*new_size);
    //printf("STACK RESIZE: %ul -> %ul\n", (unsigned long)old_size, (unsigned long)new_size);
    stack->allocated_element_no = new_size;
  }
  stack->buffer[stack->element_no] = element;
  stack->element_no ++;
}

bool epsilon_stack_empty(epsilon_stack_t stack){
  return stack->element_no == 0;;
}

epsilon_word epsilon_stack_pop(epsilon_stack_t stack){
  assert(! epsilon_stack_empty(stack));
  stack->element_no --;
  epsilon_word result = stack->buffer[stack->element_no];
  return result;
}
