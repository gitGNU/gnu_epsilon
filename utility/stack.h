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

#ifndef __EPSILON_STACK_H_
#define __EPSILON_STACK_H_

#include <stdlib.h>
#include <stdbool.h>
#include "types.h"

struct epsilon_stack{
  size_t element_no;
  size_t allocated_element_no;
  epsilon_word *buffer;
};
typedef struct epsilon_stack* epsilon_stack_t;

epsilon_stack_t epsilon_stack_make(void);
void epsilon_stack_destroy(epsilon_stack_t epsilon_stack);

/* Look for a given element (compared by identity) in all the currently-used slots: */
bool epsilon_stack_has(epsilon_stack_t epsilon_stack, epsilon_word element);
void epsilon_stack_push(epsilon_stack_t epsilon_stack, epsilon_word element);
bool epsilon_stack_empty(epsilon_stack_t epsilon_stack);

epsilon_word epsilon_stack_pop(epsilon_stack_t epsilon_stack);

#endif // #ifndef __EPSILON_STACK_H_
