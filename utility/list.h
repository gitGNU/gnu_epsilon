/* Singly-linked lists.

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


#ifndef EPSILON_LIST_H_
#define EPSILON_LIST_H_

/* A simple singly-linked list of words.  Notice that despite the Lisp
   notation allocation and destruction are manual, and this data structure
   is thought for simple lists with no sharing; making arbitrary cons
   structures would be prohibitively hard to manage without a GC. */

#include <stdbool.h>
#include "types.h"

/* A list is either a pointer to a cons or a NULL pointer: */
typedef struct epsilon_cons* epsilon_list_t;
struct epsilon_cons{
  epsilon_word car;
  epsilon_list_t cdr;
}; // struct

/* The empty list, i.e. a NULL pointer: */
extern const epsilon_list_t epsilon_nil;

/* Constructor: */
epsilon_list_t epsilon_cons(epsilon_word car, epsilon_list_t cdr)
  __attribute__(( malloc ));

/* Selectors: */
epsilon_word epsilon_car(epsilon_list_t list);
epsilon_list_t epsilon_cdr(epsilon_list_t list);

/* Is the list empty? */
bool epsilon_null(epsilon_list_t list);

/* Is the list non-empty? */
bool epsilon_nnull(epsilon_list_t list);

/* List length: */
epsilon_int epsilon_length(epsilon_list_t list);

/* Return true iff the given object is the car of some cons in the
   given list: */
bool epsilon_is_in_list(epsilon_word candidate_car,
                        epsilon_list_t list);

/* Destroy all conses, ignoring cars: */
void epsilon_destroy_list(epsilon_list_t list);

/* Destroy all conses, applying the given destructor function to cars: */
void epsilon_destroy_list_with_car_destructor(epsilon_list_t list,
                                              void(*destructor)(epsilon_word));

#endif // #ifndef EPSILON_LIST_H_
