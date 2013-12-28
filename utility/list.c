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


#include "list.h"
#include "debug.h"
#include "malloc.h"

/* The empty list is just a NULL pointer: */
const epsilon_list_t epsilon_nil = NULL;

epsilon_list_t epsilon_cons(epsilon_word car, epsilon_list_t cdr){
  epsilon_list_t result = epsilon_xmalloc(sizeof(struct epsilon_cons));
  result->car = car;
  result->cdr = cdr;
  return result;
}

epsilon_word epsilon_car(epsilon_list_t list){
  EPSILON_CHECK_ON_DEBUG(list != NULL);
  return list->car;
}
epsilon_list_t epsilon_cdr(epsilon_list_t list){
  EPSILON_CHECK_ON_DEBUG(list != NULL);
  return list->cdr;
}

bool epsilon_null(epsilon_list_t list){
  return list == NULL;
}

bool epsilon_nnull(epsilon_list_t list){
  return list != NULL;
}

epsilon_int epsilon_length(epsilon_list_t list){
  epsilon_int result = 0;
  epsilon_list_t p = list;
  for(p = list; epsilon_nnull(p); p = epsilon_cdr(p))
    result ++;
  return result;
}

bool epsilon_is_in_list(epsilon_word candidate_car,
                        epsilon_list_t list){
  epsilon_list_t p = list;
  for(p = list; epsilon_nnull(p); p = epsilon_cdr(p))
    if(epsilon_car(p) == candidate_car)
      return true;
  return false;
}

void epsilon_destroy_list_with_car_destructor(epsilon_list_t list,
                                              void(*destroy_car)(epsilon_word)){
  epsilon_list_t p = list;
  while(p != NULL){
    epsilon_list_t next = p->cdr;
    destroy_car(p->car);
    free(p);
    p = next;
  } // while
}

static void trivial_car_destructor(epsilon_word car){
  // Do nothing
}

void epsilon_destroy_list(epsilon_list_t list){
  epsilon_destroy_list_with_car_destructor(list, trivial_car_destructor);
}
