/* This file is part of GNU epsilon.

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


#include "doubly_linked_list_macros.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <malloc.h>
#include "epsilongc_debug.h"


typedef struct something* something_t;
struct something{
  EPSILONGC_LIST_ELEMENT_FIELDS(my_element_prefix, something_t)
  int q;
}; // struct

typedef struct epsilongc_doubly_linked_list* epsilongc_doubly_linked_list_t;
struct epsilongc_doubly_linked_list{
  EPSILONGC_LIST_FIELDS(my_list_prefix, something_t)
}; // struct

something_t make_something(void){
  something_t r = (something_t)epsilongc_xmalloc(sizeof(struct something));
  return r;
}

epsilongc_doubly_linked_list_t interval(const int a, const int b){
  epsilongc_doubly_linked_list_t list =
    EPSILONGC_MAKE_LIST(my_list_prefix, struct epsilongc_doubly_linked_list);
  int i;
  if((rand() % 2) == 0){
    //printf("Building forward\n");
    for(i = a; i <= b; i++){
      something_t something = make_something();
      something->q = i;
      EPSILONGC_APPEND_OBJECT_TO_LIST(my_list_prefix, my_element_prefix, something_t, something, list);
    };
  }
  else{
    //printf("Building backwards\n");
    for(i = b; i >= a; i--){
      something_t something = make_something();
      something->q = i;
      EPSILONGC_PREPEND_OBJECT_TO_LIST(my_list_prefix, my_element_prefix, something_t, something, list);
    };
  }
  return list;
}

void print_list(epsilongc_doubly_linked_list_t list){
  printf("First to last: ");
  something_t p;
  for(p = EPSILONGC_FIRST_ELEMENT_OF_LIST(my_list_prefix, list);
      p != NULL;
      p = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(my_element_prefix, p)){
    printf("%i ", p->q);
  } // for
  printf(" -- ");

  printf("Last to first: ");
  for(p = EPSILONGC_LAST_ELEMENT_OF_LIST(my_list_prefix, list);
      p != NULL;
      p = EPSILONGC_PREVIOUS_ELEMENT_OF_ELEMENT(my_element_prefix, p)){
    printf("%i ", p->q);
  } // for
  printf("(length %i)\n", (int)EPSILONGC_LENGTH_OF_LIST(my_list_prefix, list));
}

void print_element(something_t element){
    printf("[%i] ", element->q);
}

int main(void){
  while(true){
    int j;
    for(j = -1; j < 10; j ++){
      epsilongc_doubly_linked_list_t list = interval(0, j);
      //print_list(list);
      int n;
      for(n = 0; n < j; n++){
        const something_t nth_element =
          EPSILONGC_NTH_ELEMENT_OF_LIST(my_list_prefix, my_element_prefix, something_t, n, list);
        assert(nth_element->q == n);
        const something_t nth_element_from_the_end =
          EPSILONGC_NTH_ELEMENT_OF_LIST_FROM_THE_END(my_list_prefix, my_element_prefix, something_t, n, list);
        assert(nth_element_from_the_end->q == (j - n));
      }
      epsilongc_doubly_linked_list_t another_list = interval(0, (rand() % 3) - 1);
      int q = rand() % 4;
      switch(q){
      case 0:
        EPSILONGC_APPEND_SECOND_LIST_TO_FIRST_LIST(my_list_prefix, my_list_prefix, my_element_prefix, something_t, list, another_list);
        break;
      case 1:
        EPSILONGC_APPEND_SECOND_LIST_TO_FIRST_LIST(my_list_prefix, my_list_prefix, my_element_prefix, something_t, another_list, list);
        break;
      case 2:
        EPSILONGC_PREPEND_SECOND_LIST_TO_FIRST_LIST(my_list_prefix, my_list_prefix, my_element_prefix, something_t, list, another_list);
        break;
      case 3:
        EPSILONGC_PREPEND_SECOND_LIST_TO_FIRST_LIST(my_list_prefix, my_list_prefix, my_element_prefix, something_t, another_list, list);
        break;
      default:
        assert(false);
      } // switch
        
      EPSILONGC_DESTROY_LIST_AND_ELEMENTS(my_list_prefix, my_element_prefix, something_t, list, free);
      EPSILONGC_DESTROY_LIST_AND_ELEMENTS(my_list_prefix, my_element_prefix, something_t, another_list, free);
    } // for
  } // while
  
  printf("SUCCESS\n");
  return 0;
}
