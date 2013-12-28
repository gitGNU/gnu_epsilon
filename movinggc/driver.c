/* This file is part of GNU epsilon.

   Copyright (C) 2012 Université Paris 13
   Written by Luca Saiu

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
#include <unistd.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <time.h>

#include "movinggc.h"

void* cons(int untagged_car, void *tagged_cdr){
  movinggc_push_root(&tagged_cdr);
  void** new_cons = movinggc_allocate(2);
  new_cons[0] = (void*)(MOVINGGC_TAG_NONPOINTER(untagged_car));
  new_cons[1] = tagged_cdr;
  movinggc_pop_root();
  return MOVINGGC_TAG_POINTER(new_cons);
}

void dump(void **cons){
  if(cons == MOVINGGC_TAG_NONPOINTER(-1)){
    printf("\n");
    return;
  }
  void **untagged_cons = MOVINGGC_UNTAG_POINTER(cons);
  if(untagged_cons < (void**)0xa){
    printf("By the way, cons is %p\n", cons);
    printf("By the way, untagged_cons is %p\n", untagged_cons);
    printf("That stupid bug again: untagged_cons is < 0xa");
    assert(false);
  }
  dump(untagged_cons[1]);//MOVINGGC_UNTAG_POINTER((cons[1])));
  printf("%p (%s): ", untagged_cons, movinggc_semispace_name_of(untagged_cons));
  printf("%li\n", (long)MOVINGGC_UNTAG_NONPOINTER(untagged_cons[0]));
  //dump(MOVINGGC_UNTAG_POINTER((cons[1])));
}

int main(void){
  /* int j; */
  /* int *p = &j; */
  /* movinggc_verbose_log("p = %p\n", p); */
  /* movinggc_verbose_log("%p\n", MOVINGGC_TAG_POINTER(p)); */
  /* return 0; */
  //srand(time(NULL));
  srand(0); // I want deterministic results
  movinggc_initialize();
  int i, j;
  void *root_cons;
  register_root(&root_cons, 1);
  for(j = 0; j < 1000; j++){
    root_cons = MOVINGGC_TAG_NONPOINTER(-1); /* yes, the empty list is a non-pointer */
    for(i = 0; i < 100000 /* MOVINGGC_SEMISPACE_WORDS_NO / 3 */; i++){
      void *new_cons = cons(i, root_cons);
      if(i % 1000/* 6 */ == 0){//(rand() % 100 >= 95){
      //if(rand() % 200 <= 1){
        root_cons = new_cons;
        //printf("Created the new root_cons at %p (%s): %i\n", new_cons, movinggc_semispace_name_of(MOVINGGC_UNTAG_POINTER(root_cons)), i);
        //movinggc_dump_free_space_statistics();
      }
    } // inner for
  }
  printf("\n(GC'd %li times)\n", movinggc_gc_no());
  //printf("\n");
  dump(root_cons);
  printf("Exiting.\n");
  return 0;
}
