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
#include <string.h>
#include <malloc.h>
#include <pthread.h>
#include "epsilongc_types.h"
#include "allocator.h"
#include "kind.h"
#include "set_of_pages.h"
#include "trace.h"
#include "time.h"
#include <gc/gc.h>
#include <assert.h>

int main(void){
  epsilongc_integer_t i;
  while(true){
    epsilongc_initialize_set_of_pages();
    for(i = 0; i < 10000; i++){
      epsilongc_page_t page = (epsilongc_page_t)
        (epsilongc_integer_t)rand();
      //printf("%p\n", page); fflush(stdout);
      if(epsilongc_does_page_exist(page))
        continue;
      epsilongc_add_page_to_the_set_of_pages(page);
      assert(epsilongc_does_page_exist(page));
      if((rand() % 100) == 0){
        epsilongc_remove_page_from_the_set_of_pages(page);
        assert(! epsilongc_does_page_exist(page));
      } // if
    } // for
    epsilongc_finalize_set_of_pages();
  } // while
  
  return 0;
}
