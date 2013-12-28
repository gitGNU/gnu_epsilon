/* This file is part of GNU epsilon.

Copyright (C) 2002 Luca Saiu
Minor updates by Luca Saiu in 2006 and 2008
Copyright (C) 2012 Université Paris 13
Written by Luca Saiu

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. */


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "malloc.h"
#include "fatal.h"

/* Commodity versions of malloc() and realloc() which abort instead of
   returning NULL when memory is exhausted: */
void* epsilongc_xmalloc(size_t size){
  /* Try to allocate: */
  void* r = malloc(size);

  /* Check and return: */
  if(r == NULL)
    epsilongc_fatal("epsilongc_xmalloc(): memory exhausted");
  else
    return r;
}

/* Commodity version of calloc() which aborts instead of
   returning NULL when memory is exhausted: */
void* epsilongc_xcalloc(size_t elements_no, size_t element_size){
  /* Try to allocate: */
  void* r = calloc(elements_no, element_size);
  
  /* Check and return: */
  if(r == NULL)
    epsilongc_fatal("epsilongc_xcalloc(): memory exhausted");
  else
    return r;
}

void* epsilongc_xrealloc(void* old_pointer, size_t new_size){
  /* Try to allocate: */
  void* r = realloc(old_pointer, new_size);

  /* Check and return: */
  if(r == NULL)
    epsilongc_fatal("epsilongc_xrealloc(): memory exhausted");
  else
    return r;
}
