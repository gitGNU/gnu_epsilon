/* This file is part of GNU epsilon.

   Copyright (C) 2002, 2003 Luca Saiu
   Minor updates by Luca Saiu in 2006
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


#ifndef EPSILONGC_MALLOC_H_
#define EPSILONGC_MALLOC_H_

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h> /* for size_t */

/* Commodity versions of malloc(), calloc() and realloc() which abort 
   instead of returning NULL when memory is exhausted: */
void* epsilongc_xmalloc(size_t size) __attribute__ (( malloc ));
void* epsilongc_xcalloc(size_t elements_no, size_t element_size) __attribute__ (( malloc ));
void* epsilongc_xrealloc(void* old_pointer, size_t new_size);

#endif /* #ifndef EPSILONGC_MALLOC_H_ */
