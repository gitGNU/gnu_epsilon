/* Error-testing malloc wrapper.

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
#include <unistd.h>
#include "hints.h"
#include "fatal.h"
#include "malloc.h"
#include "config.h"

/* Autoconf generates #defines replacing malloc and realloc with rpl_malloc
   and rpl_realloc on systems with broken implementations: */
#if (HAVE_MALLOC == 0)
#undef malloc
#undef realloc
#include <sys/types.h>
/* Replacement malloc() and realloc() for broken systems on which malloc(0)
   returns NULL: */
void* rpl_malloc(size_t n){
  return malloc(n != 0 ? n : 1);
}
void* rpl_realloc(void *old_pointer, size_t n){
  return realloc(old_pointer, n != 0 ? n : 1);
}
#endif // #if (HAVE_MALLOC == 0)

void *epsilon_xmalloc(size_t size_in_chars){
  void *result = malloc(size_in_chars);
  EPSILON_IF_UNLIKELY(result == NULL)
    epsilon_fatal("malloc() failed");
  else
    return result;
}

void *epsilon_xrealloc(void *old_buffer, size_t new_size_in_chars){
  void *result = realloc(old_buffer, new_size_in_chars);
  EPSILON_IF_UNLIKELY(result == NULL)
    epsilon_fatal("realloc() failed");
  else
    return result;
}
