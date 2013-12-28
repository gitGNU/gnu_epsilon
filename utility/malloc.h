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


#ifndef EPSILON_UTILITY_MALLOC_H_
#define EPSILON_UTILITY_MALLOC_H_

#include <stdlib.h> // for size_t

/* The usual safe wrappers for malloc and realloc() which check for the return
   code and just exit with a fatal error on out-of-memory.  Of course no wrapper
   for free() is needed.  */
void *epsilon_xmalloc(size_t size_in_chars)
  __attribute__(( malloc ));
void *epsilon_xrealloc(void *old_buffer, size_t new_size_in_chars)
  __attribute__(( malloc ));

#endif // #ifndef EPSILON_UTILITY_MALLOC_H_
