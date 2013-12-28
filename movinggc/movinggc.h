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


#ifndef MOVINGGC_H_
#define MOVINGGC_H_

#include "tags.h"
#include "features.h"

/* Initialize and finalize: */
void movinggc_initialize(void);

/* Register an array of roots: */
void register_root(void **pointer_to_roots,
                   size_t size_in_words);

/* Handle temporary roots: */
void movinggc_push_root(void **pointer_to_root);
void movinggc_pop_root(void);

/* Allocate: this is the most frequently used function, of course. It
   returns a NON-tagged pointer. */
void *movinggc_allocate(const size_t size_in_words);

/* Explicit GC and tuning: */
void movinggc_resize_semispaces(const size_t new_semispace_size_in_words);
void movinggc_gc(void)
  __attribute__((noinline));

/* Statistics and debugging: */
void movinggc_dump_free_space_statistics(void);
float movinggc_fill_ratio(void);
const char* movinggc_semispace_name_of(void *untagged_pointer);
long movinggc_gc_no(void); // how many times did we GC?

#endif // #ifndef MOVINGGC_H_
