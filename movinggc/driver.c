/* This file is part of GNU epsilon.

   Copyright (C) 2012 Université Paris 13
   Copyright (C) 2015 Luca Saiu
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

struct temporary_roots
{
  void **root1;
  void **root2;
};
static struct temporary_roots temporary_roots;

void
pre_gc_hook (void *tr_as_void_star)
{
  struct temporary_roots *tr = tr_as_void_star;
  movinggc_push_dynamic_root (tr->root1);
  movinggc_push_dynamic_root (tr->root2);
}

void
post_gc_hook (void *tr_as_void_star)
{
  movinggc_pop_dynamic_roots (2);
}

inline void *
cons (int untagged_car, void *tagged_cdr)
{
  temporary_roots.root1 = &tagged_cdr;
  void *q = MOVINGGC_TAG_NONPOINTER (42);
  temporary_roots.root2 = &q;
  void **new_cons = movinggc_allocate_chars (2 * sizeof(void*));
  new_cons[0] = (void *) (MOVINGGC_TAG_NONPOINTER (untagged_car));
  new_cons[1] = tagged_cdr;
  //printf ("%% driver: Filled %p (tagged %p) with %p and %p\n", new_cons, MOVINGGC_TAG_POINTER (new_cons), new_cons[0], new_cons[1]);
  return MOVINGGC_TAG_POINTER (new_cons);
}

void
dump_or_check (void **list, int verbose)
{
  if (MOVINGGC_IS_NONPOINTER(list))
    {
      if (verbose)
        {
          printf ("Non-pointer %li\n", (long)MOVINGGC_UNTAG_NONPOINTER(list));
        }
      return;
    }
  void **untagged_list = MOVINGGC_UNTAG_POINTER (list);
  if (untagged_list < (void **) 0xa)
    {
      printf ("By the way, list is %p\n", list);
      printf ("By the way, untagged_list is %p\n", untagged_list);
      printf ("That stupid bug again: untagged_list is < 0xa");
      assert (false);
    }
  if (verbose)
    {
      printf ("%p (%s): ", untagged_list,
              movinggc_semispace_name_of (untagged_list));
      printf ("%li\n", (long) MOVINGGC_UNTAG_NONPOINTER (untagged_list[0]));
      //dump(MOVINGGC_UNTAG_POINTER((cons[1])));
    }
  dump_or_check (untagged_list[1], verbose); //MOVINGGC_UNTAG_POINTER((cons[1])));
}

void
dump (void **list)
{
  dump_or_check (list, 1);
}

void
check (void **list)
{
  dump_or_check (list, 0);
}

int
main (void)
{
  /* int j; */
  /* int *p = &j; */
  /* movinggc_verbose_log("p = %p\n", p); */
  /* movinggc_verbose_log("%p\n", MOVINGGC_TAG_POINTER(p)); */
  /* return 0; */
  //srand(time(NULL));
  srand (0); // I want deterministic results
  movinggc_initialize ();
  int i, j;
  void *the_root;
  register_roots (&the_root, 1);

  movinggc_set_pre_hook (pre_gc_hook);
  movinggc_set_post_hook (post_gc_hook);
  movinggc_set_hook_argument (&temporary_roots);

  for (j = 0; j < 1000; j++)
    {
      the_root = MOVINGGC_TAG_NONPOINTER (1234); /* yes, the empty list is a non-pointer */
      for (i = 0; i < 1000000 /* MOVINGGC_SEMISPACE_WORD_NO / 3 */ ; i++)
        {
          void *new_cons = cons (i, the_root);
          if (i % 1000 /* 6 */  == 0)
            {                   //(rand() % 100 >= 95){
              //if(rand() % 200 <= 1){
              the_root = new_cons;
              //printf("Created the new the_root at %p (%s): %i\n", new_cons, movinggc_semispace_name_of(MOVINGGC_UNTAG_POINTER(the_root)), i);
              //movinggc_dump_free_space_statistics();
            }
        }                       // inner for
    }
  printf ("\n(GC'd %li times; allocated %.03fGiB since program start)\n",
          movinggc_gc_no (),
          movinggc_allocated_bytes () / 1024.0 / 1024.0 / 1024.0);
  //printf("\n");
  check (the_root);
  //dump (the_root);
  printf ("Exiting.\n");
  return 0;
}
