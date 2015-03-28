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
#include <time.h>
/*#include <stdint.h>
#include <stdbool.h>
#include <string.h>
*/
#include "movinggc.h"

struct temporary_roots
{
  void *root1;
  void *root2;
};
static struct temporary_roots temporary_roots;

void
pre_gc_hook (void *tr_as_void_star)
{
  struct temporary_roots *tr = tr_as_void_star;
  egc_push_dynamic_root (&tr->root1);
  egc_push_dynamic_root (&tr->root2);
}

void
post_gc_hook (void *tr_as_void_star)
{
  egc_pop_dynamic_roots (2);
}

static long cons_call_no = 0;

inline void *
cons (int untagged_car, void *tagged_cdr)
{
  temporary_roots.root1 = EGC_TAG_NONPOINTER (untagged_car);
  temporary_roots.root2 = tagged_cdr;
  void **new_cons = egc_allocate_cons ();
  new_cons[0] = temporary_roots.root1;
  new_cons[1] = temporary_roots.root2;
  cons_call_no ++;
  return EGC_TAG_POINTER(new_cons);
}

void
dump_or_check (void **list, int verbose)
{
  long length = 0;
  while (EGC_IS_POINTER(list))
    {
      void **untagged_list = EGC_UNTAG_POINTER (list);
      if (untagged_list < (void **) 0xa)
        {
          fprintf (stderr, "By the way, list is %p\n", list);
          fprintf (stderr, "By the way, untagged_list is %p\n", untagged_list);
          fprintf (stderr, "That stupid bug again: untagged_list is < 0xa");
          exit (EXIT_FAILURE);
        }
      if (verbose)
        {
          fprintf (stderr, "%p (%s): ", untagged_list,
                  egc_semispace_name_of (untagged_list));
          fprintf (stderr, "%li\n", (long) EGC_UNTAG_NONPOINTER (untagged_list[0]));
          //dump(EGC_UNTAG_POINTER((cons[1])));
        }
      length ++;
      list = untagged_list[1];
    }
  if (verbose)
    fprintf (stderr, "Non-pointer %li\n", (long)EGC_UNTAG_NONPOINTER(list));
  fprintf (stderr, "Allocated %.02fMiB.  ",
           (float)cons_call_no * sizeof(void*) * 3 / 1024.0 / 1024.0);
  fprintf (stderr, "The list length is %li; alive heap data is %.02fkiB.\n", length,
           (float)length * sizeof(void*) * 3 / 1024.0);
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

float random_from_0_to_1 (void)
{
  float r = (float)(rand () % 10000000) / 10000000.0;
  return r;
}

int
main (void)
{
  /* int j; */
  /* int *p = &j; */
  /* egc_verbose_log("p = %p\n", p); */
  /* egc_verbose_log("%p\n", EGC_TAG_POINTER(p)); */
  /* return 0; */
  //srand(time(NULL));
  srand (0); // I want deterministic results
  egc_initialize ();
  int i, j;
  void *root1;
  egc_register_roots (&root1, 1);

  egc_set_pre_hook (pre_gc_hook);
  egc_set_post_hook (post_gc_hook);
  egc_set_hook_argument (&temporary_roots);

  fprintf (stderr, "* Start main loop\n");

#define OUTER_LOOP_LENGTH      100
#define INNER_LOOP_LENGTH      1000000
#define EXPECTED_LENGTH        100000
#define ADD_PROBABILITY        ((float)EXPECTED_LENGTH / (float)INNER_LOOP_LENGTH)
  root1 = EGC_TAG_NONPOINTER (NULL); /* yes, the empty list is a non-pointer */
  for (j = 0; j < OUTER_LOOP_LENGTH; j++)
    {
      root1 = EGC_TAG_NONPOINTER (NULL);
      for (i = 0; i < INNER_LOOP_LENGTH ; i++)
        {
          void *new_cons = cons (i, root1);
          //if (i % 10 == 0)
          if (random_from_0_to_1 () <= ADD_PROBABILITY)
            {                   //(rand() % 100 >= 95){
              //if(rand() % 200 <= 1){
              root1 = new_cons;
              //printf("Created the new the_root at %p (%s): %i\n", new_cons, egc_semispace_name_of(EGC_UNTAG_POINTER(the_root)), i);
              //egc_dump_free_space_statistics();
            }
        }                       // inner for
    }
  fprintf (stderr, "* End main loop\n");
  //printf("\n");
  /* Remove hooks.  This is absolutely essential if I want to call a
     GC from now on, since the content of temporary_roots might now
     refer garbage or even point within a semispace which has now
     been relocated. */
  egc_set_pre_hook (NULL);
  egc_set_post_hook (NULL);
  //egc_dump_generation_contents ();

  egc_dump_generations ();
  egc_dump_times ();

  fprintf (stderr, "* The root %p (tag %li) is at %p\n", root1, (long)root1 & 1, &root1);
  fprintf (stderr, "* Checking integrity...\n");
  //dump (root1);
  check (root1);
  fprintf (stderr, "* Force a final collection before integrity checks...\n");
  egc_full_gc ();
  /* Dump again, to make sure nothing broke. */
  fprintf (stderr, "* Checking integrity again...\n");
  check (root1);
  //dump (root1);

  //egc_dump_generations ();

  //egc_dump_generation_contents ();
  //egc_dump_generations ();
  fprintf (stderr, "Success.\n");
  return 0;
}
