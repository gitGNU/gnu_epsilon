/* Image interpreter.

   Copyright (C) 2012, 2016  Luca Saiu
   Updated in 2015 by Luca Saiu
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
#include <string.h>
#include "../utility/utility.h"
#include "../runtime/runtime.h"

static void
epsilon_print_version_and_exit (int argc, char **argv)
{
  char *runtime_name = epsilon_runtime_name ();
  size_t length = strlen (runtime_name);
  char *lowercase_runtime_name = epsilon_xmalloc (length); // no need to free
  int i;
  for (i = 0; i <= length; i ++)
    lowercase_runtime_name[i] = tolower (runtime_name[i]);
  printf ("epsilon-image-interpreter-%s (GNU epsilon) %s\n",
          lowercase_runtime_name,
          PACKAGE_VERSION);
  epsilon_print_short_legal_notices ();
  exit (EXIT_SUCCESS);
}

static void
epsilon_print_help_and_exit (int argc, char **argv, int exit_code)
{
  printf ("Usage: %s ARGUMENT ...\n", argv[0]);
  printf ("Interpret a GNU epsilon dumped image in the %s runtime.\n",
          epsilon_runtime_name ());
  printf ("\n");
  printf ("The first argument is mandatory: it can be \"--help\", \"--version\", or the\n");
  printf ("image file to be loaded.  Any further arguments are passed as they are to the\n");
  printf ("epsilon image, to be processed in some image-dependent way.\n");
  printf ("\n");
  printf ("        --version       show version information and exit\n");
  printf ("        --help          display this help message and exit\n");
  printf ("\n");
  epsilon_print_bugs_and_help_notices ();
  exit (exit_code);
}

int
main (int argc, char **argv)
{
  /* A first command-line argument is mandatory.  Every further one will be
     available to be handled by the image, if any */
  if (argc < 2)
    epsilon_print_help_and_exit (argc, argv, EXIT_FAILURE);
  if (! strcmp (argv[1], "--version"))
    epsilon_print_version_and_exit (argc, argv);
  else if (! strcmp (argv[1], "--help"))
    epsilon_print_help_and_exit (argc, argv, EXIT_SUCCESS);

  /* Unmarshal the pair from the file; the pair contains the symbol
     table and a main expression. */
  char *filename = argv[1];
  /* Hide the interpeter from epsilon's argv. */
  epsilon_runtime_initialize (argc - 1, argv + 1);
  FILE *f = fopen (filename, "r");
  if(f == NULL)
    epsilon_fatal ("could not open %s", filename);
  epsilon_value pair = epsilon_unmarshal_value (f);
  fclose (f);

  /* Extract the symbol table, and ensure it's not garbage-collected: */
  volatile epsilon_value symbol_table __attribute__((unused)) = epsilon_value_car(pair);

  /* Extract the expression from the pair, and interpret it: */
  epsilon_value main_expression = epsilon_value_cdr(pair);
  epsilon_value environment = epsilon_int_to_epsilon_value(0);
  epsilon_e0_eval_making_stacks(main_expression,
                                environment);

  /* Exit with success if we're still alive: */
  return EXIT_SUCCESS;
}
