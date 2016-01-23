/* Main runtime file

   Copyright (C) 2012, 2016 Luca Saiu
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


#include "runtime.h"

#if defined(HAVE_GETRLIMIT) && defined(HAVE_SETRLIMIT)
#include <sys/resource.h>
#endif // #if defined(HAVE_GETRLIMIT) && defined(HAVE_SETRLIMIT)

/* epsilon currently uses a large amount of stack space; I can do away with
   this later, after removing Guile support. */
static void
epsilon_enlarge_stack (void)
{
#if defined(HAVE_GETRLIMIT) && defined(HAVE_SETRLIMIT)
  struct rlimit rl;
  if (getrlimit (RLIMIT_STACK, &rl) != 0)
    {
      fprintf (stderr, "Couldn't read stack space limit; trying to go on anyway.\n");
      return;
    }
  if (rl.rlim_max != RLIM_INFINITY)
    fprintf (stderr, "The stack maximum limit is not infinity; trying to go on anyway.\n");
  rl.rlim_cur = rl.rlim_max;
  if (setrlimit (RLIMIT_STACK, &rl) != 0)
    fprintf (stderr, "Couldn't increase stack space; trying to go on anyway.\n");
#else
  fprintf (stderr, "Can't change or query stack space; trying to go on anyway.\n");
#endif // #if defined(HAVE_GETRLIMIT) && defined(HAVE_SETRLIMIT)
}

void
epsilon_runtime_initialize (int argc, char **argv)
{
  epsilon_enlarge_stack ();
  epsilon_data_initialize ();
  epsilon_c_primitives_initialize (argc, argv);
}
