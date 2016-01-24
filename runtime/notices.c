/* Print legal or technical notices at runtime.

   Copyright (C) 2016  Luca Saiu
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

#include "../runtime/runtime.h"
#include "../runtime/notices.h"

/* Print an abbreviated copyright notice only mentioning the last year.
   See standards.info for the rationale. */
static void
epsilon_print_short_copyright_notice (void)
{
  printf ("%s\n", SHORT_COPYRIGHT_NOTICE);
}

void
epsilon_print_short_legal_notices (void)
{
  epsilon_print_short_copyright_notice ();
  printf ("This is free software: you are free to change and redistribute it under the\n");
  printf ("terms of the GNU GPL version 3 or later: see http://gnu.org/licenses/gpl.html\n");
  printf ("There is NO WARRANTY to the extent permitted by law.\n");
}

void
epsilon_print_bugs_and_help_notices (void)
{
  printf ("Please report bugs to:           %s\n", PACKAGE_BUGREPORT);
  printf ("Development mailing list:        epsilon-devel@gnu.org\n");
  printf ("GNU epsilon home page:           %s\n", PACKAGE_URL);
  printf ("General help using GNU software: http://www.gnu.org/gethelp\n");
}

static const char*
epsilon_the_runtime_name =
#if   defined(EPSILON_RUNTIME_TAGGED)
  "tagged"
#elif defined(EPSILON_RUNTIME_UNTAGGED)
  "untagged"
#elif defined(EPSILON_RUNTIME_SMOB)
  "SMOB"
#else
#error "unknown runtime"
#endif // if   defined(EPSILON_RUNTIME_TAGGED)
  ;

const char *
epsilon_runtime_name (void)
{
  return epsilon_the_runtime_name;
}
