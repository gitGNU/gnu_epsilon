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


#ifndef EPSILON_RUNTIME_NOTICES_H_
#define EPSILON_RUNTIME_NOTICES_H_

/* Print information about license and copyright. */
void
epsilon_print_short_legal_notices (void);

/* Print information about where to send bug reports or get help. */
void
epsilon_print_bugs_and_help_notices (void);

/* Return a pointer to a static string containing a very short description of
   this runtime features. */
const char *
epsilon_runtime_name (void);

#endif // #ifndef EPSILON_RUNTIME_NOTICES_H_
