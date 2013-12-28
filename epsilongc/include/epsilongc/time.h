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


#ifndef EPSILONGC_TIME_H_
#define EPSILONGC_TIME_H_

/* Return the elapsed time since an arbitrary epoch, in seconds. The result is
   meant to be subtracted from the result of another invocation.
To do: make it more solid.
*/
double epsilongc_get_current_time(void);

/* This is useful for "profiling": */
#define EPSILONGC_BENCHMARK(MESSAGE, BODY) \
  { printf("%s: begin...\n", MESSAGE);                                  \
    const double _time_at_the_beginning = epsilongc_get_current_time(); \
    {BODY;} \
    const double _time_at_the_end = epsilongc_get_current_time(); \
    printf("%s: end (%f seconds elapsed).\n", MESSAGE, _time_at_the_end - _time_at_the_beginning); }

#endif // #ifndef EPSILONGC_TIME_H_
