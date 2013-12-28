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


#ifndef EPSILONGC_MACROS_H_
#define EPSILONGC_MACROS_H_

/* This doesn't really belong here, but I want it to be #define'd
   everywhere. In particular I want the GNU/BSD semantics for signals
   as opposed to SysV: I *don't* want to deal with EINTR. */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif //#ifndef _GNU_SOURCE

#include <stdio.h>
#include <stdbool.h>
#include <limits.h>

#ifndef EPSILONGC_I_HAVE_INCLUDED_CONFIG_H_
#define EPSILONGC_I_HAVE_INCLUDED_CONFIG_H_
#include "config.h"
#endif //#ifndef EPSILONGC_I_HAVE_INCLUDED_CONFIG_H_

/* Give a nicer interface to __builtin_expect(), so that we can give the
   GCC optimizer jump probability hints, with an easy-to-read syntax: */
#define EPSILONGC_LIKELY(PREDICATE_) \
  __builtin_expect(PREDICATE_, true)
#define EPSILONGC_UNLIKELY(PREDICATE_) \
  __builtin_expect(PREDICATE_, false)

/* Is X a power of two? Probably only useful for debugging. */
#define EPSILONGC_IS_POWER_OF_TWO(X) \
  ({ const typeof(X) my_value_of_X__ = (X); \
     (((my_value_of_X__) - 1) & (my_value_of_X__)) == 0; })

#endif // #ifndef EPSILONGC_MACROS_H_
