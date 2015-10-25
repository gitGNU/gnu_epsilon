/* A header #includ'ing the other utility headers.

   Copyright (C) 2012 Université Paris 13
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


#ifndef EPSILON_UTILITY_H_
#define EPSILON_UTILITY_H_

/* #include all utility headers: */
#include "types.h"
#include "hints.h"
#include "debug.h"
#include "malloc.h"
#include "fatal.h"
#include "dynamic-array.h"
#include "list.h"
#include "unboxed-hash.h"
#include "string-hash.h"
#include "stack.h"

#endif // #ifndef EPSILON_UTILITY_H_

long
epsilon_min_long (long a, long b);
long
epsilon_max_long (long a, long b);
