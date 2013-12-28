/* Convenient syntax for C conditional optimizer hints.

   Copyright (C) 2012 Université Paris 13
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


#ifndef EPSILON_UTILITY_HINTS_H_
#define EPSILON_UTILITY_HINTS_H_

#include <stdbool.h>

/* Optimizer branch hints for conditionals: */
#define EPSILON_LIKELY(CONDITION) \
  (__builtin_expect(CONDITION, true))
#define EPSILON_UNLIKELY(CONDITION) \
  (__builtin_expect(CONDITION, false))
#define EPSILON_IF_LIKELY(CONDITION) \
  if(EPSILON_LIKELY(CONDITION))
#define EPSILON_IF_UNLIKELY(CONDITION) \
  if(EPSILON_UNLIKELY(CONDITION))

#endif // #ifndef EPSILON_UTILITY_HINTS_H_
