/* Debugging support.

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


#ifndef EPSILON_DEBUG_H_
#define EPSILON_DEBUG_H_

#include "config.h"

#ifdef ENABLE_DEBUG
#include <assert.h>
#include "hints.h"
#include "fatal.h"
#endif // #ifdef ENABLE_DEBUG

#ifdef ENABLE_DEBUG
  #define EPSILON_CHECK_ON_DEBUG(assertion) \
    do { EPSILON_IF_UNLIKELY(! (assertion)) \
           epsilon_fatal("EPSILON_CHECK_ON_DEBUG failed"); } while(0)
#else // no debugging: optimize for performance
  #define EPSILON_CHECK_ON_DEBUG(assertion)
    /* do nothing */
#endif // #ifdef ENABLE_DEBUG

#define EPSILON_UNIMPLEMENTED \
  epsilon_fatal("To do: implement this");

#endif // #ifndef EPSILON_DEBUG_H_
