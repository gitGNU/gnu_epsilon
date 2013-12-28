/* Fatal error-reporting facility.

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


#ifndef EPSILON_UTILITY_FATAL_H_
#define EPSILON_UTILITY_FATAL_H_

#include "config.h" // we need HAVE_GCC

/* We use printf() and exit(): */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

/* Show an error message and exit with a fatal error: */
#ifdef HAVE_GCC
  #define epsilon_fatal(format, ...) \
    do { printf("FATAL ERROR (called from %s:%i): ", __FILE__, __LINE__); \
         printf(format, ## __VA_ARGS__); /* This use of ## is a GCC extension */ \
         printf("\n"); \
         exit(EXIT_FAILURE); } while(0)
#else
  /* A less powerful variant for non-GCC compilers: */ \
  void epsilon_fatal_nongcc(char *format, ...);
  #define epsilon_fatal epsilon_fatal_nongcc
#endif

#define epsilon_impossible() \
  epsilon_fatal("this should never happen")

#define epsilon_unimplemented() \
  epsilon_fatal("not implemented yet")

#endif // #ifndef EPSILON_UTILITY_FATAL_H_
