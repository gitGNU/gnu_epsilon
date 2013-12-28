/* Utility for printing SVM configuration-time parameters.

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


#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include "svm.h"
#include "../utility/utility.h"
#include <gc/gc.h> // FIXME: does this need to be here?

const char *yes = "yes";
const char *no = "no";

const char* bool_to_string(const bool b){
  return b ? yes : no;
}

int main(void){
  printf("Dispatch style: %s\n",
#ifdef ENABLE_SVM_THREADING
           "GCC threaded code"
#else
           "portable C switch"
#endif // #ifdef ENABLE_SVM_THREADING
        );
  printf("Endiannes:      %s\n",
#ifdef WORDS_BIGENDIAN
           "big-endian"
#else
           "little-endian"
#endif // #ifdef WORDS_BIGENDIAN
        );
  printf("Verbose SVM:    %s\n",
#ifdef ENABLE_VERBOSE_SVM
           "enabled at configuration time (off by default)"
#else
           "not enabled at configuration time"
#endif // #ifdef ENABLE_VERBOSE_SVM
        );
  printf("short size:       %i\n", (int)SIZEOF_SHORT);
  printf("int size:         %i\n", (int)SIZEOF_INT);
  printf("long size:        %i\n", (int)SIZEOF_LONG);
  printf("long long size:   %i\n", (int)SIZEOF_LONG_LONG);
  printf("pointer size:     %i\n", (int)SIZEOF_VOID_P);
  printf("word size:        %i\n", (int)sizeof(svm_word));

  printf("float size:       %i\n", (int)sizeof(float));
  printf("double size:      %i\n", (int)sizeof(double));
  printf("long double size: %i\n", (int)sizeof(long double));

  printf("Built on:       %s\n", BUILD);
  printf("Hosted on:      %s\n", HOST);
  printf("Targeting:      %s\n", TARGET);
  printf("Host assembly:  %s\n", NATIVE_CPU);

  printf("Boehm GC:       %i.%i", GC_VERSION_MAJOR, GC_VERSION_MINOR);
  #ifdef GC_ALPHA_VERSION
  printf("alpha%i", GC_ALPHA_VERSION);
  #endif // #ifdef GC_ALPHA_VERSION
  printf("\n");
}
