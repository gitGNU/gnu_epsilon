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


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include "epsilongc_types.h"
#include "epsilongc_debug.h"

struct s{
  int i;
  float f;
  char s[4];
};

static __thread struct s s;

void f(void){
  printf("<%i %f %s>\n", s.i, s.f, s.s);
}

void* go(void *useless){
  s.i = 43;
  s.f = 43.0;
  strcpy(s.s, "t()");
  while(1){
    sleep(1);
    f();
  }
  return NULL;
}

int main(void){
  pthread_t thread;
  s.i = 42;
  s.f = 42.0;
  strcpy(s.s, "m()");
  pthread_create(&thread, NULL, go, NULL);
  while(1){
    sleep(1);
    f();
  }
  return 0;
}
