/* [the file purpose in one line ???]

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
#include <malloc.h>
#include <setjmp.h>
#include "c-wrapper.h"


void foo(void){
  printf("Foo!\n");
  epsilon_pop_epsilon_context();
}

void q(void){
  printf("Hello from q.\n");
}
int identity(int a){
  return a;
}
int average2(int a, int b){
  return (a + b) / 2;
}
int average3(int a, int b, int c){
  return (a + b + c) / 2;
}
int average4(int a, int b, int c, int d){
  return (a + b + c + d) / 2;
}

/* These are defined in assembly: */
void epsilon_routine(const void *stack_minimum, const void *stack_maximum);
int successor_in_assembly(int);

void test(const void *stack_minimum, const void *stack_maximum){
  int x = 42;
  printf("======================================\n");
  printf("x is %i\n", x);
  //  printf("\n");
  printf("successor_in_assembly(x) is %i\n", successor_in_assembly(x));
  printf("======================================\n");
  //epsilon_pop_epsilon_context();
  asm("jmp epsilon_pop_epsilon_context");
}

int main(int argc, char **argv){
  //test();
  epsilon_push_epsilon_context(epsilon_routine);
  //epsilon_push_epsilon_context(test);
  return 0;
}
