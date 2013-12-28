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
#include <unistd.h>
#include "runtime.h"
#include "../utility/utility.h"

void assembly_routine(void){
  printf("  Well, this should be written in assembly...\n");
  printf("  [[Hello from the procedure at %p]]\n", assembly_routine);
  epsilon_leave_epsilon_context();
}

int main(void){
  /* Make a context: */
  epsilon_epsilon_thread_context_t epsilon_context =
    epsilon_make_epsilon_thread_context(assembly_routine);
  
  /* Jump to it: */
  printf("If everything works we will jump to %p\n", assembly_routine);
  epsilon_enter_epsilon_context(epsilon_context);
  
  /* If we're still alive, exit: */
  printf("We're still alive! Exiting with success from main.c.\n");
  exit(EXIT_SUCCESS);
}
