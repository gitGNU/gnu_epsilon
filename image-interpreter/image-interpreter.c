/* Image interpreter.

   Copyright (C) 2012  Luca Saiu
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


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "../utility/utility.h"
#include "../runtime/runtime.h"

// FIXME: support GNU standard command-line options
int main(int argc, char **argv){
  if(argc != 2)
    epsilon_fatal("There should only be one argument, the image file.  You provided %i arguments instead", argc - 1);
  
  /* Unmarshal the pair from the file; the pair contains the symbol
     table and a main expression. */
  char *filename = argv[1];
  epsilon_runtime_initialize();
  FILE *f = fopen(filename, "r");
  if(f == NULL)
    epsilon_fatal("could not open %s", filename);
  epsilon_value pair = epsilon_unmarshal_value(f);
  fclose(f);
  
  /* Extract the symbol table, and ensure it's not garbage-collected: */
  volatile epsilon_value symbol_table __attribute__((unused)) = epsilon_value_car(pair);
  
  /* Extract the expression from the pair, and interpret it: */
  epsilon_value main_expression = epsilon_value_cdr(pair);
  epsilon_value environment = epsilon_int_to_epsilon_value(0);
  epsilon_e0_eval_making_stacks(main_expression,
                                environment);
  
  /* Exit with success if we're still alive: */
  return EXIT_SUCCESS;
}
