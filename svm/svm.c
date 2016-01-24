/* SVM driver.

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
#include <string.h>
#include "svm.h"
#include "frontend.h"
#include "parser.h"
#include "registers.h"
#include "interpreter.h"
#include "../runtime/runtime.h" // we need to enter/exit epsilon contexts

/* static svm_program_t svm_the_program; */
/* static void svm_run_from_new_context(){ // To do: this should have some reasonable parameters... */
/*   // To do: setup registers, and in particular %sp */
/*   svm_run_program(svm_the_program); */
/* } */

int main(int argc, char **argv){
  char *file_name_or_NULL;
  if(argc == 2){
    if(! strcmp(argv[1], "-"))
      file_name_or_NULL = NULL;
    else
      file_name_or_NULL = argv[1];         
  }
  else if (argc == 1)
    file_name_or_NULL = NULL;
  else
    epsilon_fatal("Too many arguments (%i); there should only be an optional filename", argc - 1);
  svm_initialize_frontend();
   
  //printf("Parsing...\n");
  svm_unresolved_program_t up =
    (file_name_or_NULL == NULL) ? svm_parse_stdin() : svm_parse_file(file_name_or_NULL);
  //printf("Parsed with success.\n");
  //printf("Resolving...\n");
  svm_program_t p = svm_resolve_program(up);
  //printf("Resolved with success (%i instructions, %i global words)\n", (int)p->instruction_no, (int)p->global_word_no);

  //printf("Destroying the unresolved program...\n");
  svm_destroy_unresolved_program(up);
  //printf("Destroyed the unresolved program with success.\n");

  //printf("Running...\n");
  /* Notice that the parameter of epsilon_make_epsilon_thread_context() is an
     SVM program, not an instruction counter.  See the comment in the backend-specific
     C file for details: */

#if 1
  epsilon_epsilon_thread_context_t epsilon_context =
    epsilon_make_epsilon_thread_context(p);
  epsilon_enter_epsilon_context(epsilon_context);
#endif

  //svm_run_program(svm_the_program);
  //printf("Run with success.\n");
  
  //printf("Destroying...\n");
  svm_destroy_program(p);
  //printf("Destroyed with success.\n");
  
  svm_finalize_frontend(); // just for debugging
  printf("Calling exit from svm.c...\n");
  exit(EXIT_SUCCESS);
}
