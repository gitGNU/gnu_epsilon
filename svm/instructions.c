/* SVM programs.

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
#include "instructions.h"

svm_program_t svm_make_program(int instruction_no, int global_word_no){
  svm_program_t result = (svm_program_t)
    epsilon_xmalloc(sizeof(struct svm_program));
  result->instruction_no = instruction_no;
  result->global_word_no = global_word_no;
  result->instructions = (struct svm_instruction*)
    epsilon_xmalloc(sizeof(struct svm_instruction) * (instruction_no));
  result->global_words = (epsilon_word_or_float *)
    epsilon_xmalloc(sizeof(epsilon_word_or_float) * (global_word_no));
  return result;
}

void svm_destroy_program(svm_program_t program){
  //printf("svm_destroy_program(): the program has %i instructions\n", (int)program->instruction_no);
  //printf("svm_destroy_program(): the program has %i global words\n", (int)program->global_word_no);
  //int i;
  //for(i = 0; i < (int)program->instruction_no; i ++)
  //  printf("Instruction %i at %p\n", i, &(program->instructions[i]));
  //for(i = 0; i < (int)program->global_word_no; i ++)
  //  printf("Word %i at %p: %p\n", i, &(program->global_words[i]), program->global_words[i].non_float_value);
  //printf("Destroying instructions\n");
  free(program->instructions);
  //printf("Destroying global words\n");
  free(program->global_words);
  //printf("Destroying program\n");
  free(program);
}
