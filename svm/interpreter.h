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


#ifndef SLOW_VIRTUAL_MACHINE_INTERPRETER_H_
#define SLOW_VIRTUAL_MACHINE_INTERPRETER_H_

#include "instructions.h"
#include "svm.h"
#include "registers.h"

void svm_initialize_program(svm_program_t program);

/* Run the SVM starting from the given state, which includes an instruction
   pointer.  The program must be explicitly initialized before calling this. */
void svm_run_program(svm_register_file_t register_file)
  __attribute__(( hot ));

#endif // #ifndef SLOW_VIRTUAL_MACHINE_INTERPRETER_H_
