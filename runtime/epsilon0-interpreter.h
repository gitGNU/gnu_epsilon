/* epsilon0 interpreter in C.

   Copyright (C) 2012, 2015 Luca Saiu
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


#ifndef __EPSILON_EPSILON0_INTERPRETER_H_
#define __EPSILON_EPSILON0_INTERPRETER_H_

#include "data.h"

enum epsilon_opcode {
  e0_variable_opcode = 0,//1000,
  e0_value_opcode,
  e0_bundle_opcode,
  e0_primitive_opcode,
  e0_let_opcode,
  e0_call_opcode,
  e0_call_indirect_opcode,
  e0_if_in_opcode,
  e0_fork_opcode,
  e0_join_opcode,
};

/* Return the list of results. */
epsilon_value epsilon_e0_eval_making_stacks(epsilon_value expression,
                                            epsilon_value local_environment_as_alist);

#endif // #ifndef __EPSILON_EPSILON0_INTERPRETER_H_
