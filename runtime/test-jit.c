/* A program for testing and developing threaded code.

   Copyright (C) 2015 Luca Saiu
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
#include <string.h>
#include <stdbool.h>
#include <assert.h>

#include "runtime.h"
#include "data.h"

epsilon_value
make_list_1 (epsilon_value x)
{
  return epsilon_value_cons (x,
                             epsilon_int_to_epsilon_value (0));
}
epsilon_value
make_list_2 (epsilon_value x, epsilon_value y)
{
  return epsilon_value_cons (x,
                             epsilon_value_cons (y,
                                                 epsilon_int_to_epsilon_value (0)));
}
epsilon_value
make_list_3 (epsilon_value x, epsilon_value y, epsilon_value z)
{
  return epsilon_value_cons (x,
                             epsilon_value_cons (y,
                                                 epsilon_value_cons (z,
                                                                     epsilon_int_to_epsilon_value (0))));
}

epsilon_value
make_variables_1 (long name1)
{
  return make_list_1 (epsilon_int_to_epsilon_value (name1));
}
epsilon_value
make_variables_2 (long name1, long name2)
{
  return make_list_2 (epsilon_int_to_epsilon_value (name1),
                      epsilon_int_to_epsilon_value (name2));
}
epsilon_value
make_variables_3 (long name1, long name2, long name3)
{
  return make_list_3 (epsilon_int_to_epsilon_value (name1),
                      epsilon_int_to_epsilon_value (name2),
                      epsilon_int_to_epsilon_value (name3));
}

epsilon_value
make_e0_variable (long name)
{
  epsilon_value res = epsilon_gc_allocate_with_epsilon_int_length (3);
  epsilon_store_with_epsilon_int_offset (res, 0, epsilon_int_to_epsilon_value (e0_variable_opcode));
  epsilon_store_with_epsilon_int_offset (res, 2, epsilon_int_to_epsilon_value (name)); // supposed to be a symbol
  return res;
}
epsilon_value
make_e0_value (epsilon_value v)
{
  epsilon_value res = epsilon_gc_allocate_with_epsilon_int_length (3);
  epsilon_store_with_epsilon_int_offset (res, 0, epsilon_int_to_epsilon_value (e0_value_opcode));
  epsilon_store_with_epsilon_int_offset (res, 2, v);
  return res;
}
epsilon_value
make_e0_bundle_1 (epsilon_value exp1)
{
  epsilon_value res = epsilon_gc_allocate_with_epsilon_int_length (3);
  epsilon_store_with_epsilon_int_offset (res, 0, epsilon_int_to_epsilon_value (e0_bundle_opcode));
  epsilon_store_with_epsilon_int_offset (res, 2, make_list_1 (exp1));
  return res;
}
epsilon_value
make_e0_bundle_2 (epsilon_value exp1, epsilon_value exp2)
{
  epsilon_value res = epsilon_gc_allocate_with_epsilon_int_length (3);
  epsilon_store_with_epsilon_int_offset (res, 0, epsilon_int_to_epsilon_value (e0_bundle_opcode));
  epsilon_store_with_epsilon_int_offset (res, 2, make_list_2 (exp1, exp2));
  return res;
}
epsilon_value
make_e0_bundle_3 (epsilon_value exp1, epsilon_value exp2, epsilon_value exp3)
{
  epsilon_value res = epsilon_gc_allocate_with_epsilon_int_length (3);
  epsilon_store_with_epsilon_int_offset (res, 0, epsilon_int_to_epsilon_value (e0_bundle_opcode));
  epsilon_store_with_epsilon_int_offset (res, 2, make_list_3 (exp1, exp2, exp3));
  return res;
}
epsilon_value
make_e0_let_1 (long x1, epsilon_value bound_exp, epsilon_value body_exp)
{
  epsilon_value res = epsilon_gc_allocate_with_epsilon_int_length (5);
  epsilon_store_with_epsilon_int_offset (res, 0, epsilon_int_to_epsilon_value (e0_let_opcode));
  epsilon_store_with_epsilon_int_offset (res, 2, make_variables_1 (x1));
  epsilon_store_with_epsilon_int_offset (res, 3, bound_exp);
  epsilon_store_with_epsilon_int_offset (res, 4, body_exp);
  return res;
}
epsilon_value
make_e0_let_2 (long x1, long x2, epsilon_value bound_exp, epsilon_value body_exp)
{
  epsilon_value res = epsilon_gc_allocate_with_epsilon_int_length (5);
  epsilon_store_with_epsilon_int_offset (res, 0, epsilon_int_to_epsilon_value (e0_let_opcode));
  epsilon_store_with_epsilon_int_offset (res, 2, make_variables_2 (x1, x2));
  epsilon_store_with_epsilon_int_offset (res, 3, bound_exp);
  epsilon_store_with_epsilon_int_offset (res, 4, body_exp);
  return res;
}
epsilon_value
make_e0_let_3 (long x1, long x2, long x3, epsilon_value bound_exp, epsilon_value body_exp)
{
  epsilon_value res = epsilon_gc_allocate_with_epsilon_int_length (5);
  epsilon_store_with_epsilon_int_offset (res, 0, epsilon_int_to_epsilon_value (e0_let_opcode));
  epsilon_store_with_epsilon_int_offset (res, 2, make_variables_3 (x1, x2, x3));
  epsilon_store_with_epsilon_int_offset (res, 3, bound_exp);
  epsilon_store_with_epsilon_int_offset (res, 4, body_exp);
  return res;
}

epsilon_value
make_e0_if_in_1 (epsilon_value discriminand,
                 epsilon_value v1,
                 epsilon_value then_branch,
                 epsilon_value else_branch)
{
  epsilon_value res = epsilon_gc_allocate_with_epsilon_int_length (6);
  epsilon_store_with_epsilon_int_offset (res, 0, epsilon_int_to_epsilon_value (e0_if_in_opcode));
  epsilon_store_with_epsilon_int_offset (res, 2, discriminand);
  epsilon_store_with_epsilon_int_offset (res, 3, make_list_1 (v1));
  epsilon_store_with_epsilon_int_offset (res, 4, then_branch);
  epsilon_store_with_epsilon_int_offset (res, 5, else_branch);
  return res;
}
epsilon_value
make_e0_if_in_2 (epsilon_value discriminand,
                 epsilon_value v1,
                 epsilon_value v2,
                 epsilon_value then_branch,
                 epsilon_value else_branch)
{
  epsilon_value res = epsilon_gc_allocate_with_epsilon_int_length (6);
  epsilon_store_with_epsilon_int_offset (res, 0, epsilon_int_to_epsilon_value (e0_if_in_opcode));
  epsilon_store_with_epsilon_int_offset (res, 2, discriminand);
  epsilon_store_with_epsilon_int_offset (res, 3, make_list_2 (v1, v2));
  epsilon_store_with_epsilon_int_offset (res, 4, then_branch);
  epsilon_store_with_epsilon_int_offset (res, 5, else_branch);
  return res;
}
epsilon_value
make_e0_if_in_3 (epsilon_value discriminand,
                 epsilon_value v1,
                 epsilon_value v2,
                 epsilon_value v3,
                 epsilon_value then_branch,
                 epsilon_value else_branch)
{
  epsilon_value res = epsilon_gc_allocate_with_epsilon_int_length (6);
  epsilon_store_with_epsilon_int_offset (res, 0, epsilon_int_to_epsilon_value (e0_if_in_opcode));
  epsilon_store_with_epsilon_int_offset (res, 2, discriminand);
  epsilon_store_with_epsilon_int_offset (res, 3, make_list_3 (v1, v2, v3));
  epsilon_store_with_epsilon_int_offset (res, 4, then_branch);
  epsilon_store_with_epsilon_int_offset (res, 5, else_branch);
  return res;
}

int
main (void)
{
  epsilon_value epsilon_null = epsilon_int_to_epsilon_value (0);
  /* epsilon_value formals */
  /*   = make_variables_2 (5, 6); */
  /*   //= make_variables_1 (5); */
  epsilon_value expression_variable_5 = make_e0_variable (5);
  epsilon_value expression_variable_6 = make_e0_variable (6);
  epsilon_value expression_variable_10 = make_e0_variable (10);
  epsilon_value expression_variable_11 = make_e0_variable (11);
  epsilon_value expression_value_5 = make_e0_value (epsilon_int_to_epsilon_value (5));
  epsilon_value expression_value_57 = make_e0_value (epsilon_int_to_epsilon_value (57));

  epsilon_value expression_bundle_variable_6_variable_5
    = make_e0_bundle_2 (expression_variable_6, expression_variable_5);

  //epsilon_value body = expression_variable_5;
  //epsilon_value body = expression_bundle_variable_6_variable_5;
  epsilon_value e
    = make_e0_let_2 (10, 11,
                     make_e0_bundle_2 (make_e0_value (epsilon_int_to_epsilon_value (42)),
                                       make_e0_value (epsilon_int_to_epsilon_value (43))),
                     make_e0_variable (11));
    /* = make_e0_let_2 (10, 11, */
    /*                  make_e0_bundle_2 (expression_variable_5, expression_variable_5), */
    /*                  make_e0_variable (10)); */
    /* = make_e0_let_2 (10, 11, */
    /*                  make_e0_bundle_3 (make_e0_variable (6), */
    /*                                    make_e0_value (epsilon_int_to_epsilon_value (20)), */
    /*                                    make_e0_value (epsilon_int_to_epsilon_value (30))), */
    /*                  make_e0_bundle_3 (expression_value_57, */
    /*                                    expression_variable_10, */
    /*                                    expression_value_57)); */
    /* = make_e0_let_1(10, */
    /*                 make_e0_if_in_3 (make_e0_variable (5), */
    /*                                  epsilon_int_to_epsilon_value (100), */
    /*                                  epsilon_int_to_epsilon_value (42), */
    /*                                  epsilon_int_to_epsilon_value (102), */
    /*                                  make_e0_value (epsilon_int_to_epsilon_value (200)), */
    /*                                  make_e0_value (epsilon_int_to_epsilon_value (300))), */
    /*                 make_e0_variable (10)); */
    /* = make_e0_if_in_3 (make_e0_variable (5), */
    /*                    epsilon_int_to_epsilon_value (100), */
    /*                    epsilon_int_to_epsilon_value (43), */
    /*                    epsilon_int_to_epsilon_value (102), */
    /*                    make_e0_bundle_2 (make_e0_value (epsilon_int_to_epsilon_value (200)), */
    /*                                      make_e0_value (epsilon_int_to_epsilon_value (201))), */
    /*                    make_e0_value (epsilon_int_to_epsilon_value (300))); */
  ejit_evaluate_expression (e);

  fprintf (stderr, "Success\n");
  return EXIT_SUCCESS;
}
