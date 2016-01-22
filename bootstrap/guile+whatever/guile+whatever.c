/* `Whatever' SMOB type for Guile, plus epsilonzero primitives

   Copyright (C) 2012 Luca Saiu
   Copyright (C) 2012 Université Paris 13
   Updated in 2016 by Luca Saiu
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
#include <assert.h> // FIXME: do I need it here?
#include <stdlib.h>
#include <stdbool.h>
#include <libguile.h>
#include "../../utility/utility.h"
#include "../../runtime/runtime.h"

/* Utility
 * *************************** */

SCM whatever_to_guile_fixnum(SCM smob){
  long fixnum = epsilon_value_to_epsilon_int(smob);
  return scm_from_long(fixnum);
}

SCM whatever_to_guile_boolean(SCM smob){
  long fixnum = epsilon_value_to_epsilon_int(smob);
  if (fixnum == 0)
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}

SCM whatever_to_guile_thread(SCM smob){
  return (SCM)epsilon_value_to_thread(smob);
}

extern scm_t_bits epsilon_whatever_tag; // in data-smob.c

/* This is not declared in data.h, as it's only for the SMOB runtime. */


SCM epsilon_guile_sexpression_to_whatever(SCM unboxed_as_scm){
  if(unboxed_as_scm == SCM_EOL){
    return epsilon_int_to_epsilon_value(0);
  } else if(scm_is_bool(unboxed_as_scm)){
    if(unboxed_as_scm == SCM_BOOL_T)
      return epsilon_int_to_epsilon_value(1);
    else
      return epsilon_int_to_epsilon_value(0);
  } else if(scm_is_integer(unboxed_as_scm)){
    long unboxed_as_long = scm_to_long(unboxed_as_scm);
    return epsilon_int_to_epsilon_value(unboxed_as_long);
  } else if(scm_char_p(unboxed_as_scm) == SCM_BOOL_T) {
    return epsilon_guile_sexpression_to_whatever(scm_char_to_integer(unboxed_as_scm));
  } else if(SCM_I_IS_THREAD(unboxed_as_scm)){
    return epsilon_thread_to_epsilon_value(unboxed_as_scm);
    // return SCM_thread_to_smob(unboxed_as_scm);
    // epsilon_runtime_appropriate_fail("epsilon_guile_sexpression_to_whatever [threads are currently broken]");
  } else if(SCM_SMOB_PREDICATE(epsilon_whatever_tag, unboxed_as_scm)){
    return unboxed_as_scm;
  } else{
    scm_wrong_type_arg("guile-sexpression->whatever", 1, unboxed_as_scm);
    /* printf("FATAL ERROR: epsilon_guile_sexpression_to_whatever: unsupported object %s\n", */
    /*        scm_to_locale_string(scm_object_to_string(unboxed_as_scm, */
    /*                                                  scm_variable_ref(scm_c_lookup("write"))))); */
    /* exit(EXIT_FAILURE); */
  }
}

static SCM whatever_p(SCM a){
  if(SCM_SMOB_PREDICATE(epsilon_whatever_tag, a))
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

/* Also accept #f as "no limit" */
SCM epsilon_set_dump_maximum_depth(SCM new_depth){
  if(new_depth == SCM_BOOL_F)
    epsilon_dump_maximum_depth = -1;
  else if(scm_is_integer(new_depth)){
    epsilon_dump_maximum_depth = scm_to_long(new_depth);
  }
  else
    scm_wrong_type_arg("epsilon_set_dump_maximum_depth", 1, new_depth);
  return SCM_UNSPECIFIED;
}

/* Return #f for "no limit" */
SCM epsilon_get_dump_maximum_depth(void){
  if(epsilon_dump_maximum_depth == -1)
    return SCM_BOOL_F;
  else
    return scm_from_long(epsilon_dump_maximum_depth);
}

/* bool e0_is_thread(SCM smob){ */
/*   struct whatever *struct_pointer = smob_to_struct_pointer(smob); */
/*   return struct_pointer->whatever_case == whatever_thread; */
/* } */

SCM epsilon_call_primitive_with_guile_parameters(SCM name_as_guile_sexpression_symbol, SCM actuals_as_guile_sexpression_list){
  /* Lookup the primitive: */
  char *name = scm_to_locale_string(scm_symbol_to_string(name_as_guile_sexpression_symbol));
  struct epsilon_c_primitive_descriptor *descriptor = epsilon_lookup_c_primitive_descriptor(name);
  free(name);

  /* Make a temporaty stack and push arguments onto it: */
  SCM p;
  epsilon_value stack[EPSILON_MAXIMUM_INOUT_DIMENSION];
  epsilon_int actual_no = 0;
  for(p = actuals_as_guile_sexpression_list; ! scm_is_null(p); p = scm_cdr(p))
    stack[actual_no ++] = scm_car(p);
  if(actual_no != descriptor->in_dimension)
    epsilon_runtime_appropriate_fail("call_primitive_with_guile_parameters: in-dimension mismatch");
  
  /* Call the primitive: */
  epsilon_c_primitive_function function_pointer = descriptor->function_pointer;
  function_pointer(stack);
  
  /* Return results: */
  epsilon_int out_dimension = descriptor->out_dimension;
  if(out_dimension == 1)
    return stack[0];
  else{
    epsilon_value results = SCM_EOL;
    epsilon_int i;
    for(i = 0; i < out_dimension; i ++)
      results = scm_cons(stack[i], results);
    return scm_values(results);
  } // else
}

int main(int argc, char **argv){
  epsilon_runtime_initialize (argc, argv);
  //scm_boot_guile (argc, argv, inner_main, 0);

  scm_c_define_gsubr ("whatever-call-with-guile-parameters", 2, 0, 0, epsilon_call_primitive_with_guile_parameters);//FIXME: rename
  scm_c_define_gsubr ("guile-sexpression->whatever", 1, 0, 0, epsilon_guile_sexpression_to_whatever);
  scm_c_define_gsubr ("set-whatever-dump-maximum-depth!", 1, 0, 0, epsilon_set_dump_maximum_depth);
  scm_c_define_gsubr ("get-whatever-dump-maximum-depth", 0, 0, 0, epsilon_get_dump_maximum_depth);

  scm_c_define_gsubr ("whatever?", 1, 0, 0, whatever_p);
  scm_c_define_gsubr ("whatever->guile-fixnum", 1, 0, 0, whatever_to_guile_fixnum);
  scm_c_define_gsubr ("whatever->guile-boolean", 1, 0, 0, whatever_to_guile_boolean);
  scm_c_define_gsubr ("whatever->guile-thread", 1, 0, 0, whatever_to_guile_thread);

  scm_shell (argc, argv);
  return 0; /* never reached */
}
