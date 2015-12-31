/* epsilon0 interpreter in C.

   Copyright (C) 2012 Université Paris 13
   Copyright (C) 2012, 2015 Luca Saiu
   Written by Luca Saiu
   Updated in 2013, 2014 and 2015 by Luca Saiu

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
#include "../utility/utility.h"
#include "epsilon0-interpreter.h"
#include "c-primitives.h"
#include <libguile.h> // FIXME: remove this dependency

#define EPSILON_EVAL_STACK_ELEMENT_NO 1000000

/* An imperative environment is a stack of struct
   imperative_environment_item objects.  We can push for binding, and
   look at previous elements when searching.  An element whose name is
   NULL is used as a terminator marker, enabling to use the same stack
   for different environments, accessed in a LIFO fashion.  Notice
   that the stack pointer is not held within any structure: it must be
   threaded aepsilon_int explicitly, when needed. */
struct imperative_environment_item{
  epsilon_value name; // name is a SMOB, or name = NULL an end marker.
  epsilon_value value; // a SMOB, or unspecified when name is NULL
}; // union

typedef struct imperative_environment_item* imperative_environment_t;

/* Take an environment and return the "updated" version of it; of
   course the result shares memory with the input. */
imperative_environment_t epsilon_bind_in_environment(imperative_environment_t e, epsilon_value name, epsilon_value value){
  e->name = name;
  e->value = value;
  return e + 1;
}
imperative_environment_t epsilon_mark_environment_end(imperative_environment_t e){
  e->name = NULL;
  return e + 1;
}
imperative_environment_t epsilon_environment_unbind_all_locals(imperative_environment_t e){
  if(e[-1].name == NULL)
    return e;
  else
    return epsilon_environment_unbind_all_locals(e - 1);
}
imperative_environment_t epsilon_bind_in_environment_from_value_stack(imperative_environment_t e, epsilon_value name_list, epsilon_value *value_stack){
  /* We bind as many values as the number of elements in name_list:
     this particularly useful when we want to silently ignore excess
     actuals. */
  if(epsilon_value_is_null(name_list))
    return e;
  else
    return epsilon_bind_in_environment_from_value_stack(epsilon_bind_in_environment(e, epsilon_value_car(name_list), *value_stack),
                                    epsilon_value_cdr(name_list),
                                    value_stack + 1);
}

static bool epsilon_has_environment(imperative_environment_t e, epsilon_value name){
  imperative_environment_t p;
  for(p = e - 1; p->name != NULL; p --)
    if(epsilon_value_eq(name, p->name))
      return true;
  return false;
}

epsilon_value epsilon_lookup_environment(imperative_environment_t e, epsilon_value name){
  imperative_environment_t p;
  for(p = e - 1; p->name != NULL; p --)
    if(epsilon_value_eq(name, p->name))
      return p->value;
  epsilon_runtime_appropriate_fail("epsilon_lookup_environment [unbound variable]");
}

static bool epsilon_belongs_to(epsilon_value x, epsilon_value list){
  if(epsilon_value_is_null(list))
    return false;
  else if(epsilon_value_eq(epsilon_value_car(list), x))
    return true;
  else
    return epsilon_belongs_to(x, epsilon_value_cdr(list));
}

static void epsilon_print_e0_symbol(epsilon_value name_as_whatever){
  printf("About the symbol\n");
  char *s = epsilon_symbol_to_malloced_char_star(name_as_whatever);
  printf(">%s<:\n", s);
  free(s);
}

static epsilon_int epsilon_e0_eval_with_stack(epsilon_value expression, imperative_environment_t local_environment, epsilon_value *value_stack, bool tail)
  __attribute__(( hot ));
static epsilon_value e0_eval_out_dimension_1_expression_with_stack(epsilon_value expression, imperative_environment_t local_environment, epsilon_value *value_stack)
  __attribute__(( hot ));
static epsilon_int e0_eval_expressions_with_stack(epsilon_value expressions, imperative_environment_t local_environment, epsilon_value *value_stack)
  __attribute__(( hot ));

// As far as I see, #pragma GCC optimize uses the given options as *the only*
// optimization options.  That's not what I want.
/* /\* It's absolutely essential that the interpreter is compiled with */
/*    tail-calls enabled, even at -O1: *\/ */
/* #pragma GCC optimize ("-foptimize-sibling-calls") */
//#pragma GCC optimize ("-fno-lto")

static epsilon_value e0_eval_out_dimension_1_expression_with_stack(epsilon_value expression, imperative_environment_t local_environment, epsilon_value *value_stack){
  epsilon_int expression_result_no =
    epsilon_e0_eval_with_stack(expression, local_environment, value_stack, false);
  if(expression_result_no != 1){
    printf("Expression has dimension %li instead of 1\n", (long)expression_result_no);
    epsilon_runtime_appropriate_fail("e0_eval_out_dimension_1_expression_with_stack");
  }
  return *value_stack;
}

/* The parameters is actually a epsilon_value buffer whose first element
   contains the procedure name, while and the other elements contain
   the actuals. */
static epsilon_value e0_fork_body_do_the_real_work(epsilon_value data) __attribute__((noinline,noclone));
static epsilon_value e0_fork_body_do_the_real_work(epsilon_value data){
  epsilon_value procedure_name =
    epsilon_load_with_epsilon_int_offset(data, 0);
  epsilon_value procedure_formals =
    epsilon_load_with_epsilon_int_offset(procedure_name, 3);
  epsilon_int formal_no = epsilon_value_length(procedure_formals);
  
  /* Preapare stacks, and copy actuals there: */
  epsilon_value value_stack[EPSILON_EVAL_STACK_ELEMENT_NO];
  struct imperative_environment_item imperative_environment_stack[EPSILON_EVAL_STACK_ELEMENT_NO];
  imperative_environment_t imperative_environment = epsilon_mark_environment_end(imperative_environment_stack);
  value_stack[0] = epsilon_thread_to_epsilon_value(epsilon_current_thread());
  int i;
  for(i = 1; i < formal_no; i ++)
    value_stack[i] = epsilon_load_with_epsilon_int_offset(data, i);
  epsilon_gc_destroy(data);
  imperative_environment = epsilon_bind_in_environment_from_value_stack(imperative_environment, procedure_formals, value_stack);
  
  /* Evaluate the procedure body: */
  epsilon_value procedure_body = epsilon_load_with_epsilon_int_offset(procedure_name, 4);
  epsilon_int result_no = epsilon_e0_eval_with_stack(procedure_body, imperative_environment, value_stack, true);
  
  /* The result (there should be at least one) is on the stack: */
  if(result_no == 0)
    printf("WARNING: e0_fork_body: wrong out-dimension; the future will return some trash\n");
  return value_stack[0];
}

static epsilon_value e0_fork_body(epsilon_value data) __attribute__((noinline,noclone));
static epsilon_value e0_fork_body(epsilon_value data){
  return epsilon_call_with_initialized_gc(e0_fork_body_do_the_real_work, data);
}

/* The result is the number of results pushed on the stack: */
static epsilon_int epsilon_e0_eval_with_stack(epsilon_value expression, imperative_environment_t local_environment, epsilon_value *value_stack, bool tail){
  epsilon_int opcode = epsilon_value_to_epsilon_int(epsilon_load_with_epsilon_int_offset(expression, 0));
  switch(opcode){
  case e0_variable_opcode:{
    // fprintf (stderr, "%s: variable\n", __func__);
    epsilon_value name = epsilon_load_with_epsilon_int_offset(expression, 2);
    if(epsilon_has_environment(local_environment, name)){
      *value_stack = epsilon_lookup_environment(local_environment, name);
      return 1;
    }
    else{
      if(epsilon_value_to_epsilon_int(epsilon_load_with_epsilon_int_offset(name, 1)) == 0){
        epsilon_print_e0_symbol(name);
        epsilon_runtime_appropriate_fail("epsilon_e0_eval_with_stack [unbound identifier]");
      }
      epsilon_value global_value = epsilon_load_with_epsilon_int_offset(name, 2);
      *value_stack = global_value;
      return 1;
    } // else
  }
  case e0_value_opcode:{
    // fprintf (stderr, "%s: value\n", __func__);
    *value_stack = epsilon_load_with_epsilon_int_offset(expression, 2);
    return 1;
  }
  case e0_bundle_opcode:{
    // fprintf (stderr, "%s: bundle\n", __func__);
    epsilon_value items = epsilon_load_with_epsilon_int_offset(expression, 2);
    return e0_eval_expressions_with_stack(items, local_environment, value_stack);
  }
  case e0_primitive_opcode:{
    // fprintf (stderr, "%s: primitive\n", __func__);
    epsilon_value primitive_name = epsilon_load_with_epsilon_int_offset(expression, 2);
    epsilon_value primitive_descriptor = epsilon_load_with_epsilon_int_offset(primitive_name, 7);
    if(epsilon_value_is_zero(primitive_descriptor))
      {
        epsilon_print_e0_symbol (primitive_name);
        epsilon_runtime_appropriate_fail ("epsilon_e0_eval_with_stack [primitive: unbound primitive name]");
      }
    epsilon_int primitive_in_dimension = epsilon_value_to_epsilon_int(epsilon_load_with_epsilon_int_offset(primitive_descriptor, 0));
    epsilon_value actuals = epsilon_load_with_epsilon_int_offset(expression, 3);
    epsilon_int actual_value_no = e0_eval_expressions_with_stack(actuals, local_environment, value_stack);
    if(primitive_in_dimension != actual_value_no)
      epsilon_runtime_appropriate_fail("epsilon_e0_eval_with_stack [primitive: in-dimension mismatch]");
    epsilon_int primitive_index = epsilon_value_to_epsilon_int(epsilon_load_with_epsilon_int_offset(primitive_descriptor, 4));
    epsilon_call_c_primitive_by_index(primitive_index, value_stack);
    epsilon_int primitive_out_dimension = epsilon_value_to_epsilon_int(epsilon_load_with_epsilon_int_offset(primitive_descriptor, 1));
    return primitive_out_dimension;
  }
  case e0_let_opcode:{
    // fprintf (stderr, "%s: let\n", __func__);
    epsilon_value bound_expression = epsilon_load_with_epsilon_int_offset(expression, 3);
    epsilon_int bound_expression_result_no =
      epsilon_e0_eval_with_stack(bound_expression, local_environment, value_stack, false);
    epsilon_value bound_variables = epsilon_load_with_epsilon_int_offset(expression, 2);
    epsilon_int bound_variable_no = epsilon_value_length(bound_variables);
    if(bound_variable_no > bound_expression_result_no){
      printf("%i bound variables for %i-dimension expression\n", (int)bound_variable_no, (int)bound_expression_result_no);
      epsilon_value p;
      for(p = bound_variables; ! epsilon_value_is_null(p); p = epsilon_value_cdr(p))
        epsilon_print_e0_symbol(epsilon_value_car(p));
      epsilon_runtime_appropriate_fail("epsilon_e0_eval_with_stack [let: too many bound variables]");
    }
    local_environment = epsilon_bind_in_environment_from_value_stack(local_environment, bound_variables, value_stack);
    epsilon_value body_expression = epsilon_load_with_epsilon_int_offset(expression, 4);
    return epsilon_e0_eval_with_stack(body_expression, local_environment, value_stack, tail);
  }
  case e0_call_opcode:{
    // fprintf (stderr, "%s: call\n", __func__);
#ifdef EPSILON_RUNTIME_SMOB
    SCM_TICK; // Guile can be interrupted here; this single epsilon_value_TICK should be reachable periodically
#endif // EPSILON_RUNTIME_SMOB
    epsilon_value procedure_name = epsilon_load_with_epsilon_int_offset(expression, 2);
    epsilon_value actuals = epsilon_load_with_epsilon_int_offset(expression, 3);
    epsilon_int actual_value_no = e0_eval_expressions_with_stack(actuals, local_environment, value_stack);
    epsilon_value procedure_formals = epsilon_load_with_epsilon_int_offset(procedure_name, 3);
    epsilon_value procedure_body = epsilon_load_with_epsilon_int_offset(procedure_name, 4);
    if(epsilon_value_is_zero(procedure_body)){
      printf("About the procedure named:\n"); epsilon_print_e0_symbol(procedure_name);
      epsilon_runtime_appropriate_fail("epsilon_e0_eval_with_stack [call: unbound procedure name]");
    }
    epsilon_int formal_no = epsilon_value_length(procedure_formals);
    //if(formal_no > actual_value_no){
    if(formal_no != actual_value_no){
      printf("About the procedure named:\n"); epsilon_print_e0_symbol(procedure_name);
      printf("(formal_no: %i; actual_no: %i)\n", (int)formal_no, (int)actual_value_no);
      epsilon_runtime_appropriate_fail("epsilon_e0_eval_with_stack [call: in-dimension mismatch]");
    }
    if(tail)
      local_environment = epsilon_environment_unbind_all_locals(local_environment);
    else
      local_environment = epsilon_mark_environment_end(local_environment);
    local_environment = epsilon_bind_in_environment_from_value_stack(local_environment, procedure_formals, value_stack);
    return epsilon_e0_eval_with_stack(procedure_body, local_environment, value_stack, true);
  }
  case e0_call_indirect_opcode:{
    // fprintf (stderr, "%s: call-indirect\n", __func__);
    epsilon_value procedure_expression = epsilon_load_with_epsilon_int_offset(expression, 2);
    epsilon_value procedure_name =
      e0_eval_out_dimension_1_expression_with_stack(procedure_expression, local_environment, value_stack);
    epsilon_value actuals = epsilon_load_with_epsilon_int_offset(expression, 3);
    epsilon_int actual_value_no = e0_eval_expressions_with_stack(actuals, local_environment, value_stack);
    epsilon_value procedure_formals = epsilon_load_with_epsilon_int_offset(procedure_name, 3);
    epsilon_value procedure_body = epsilon_load_with_epsilon_int_offset(procedure_name, 4);
    if(epsilon_value_is_zero(procedure_body)){
      printf("About the procedure named:\n"); epsilon_print_e0_symbol(procedure_name);
      epsilon_runtime_appropriate_fail("epsilon_e0_eval_with_stack [call-indirect: unbound procedure name]");
    }
    epsilon_int formal_no = epsilon_value_length(procedure_formals);
    if(formal_no > actual_value_no){
    //if(formal_no != actual_value_no){
      printf("About the procedure named:\n"); epsilon_print_e0_symbol(procedure_name);
      printf("(formal_no: %i; actual_no: %i)\n", (int)formal_no, (int)actual_value_no);
      epsilon_runtime_appropriate_fail("epsilon_e0_eval_with_stack [call-indirect: in-dimension mismatch]");
    }
    if(tail)
      local_environment = epsilon_environment_unbind_all_locals(local_environment);
    else
      local_environment = epsilon_mark_environment_end(local_environment);
    local_environment = epsilon_bind_in_environment_from_value_stack(local_environment, procedure_formals, value_stack);
    return epsilon_e0_eval_with_stack(procedure_body, local_environment, value_stack, true);
  }
  case e0_if_in_opcode:{
    // fprintf (stderr, "%s: if-in\n", __func__);
    epsilon_value discriminand_expression = epsilon_load_with_epsilon_int_offset(expression, 2);
    epsilon_value discriminand_value =
      e0_eval_out_dimension_1_expression_with_stack(discriminand_expression,
                                                    local_environment,
                                                    value_stack);
    epsilon_value constants = epsilon_load_with_epsilon_int_offset(expression, 3);
    if(epsilon_belongs_to(discriminand_value, constants))
      return epsilon_e0_eval_with_stack(epsilon_load_with_epsilon_int_offset(expression, 4), local_environment, value_stack, tail);
    else
      return epsilon_e0_eval_with_stack(epsilon_load_with_epsilon_int_offset(expression, 5), local_environment, value_stack, tail);
  }
  case e0_fork_opcode:{
    // fprintf (stderr, "%s: fork\n", __func__);
    epsilon_value procedure_name = epsilon_load_with_epsilon_int_offset(expression, 2);
    epsilon_value actuals = epsilon_load_with_epsilon_int_offset(expression, 3);
    epsilon_int actual_value_no = e0_eval_expressions_with_stack(actuals, local_environment, value_stack);
    epsilon_value procedure_formals = epsilon_load_with_epsilon_int_offset(procedure_name, 3);
    epsilon_value procedure_body = epsilon_load_with_epsilon_int_offset(procedure_name, 4);
    if(epsilon_value_is_zero(procedure_body)){
      printf("About the procedure named:\n"); epsilon_print_e0_symbol(procedure_name);
      epsilon_runtime_appropriate_fail("epsilon_e0_eval_with_stack [fork: unbound procedure name]");
    }
    epsilon_int formal_no = epsilon_value_length(procedure_formals);
    if(formal_no > (/* thread id */ 1 + actual_value_no)){
    //if(formal_no != actual_value_no){
      printf("About the procedure named:\n"); epsilon_print_e0_symbol(procedure_name);
      printf("(formal_no: %i; actual_no: %i)\n", (int)formal_no, (int)actual_value_no);
      epsilon_runtime_appropriate_fail("epsilon_e0_eval_with_stack [fork: in-dimension mismatch]");
    }
    /* Make a temporary data structure (known to the GC, to have at
       least a copy of parameters always referenced) to pass
       parameters to the new thread.  The first element is the
       procedure name. */
    size_t thread_argument_size = formal_no; /* minus thread id, plus procedure name */
    epsilon_value thread_arguments = epsilon_gc_allocate_with_epsilon_int_length(thread_argument_size);
    epsilon_store_with_epsilon_int_offset(thread_arguments, 0, procedure_name);
    int i;
    for(i = 1; i < thread_argument_size; i ++)
      epsilon_store_with_epsilon_int_offset(thread_arguments, i, value_stack[i - 1]);
    epsilon_thread thread = epsilon_make_thread(e0_fork_body, thread_arguments);
    
    /* Make the thread, and return: */
    *value_stack = epsilon_thread_to_epsilon_value(thread);
    return 1;
  }
  case e0_join_opcode:{
    // fprintf (stderr, "%s: join\n", __func__);
    epsilon_value future_expression = epsilon_load_with_epsilon_int_offset(expression, 2);
    epsilon_value future_value __attribute__((unused)) =
      e0_eval_out_dimension_1_expression_with_stack(future_expression,
                                                    local_environment,
                                                    value_stack);
    epsilon_thread thread = epsilon_value_to_thread(future_value);
    epsilon_value result = epsilon_join_thread(thread);
    
    *value_stack = result;
    return 1;
  }
  default:
    printf("About the expression case >%i<\n", (int)opcode);
    epsilon_runtime_appropriate_fail("epsilon_e0_eval_with_stack [unknown expression case]");
  }; // switch
  epsilon_runtime_appropriate_fail("epsilon_e0_eval_with_stack [impossible]");
}

/* Each expression in the list must return a 1-dimension bundle;
   return the result number: */
static epsilon_int e0_eval_expressions_with_stack(epsilon_value expressions, imperative_environment_t local_environment, epsilon_value *value_stack){
  epsilon_int result = 0;
  epsilon_value p;
  for(p = expressions; ! epsilon_value_is_null(p); p = epsilon_value_cdr(p)){
    *value_stack = e0_eval_out_dimension_1_expression_with_stack(epsilon_value_car(p), local_environment, value_stack);
    value_stack ++;
    result ++;
  }
  return result;
}

epsilon_value epsilon_e0_eval_making_stacks(epsilon_value expression, epsilon_value local_environment_as_alist){
  // FIXME: this would be the right thing to do, but it interferes with GC roots in
  // Guile 1, which is still the most convenient one to use.
#if 0
  /* Allocate imperative stacks.  By doing this on the heap I no longer require
     extreme ulimit -s settings. */
  epsilon_value *value_stack
    = epsilon_xmalloc (sizeof (epsilon_value) * EPSILON_EVAL_STACK_ELEMENT_NO);
  struct imperative_environment_item *imperative_environment_stack
    = epsilon_xmalloc (sizeof (struct imperative_environment_item)
                       * EPSILON_EVAL_STACK_ELEMENT_NO);
  GC_add_roots (value_stack, value_stack + EPSILON_EVAL_STACK_ELEMENT_NO);
  GC_add_roots (imperative_environment_stack,
                imperative_environment_stack + EPSILON_EVAL_STACK_ELEMENT_NO);
#else
  epsilon_value value_stack [EPSILON_EVAL_STACK_ELEMENT_NO];
  struct imperative_environment_item
    imperative_environment_stack [EPSILON_EVAL_STACK_ELEMENT_NO];
#endif // #if 0

  /* Add local environment bindings: */
  imperative_environment_t imperative_environment = epsilon_mark_environment_end(imperative_environment_stack);
  epsilon_value p;
  for(p = local_environment_as_alist; ! epsilon_value_is_null(p); p = epsilon_value_cdr(p)){
    epsilon_value first_pair = epsilon_value_car(p);
    imperative_environment = epsilon_bind_in_environment(imperative_environment, epsilon_value_car(first_pair), epsilon_value_cdr(first_pair));
  }
  /* Use imperative stacks for doing the heavyweight evaluation: */
  epsilon_int result_no = epsilon_e0_eval_with_stack(expression, imperative_environment, value_stack, true);

  /* Make a list of all results, to be returned: */
  epsilon_value result = epsilon_int_to_epsilon_value(0);
  epsilon_int i;
  for(i = result_no - 1; i >= 0; i --)
    result = epsilon_value_cons(value_stack[i], result);

  // See the comment above.x
#if 0
  /* Destroy stacks. */
  GC_remove_roots (value_stack, value_stack + EPSILON_EVAL_STACK_ELEMENT_NO);
  GC_remove_roots (imperative_environment_stack,
                imperative_environment_stack + EPSILON_EVAL_STACK_ELEMENT_NO);
  free (value_stack);
  free (imperative_environment_stack);
#endif // #if 0

  return result;
}
