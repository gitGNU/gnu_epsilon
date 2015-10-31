/* JIT engine for epsilon0, currently based on threaded code.

   Copyright (C) 2015  Luca Saiu
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
#include <assert.h>

#include "../utility/utility.h"
#include "runtime.h"
#include "config.h"

//#undef ENABLE_JIT_THREADING

struct ejit_thread_state
{
  //epsilon_value *stack_overtop;
  epsilon_value *stack_bottom;
  epsilon_value *frame_bottom;
  epsilon_word *return_stack_overtop;
  epsilon_word *return_stack_bottom;
};

#define STACK_ELEMENT_NO        10000
#define RETURN_STACK_ELEMENT_NO 5000

ejit_thread_state_t
ejit_make_thread_state (void)
{
  ejit_thread_state_t res = epsilon_xmalloc (sizeof (struct ejit_thread_state));
  res->frame_bottom
    = res->stack_bottom
    //= res->stack_overtop
    = epsilon_xmalloc (STACK_ELEMENT_NO * sizeof (epsilon_word));
  res->return_stack_bottom
    = res->return_stack_overtop
    = epsilon_xmalloc (RETURN_STACK_ELEMENT_NO * sizeof (epsilon_word));

  /* Fill the stack with epsilon 0 values, so that it can be safely scanned by
     the GC.  There's no need to do the same with the return stack, which
     never contains any epsilon value. */
  const epsilon_value epsilon_value_zero
    = epsilon_int_to_epsilon_value (0);
  int i;
  for (i = 0; i < STACK_ELEMENT_NO; i ++)
    res->stack_bottom[i] = epsilon_value_zero;

  // FIXME: make the stack a GC root.

  return res;
}

void
ejit_destroy_thread_state (const ejit_thread_state_t s)
{
  free (s->stack_bottom);
  free (s->return_stack_bottom);
  free (s);
}

/* void */
/* ejit_push_on_thread_state (const ejit_thread_state_t s, epsilon_value v) */
/* { */
/*   * (s->stack_overtop ++) = v; */
/* } */

void
print_values (ejit_thread_state_t s)
{
  int i;
  epsilon_value *p;
  for (p = s->frame_bottom, i = 0;
       i < 6; // FIXME: take frame height as a parameter
       p ++, i ++)
#ifdef EPSILON_RUNTIME_UNTAGGED
    fprintf (stderr, "  %2i. %p: %li, %p\n", i, p, (long)*p, *p);
#else
    if (epsilon_is_pointer (*p))
      fprintf (stderr, "  %2i. %p: <pointer>\n", i, p);
    else
      fprintf (stderr, "  %2i. %p: %li\n", i, p, epsilon_value_to_epsilon_int (*p));
#endif // EPSILON_RUNTIME_UNTAGGED
}


enum ejit_opcode_enum
  {
    ejit_opcode_copy_1,
    ejit_opcode_copy_2,
    ejit_opcode_copy_3,
    ejit_opcode_copy_4,
    ejit_opcode_copy_5,
    ejit_opcode_copy_and_tail_call,
    ejit_opcode_copy_and_tail_call_0,
    ejit_opcode_copy_and_tail_call_1,
    ejit_opcode_copy_and_tail_call_2,
    ejit_opcode_copy_and_tail_call_3,
    ejit_opcode_copy_and_tail_call_4,
    ejit_opcode_copy_and_tail_call_5,
    ejit_opcode_jump,
    ejit_opcode_jump_if_equal_boxed,
    ejit_opcode_jump_if_equal_unboxed,
    ejit_opcode_global,
    ejit_opcode_literal_boxed,
    ejit_opcode_literal_unboxed,
    ejit_opcode_nontail_call,
    ejit_opcode_primitive,
    //ejit_opcode_procedure_prolog,
    ejit_opcode_return_0,
    ejit_opcode_return_1,
    ejit_opcode_return_2,
    ejit_opcode_return_3,
    ejit_opcode_return_4,
    ejit_opcode_return_5,
    ejit_opcode_return,
    ejit_opcode_set_literals,

    ejit_opcode_end,
    ejit_opcode_last, // not a real instruction.
  };
typedef enum ejit_opcode_enum ejit_opcode_t;

typedef void* ejit_label_t;

union ejit_instruction
{
  ejit_opcode_t opcode;
  epsilon_word literal;
  ejit_label_t label;
};
typedef union ejit_instruction ejit_instruction_t;

struct ejit_code
{
  /* An array of instructions or literal instruction arguments, sequentially
     allocated in the C heap. */
  ejit_instruction_t* instructions;
  size_t instruction_no;

  /* An epsilon buffer holding the epsilon literals referred in the code.  The
     constants aren't directly embedded within the instruction because that
     would interfere with moving GCs; hence instructions needing epsilon
     literals as parameters only refer them thru *indices* of elements in this
     array.  Non-epsilon literals (offsets or other integers, foreign pointers)
     aren't here: they're included within instructions for efficiency. */
  epsilon_value literals; // FIXME: make this a GC root

  /* Only used for debugging (FIXME: but so cheap that I might alway keep it
     enabled). */
  int initialized;
};

/* The internal temporary state of the compiler. */
struct ejit_compiler_state
{
  struct epsilon_stack instructions;
  struct epsilon_stack literal_stack;
  struct epsilon_stack locals;
  //long target_slot;
};
typedef struct ejit_compiler_state* ejit_compiler_state_t;

static void
ejit_initialize_compiler_state (ejit_compiler_state_t s)
{
  epsilon_stack_initialize (& s->instructions);
  epsilon_stack_initialize (& s->literal_stack);
  epsilon_stack_initialize (& s->locals);
  //s->target_slot = 0;
}
/* static void */
/* ejit_finalize_compiler_state (ejit_compiler_state_t s) */
/* { */
/*   epsilon_stack_finalize (& s->instructions); */
/*   epsilon_stack_finalize (& s->literal_stack); */
/*   epsilon_stack_finalize (& s->locals); */
/* } */

/* Return the index of the pushed instruction. */
static int
ejit_push_instruction (ejit_compiler_state_t s, long n)
{
  int res = s->instructions.element_no;
  epsilon_stack_push (& s->instructions, (epsilon_value) (long) n);
  return res;
}
static void
ejit_push_non_epsilon_literal (ejit_compiler_state_t s, epsilon_word literal)
{
  epsilon_stack_push (& s->instructions, literal);
}
static void
ejit_push_epsilon_literal (ejit_compiler_state_t s, epsilon_value literal)
{
  long literal_stack_height = s->literal_stack.element_no;
  epsilon_stack_push (& s->literal_stack, literal);
  epsilon_stack_push (& s->instructions, (epsilon_word)literal_stack_height);
}

#define CASE_SET_LABEL(name, arity) \
  case ejit_opcode_ ## name: \
  /* PRINT_JIT_DEBUGGING */fprintf(stderr, "Initializing: %3i. %s\n", (int)(p - instructions), #name); \
    p->label = && label_ ## name; \
    p += (arity) + 1;             \
    break

#ifdef ENABLE_JIT_THREADING
#define GOTO(target)                            \
  goto* (ip = (ejit_instruction_t*)(target))->label
#else
#define GOTO(target)                            \
  /*do*/ { ip = (target); continue; } /*while (0)*/
#endif // #ifdef ENABLE_JIT_THREADING

#ifdef ENABLE_JIT_THREADING
#define FIRST \
  goto* ip->label
#else
#define FIRST \
  do {} while (0) /* do nothing */
#endif // #ifdef ENABLE_JIT_THREADING

#ifdef ENABLE_JIT_THREADING
#define NEXT                                    \
  goto* (++ ip)->label
#else
#define NEXT                                    \
  /*do*/ { ip ++; continue; } /*while (0)*/
#endif // #ifdef ENABLE_JIT_THREADING

#ifdef ENABLE_VERBOSE_JIT_DEBUG
#define PRINT_JIT_DEBUGGING \
  fprintf
#else
#define PRINT_JIT_DEBUGGING(...) \
  /* nothing */
#endif // #ifdef ENABLE_VERBOSE_JIT_DEBUG

#ifdef ENABLE_JIT_THREADING
#define LABEL(name) \
  label_ ## name:   \
  __asm__ ("# Instruction "#name".\n"); \
  PRINT_JIT_DEBUGGING (stderr, "%i. %s\n", (int)(ip - instructions), #name); \
  /* printf("  ip == %p, ip->label == %p, label_%s == %p\n", ip, ip->label, #name, && label_ ## name);  */\
  /* printf("  Heights at entry: %i %i\n", (int)(state.stack_overtop - state.stack_bottom), (int)(state.return_stack_overtop - state.return_stack_bottom)); */
#else
#define LABEL(name) \
  case ejit_opcode_ ## name: \
    PRINT_JIT_DEBUGGING (stderr, "%i. %s\n", (int)(ip - instructions), #name);
#endif // #ifdef ENABLE_JIT_THREADING

static void
jit_copy_slots (epsilon_value *frame_bottom,
                long first_source_slot, long first_target_slot, size_t slot_no)
  __attribute__ ((hot, always_inline));

static void
jit_copy_slots (epsilon_value *frame_bottom,
                long first_source_slot, long first_target_slot, size_t slot_no)
{
  long i;
  for (i = 0; i < slot_no; i ++)
    frame_bottom[first_target_slot + i] = frame_bottom[first_source_slot + i];
}

static void
ejit_initialize_or_run_code (int initialize, ejit_code_t code,
                             ejit_thread_state_t state)
  __attribute__((__noinline__, __noclone__, __hot__));

static void
ejit_initialize_or_run_code (int initialize, ejit_code_t code,
                             ejit_thread_state_t original_state)
{
  const epsilon_value epsilon_value_zero
    __attribute__ ((unused))
    = epsilon_int_to_epsilon_value (0);
  const ejit_instruction_t *instructions = code->instructions;
  const ejit_instruction_t *past_last __attribute__((__unused__))
    = instructions + code->instruction_no;
  if (initialize)
    {
#ifdef ENABLE_JIT_THREADING
      ejit_instruction_t *p = code->instructions;
      for (p = code->instructions; p < past_last; )
        {
          switch (p->opcode)
            {
              CASE_SET_LABEL(copy_1, 2);
              CASE_SET_LABEL(copy_2, 2);
              CASE_SET_LABEL(copy_3, 2);
              CASE_SET_LABEL(copy_4, 2);
              CASE_SET_LABEL(copy_5, 2);
              CASE_SET_LABEL(copy_and_tail_call, 3);
              CASE_SET_LABEL(copy_and_tail_call_0, 2);
              CASE_SET_LABEL(copy_and_tail_call_1, 2);
              CASE_SET_LABEL(copy_and_tail_call_2, 2);
              CASE_SET_LABEL(copy_and_tail_call_3, 2);
              CASE_SET_LABEL(copy_and_tail_call_4, 2);
              CASE_SET_LABEL(copy_and_tail_call_5, 2);
              CASE_SET_LABEL(jump, 1);
              CASE_SET_LABEL(jump_if_equal_boxed, 3);
              CASE_SET_LABEL(jump_if_equal_unboxed, 3);
              CASE_SET_LABEL(global, 2);
              CASE_SET_LABEL(literal_boxed, 2);
              CASE_SET_LABEL(literal_unboxed, 2);
              CASE_SET_LABEL(nontail_call, 3);
              CASE_SET_LABEL(primitive, 2);
              CASE_SET_LABEL(return, 2);
              CASE_SET_LABEL(return_0, 0);
              CASE_SET_LABEL(return_1, 1);
              CASE_SET_LABEL(return_2, 1);
              CASE_SET_LABEL(return_3, 1);
              CASE_SET_LABEL(return_4, 1);
              CASE_SET_LABEL(return_5, 1);
              CASE_SET_LABEL(set_literals, 1);

              CASE_SET_LABEL(end, 0);
            default:
              assert (0);
            }; /* switch */
        } /* for */
#endif // #ifdef ENABLE_JIT_THREADING
      code->initialized = 1;
      return;
    } /* if (initialize) */

  assert (code->initialized);

  /* Copy the original state in a local structure, so that we don't have to
     access fields thru a pointer.  We will work on this local structure, which
     should be kept in registers, and write back the contents only at exit. */
  struct ejit_thread_state state = *original_state;

  const ejit_instruction_t *ip = instructions;
  epsilon_value literals;
  /* printf ("\ninstructions: %p\n", instructions); */
  /* printf ("p:            %p\n", p); */
  /* printf ("overtop:      %p\n", state.stack_overtop); */
  FIRST;

#ifndef ENABLE_JIT_THREADING
  while (1)
    switch (ip->opcode)
      {
#endif // #ifndef ENABLE_JIT_THREADING

  LABEL(copy_1); // Parameters: from_slot, to_slot
  //fprintf (stderr, "\ncopy %li %li\n", (long)(ip[1].literal), (long)(ip[2].literal));
  //fprintf (stderr, "\nRight before executing copy:\n"); print_values (& state);
  jit_copy_slots (state.frame_bottom,
                  (long)((ip + 1)->literal), (long)((ip + 2)->literal), 1);
  /* state.frame_bottom[(long)((ip + 2)->literal)] */
  /*   = state.frame_bottom[(long)((ip + 1)->literal)]; */
  ip += 2;
  NEXT;

  LABEL(copy_2); // Parameters: from_first_slot, to_first_slot
  jit_copy_slots (state.frame_bottom,
                  (long)((ip + 1)->literal), (long)((ip + 2)->literal), 2);
  ip += 2;
  NEXT;

  LABEL(copy_3); // Parameters: from_first_slot, to_first_slot
  jit_copy_slots (state.frame_bottom,
                  (long)((ip + 1)->literal), (long)((ip + 2)->literal), 3);
  ip += 2;
  NEXT;

  LABEL(copy_4); // Parameters: from_first_slot, to_first_slot
  jit_copy_slots (state.frame_bottom,
                  (long)((ip + 1)->literal), (long)((ip + 2)->literal), 4);
  ip += 2;
  NEXT;

  LABEL(copy_5); // Parameters: from_first_slot, to_first_slot
  jit_copy_slots (state.frame_bottom,
                  (long)((ip + 1)->literal), (long)((ip + 2)->literal), 5);
  ip += 2;
  NEXT;

#define COPY_AND_TAIL_CALL_COMMON_PART                                  \
    epsilon_value symbol                                                \
      = epsilon_load_with_epsilon_int_offset (literals,                 \
                                              (long)((ip + 1)->literal)); \
    ejit_compile_procedure_if_needed (symbol);                          \
    epsilon_value target_as_value = epsilon_load_with_epsilon_int_offset (symbol, 8); \
    ejit_label_t target = epsilon_value_to_foreign_pointer (target_as_value); \
    literals = epsilon_load_with_epsilon_int_offset (symbol, 9);        \
    jit_copy_slots (state.frame_bottom, (long)((ip + 2)->literal), 0, actual_no); \
    state.frame_bottom[actual_no] = (void*)literals;                    \
    /*fprintf (stderr, "Going to %p, with %li parameters\n", target, actual_no);*/ \
    GOTO (target);

  LABEL(copy_and_tail_call); // Parameters: symbol, first_actual_slot, literals_slot
  {
    const long actual_no = (long)((ip + 3)->literal);
    COPY_AND_TAIL_CALL_COMMON_PART;
    /* epsilon_value symbol */
    /*   = epsilon_load_with_epsilon_int_offset (literals, */
    /*                                           (long)((ip + 1)->literal)); */
    /* ejit_compile_procedure_if_needed (symbol); */
    /* epsilon_value target_as_value = epsilon_load_with_epsilon_int_offset (symbol, 8); */
    /* ejit_label_t target = epsilon_value_to_foreign_pointer (target_as_value); */
    /* literals = epsilon_load_with_epsilon_int_offset (symbol, 9); */
    /* jit_copy_slots (state.frame_bottom, (long)((ip + 2)->literal), 0, actual_no); */
    /* state.frame_bottom[actual_no] = (void*)literals; */
    /* GOTO (target); */
  }
  LABEL(copy_and_tail_call_0); // Parameters: symbol, first_actual_slot
  {
    const long actual_no = 0;
    COPY_AND_TAIL_CALL_COMMON_PART;
  }
  LABEL(copy_and_tail_call_1); // Parameters: symbol, first_actual_slot
  {
    const long actual_no = 1;
    COPY_AND_TAIL_CALL_COMMON_PART;
  }
  LABEL(copy_and_tail_call_2); // Parameters: symbol, first_actual_slot
  {
    const long actual_no = 2;
    COPY_AND_TAIL_CALL_COMMON_PART;
  }
  LABEL(copy_and_tail_call_3); // Parameters: symbol, first_actual_slot
  {
    const long actual_no = 3;
    COPY_AND_TAIL_CALL_COMMON_PART;
  }
  LABEL(copy_and_tail_call_4); // Parameters: symbol, first_actual_slot
  {
    const long actual_no = 4;
    COPY_AND_TAIL_CALL_COMMON_PART;
  }
  LABEL(copy_and_tail_call_5); // Parameters: symbol, first_actual_slot
  {
    const long actual_no = 5;
    COPY_AND_TAIL_CALL_COMMON_PART;
  }
#undef COPY_AND_TAIL_CALL_COMMON_PART                                  \

  LABEL(global); // Parameters: symbol, to_slot
  {
    epsilon_value symbol
      = epsilon_load_with_epsilon_int_offset (literals,
                                              (long)((ip + 1)->literal));
#ifdef ENABLE_DEBUG
    if (EPSILON_UNLIKELY (epsilon_load_with_epsilon_int_offset (symbol, 1))
        == epsilon_value_zero)
      epsilon_fatal ("%s accessing unbound global", __func__);
#endif // #ifdef ENABLE_DEBUG
    epsilon_value global_binding
      = epsilon_load_with_epsilon_int_offset (symbol, 2);
    state.frame_bottom[(long)((ip + 2)->literal)] = global_binding;
    ip += 2;
    NEXT;
  }

  LABEL(jump); // Parameters: instruction_offset
  GOTO (ip + (long)((ip + 1)->literal));

  LABEL(jump_if_equal_boxed); // Parameters: from_slot, literal_index, instruction_offset
  if (epsilon_load_with_epsilon_int_offset (literals, (long)((ip + 2)->literal))
      == state.frame_bottom[(long)((ip + 1)->literal)])
    GOTO (ip + (long)((ip + 3)->literal));
  ip += 3;
  NEXT;

  LABEL(jump_if_equal_unboxed); // Parameters: from_slot, literal, instruction_offset
  if (((ip + 2)->literal)
      == state.frame_bottom[(long)((ip + 1)->literal)])
    GOTO (ip + (long)((ip + 3)->literal));
  ip += 3;
  NEXT;

  LABEL(literal_boxed); // Parameters: literal_index, to_slot
  state.frame_bottom[(long)((ip + 2)->literal)]
    = epsilon_load_with_epsilon_int_offset (literals,
                                            (long)((ip + 1)->literal));
  ip += 2;
  NEXT;

  LABEL(literal_unboxed); // Parameters: literal, to_slot
  state.frame_bottom[(long)((ip + 2)->literal)] = ((ip + 1)->literal);
  ip += 2;
  NEXT;

  LABEL(nontail_call); // Parameters: symbol, first_actual_slot, literals_slot
  {
    //fprintf (stderr, "OK-E 1000\n");
    //print_values (& state); // #######
    state.return_stack_overtop[0] = state.frame_bottom;
    state.return_stack_overtop[1] = (void*)(ip + 4);
    state.return_stack_overtop += 2;
    epsilon_value symbol
      = epsilon_load_with_epsilon_int_offset (literals,
                                              (long)((ip + 1)->literal));
    //fprintf (stderr, "OK-E 2000\n");
    ejit_compile_procedure_if_needed (symbol);
    //fprintf (stderr, "OK-E 3000\n");
    epsilon_value target_as_value = epsilon_load_with_epsilon_int_offset (symbol, 8);
    //fprintf (stderr, "OK-E 4000\n");
    ejit_label_t target = epsilon_value_to_foreign_pointer (target_as_value);
    //fprintf (stderr, "OK-E 5000\n");
    literals = epsilon_load_with_epsilon_int_offset (symbol, 9);
    //fprintf (stderr, "OK-E 6000\n");
    state.frame_bottom[(long)((ip + 3)->literal)] = (void*)literals;
    //fprintf (stderr, "OK-E 7000\n");
    state.frame_bottom += (long)((ip + 2)->literal);
    //fprintf (stderr, "OK-E 8000: target is %p (here is %p)\n", target, &symbol);
    GOTO (target);
  }

  LABEL(primitive); // Parameters: primitive_function_pointer, inout_slot
  /* fprintf (stderr, "\nprimitive %p %li\n", ip[1].literal, (long)(ip[2].literal)); */
  /* fprintf (stderr, "\nRight before executing primitive:\n"); print_values (& state); */
  //{
    /* epsilon_c_primitive_function fp = */
    /*   ((epsilon_c_primitive_function)((ip + 1)->literal)); */
    //fprintf (stderr, "B: primitive function pointer is %p\n", fp);
  ((epsilon_c_primitive_function)((ip + 1)->literal))(state.frame_bottom
                                                      + (long)((ip + 2)->literal));
  ip += 2;
  NEXT;

#define RETURN_COMMON_PART \
  state.return_stack_overtop -= 2; \
  state.frame_bottom = state.return_stack_overtop[0]; \
  GOTO (state.return_stack_overtop[1]);

#define RETURN_N(result_no) \
  jit_copy_slots (state.frame_bottom, (long)(ip[1].literal), 0, result_no); \
  RETURN_COMMON_PART;

  LABEL(return); // Parameters: first_result_slot, result_no
  jit_copy_slots (state.frame_bottom,
                  (long)(ip[1].literal), 0, (long)(ip[2].literal));
  RETURN_COMMON_PART;

  LABEL(return_0); // Parameters: none
  RETURN_COMMON_PART;
  LABEL(return_1); // Parameters: result_slot
  RETURN_N (1);
  LABEL(return_2); // Parameters: result_slot
  RETURN_N (2);
  LABEL(return_3); // Parameters: result_slot
  RETURN_N (3);
  LABEL(return_4); // Parameters: result_slot
  RETURN_N (4);
  LABEL(return_5); // Parameters: result_slot
  RETURN_N (5);

#undef RETURN_COMMON_PART
#undef RETURN_N

  LABEL(set_literals); // Parameters: literals_slot
  literals = state.frame_bottom[(long)(++ ip)->literal];
  NEXT;

  LABEL(end); // Parameters: none
  *original_state = state;
  return;

#ifndef ENABLE_JIT_THREADING
      default:
        epsilon_fatal ("this should be unreachable");
      } /* switch */
#endif // #ifndef ENABLE_JIT_THREADING
}
#undef SET_LABEL
#undef FIRST
#undef NEXT
#undef GOTO
#undef LABEL

static void
ejit_initialize_code (ejit_code_t code)
{
  ejit_initialize_or_run_code (1, code, NULL);
}

void
ejit_run_code (ejit_code_t code, ejit_thread_state_t state)
{
  ejit_initialize_or_run_code (0, code, state);
}

static long
epsilon_max_nonformal_local_no (epsilon_value expression);
static long
epsilon_max_nonformal_local_no_in (epsilon_value expressions)
{
  long res = 0;
  epsilon_value rest;
  for (rest = expressions;
       ! epsilon_value_is_null (rest);
       rest = epsilon_value_cdr (rest))
    res = epsilon_max_long (res, epsilon_max_nonformal_local_no (epsilon_value_car (rest)));
  return res;
}
static long
epsilon_max_nonformal_local_no (epsilon_value expression)
{
  epsilon_int opcode
    = epsilon_value_to_epsilon_int (epsilon_load_with_epsilon_int_offset (expression, 0));
  switch (opcode)
    {
    case e0_variable_opcode:
    case e0_value_opcode:
      return 0;
    case e0_bundle_opcode:
      return epsilon_max_nonformal_local_no_in (epsilon_load_with_epsilon_int_offset (expression, 2));
    case e0_primitive_opcode:
    case e0_call_opcode:
    case e0_fork_opcode:
      return epsilon_max_nonformal_local_no_in (epsilon_load_with_epsilon_int_offset (expression, 3));
    case e0_let_opcode:
      {
        long bound_no = epsilon_value_length (epsilon_load_with_epsilon_int_offset (expression, 2));
        return epsilon_max_long (epsilon_max_nonformal_local_no (epsilon_load_with_epsilon_int_offset (expression, 3)),
                                 bound_no
                                 + epsilon_max_nonformal_local_no (epsilon_load_with_epsilon_int_offset (expression, 4)));
      }
    case e0_call_indirect_opcode:
      return epsilon_max_long (epsilon_max_nonformal_local_no (epsilon_load_with_epsilon_int_offset (expression, 2)),
                               epsilon_max_nonformal_local_no_in (epsilon_load_with_epsilon_int_offset (expression, 3)));
    case e0_if_in_opcode:
      return epsilon_max_long (epsilon_max_nonformal_local_no (epsilon_load_with_epsilon_int_offset (expression, 2)),
                               epsilon_max_long (epsilon_max_nonformal_local_no (epsilon_load_with_epsilon_int_offset (expression, 4)),
                                                 epsilon_max_nonformal_local_no (epsilon_load_with_epsilon_int_offset (expression, 5))));
    case e0_join_opcode:
      return epsilon_max_nonformal_local_no (epsilon_load_with_epsilon_int_offset (expression, 2));
    default:
      epsilon_fatal ("%s: ill-formed epsilon0 expression", __func__);
    } // switch
}

static long
epsilon_result_no (epsilon_value expression)
{
  epsilon_int opcode
    = epsilon_value_to_epsilon_int (epsilon_load_with_epsilon_int_offset (expression, 0));
  switch (opcode)
    {
    case e0_variable_opcode:
    case e0_value_opcode:
    case e0_fork_opcode:
    case e0_join_opcode:
      return 1;
    case e0_bundle_opcode:
      return epsilon_value_length (epsilon_load_with_epsilon_int_offset (expression, 2));
    case e0_primitive_opcode:
      {
        epsilon_value primitive_name = epsilon_load_with_epsilon_int_offset (expression, 2);
        epsilon_value primitive_descriptor = epsilon_load_with_epsilon_int_offset (primitive_name, 7);
        if (EPSILON_UNLIKELY(epsilon_value_is_zero (primitive_descriptor)))
          epsilon_fatal ("%s: unbound primitive name", __func__);
        epsilon_int primitive_out_dimension = epsilon_value_to_epsilon_int (epsilon_load_with_epsilon_int_offset (primitive_descriptor, 1));
        return primitive_out_dimension;
      }
    case e0_let_opcode:
      {
        epsilon_value body
          = epsilon_load_with_epsilon_int_offset (expression, 4);
        return epsilon_result_no (body);
      }
    case e0_call_opcode:
    case e0_call_indirect_opcode:
      return 0; // doesn't matter
    case e0_if_in_opcode:
      {
        epsilon_value then_branch
          = epsilon_load_with_epsilon_int_offset (expression, 4);
        epsilon_value else_branch
          = epsilon_load_with_epsilon_int_offset (expression, 5);
        return epsilon_max_long (epsilon_result_no (then_branch),
                                 epsilon_result_no (else_branch));
      }
    default:
      epsilon_fatal ("%s: ill-formed epsilon0 expression", __func__);
    } // switch
  epsilon_fatal ("%s: this should be unreachable", __func__);
}

static void
ejit_compile_epilog (ejit_compiler_state_t s,
                     long result_no,
                     long target_slot,
                     bool tail)
{
  if (! tail)
    return;

  switch (result_no)
    {
    case 0:
      ejit_push_instruction (s, ejit_opcode_return_0);
      break;
    case 1: case 2: case 3: case 4: case 5:
      ejit_push_instruction (s, ejit_opcode_return_0 + result_no);
      ejit_push_instruction (s, target_slot);
      break;
    default:
      ejit_push_instruction (s, ejit_opcode_return);
      ejit_push_instruction (s, target_slot);
      ejit_push_instruction (s, result_no);
    }
}

static void
ejit_compile_expressions (ejit_compiler_state_t s,
                          epsilon_value expression,
                          long formal_no,
                          long nonformal_local_no,
                          long result_no,
                          long literals_slot, // -1 if not used
                          long first_target_slot);

static bool
ejit_is_boxed (epsilon_value literal)
{
#ifdef EPSILON_RUNTIME_UNTAGGED
  return false;
#else
  return epsilon_is_pointer (literal);
#endif // #ifdef EPSILON_RUNTIME_UNTAGGED
}

// FIXME: possible optimizations: don't generate at all the set_literals
// instructions when no literals are needed.  This will be frequent once
// I add the possibility of setting unboxed literals in a more efficient
// way, without indirection..

static void
ejit_compile_expression (ejit_compiler_state_t s,
                         epsilon_value expression,
                         long formal_no,
                         long nonformal_local_no,
                         long result_no,
                         long literals_slot, // -1 if not used
                         long target_slot,
                         bool tail)
{
  epsilon_int opcode
    = epsilon_value_to_epsilon_int (epsilon_load_with_epsilon_int_offset (expression, 0));
  switch (opcode)
    {
    case e0_variable_opcode:
      {
        // FIXME: optimizable: in the tail case directly write to slot 0
        epsilon_value name = epsilon_load_with_epsilon_int_offset (expression, 2);
        long local_index = epsilon_stack_search_last (& s->locals, name);
        if (local_index < 0)
          {
            ejit_push_instruction (s, ejit_opcode_global);
            ejit_push_epsilon_literal (s, name);
            ejit_push_instruction (s, target_slot);
          }
        else
          {
            ejit_push_instruction (s, ejit_opcode_copy_1);
            ejit_push_instruction (s, local_index);
            ejit_push_instruction (s, target_slot);
          }
        ejit_compile_epilog (s, 1, target_slot, tail);
        break;
      }
    case e0_value_opcode:
      {
        // FIXME: optimizable: in the tail case directly write to slot 0
        epsilon_value literal
          = epsilon_load_with_epsilon_int_offset (expression, 2);
        if (ejit_is_boxed (literal))
          {
            ejit_push_instruction (s, ejit_opcode_literal_boxed);
            ejit_push_epsilon_literal (s, literal);
            ejit_push_instruction (s, target_slot);
          }
        else
          {
            ejit_push_instruction (s, ejit_opcode_literal_unboxed);
            ejit_push_instruction (s, (long)literal);
            ejit_push_instruction (s, target_slot);
          }
        ejit_compile_epilog (s, 1, target_slot, tail);
        break;
      }
   case e0_bundle_opcode:
      {
        epsilon_value items = epsilon_load_with_epsilon_int_offset (expression, 2);
        long item_no = epsilon_value_length (items);
        if (item_no == 1)
          {
            ejit_compile_expression (s,
                                     epsilon_value_car (items),
                                     formal_no, nonformal_local_no, result_no,
                                     literals_slot,
                                     target_slot,
                                     tail);
            break;
          }
        ejit_compile_expressions (s,
                                  items,
                                  formal_no,
                                  nonformal_local_no,
                                  result_no,
                                  literals_slot,
                                  target_slot);
        ejit_compile_epilog (s, item_no, target_slot, tail);
        break;
      }
    case e0_primitive_opcode:
      {
        epsilon_value actuals = epsilon_load_with_epsilon_int_offset (expression, 3);
        ejit_compile_expressions (s,
                                  actuals,
                                  formal_no,
                                  nonformal_local_no,
                                  result_no,
                                  literals_slot,
                                  target_slot);
        epsilon_value name = epsilon_load_with_epsilon_int_offset (expression, 2);
        epsilon_value primitive_descriptor
          = epsilon_load_with_epsilon_int_offset(name, 7);
        epsilon_int primitive_index
          = epsilon_value_to_epsilon_int (epsilon_load_with_epsilon_int_offset (primitive_descriptor, 4));
        epsilon_c_primitive_function primitive_function
          = epsilon_c_primitive_functions[primitive_index];
        //fprintf (stderr, "A: primitive function pointer is %p\n", primitive_function);
        // FIXME: check in-dimension and warn on mismatch
        epsilon_int primitive_out_dimension = epsilon_value_to_epsilon_int(epsilon_load_with_epsilon_int_offset(primitive_descriptor, 1));
        ejit_push_instruction (s, ejit_opcode_primitive);
        ejit_push_instruction (s, (long)primitive_function);
        ejit_push_instruction (s, target_slot);
        ejit_compile_epilog (s, primitive_out_dimension, target_slot, tail);
        break;
      }
    case e0_let_opcode:
      {
        epsilon_value variables = epsilon_load_with_epsilon_int_offset (expression, 2);
        long i, variable_no = epsilon_value_length (variables);
        epsilon_value bound_expression = epsilon_load_with_epsilon_int_offset (expression, 3);
        epsilon_value body = epsilon_load_with_epsilon_int_offset (expression, 4);
        ejit_compile_expression (s,
                                 bound_expression,
                                 formal_no, nonformal_local_no, result_no,
                                 literals_slot,
                                 target_slot,
                                 false);
        epsilon_value more_locals;
        for (more_locals = variables, i = 0;
             ! epsilon_value_is_null (more_locals);
             more_locals = epsilon_value_cdr (more_locals), i ++)
          {
            epsilon_value variable = epsilon_value_car (more_locals);
            epsilon_stack_push (& s->locals, variable);
            long local_index = epsilon_stack_search_last (& s->locals, variable);
            // FIXME: this is optimizable.  If the variables are more than one,
            // I could generate a single copy_multiple instruction.
            ejit_push_instruction (s, ejit_opcode_copy_1);
            ejit_push_instruction (s, target_slot + i);
            ejit_push_instruction (s, local_index);
          }
        ejit_compile_expression (s,
                                 body,
                                 formal_no, nonformal_local_no, result_no,
                                 literals_slot,
                                 target_slot,
                                 tail);
        for (i = 0; i < variable_no; i ++)
          epsilon_stack_pop (& s->locals);
        break;
      }
    case e0_call_opcode:
      {
        epsilon_value name = epsilon_load_with_epsilon_int_offset (expression, 2);
        epsilon_value actuals = epsilon_load_with_epsilon_int_offset (expression, 3);
        long actual_no = epsilon_value_length (actuals);
        ejit_compile_expressions (s,
                                  actuals,
                                  formal_no,
                                  nonformal_local_no,
                                  result_no,
                                  literals_slot,
                                  target_slot);
        if (tail)
          {
            switch (actual_no)
              {
              case 0: case 1: case 2: case 3: case 4: case 5:
                ejit_push_instruction (s, ejit_opcode_copy_and_tail_call_0 + actual_no);
                ejit_push_epsilon_literal (s, name);
                ejit_push_instruction (s, target_slot);
                break;
              default:
                {
                  // FIXME: instead of this, it might be faster to have a
                  // generic tail_call instruction which performs no copy,
                  // and perform the copy here with a few instructions.
                  ejit_push_instruction (s, ejit_opcode_copy_and_tail_call);
                  ejit_push_epsilon_literal (s, name);
                  ejit_push_instruction (s, target_slot);
                  ejit_push_instruction (s, actual_no);
                } // default
              } // switch
          }
        else
          {
            ejit_push_instruction (s, ejit_opcode_nontail_call);
            ejit_push_epsilon_literal (s, name);
            ejit_push_instruction (s, target_slot);
            ejit_push_instruction (s, target_slot + actual_no);
            // FIXME: don't do this in case of a directly recursive call.  Actually
            // I could also avoid it whenever the continuation of the procedure call
            // doesn't need literals...
            if (literals_slot != -1)
              {
                ejit_push_instruction (s, ejit_opcode_set_literals);
                ejit_push_instruction (s, literals_slot);
              }
          } // nontail call
        break;
      }
    case e0_call_indirect_opcode:
      epsilon_fatal ("unimplemented in the compiler: call-indirect");
    case e0_if_in_opcode:
      {
        epsilon_value discriminand = epsilon_load_with_epsilon_int_offset (expression, 2);
        epsilon_value literals = epsilon_load_with_epsilon_int_offset (expression, 3);
        epsilon_value then_branch = epsilon_load_with_epsilon_int_offset (expression, 4);
        epsilon_value else_branch = epsilon_load_with_epsilon_int_offset (expression, 5);
        // FIXME: important optimization: when the discriminand is a nonglobal
        // variable don't push it: we can test it in its current slot.
        ejit_compile_expression (s,
                                 discriminand,
                                 formal_no, nonformal_local_no, result_no,
                                 literals_slot,
                                 target_slot,
                                 false);
        struct epsilon_stack indices_to_backpatch;
        epsilon_stack_initialize (& indices_to_backpatch);
        epsilon_value more_literals;
        for (more_literals = literals;
             ! epsilon_value_is_null (more_literals);
             more_literals = epsilon_value_cdr (more_literals))
          {
            epsilon_value literal = epsilon_value_car (more_literals);
            long this_instruction_index = s->instructions.element_no;
            if (ejit_is_boxed (literal))
              {
                ejit_push_instruction (s, ejit_opcode_jump_if_equal_boxed);
                ejit_push_instruction (s, target_slot);
                ejit_push_epsilon_literal (s, literal);
              }
            else
              {
                ejit_push_instruction (s, ejit_opcode_jump_if_equal_unboxed);
                ejit_push_instruction (s, target_slot);
                ejit_push_instruction (s, (long)literal);
              }
            epsilon_stack_push (& indices_to_backpatch,
                                (epsilon_word)(long)(s->instructions.element_no));
            // This will be backpatched later, to compute an offset.
            ejit_push_instruction (s, - this_instruction_index);
          }
        ejit_compile_expression (s,
                                 else_branch,
                                 formal_no, nonformal_local_no, result_no,
                                 literals_slot,
                                 target_slot,
                                 tail);
        long after_then_source;
        if (! tail)
          {
            long this_instruction_index = s->instructions.element_no;
            ejit_push_instruction (s, ejit_opcode_jump);
            after_then_source = s->instructions.element_no;
            // This will be backpatched later, to compute an offset.
            ejit_push_instruction (s, - this_instruction_index);
          }
        long then_target = s->instructions.element_no;
        while (! epsilon_stack_empty (& indices_to_backpatch))
          ((long*)s->instructions.buffer)
            [(long)epsilon_stack_pop (& indices_to_backpatch)]
            += then_target;
        ejit_compile_expression (s,
                                 then_branch,
                                 formal_no, nonformal_local_no, result_no,
                                 literals_slot,
                                 target_slot,
                                 tail);
        long after_then_target = s->instructions.element_no;
        if (! tail)
          ((long*)s->instructions.buffer)[after_then_source]
            += after_then_target;
        epsilon_stack_finalize (& indices_to_backpatch);
        break;
      }
    case e0_fork_opcode:
      epsilon_fatal ("unimplemented in the compiler: fork");
    case e0_join_opcode:
      epsilon_fatal ("unimplemented in the compiler: join");
    default:
      epsilon_fatal ("%s: ill-formed epsilon0 expression", __func__);
    }
}

static void
ejit_compile_expressions (ejit_compiler_state_t s,
                          epsilon_value expressions,
                          long formal_no,
                          long nonformal_local_no,
                          long result_no,
                          long literals_slot, // -1 if not used
                          long first_target_slot)
{
  int i;
  epsilon_value rest;
  for (rest = expressions, i = 0;
       ! epsilon_value_is_null (rest);
       rest = epsilon_value_cdr (rest), i ++)
    ejit_compile_expression (s,
                             epsilon_value_car (rest),
                             formal_no,
                             nonformal_local_no,
                             result_no,
                             literals_slot,
                             first_target_slot + i,
                             false);
}

long
e0_literal_no_in (epsilon_value expressions);

long
e0_literal_no (epsilon_value expression)
{
  epsilon_int opcode
    = epsilon_value_to_epsilon_int (epsilon_load_with_epsilon_int_offset (expression, 0));
  switch (opcode)
    {
    case e0_variable_opcode:
      // FIXME: this works but is a crude conservative approximation: I only
      // need a literal per *global* variable reference -- actually even less,
      // if I reused literal slots.
      return 1;
    case e0_value_opcode:
      return 1;
    case e0_bundle_opcode:
      return e0_literal_no_in (epsilon_load_with_epsilon_int_offset (expression, 2));
    case e0_primitive_opcode:
      return e0_literal_no_in (epsilon_load_with_epsilon_int_offset (expression, 3));
    case e0_call_opcode:
    case e0_fork_opcode:
      return 1 + e0_literal_no_in (epsilon_load_with_epsilon_int_offset (expression, 3));
    case e0_let_opcode:
      return e0_literal_no (epsilon_load_with_epsilon_int_offset (expression, 3))
             + e0_literal_no (epsilon_load_with_epsilon_int_offset (expression, 4));
    case e0_call_indirect_opcode:
      return e0_literal_no (epsilon_load_with_epsilon_int_offset (expression, 2))
             + e0_literal_no_in (epsilon_load_with_epsilon_int_offset (expression, 3));
    case e0_if_in_opcode:
      return e0_literal_no (epsilon_load_with_epsilon_int_offset (expression, 2))
             + epsilon_value_length (epsilon_load_with_epsilon_int_offset (expression, 3))
             + e0_literal_no (epsilon_load_with_epsilon_int_offset (expression, 4))
             + e0_literal_no (epsilon_load_with_epsilon_int_offset (expression, 5));
    case e0_join_opcode:
      return e0_literal_no (epsilon_load_with_epsilon_int_offset (expression, 2));
    default:
      epsilon_fatal ("%s: ill-formed epsilon0 expression", __func__);
    }
}

long
e0_literal_no_in (epsilon_value expressions)
{
  long res = 0;
  epsilon_value rest;
  for (rest = expressions;
       ! epsilon_value_is_null (rest);
       rest = epsilon_value_cdr (rest))
    res += e0_literal_no (epsilon_value_car (rest));
  return res;
}

ejit_code_t
ejit_compile (epsilon_value formal_list, epsilon_value expression,
              bool return_in_the_end)
{
  ejit_code_t res = epsilon_xmalloc (sizeof (struct ejit_code));
  res->initialized = 0;
  struct ejit_compiler_state s;
  ejit_initialize_compiler_state (&s);

  size_t formal_no = epsilon_value_length (formal_list);
  //fprintf (stderr, "The procedure has %li formals\n", formal_no);
  size_t nonformal_local_no  = epsilon_max_nonformal_local_no (expression);
  //fprintf (stderr, "The procedure needs %li nonformal locals\n", nonformal_local_no);
  size_t result_no  = epsilon_result_no (expression);
  //fprintf (stderr, "The procedure has %li results\n", result_no);

  long literal_no = e0_literal_no (expression);
  /* I have to allocate res->literals before compiling: with a moving collector
     this allocation might trigger a GC and change epsilon literals.  Compiling
     doesn't need epsilon heap allocation. */
  res->literals = epsilon_gc_allocate_with_epsilon_int_length (literal_no);
  //fprintf (stderr, "The procedure has %li literals\n", literal_no);

  long literals_slot;
  long target_slot;
  if (literal_no == 0)
    {
      literals_slot = -1;
      target_slot = epsilon_max_long (formal_no + nonformal_local_no,
                                      result_no);
    }
  else
    {
      literals_slot = formal_no;
      target_slot = epsilon_max_long (formal_no + 1 + nonformal_local_no,
                                      result_no);
      // No, this is not needed: the call instruction takes care of it.
      //ejit_push_instruction (&s, ejit_opcode_set_literals);
      //ejit_push_instruction (&s, literals_slot);
    }
  //fprintf (stderr, "literals_slot is %li\n", literals_slot);
  //fprintf (stderr, "target_slot is %li\n", target_slot);
  epsilon_value more_formals;
  for (more_formals = formal_list;
       ! epsilon_value_is_null (more_formals);
       more_formals = epsilon_value_cdr (more_formals))
    epsilon_stack_push (& s.locals, epsilon_value_car (more_formals));

  /* If we're using a local slot, push a dummy variable between formals and
     nonformal locals, to keep mirroring the actual stack configuration. */
  if (literals_slot != -1)
    epsilon_stack_push (& s.locals, NULL);
  if (! return_in_the_end)
    {
      //fprintf (stderr, "Adding main stub prolog...\n");
      ejit_push_instruction (&s, ejit_opcode_set_literals);
      ejit_push_instruction (&s, literals_slot);
    }
  ejit_compile_expression (&s, expression,
                           formal_no, nonformal_local_no, result_no,
                           literals_slot,
                           target_slot,
                           return_in_the_end);
  if (! return_in_the_end)
    {
      //fprintf (stderr, "Adding main stub epilog...\n");
      ejit_push_instruction (& s, ejit_opcode_end);
    }

  /* Instead of copying from the two stacks and then finalizing them, just steal
     their buffers and use them as the buffers pointed by res.  This saves a
     copy, which may be important if JITting and run time.  I guess this makes me
     a horrible person. */
  if (EPSILON_UNLIKELY(sizeof (ejit_instruction_t) != sizeof (epsilon_word)))
    epsilon_fatal ("this compiler represents unions in an impossibly stupid way");
  res->instruction_no = s.instructions.element_no;
  res->instructions = (ejit_instruction_t*)s.instructions.buffer;

  assert (s.literal_stack.element_no <= literal_no);
  int i;
  for (i = 0; i < s.literal_stack.element_no; i ++)
    epsilon_store_with_epsilon_int_offset (res->literals, i,
                                           s.literal_stack.buffer[i]);
  for (; i < literal_no; i ++)
    epsilon_store_with_epsilon_int_offset (res->literals, i,
                                           epsilon_int_to_epsilon_value (0));
  /* ejit_finalize_compiler_state (& s); */ // No!
  epsilon_stack_finalize (& s.locals);
  epsilon_stack_finalize (& s.literal_stack);

  /* Convert opcodes to labels: */
  ejit_initialize_code (res);

  return res;
}

void
ejit_destroy_code (const ejit_code_t c)
{
  free (c->instructions);
  free (c);
}

void
ejit_compile_procedure (epsilon_value symbol)
{
#ifdef ENABLE_DEBUG
  const epsilon_value zero = epsilon_int_to_epsilon_value (0);
  epsilon_value compiled_code_as_value
    = epsilon_load_with_epsilon_int_offset(symbol, 8);
  if (compiled_code_as_value != zero)
    epsilon_fatal ("%s: already compiled", __func__);
#endif // #ifdef ENABLE_DEBUG
  epsilon_value formals
    = epsilon_load_with_epsilon_int_offset(symbol, 3);
  epsilon_value body
    = epsilon_load_with_epsilon_int_offset(symbol, 4);
#ifdef ENABLE_DEBUG
  if (body == zero)
    epsilon_fatal ("%s: compiling an undefined procedure", __func__);
#endif // #ifdef ENABLE_DEBUG
  ejit_code_t c = ejit_compile (formals, body, true);
  size_t instruction_size = sizeof (ejit_instruction_t) * c->instruction_no;
  ejit_instruction_t *instructions = epsilon_xmalloc (instruction_size);
  memcpy (instructions, c->instructions, instruction_size); // FIXME: avoid this copy, in a clean way
  epsilon_value instructions_as_value
    = epsilon_foreign_pointer_to_epsilon_value (instructions);
  epsilon_store_with_epsilon_int_offset(symbol, 8, instructions_as_value);
  epsilon_store_with_epsilon_int_offset(symbol, 9, c->literals);
  ejit_destroy_code (c);
}

void
ejit_uncompile_procedure (epsilon_value symbol)
{
  const epsilon_value zero = epsilon_int_to_epsilon_value (0);
  epsilon_value compiled_code_as_value
    = epsilon_load_with_epsilon_int_offset(symbol, 8);
  epsilon_store_with_epsilon_int_offset(symbol, 9, zero);
  if (compiled_code_as_value == zero)
    return;
  ejit_label_t compiled_code
    = epsilon_value_to_foreign_pointer (compiled_code_as_value);
  free (compiled_code);
  epsilon_store_with_epsilon_int_offset(symbol, 8, zero);
}

void
ejit_recompile_procedure (epsilon_value symbol)
{
  ejit_uncompile_procedure (symbol);
  ejit_compile_procedure (symbol);
}

void
ejit_compile_procedure_if_needed (epsilon_value symbol)
{
  epsilon_value target_as_value
    = epsilon_load_with_epsilon_int_offset (symbol, 8);
  if (EPSILON_UNLIKELY (! epsilon_value_to_epsilon_int (target_as_value)))
    {
      epsilon_value symbol_name_value
        = epsilon_load_with_epsilon_int_offset (symbol, 0);
      char *symbol_name
        = epsilon_string_to_malloced_char_star (symbol_name_value);
      fprintf (stderr, "Compiling %s...\n", symbol_name);
      ejit_compile_procedure (symbol);
      fprintf (stderr, "  ... the procedure %s is now compiled.\n", symbol_name);
      free (symbol_name);
    }
}

void
ejit_evaluate_expression (epsilon_value expression)
{
  //fprintf (stderr, "OK-A 100 before compiling\n");

  /* Compile the code, with an end instruction in the end.  There's no return
     instruction, and results won't end up in their appropriate slots. */
  epsilon_value epsilon_null = epsilon_int_to_epsilon_value (0);
  ejit_code_t c = ejit_compile (epsilon_null, expression, false);

  //fprintf (stderr, "OK-A 200 after compiling\n");

  /* Make an empty thread state.  Since the code is not reached thru a call
     instruction we have to manually set the literals pointer in the stack;
     everything else starts out empty, including the return stack.
     There are no formals, therefore the literal pointers ends up in the
     first slot. */
  ejit_thread_state_t s = ejit_make_thread_state ();
  s->stack_bottom[0] = c->literals;

  //fprintf (stderr, "OK-A 1000 before running\n");

  /* Run the code.  In typical usage this will call some procedures to be
     compiled on the fly, and act as the main expression of a larger program. */
  ejit_run_code (c, s);

  //fprintf (stderr, "OK-A 2000 after running\n");

  print_values (s);

  /* If everything worked at this point we can return, destroying the compiled
     code and the thread state.  Neither will be used anywhere else. */
  ejit_destroy_thread_state (s);
  ejit_destroy_code (c);

  /* We can't return any result; we don't even know how many they are. */
}
