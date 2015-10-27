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

//////////////////////////////

#ifndef EPSILON_JIT_H_
#define EPSILON_JIT_H_

#include "runtime.h"

/* The state of a thread running JITted code.  The structure is intentionally
   opaque and its inner content is subject to change in the future, possibly
   according to the configuration.

   FIXME: should this be heap-allocated, or be a manually managed root poiting
   to heap objects? */
typedef struct ejit_thread_state* ejit_thread_state_t;

ejit_thread_state_t
ejit_make_thread_state (void)
  __attribute__ ((malloc));

void
ejit_destroy_thread_state (const ejit_thread_state_t s);

/* void */
/* ejit_push_on_thread_state (const ejit_thread_state_t s, epsilon_value v); */

/* The generated code, which is allocated on the C heap and manually freed.
   Again, the structure is intentionally opaque. */
typedef struct ejit_code* ejit_code_t;

/* If literals_slot_pointer is non-NULL generate a final end instruction and
   place the literals slot index into the given location, so that the code can
   be easily executed even if there is no associated symbol.

   FIXME: say that the literals part which is generated is not a GC root and should
   be used immediately (by being put on a stack), before GC-allocating. */
ejit_code_t
ejit_compile (epsilon_value expression, epsilon_value formal_list,
              long *literals_slot_pointer)
  __attribute__(( malloc ));

void
ejit_destroy_code (const ejit_code_t c);

void
ejit_run_code (ejit_code_t code, ejit_thread_state_t state);


#endif // #ifndef EPSILON_JIT_H_

//////////////////////////////

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>

#include "../utility/utility.h"
#include "runtime.h"
#include "config.h"

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
  int i;
  for (i = 0; i < STACK_ELEMENT_NO; i ++)
    res->stack_bottom[i] = epsilon_int_to_epsilon_value (0);

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
       i < 15; // FIXME: take frame height as a parameter
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
    ejit_opcode_copy,
    ejit_opcode_jump,
    ejit_opcode_jump_if_equal,
    ejit_opcode_literal,
    //ejit_opcode_procedure_prolog,
    ejit_opcode_return,
    ejit_opcode_set_literals,

    ejit_opcode_end,

    /* ejit_opcode_cr, */
    /* ejit_opcode_divided, */
    /* ejit_opcode_drop, */
    /* ejit_opcode_dup, */
    /* ejit_opcode_for, */
    /* ejit_opcode_i, */
    /* ejit_opcode_j, */
    /* ejit_opcode_lit, */
    /* ejit_opcode_minus, */
    /* ejit_opcode_next, */
    /* ejit_opcode_nop, */
    /* ejit_opcode_over, */
    /* ejit_opcode_plus, */
    /* ejit_opcode_print, */
    /* ejit_opcode_swap, */
    /* ejit_opcode_times, */
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
    PRINT_JIT_DEBUGGING(stderr, "Initializing: %3i. %s\n", (int)(p - instructions), #name); \
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
  PRINT_JIT_DEBUGGING (stderr, "%i. %s\n", (int)(ip - instructions), #name); \
  /* printf("  ip == %p, ip->label == %p, label_%s == %p\n", ip, ip->label, #name, && label_ ## name);  */\
  /* printf("  Heights at entry: %i %i\n", (int)(state.stack_overtop - state.stack_bottom), (int)(state.return_stack_overtop - state.return_stack_bottom)); */
#else
#define LABEL(name) \
  case ejit_opcode_ ## name: \
    PRINT_JIT_DEBUGGING (stderr, "%i. %s\n", (int)(ip - instructions), #name);
#endif // #ifdef ENABLE_JIT_THREADING

static void
ejit_initialize_or_run_code (int initialize, ejit_code_t code,
                             ejit_thread_state_t state)
  __attribute__((__noinline__, __noclone__, __hot__));

static void
ejit_initialize_or_run_code (int initialize, ejit_code_t code,
                             ejit_thread_state_t original_state)
{
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
              CASE_SET_LABEL(copy, 2);
              CASE_SET_LABEL(jump, 1);
              CASE_SET_LABEL(jump_if_equal, 3);
              CASE_SET_LABEL(literal, 2);
              //CASE_SET_LABEL(procedure_prolog, 1);
              CASE_SET_LABEL(return, 2);
              CASE_SET_LABEL(set_literals, 1);

              CASE_SET_LABEL(end, 0);

              /* CASE_SET_LABEL(cr, 0); */
              /* CASE_SET_LABEL(divided, 0); */
              /* CASE_SET_LABEL(drop, 0); */
              /* CASE_SET_LABEL(dup, 0); */
              /* CASE_SET_LABEL(for, 0); */
              /* CASE_SET_LABEL(i, 0); */
              /* CASE_SET_LABEL(j, 0); */
              /* CASE_SET_LABEL(nop, 0); */
              /* CASE_SET_LABEL(over, 0); */
              /* CASE_SET_LABEL(lit, 1); */
              /* CASE_SET_LABEL(minus, 0); */
              /* CASE_SET_LABEL(next, 0); */
              /* CASE_SET_LABEL(plus, 0); */
              /* CASE_SET_LABEL(print, 0); */
              /* CASE_SET_LABEL(swap, 0); */
              /* CASE_SET_LABEL(times, 0); */
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

  LABEL(copy); // Parameters: from_slot, to_slot
  /* fprintf (stderr, "## Copying %li or %p from slot%li to slot%li\n", */
  /*          (long)state.frame_bottom[(long)((ip + 1)->literal)], */
  /*          state.frame_bottom[(long)((ip + 1)->literal)], */
  /*          (long)((ip + 1)->literal), */
  /*          (long)((ip + 2)->literal)); */
  state.frame_bottom[(long)((ip + 2)->literal)]
    = state.frame_bottom[(long)((ip + 1)->literal)];
  /* fprintf (stderr, "after copy:\n"); print_values (& state); */
  ip += 2;
  NEXT;

  LABEL(jump); // Parameters: instruction_index
  GOTO (instructions + (long)((ip + 1)->literal));

  LABEL(jump_if_equal); // Parameters: from_slot, literal_index, instruction_index
  /* fprintf (stderr, "jump_if_equal: discriminand is %li %p\n", */
  /*          (long)state.frame_bottom[(long)((ip + 1)->literal)], */
  /*          state.frame_bottom[(long)((ip + 1)->literal)]); */
  /* fprintf (stderr, "jump_if_equal: immediate is %li %p\n", */
  /*          (long)epsilon_load_with_epsilon_int_offset (code->literals, */
  /*                                                      (long)((ip + 2)->literal)), */
  /*          epsilon_load_with_epsilon_int_offset (code->literals, */
  /*                                                (long)((ip + 2)->literal))); */
  /* fprintf (stderr, "jump_if_equal: instruction_index is %li\n", (long)((ip + 3)->literal)); */
  if (epsilon_load_with_epsilon_int_offset (literals, (long)((ip + 2)->literal))
      == state.frame_bottom[(long)((ip + 1)->literal)])
    {
      /* fprintf (stderr, "JUMP!\n"); */
      GOTO (instructions + (long)((ip + 3)->literal));
    }
  /* else */
  /*   fprintf (stderr, "DON'T jump!\n"); */
  ip += 3;
  NEXT;

  LABEL(literal); // Parameters: literal_index, to_slot
  state.frame_bottom[(long)((ip + 2)->literal)]
    = epsilon_load_with_epsilon_int_offset (literals,
                                            (long)((ip + 1)->literal));
  ip += 2;
  /* fprintf (stderr, "after literal pushing:\n"); print_values (& state); */
  NEXT;

  /* LABEL(procedure_prolog); // Parameters: first_parameter_slot */
  /* // FIXME: shall I save the caller status on the return stack here, or at call time? */
  /* /\* state.return_stack_overtop[0] = state.frame_bottom; *\/ */
  /* /\* state.return_stack_overtop[1] = (void*)(ip + 2); *\/ */
  /* /\* state.return_stack_overtop += 2; *\/ */
  /* state.frame_bottom = state.stack_bottom + (long)((++ ip)->literal); */
  /* //state.stack_overtop += (long)((++ ip)->literal); */
  /* fprintf (stderr, "after procedure_prolog:\n"); print_values (& state); */
  /* NEXT; */

  // FIXME: also add a return1 instruction as an optimization, for the common case.
  LABEL(return); // Parameters: first_result_slot, result_no
  memcpy (state.frame_bottom,
          state.stack_bottom + (long)(ip[1].literal),
          (long)(ip[2].literal) * sizeof (epsilon_value));
  /* fprintf (stderr, "at return: copied %li words from slot%li (to slot0, as always)\n", */
  /*          (long)(ip[2].literal), */
  /*          (long)(ip[1].literal)); */
  // FIXME: this is only for debugging: trash the now-unused part of the stack
  {
    epsilon_value minus1 = epsilon_int_to_epsilon_value (-1);
    int i;
    for (i = 0; i < 10; i ++)
      state.frame_bottom[(long)ip[2].literal + i] = minus1;
  }
  ip += 2;
  /* fprintf (stderr, "at return, before altering pointers:\n"); print_values (& state); */
  state.return_stack_overtop -= 2;
  /* // Set the stack overtop to make place for results. */
  /* state.stack_overtop = state.frame_bottom + ((long)(ip[1].literal)); */
  // Restore the saved frame bottom.
  state.frame_bottom = state.return_stack_overtop[0];
  /* fprintf (stderr, "at return, before jumping:\n"); print_values (& state); */
  /* fprintf (stderr, "at return: jumping.\n"); */
  GOTO (state.return_stack_overtop[1]);

  LABEL(set_literals); // Parameters: literals_slot
  literals = state.stack_bottom[(long)(++ ip)->literal];
  NEXT;


  LABEL(end);
  *original_state = state;
  return;

  /* LABEL(cr); */
  /* printf ("\n"); */
  /* NEXT; */

  /* LABEL(divided); */
  /* ((long*)state.stack_overtop)[-2] /= ((long*)state.stack_overtop)[-1]; */
  /* state.stack_overtop --; */
  /* NEXT; */

  /* LABEL(drop); */
  /* state.stack_overtop --; */
  /* NEXT; */

  /* LABEL(dup); */
  /* *state.stack_overtop = state.stack_overtop[-1]; */
  /* state.stack_overtop ++; */
  /* NEXT; */

  /* LABEL(for); */
  /* state.return_stack_overtop[0] = (epsilon_word)(ip + 1); */
  /* state.return_stack_overtop[1] = *(-- state.stack_overtop); */
  /* state.return_stack_overtop += 2; */
  /* NEXT; */

  /* LABEL(i); */
  /* *(state.stack_overtop ++) = state.return_stack_overtop[-1]; */
  /* NEXT; */

  /* LABEL(j); */
  /* *(state.stack_overtop ++) = state.return_stack_overtop[-3]; */
  /* NEXT; */

  /* LABEL(lit); */
  /* *state.stack_overtop = (++ ip)->literal; */
  /* state.stack_overtop ++; */
  /* NEXT; */

  /* LABEL(minus); */
  /* ((long*)state.stack_overtop)[-2] -= ((long*)state.stack_overtop)[-1]; */
  /* state.stack_overtop --; */
  /* NEXT; */

  /* LABEL(next); */
  /* if (__builtin_expect(((long*)state.return_stack_overtop)[-1], 1) == 0) */
  /*   { */
  /*     state.return_stack_overtop -= 2; */
  /*     NEXT; */
  /*   } */
  /* else */
  /*   { */
  /*     ((long*)state.return_stack_overtop)[-1] --; */
  /*     GOTO (state.return_stack_overtop[-2]); */
  /*   } */

  /* LABEL(nop); */
  /* NEXT; */

  /* LABEL(over); */
  /* state.stack_overtop[0] = state.stack_overtop[-2]; */
  /* state.stack_overtop ++; */
  /* NEXT; */

  /* LABEL(plus); */
  /* // FIXME: this doesn't correctly use tagging */
  /* ((long*)state.stack_overtop)[-2] += ((long*)state.stack_overtop)[-1]; */
  /* state.stack_overtop --; */
  /* NEXT; */

  /* LABEL(print); */
  /* printf ("%li ", */
  /*         epsilon_value_to_epsilon_int (((long**)state.stack_overtop --)[-1])); */
  /* NEXT; */

  /* LABEL(swap); */
  /* { */
  /*   asm("#aaa\n"); */
  /*   epsilon_word tmp = state.stack_overtop[-1]; */
  /*   state.stack_overtop[-1] = state.stack_overtop[-2]; */
  /*   state.stack_overtop[-2] = tmp; */
  /*   NEXT; */
  /* } */

  /* LABEL(times); */
  /* ((long*)state.stack_overtop)[-2] *= ((long*)state.stack_overtop)[-1]; */
  /* state.stack_overtop --; */
  /* NEXT; */

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
  if (tail)
    {
      ejit_push_instruction (s, ejit_opcode_return);
      ejit_push_instruction (s, target_slot);
      ejit_push_instruction (s, result_no);
    }
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
        fprintf (stderr, "compiling variable\n");
        epsilon_value name = epsilon_load_with_epsilon_int_offset (expression, 2);
        long local_index = epsilon_stack_search_last (& s->locals, name);
        if (local_index < 0)
          {
            // FIXME: globals have to be included as literals!
            epsilon_fatal ("%s: unimplemented: global", __func__);
          }
        else
          {
            ejit_push_instruction (s, ejit_opcode_copy);
            ejit_push_instruction (s, local_index);
            ejit_push_instruction (s, target_slot);
          }
        ejit_compile_epilog (s, 1, target_slot, tail);
        break;
      }
    case e0_value_opcode:
      {
        // FIXME: optimizable: in the tail case directly write to slot 0
        fprintf (stderr, "compiling value\n");
        epsilon_value literal
          = epsilon_load_with_epsilon_int_offset (expression, 2);
        ejit_push_instruction (s, ejit_opcode_literal);
        ejit_push_epsilon_literal (s, literal);
        ejit_push_instruction (s, target_slot);
        ejit_compile_epilog (s, 1, target_slot, tail);
        break;
      }
   case e0_bundle_opcode:
      {
        fprintf (stderr, "compiling bundle\n");
        epsilon_value items = epsilon_load_with_epsilon_int_offset (expression, 2);
        long i, item_no = epsilon_value_length (items);
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
        fprintf (stderr, "  items are %li\n", item_no);
        epsilon_value more_items;
        for (more_items = items, i = 0;
             ! epsilon_value_is_null (more_items);
             more_items = epsilon_value_cdr (more_items), i ++)
          {
            fprintf (stderr, "  compiling item %li...\n", i);
            ejit_compile_expression (s,
                                     epsilon_value_car (more_items),
                                     formal_no, nonformal_local_no, result_no,
                                     literals_slot,
                                     target_slot + i,
                                     false);
            fprintf (stderr, "  ...compiled item %li\n", i);
          }
        ejit_compile_epilog (s, item_no, target_slot, tail);
        break;
      }
    case e0_primitive_opcode:
      epsilon_fatal ("unimplemented in the compiler: primitive");
    case e0_let_opcode:
      {
        fprintf (stderr, "compiling let\n");
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
            ejit_push_instruction (s, ejit_opcode_copy);
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
      epsilon_fatal ("unimplemented in the compiler: call");
    case e0_call_indirect_opcode:
      epsilon_fatal ("unimplemented in the compiler: call-indirect");
    case e0_if_in_opcode:
      {
        fprintf (stderr, "compiling if-in\n");
        epsilon_value discriminand = epsilon_load_with_epsilon_int_offset (expression, 2);
        epsilon_value literals = epsilon_load_with_epsilon_int_offset (expression, 3);
        epsilon_value then_branch = epsilon_load_with_epsilon_int_offset (expression, 4);
        epsilon_value else_branch = epsilon_load_with_epsilon_int_offset (expression, 5);
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
            ejit_push_instruction (s, ejit_opcode_jump_if_equal);
            ejit_push_instruction (s, target_slot);
            ejit_push_epsilon_literal (s, literal);
            epsilon_stack_push (& indices_to_backpatch,
                                (epsilon_word)(long)(s->instructions.element_no));
            ejit_push_instruction (s, -1); // to backpatch later
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
            ejit_push_instruction (s, ejit_opcode_jump);
            after_then_source = s->instructions.element_no;
            ejit_push_instruction (s, -1); // to backpatch later
          }
        long then_target = s->instructions.element_no;
        while (! epsilon_stack_empty (& indices_to_backpatch))
          s->instructions.buffer[(long)epsilon_stack_pop (& indices_to_backpatch)]
            = (epsilon_word)then_target;
        ejit_compile_expression (s,
                                 then_branch,
                                 formal_no, nonformal_local_no, result_no,
                                 literals_slot,
                                 target_slot,
                                 tail);
        long after_then_target = s->instructions.element_no;
        if (! tail)
          s->instructions.buffer[after_then_source] = after_then_target;
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
      return 0;
    case e0_value_opcode:
      return 1;
    case e0_bundle_opcode:
      return e0_literal_no_in (epsilon_load_with_epsilon_int_offset (expression, 2));
    case e0_primitive_opcode:
    case e0_call_opcode:
    case e0_fork_opcode:
      return e0_literal_no_in (epsilon_load_with_epsilon_int_offset (expression, 3));
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
              long *literals_slot_pointer)
{
  ejit_code_t res = epsilon_xmalloc (sizeof (struct ejit_code));
  res->initialized = 0;
  struct ejit_compiler_state s;
  ejit_initialize_compiler_state (&s);

  size_t formal_no = epsilon_value_length (formal_list);
  size_t nonformal_local_no  = epsilon_max_nonformal_local_no (expression);
  size_t result_no  = epsilon_result_no (expression);
  fprintf (stderr, "The procedure has %li formals\n", formal_no);
  fprintf (stderr, "The procedure needs %li nonformal locals\n", nonformal_local_no);
  fprintf (stderr, "The procedure has %li results\n", result_no);

  long literal_no = e0_literal_no (expression);
  /* I have to allocate res->literals before compiling: with a moving collector
     this allocation might trigger a GC and change epsilon literals.  Compiling
     doesn't need epsilon heap allocation. */
  res->literals = epsilon_gc_allocate_with_epsilon_int_length (literal_no);
#warning FIXME: make res->literals a GC root unless it's immediately stored into a symbol as it's supposed to be.
  fprintf (stderr, "The procedure has %li literals\n", literal_no);

  long first_slot_after_locals
    =  epsilon_max_long (formal_no + nonformal_local_no,
                         result_no);
  long literals_slot;
  long target_slot;
  if (literal_no == 0)
    {
      literals_slot = -1;
      target_slot = first_slot_after_locals;
    }
  else
    {
      literals_slot = first_slot_after_locals;
      target_slot = first_slot_after_locals + 1;
      ejit_push_instruction (&s, ejit_opcode_set_literals);
      ejit_push_instruction (&s, literals_slot);
    }
  fprintf (stderr, "literals_slot is %li\n", literals_slot);
  fprintf (stderr, "target_slot is %li\n", target_slot);
  epsilon_value more_formals;
  for (more_formals = formal_list;
       ! epsilon_value_is_null (more_formals);
       more_formals = epsilon_value_cdr (more_formals))
    epsilon_stack_push (& s.locals, epsilon_value_car (more_formals));
  ejit_compile_expression (&s, expression,
                           formal_no, nonformal_local_no, result_no,
                           literals_slot,
                           target_slot,
                           literals_slot_pointer == NULL);

  if (literals_slot_pointer)
    {
      * literals_slot_pointer = literals_slot;
      /* Add stub code: */
      fprintf (stderr, "Adding stub end instruction...\n");
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
}

void
ejit_evaluate_expression (epsilon_value expression)
{
  /* Compile the code, with an end instruction in the end.  There's no return
     instruction, and results won't end up in their appropriate slots. */
  epsilon_value epsilon_null = epsilon_int_to_epsilon_value (0);
  long literals_slot;
  ejit_code_t c = ejit_compile (epsilon_null, expression, &literals_slot);

  /* Make an empty thread state.  Since the code is not reached thru a call
     instruction we have to manually set the literals pointer in the stack;
     everything else starts out empty, including the return stack. */
  ejit_thread_state_t s = ejit_make_thread_state ();
  if (literals_slot != -1)
    s->stack_bottom[literals_slot] = c->literals;

  /* Run the code.  In typical usage this will call some procedures to be
     compiled on the fly, and act as the main expression of a larger program. */
  ejit_run_code (c, s);

  print_values (s);

  /* If everything worked at this point we can return, destroying the compiled
     code and the thread state.  Neither will be used anywhere else. */
  ejit_destroy_code (c);
  ejit_destroy_thread_state (s);

  /* We can't return any result; we don't even know how many they are. */
}

////////////////////////////// Scratch

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
