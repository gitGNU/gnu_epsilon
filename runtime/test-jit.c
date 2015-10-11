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

/* The generated code, which is allocated on the C heap and manually freed.
   Again, the structure is intentionally opaque. */
typedef struct ejit_code* ejit_code_t;

ejit_code_t
ejit_compile (epsilon_value expression, epsilon_value formal_list)
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
#include <assert.h>

#include "runtime.h"
#include "config.h"

struct ejit_thread_state
{
  epsilon_word *stack_overtop;
  epsilon_word *stack_bottom;
  epsilon_word *return_stack_overtop;
  epsilon_word *return_stack_bottom;
};

enum ejit_opcode_enum
  {
    ejit_opcode_cr,
    ejit_opcode_divided,
    ejit_opcode_drop,
    ejit_opcode_dup,
    ejit_opcode_end,
    ejit_opcode_for,
    ejit_opcode_i,
    ejit_opcode_j,
    ejit_opcode_lit,
    ejit_opcode_minus,
    ejit_opcode_next,
    ejit_opcode_nop,
    ejit_opcode_over,
    ejit_opcode_plus,
    ejit_opcode_print,
    ejit_opcode_swap,
    ejit_opcode_times,
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
  /* An array of instructions or instruction arguments, sequentially allocated
     in the C heap. */
  ejit_instruction_t* instructions;
  size_t instruction_no;

  /* An epsilon buffer holding the epsilon constants referred in the code.  The
     constants aren't directly embedded within the instruction because that
     would interfere with moving GCs; hence instructions needing epsilon
     literals as parameters only refer them thru *indices* of elements in this
     array. */
  epsilon_word referred_constants; // FIXME: make this a GC root
  size_t referred_constant_no;

  /* Only used for debugging (FIXME: but so cheap that I might alway keep it
     enabled). */
  int initialized;
};

#define CASE_SET_LABEL(name, arity) \
  case ejit_opcode_ ## name:           \
    printf("Initializing: %i. %s\n", (int)(p - instructions), #name);  \
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

#ifdef ENABLE_JIT_THREADING
#define LABEL(name) \
  label_ ## name:   \
  /* printf("%i. %s\n", (int)(ip - instructions), #name); */ \
  /* printf("  ip == %p, ip->label == %p, label_%s == %p\n", ip, ip->label, #name, && label_ ## name);  */\
  /* printf("  Heights at entry: %i %i\n", (int)(state.stack_overtop - state.stack_bottom), (int)(state.return_stack_overtop - state.return_stack_bottom)); */
#else
#define LABEL(name) \
  case ejit_opcode_ ## name:
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
              CASE_SET_LABEL(cr, 0);
              CASE_SET_LABEL(divided, 0);
              CASE_SET_LABEL(drop, 0);
              CASE_SET_LABEL(dup, 0);
              CASE_SET_LABEL(end, 0);
              CASE_SET_LABEL(for, 0);
              CASE_SET_LABEL(i, 0);
              CASE_SET_LABEL(j, 0);
              CASE_SET_LABEL(nop, 0);
              CASE_SET_LABEL(over, 0);
              CASE_SET_LABEL(lit, 1);
              CASE_SET_LABEL(minus, 0);
              CASE_SET_LABEL(next, 0);
              CASE_SET_LABEL(plus, 0);
              CASE_SET_LABEL(print, 0);
              CASE_SET_LABEL(swap, 0);
              CASE_SET_LABEL(times, 0);
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
  /* printf ("\ninstructions: %p\n", instructions); */
  /* printf ("p:            %p\n", p); */
  /* printf ("overtop:      %p\n", state.stack_overtop); */
  FIRST;

#ifndef ENABLE_JIT_THREADING
  while (1)
    switch (ip->opcode)
      {
#endif // #ifndef ENABLE_JIT_THREADING

  LABEL(cr);
  printf ("\n");
  NEXT;

  LABEL(divided);
  ((long*)state.stack_overtop)[-2] /= ((long*)state.stack_overtop)[-1];
  state.stack_overtop --;
  NEXT;

  LABEL(drop);
  state.stack_overtop --;
  NEXT;

  LABEL(dup);
  *state.stack_overtop = state.stack_overtop[-1];
  state.stack_overtop ++;
  NEXT;

  LABEL(for);
  state.return_stack_overtop[0] = (epsilon_word)(ip + 1);
  state.return_stack_overtop[1] = *(-- state.stack_overtop);
  state.return_stack_overtop += 2;
  NEXT;

  LABEL(i);
  *(state.stack_overtop ++) = state.return_stack_overtop[-1];
  NEXT;

  LABEL(j);
  *(state.stack_overtop ++) = state.return_stack_overtop[-3];
  NEXT;

  LABEL(end);
  *original_state = state;
  return;

  LABEL(lit);
  *state.stack_overtop = (++ ip)->literal;
  state.stack_overtop ++;
  NEXT;

  LABEL(minus);
  ((long*)state.stack_overtop)[-2] -= ((long*)state.stack_overtop)[-1];
  state.stack_overtop --;
  NEXT;

  LABEL(next);
  if (__builtin_expect(((long*)state.return_stack_overtop)[-1], 1) == 0)
    {
      state.return_stack_overtop -= 2;
      NEXT;
    }
  else
    {
      ((long*)state.return_stack_overtop)[-1] --;
      GOTO (state.return_stack_overtop[-2]);
    }

  LABEL(nop);
  NEXT;

  LABEL(over);
  state.stack_overtop[0] = state.stack_overtop[-2];
  state.stack_overtop ++;
  NEXT;

  LABEL(plus);
  ((long*)state.stack_overtop)[-2] += ((long*)state.stack_overtop)[-1];
  state.stack_overtop --;
  NEXT;

  LABEL(print);
  printf ("%li ", ((long*)state.stack_overtop --)[-1]);
  NEXT;

  LABEL(swap);
  {
    asm("#aaa\n");
    epsilon_word tmp = state.stack_overtop[-1];
    state.stack_overtop[-1] = state.stack_overtop[-2];
    state.stack_overtop[-2] = tmp;
    NEXT;
  }

  LABEL(times);
  ((long*)state.stack_overtop)[-2] *= ((long*)state.stack_overtop)[-1];
  state.stack_overtop --;
  NEXT;

#ifndef ENABLE_JIT_THREADING
      } /* switch */
#endif // #ifndef ENABLE_JIT_THREADING
}
#undef SET_LABEL
#undef FIRST
#undef NEXT
#undef GOTO
#undef LABEL

void
ejit_initialize_code (ejit_code_t code)
{
  ejit_initialize_or_run_code (1, code, NULL);
}

void
ejit_run_code (ejit_code_t code, ejit_thread_state_t state)
{
  ejit_initialize_or_run_code (0, code, state);
}

ejit_code_t
ejit_compile (epsilon_value expression, epsilon_value formal_list)
{
  /*
  ejit_instruction_t* instructions;
  size_t instruction_no;

  epsilon_word referred_constants;
  size_t referred_constant_no;

  int initialized;
*/
  ejit_code_t res =
    epsilon_xmalloc (sizeof (struct ejit_code));
  res->initialized = 0;
  epsilon_fatal ("FIXME: compile...");
  // FIXME: set res->instructions, res->instruction_no, res->referred_constants
  // and referred_constant_no after first building a resizable vector.
  ejit_initialize_code (res);
  return res;
}

void
ejit_destroy_code (const ejit_code_t c)
{
  free (c->instructions);
  // FIXME: remove referred_constants from GC roots.
  // No need to touch referred_constants, which are GC-allocated.
  free (c);
}

//////////////////////////////


#define N 1000

int
main (void)
{
  ejit_instruction_t the_instructions[N];
  struct ejit_code the_code;
  the_code.instructions = the_instructions;
  int i = 0;

  the_code.instructions[i ++].opcode = ejit_opcode_lit;
  the_code.instructions[i ++].literal = (epsilon_word)(long)10000000;
  the_code.instructions[i ++].opcode = ejit_opcode_for;

  the_code.instructions[i ++].opcode = ejit_opcode_lit;
  the_code.instructions[i ++].literal = (epsilon_word)(long)10;
  the_code.instructions[i ++].opcode = ejit_opcode_for;

  the_code.instructions[i ++].opcode = ejit_opcode_lit;
  the_code.instructions[i ++].literal = (epsilon_word)(long)10;
  the_code.instructions[i ++].opcode = ejit_opcode_for;
  the_code.instructions[i ++].opcode = ejit_opcode_lit;
  the_code.instructions[i ++].literal = (epsilon_word)(long)10;
  the_code.instructions[i ++].opcode = ejit_opcode_i;
  the_code.instructions[i ++].opcode = ejit_opcode_minus;
  the_code.instructions[i ++].opcode = ejit_opcode_drop;
  /* the_code.instructions[i ++].opcode = ejit_opcode_print; */
  /* the_code.instructions[i ++].opcode = ejit_opcode_j; */
  /* the_code.instructions[i ++].opcode = ejit_opcode_print; */
  the_code.instructions[i ++].opcode = ejit_opcode_next;

  /* the_code.instructions[i ++].opcode = ejit_opcode_cr; */

  the_code.instructions[i ++].opcode = ejit_opcode_next;

  /* the_code.instructions[i ++].opcode = ejit_opcode_cr; */

  the_code.instructions[i ++].opcode = ejit_opcode_next;

  the_code.instructions[i ++].opcode = ejit_opcode_end;
  the_code.instruction_no = i;

  printf ("%i instructions (%i bytes)\n", (int)the_code.instruction_no,
          (int)(the_code.instruction_no * sizeof (ejit_instruction_t)));

  ejit_initialize_code (&the_code);

  struct ejit_thread_state the_state;
  epsilon_word the_stack[N];
  epsilon_word the_return_stack[N];
  the_state.stack_bottom = the_stack;
  the_state.stack_overtop = the_stack;
  the_state.return_stack_bottom = the_return_stack;
  the_state.return_stack_overtop = the_return_stack;

  /* for (i = 0; i < 100000000; i ++) */
    {
      //printf ("%i\n", i);
      ejit_run_code (&the_code, &the_state);
    }
  return EXIT_SUCCESS;
}
