/* This file is part of GNU epsilon.

   Copyright (C) 2012 Université Paris 13
   Copyright (C) 2015 Luca Saiu
   Written by Luca Saiu

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
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>

#include "features.h"
#include "movinggc.h"

#ifdef MOVINGGC_USE_GLOBAL_POINTERS
#ifdef MOVINGGC_USE_REGISTER_POINTERS
// esi edi ebx
//register void **movinggc_fromspace_next_unallocated_word asm ("%esi");
//register void **movinggc_fromspace_after_payload_end asm ("%ebx");
#else // in the #else branch we have !defined(MOVINGGC_USE_REGISTER_POINTERS)
static void **movinggc_fromspace_next_unallocated_word = NULL;;
static void **movinggc_fromspace_after_payload_end = NULL;;
#endif // #ifdef MOVINGGC_USE_REGISTER_POINTERS
#endif // #ifdef MOVINGGC_USE_GLOBAL_POINTERS

#ifdef MOVINGGC_VERBOSE
#define movinggc_log(format, ...) \
  do { fprintf (stderr, format, ## __VA_ARGS__); fflush(stderr); } while (0)
#else
#define movinggc_log(...)       /* do nothing */
#endif // #ifdef MOVINGGC_VERBOSE

#ifdef MOVINGGC_VERY_VERBOSE
#define movinggc_verbose_log(format, ...) \
  do { fprintf (stderr, format, ## __VA_ARGS__); fflush(stderr); } while (0)
#else
#define movinggc_verbose_log(...)       /* do nothing */
#endif // #ifdef MOVINGGC_VERBOSE

#define MOVINGGC_INITIAL_ROOTS_ALLOCATED_SIZE 64

#define MOVINGGC_GENERATION_0_SEMISPACE_WORD_NO \
  (4 * 1024L / sizeof(void*))//(1 * 1024L / sizeof(void*)) //(32 * 1024 * 1024L / sizeof (void*))

#define MOVINGGC_GENERATION_1_SEMISPACE_WORD_NO \
  (256 * 1024L / sizeof(void*)) //(32 * 1024 * 1024L / sizeof (void*))

#define MOVINGGC_GENERATION_2_SEMISPACE_WORD_NO \
  (32 * 1024 * 1024L / sizeof(void*)) //(32 * 1024 * 1024L / sizeof (void*))

#define MOVINGGC_INITIAL_ALLOCATED_ROOT_NO  1 // FIXME: increase

#define if_likely(CONDITION)                    \
  if(__builtin_expect(CONDITION, true))
#define if_unlikely(CONDITION) \
  if(__builtin_expect(CONDITION, false))
#define while_likely(CONDITION) \
  while(__builtin_expect(CONDITION, true))
#define while_unlikely(CONDITION) \
  while(__builtin_expect(CONDITION, false))

#define movinggc_fatal(message, ...)                                        \
  do {                                                                      \
    fprintf(stderr, "Movinggc fatal error: " message "\n", ## __VA_ARGS__); \
    exit(EXIT_FAILURE);                                                     \
  } while (0)

static movinggc_hook_t movinggc_pre_hook;
static movinggc_hook_t movinggc_post_hook;
static void *movinggc_hook_argument;

static const char *movinggc_n0_name = "N0";
static const char *movinggc_a1_name = "A1";
static const char *movinggc_b1_name = "B1";
static const char *movinggc_a2_name = "A2";
static const char *movinggc_b2_name = "B2";
static const char *nonheap_name = "out-of-heap";

struct movinggc_semispace
{
  void **next_unallocated_word;
  void **after_payload_end;
  void **payload_beginning;
  size_t payload_size_in_words;
  const char *name;
}; // struct
typedef struct movinggc_semispace *movinggc_semispace_t;

static struct movinggc_semispace movinggc_semispace_n0;
static struct movinggc_semispace movinggc_semispace_a1;
static struct movinggc_semispace movinggc_semispace_b1;
static struct movinggc_semispace movinggc_semispace_a2;
static struct movinggc_semispace movinggc_semispace_b2;
/* static movinggc_semispace_t movinggc_fromspace; */
/* static movinggc_semispace_t movinggc_tospace; */

static void
movinggc_dump_semispace (movinggc_semispace_t semispace)
{
  long payload_words =
    semispace->after_payload_end - semispace->payload_beginning;
  long words_free =
    semispace->after_payload_end - semispace->next_unallocated_word;
  float words_free_percentage __attribute__ ((unused)) =
    ((float) words_free) / payload_words * 100.0;
  long chars_free __attribute__ ((unused)) = words_free * sizeof (void*);
  fprintf (stderr, "%s %.01fkiB [%p, %p) %.01fkiB(%.01f%%) free\n",
           semispace->name, (float)payload_words * sizeof (void*) / 1024.0,
           semispace->payload_beginning, semispace->after_payload_end,
           chars_free / 1024.0,words_free_percentage);
}

struct movinggc_roots
{
  size_t root_no;
  size_t allocated_root_no;
  void ***roots;
};

typedef struct movinggc_roots*
movinggc_roots_t;

void
movinggc_initialize_roots (movinggc_roots_t roots)
{
  roots->allocated_root_no = MOVINGGC_INITIAL_ALLOCATED_ROOT_NO;
  roots->root_no = 0;
  roots->roots = (void***)
    malloc (sizeof (void**) * MOVINGGC_INITIAL_ALLOCATED_ROOT_NO);
  if_unlikely (roots->roots == NULL)
    movinggc_fatal ("couldn't initialize roots");
}

void
movinggc_finalize_roots (movinggc_roots_t roots)
{
  free (roots->roots);
  roots->root_no = 0;
  roots->allocated_root_no = 0;
  roots->roots = NULL;
}

void
movinggc_push_root (movinggc_roots_t roots, void **new_root)
{
  if_unlikely (roots->root_no == roots->allocated_root_no)
    {
      roots->allocated_root_no *= 2;
      roots->roots = (void***)
        realloc (roots->roots, sizeof (void**) * roots->allocated_root_no);
      if_unlikely (roots->roots == NULL)
        movinggc_fatal ("couldn't resize roots");
    }
  roots->roots[roots->root_no ++] = new_root;
}

void
movinggc_clear_roots (movinggc_roots_t roots)
{
  roots->root_no = 0;
}

typedef struct movinggc_generation*
movinggc_generation_t;

typedef void* (*movinggc_allocate_chars_function_t)
(movinggc_generation_t g, size_t char_no);

/* Allocating in tospace may mean allocating in the older generation. */
typedef void* (*movinggc_allocate_chars_in_tospace_function_t)
(movinggc_generation_t g, size_t char_no);

typedef size_t (*movinggc_gc_words_t)
(movinggc_generation_t g);

typedef size_t (*movinggc_gc_free_words_t)
(movinggc_generation_t g);

typedef void (*movinggc_gc_function_t)
(movinggc_generation_t g);


struct movinggc_generation
{
  const movinggc_generation_index_t generation_index;
  const movinggc_allocate_chars_function_t allocate_chars;
  const movinggc_allocate_chars_in_tospace_function_t allocate_chars_in_tospace;
  const movinggc_gc_words_t words;
  const movinggc_gc_free_words_t free_words;
  const movinggc_gc_function_t gc;
  movinggc_semispace_t fromspace;
  movinggc_semispace_t tospace; /* NULL if not used. */
  movinggc_generation_t const younger_generation;
  movinggc_generation_t const older_generation;
  struct movinggc_roots roots_from_older_generations;
  int gc_no;
};

extern struct movinggc_generation movinggc_generation_0;
extern struct movinggc_generation movinggc_generation_1;
extern struct movinggc_generation movinggc_generation_2;

static void*
movinggc_allocate_chars_from_semispace_generation (movinggc_generation_t
                                                          g,
                                                          size_t size_in_chars)
  __attribute__ ((hot, malloc));

static void*
movinggc_allocate_chars_in_tospace_from_single_semispace_generation (movinggc_generation_t
                                                                     g,
                                                                     size_t size_in_chars)
  __attribute__ ((hot, malloc, unused));

static void*
movinggc_allocate_chars_in_tospace_from_double_semispace_generation (movinggc_generation_t
                                                                     g,
                                                                     size_t size_in_chars)
  __attribute__ ((hot, malloc));

static void
movinggc_gc_generation (movinggc_generation_t g)
  __attribute__ ((cold, noinline));

static size_t
movinggc_words_in_semispace_generation (movinggc_generation_t g);

static size_t
movinggc_free_words_in_semispace_generation (movinggc_generation_t g);

/* struct movinggc_generation */
/* movinggc_generation_0 = */
/*   { */
/*     0, */
/*     movinggc_allocate_chars_from_semispace_generation, */
/*     movinggc_allocate_chars_in_tospace_from_double_semispace_generation, */
/*     movinggc_words_in_semispace_generation, */
/*     movinggc_free_words_in_semispace_generation, */
/*     movinggc_gc_generation, */
/*     & movinggc_semispace_a0, & movinggc_semispace_b0, */
/*     NULL, NULL, */
/*     {0, 0, NULL}, */
/*     0, */
/*   }; */

struct movinggc_generation
movinggc_generation_0 =
  {
    0,
    movinggc_allocate_chars_from_semispace_generation,
    movinggc_allocate_chars_in_tospace_from_single_semispace_generation,
    movinggc_words_in_semispace_generation,
    movinggc_free_words_in_semispace_generation,
    movinggc_gc_generation,
    & movinggc_semispace_n0, NULL,
    NULL, &movinggc_generation_1,
    {0, 0, NULL},
    0,
  };

struct movinggc_generation
movinggc_generation_1 =
  {
    1,
    movinggc_allocate_chars_from_semispace_generation,
    movinggc_allocate_chars_in_tospace_from_single_semispace_generation,
    movinggc_words_in_semispace_generation,
    movinggc_free_words_in_semispace_generation,
    movinggc_gc_generation,
    & movinggc_semispace_a1, NULL,
    & movinggc_generation_0, & movinggc_generation_2,
    {0, 0, NULL},
    0,
  };


struct movinggc_generation
movinggc_generation_2 =
  {
    2,
    movinggc_allocate_chars_from_semispace_generation,
    movinggc_allocate_chars_in_tospace_from_double_semispace_generation,
    movinggc_words_in_semispace_generation,
    movinggc_free_words_in_semispace_generation,
    movinggc_gc_generation,
    & movinggc_semispace_a2, & movinggc_semispace_b2,
    & movinggc_generation_1, NULL,
    {0, 0, NULL},
    0,
  };

/* static const size_t movinggc_generation_no = 1; */
/* static const movinggc_generation_t movinggc_generations[] = */
/*   { & movinggc_generation_0, }; */

static const size_t movinggc_generation_no = 3;
static const movinggc_generation_t movinggc_generations[] =
  {
    & movinggc_generation_0,
    & movinggc_generation_1,
    & movinggc_generation_2,
  };

/* ************************************************************* */

static void
movinggc_dump_semispace_content (movinggc_semispace_t semispace)
{
  movinggc_dump_semispace (semispace);
  void **p;
  for (p = semispace->payload_beginning ;
       p < semispace->next_unallocated_word;//p < semispace->after_payload_end;
       p ++)
    {
      fprintf (stderr, "    %p: untagged %p or %li, tag %li (", p,
               MOVINGGC_UNTAG_POINTER(*p), (long)MOVINGGC_UNTAG_NONPOINTER(*p),
               (long)*p & 1);
      if (MOVINGGC_IS_POINTER(*p))
        fprintf (stderr, "pointer to %s)\n",
                 movinggc_semispace_name_of(MOVINGGC_UNTAG_POINTER(*p)));
      else
        fprintf (stderr, "non-pointer)\n");
    } // for
  fprintf (stderr, "    semispace->next_unallocated_word: %p\n", semispace->next_unallocated_word);
  fprintf (stderr, "    semispace->after_payload_end: %p\n", semispace->after_payload_end);
}

static void movinggc_dump_older_generation_pointers (movinggc_generation_t g)
{
  fprintf (stderr, "  Older generation roots:\n");
  int i;
  for (i = 0; i < g->roots_from_older_generations.root_no; i ++)
    {
      void **p = g->roots_from_older_generations.roots[i];
      fprintf (stderr, "    %p: untagged %p or %li, tag %li (%s)\n", p,
               MOVINGGC_UNTAG_POINTER(*p), (long)MOVINGGC_UNTAG_NONPOINTER(*p),
               (long)*p & 1, MOVINGGC_IS_POINTER(*p) ? "pointer" : "non-pointer");
    } // for
}

typedef void (*movinggc_semispace_function_t) (movinggc_semispace_t s);
typedef void (*movinggc_generation_function_t) (movinggc_generation_t s);

static void
movinggc_call_on_generation (movinggc_generation_t g,
                             movinggc_semispace_function_t sf,
                             movinggc_generation_function_t gf)
{
  while (g != NULL)
    {
      fprintf (stderr, "Generation %i (collected %i times):\n",
               (int)g->generation_index, (int)g->gc_no);
      fprintf (stderr, "  Fromspace: ");
      sf (g->fromspace);
      if (g->tospace)
        {
          fprintf (stderr, "  Tospace:   ");
          sf (g->tospace);
        }
      if (gf != NULL)
        gf (g);
      g = g->older_generation;
    } // while
}

void
movinggc_dump_generations (void)
{
  movinggc_call_on_generation (& movinggc_generation_0,
                               movinggc_dump_semispace,
                               NULL);
}

void
movinggc_dump_generation_contents (void)
{
  movinggc_call_on_generation (& movinggc_generation_0,
                               movinggc_dump_semispace_content,
                               movinggc_dump_older_generation_pointers);
}

static bool
movinggc_is_in_semispace (const void * const untagged_pointer_as_void_star,
                          const movinggc_semispace_t const semispace)
{
  void **untagged_pointer = (void **) untagged_pointer_as_void_star;
  return untagged_pointer >= semispace->payload_beginning
         && untagged_pointer < semispace->after_payload_end;
}

static movinggc_semispace_t
movinggc_semispace_of (const void *untagged_pointer)
{
  movinggc_generation_t g = & movinggc_generation_0;
  while (g != NULL)
    {
      if (movinggc_is_in_semispace (untagged_pointer, g->fromspace))
        return g->fromspace;
      else if (g->tospace != NULL
               && movinggc_is_in_semispace (untagged_pointer, g->tospace))
        return g->tospace;
      g = g->older_generation;
    } // while
  return NULL;
}

const char *
movinggc_semispace_name_of (const void *untagged_pointer_as_void_star)
{
  movinggc_semispace_t semispace =
    movinggc_semispace_of (untagged_pointer_as_void_star);
  if (semispace != NULL)
    return semispace->name;
  else
    return nonheap_name;
}

void
movinggc_finalize_semispace (movinggc_semispace_t semispace)
{
  free (semispace->payload_beginning);
}

void
movinggc_initialize_semispace (movinggc_semispace_t semispace,
                               const char *name,
                               const size_t payload_size_in_words)
{
  /* Allocate the payload: */
  void **semispace_payload;
  semispace_payload = (void **) malloc (payload_size_in_words * sizeof(void*));
  if_unlikely (semispace_payload == NULL)
    movinggc_fatal ("movinggc_initialize_semispace: couldn't allocate");

  /* Set fields: */
  semispace->name = name;
  semispace->payload_beginning = semispace_payload;
  semispace->after_payload_end = semispace_payload + payload_size_in_words;
  semispace->payload_size_in_words = payload_size_in_words;
  semispace->next_unallocated_word = semispace_payload;
}

static void
movinggc_destructively_grow_semispace (movinggc_semispace_t semispace,
                                       size_t new_payload_size_in_words)
  __attribute__ ((unused));
static void
movinggc_destructively_grow_semispace (movinggc_semispace_t semispace,
                                       size_t new_payload_size_in_words)
{
  const char *name = semispace->name;
  movinggc_finalize_semispace (semispace);
  movinggc_initialize_semispace (semispace, name, new_payload_size_in_words);
}

/* This is supposed not to fail.  The function should only be called
   when there is sufficient room in the given semispace. */
static inline void *
movinggc_allocate_from_semispace (movinggc_generation_index_t gi,
                                  movinggc_semispace_t semispace,
                                  size_t size_in_chars)
  __attribute__ ((hot, always_inline, flatten));
static inline void *
movinggc_allocate_from_semispace (movinggc_generation_index_t gi,
                                  movinggc_semispace_t semispace,
                                  size_t size_in_chars)
{
#ifdef MOVINGGC_DEBUG
  if_unlikely (size_in_chars <= 0)
    movinggc_fatal ("object size not positive");
  if_unlikely (size_in_chars % sizeof (void *) != 0)
    movinggc_fatal ("object size not a wordsize multiple");
#endif // #ifdef MOVINGGC_DEBUG
  void **const next_unallocated_word = semispace->next_unallocated_word;
  void **const next_unallocated_word_after_the_new_objext = (void **)
    (((char *) next_unallocated_word) + size_in_chars + sizeof (void *)); // count the header word
#ifdef MOVINGGC_DEBUG
  if_unlikely (next_unallocated_word_after_the_new_objext >
               semispace->after_payload_end)
    {
      movinggc_verbose_log
        ("we were trying to allocate from %s\n",
         semispace->name);
      movinggc_fatal ("not enough space allocating %li chars from %s",
                      (long)size_in_chars, semispace->name);
    }                           // if_unlikely
#endif // #ifdef MOVINGGC_DEBUG

  /* Ok, there is space available; fill the header word, bump the pointer
     and return the next unallocated object: */
  (*next_unallocated_word) =
    MOVINGGC_NONFORWARDING_HEADER (size_in_chars, gi);

#ifdef MOVINGGC_VERBOSE
  /* fprintf (stderr, "size: %li  generation: %li\n", */
  /*          (long)MOVINGGC_NONFORWARDING_HEADER_TO_SIZE(*next_unallocated_word), */
  /*          (long)MOVINGGC_NONFORWARDING_HEADER_TO_GENERATION(*next_unallocated_word)); */
#endif // #ifdef MOVINGGC_VERBOSE

#ifdef MOVINGGC_DEBUG
  /* Initialize the object with invalid words, to make the program
     fail in case a collection is triggered before every field is
     initialized.  This should alert the user. */
  const size_t size_in_words = size_in_chars / sizeof (void*);
  int i;
  for (i = 0; i < size_in_words; i ++)
    next_unallocated_word[i + 1] = MOVINGGC_TAG_POINTER ((void*)0xbadbad0);
#endif // #ifdef MOVINGGC_DEBUG

  semispace->next_unallocated_word =
    next_unallocated_word_after_the_new_objext;
  return ((char *) next_unallocated_word) + sizeof (void *);
}

float
movinggc_fill_ratio_of (movinggc_semispace_t semispace,
                        size_t char_no_to_be_allocated)
{
  const size_t free_word_no =
    semispace->after_payload_end -
    semispace->next_unallocated_word - char_no_to_be_allocated;
  const size_t semispace_size_in_words = semispace->payload_size_in_words;
  return 1.0 - (float) free_word_no / (float) semispace_size_in_words;
}

void
movinggc_initialize (void)
{
  movinggc_initialize_semispace (&movinggc_semispace_n0, movinggc_n0_name,
                                 MOVINGGC_GENERATION_0_SEMISPACE_WORD_NO);
  movinggc_initialize_semispace (&movinggc_semispace_a1, movinggc_a1_name,
                                 MOVINGGC_GENERATION_1_SEMISPACE_WORD_NO);
  movinggc_initialize_semispace (&movinggc_semispace_b1, movinggc_b1_name,
                                 MOVINGGC_GENERATION_1_SEMISPACE_WORD_NO);
  movinggc_initialize_semispace (&movinggc_semispace_a2, movinggc_a2_name,
                                 MOVINGGC_GENERATION_2_SEMISPACE_WORD_NO);
  movinggc_initialize_semispace (&movinggc_semispace_b2, movinggc_b2_name,
                                 MOVINGGC_GENERATION_2_SEMISPACE_WORD_NO);
  /* movinggc_fromspace = &movinggc_semispace_a0; */
  /* movinggc_tospace = &movinggc_semispace_b0; */

  movinggc_pre_hook = NULL;
  movinggc_post_hook = NULL;
  movinggc_hook_argument = NULL;

  movinggc_dump_generations ();
}

struct movinggc_root
{
  /* The address of the candidate pointer *must* be indirect, as we're
     gonna move it at collection time. */
  void **pointer_to_roots;
  size_t size_in_words;
};                              // struct

struct movinggc_root *movinggc_roots = NULL;
size_t movinggc_roots_allocated_size = 0;
size_t movinggc_roots_no = 0;
void
register_roots (void **pointer_to_roots, size_t size_in_words)
{
  /* Grow the array of roots, if needed: */
  if_unlikely (movinggc_roots_no == movinggc_roots_allocated_size)
    {
      movinggc_verbose_log ("Enlarging the root array from %i ",
                            (int) movinggc_roots_allocated_size);
      if (movinggc_roots_allocated_size == 0)
        movinggc_roots_allocated_size = MOVINGGC_INITIAL_ROOTS_ALLOCATED_SIZE;
      else
        movinggc_roots_allocated_size *= 2;
      movinggc_verbose_log ("to %i\n", (int) movinggc_roots_allocated_size);
      movinggc_roots = (struct movinggc_root *)
        realloc (movinggc_roots,
                 sizeof (struct movinggc_root) * movinggc_roots_allocated_size);
      if_unlikely (movinggc_roots ==
                   NULL)
        movinggc_fatal ("register_roots: couldn't enlerge the array");
    } // if

  /* Add the new root: */
  movinggc_roots[movinggc_roots_no].pointer_to_roots = pointer_to_roots;
  movinggc_roots[movinggc_roots_no].size_in_words = size_in_words;
  movinggc_roots_no++;
  /* movinggc_verbose_log("Registered the root %p, whose first word contains %p\n", */
  /*        pointer_to_roots, *pointer_to_roots); */
  /* movinggc_verbose_log("Roots are now %i\n", (int)movinggc_roots_no); */
}

void
movinggc_push_dynamic_root (void **root_pointer)
{
  register_roots (root_pointer, 1);
}

void
movinggc_pop_dynamic_root (void)
{
  movinggc_roots_no--;
}

void
movinggc_pop_dynamic_roots (size_t how_many)
{
  movinggc_roots_no -= how_many;
}

void
movinggc_set_pre_hook (movinggc_hook_t hook)
{
  movinggc_pre_hook = hook;
}

void
movinggc_set_post_hook (movinggc_hook_t hook)
{
  movinggc_post_hook = hook;
}

void
movinggc_set_hook_argument (void *argument)
{
  movinggc_hook_argument = argument;
}

static void
movinggc_flip_spaces (movinggc_generation_t g)
{
  movinggc_semispace_t copy = g->fromspace;
  g->fromspace = g->tospace;
  g->tospace = copy;

  /* Reset the next_unallocated_word of what is now tospace, so that the
     next collection will start to fill it from the beginning: */
  g->tospace->next_unallocated_word = g->tospace->payload_beginning;

#ifdef MOVINGGC_DEBUG
  void **p;
  for (p = g->tospace->payload_beginning;
       p < g->tospace->after_payload_end; p++)
    *p = MOVINGGC_TAG_POINTER((void *) 0xdead20);
  for (p = g->fromspace->next_unallocated_word;
       p < g->fromspace->after_payload_end; p++)
    *p = MOVINGGC_TAG_POINTER((void *) 0xdead30);
#endif // #ifdef MOVINGGC_DEBUG
  movinggc_verbose_log ("Generation %i: flip: the new fromspace is %s\n",
                        (int)g->generation_index, g->fromspace->name);
#ifdef MOVINGGC_VERY_VERBOSE
  movinggc_dump_generations ();
#endif // #ifdef MOVINGGC_VERBOSE
}

static void
movinggc_scavenge_pointer_to_candidate_pointer (movinggc_generation_t fromg,
                                                movinggc_generation_t tog,
                                                movinggc_semispace_t tospace,
                                                void **pointer_to_candidate_pointer);

inline static void *
movinggc_scavenge_pointer (movinggc_generation_t fromg,
                           movinggc_generation_t tog,
                           movinggc_semispace_t tospace,
                           const void *untagged_pointer)
  __attribute__ ((always_inline));

/* Move the given fromspace object and return a tagged pointer to the new tospace
   copy, unless it the parameter points to a forwarding pointer; in that case just
   return a tagged pointer to the tospace copy: */
inline static void *
movinggc_scavenge_pointer (movinggc_generation_t fromg,
                           movinggc_generation_t tog,
                           movinggc_semispace_t tospace,
                           const void *untagged_pointer)
{
  /* If we arrived here then the parameter refers a valid tagged pointer pointing
     within fromspace. */
#ifdef MOVINGGC_DEBUG
  // This is now incorrect: a root can very well point to a semispace
  // not belonging to the 0-th generation.
  /* if_unlikely (movinggc_semispace_of (untagged_pointer) != fromspace) */
  /*   { */
  /*     movinggc_dump_generation_contents (); */
  /*     movinggc_fatal ("%p (%s) is not in fromspace (%s)", untagged_pointer, */
  /*                     movinggc_semispace_name_of (untagged_pointer), */
  /*                     fromspace->name); */
  /*   } */
#endif // #ifdef MOVINGGC_DEBUG

  /* Check whether the parameter refers a forwarding pointer: */
  const void *tagged_header = ((const void **) untagged_pointer)[-1];
  if_unlikely (MOVINGGC_IS_FORWARDING (tagged_header))
    {
      void **untagged_forwarding_pointer =
        MOVINGGC_FORWARDING_HEADER_TO_DESTINATION (tagged_header);
      movinggc_verbose_log ("%p (%s) forwards to %p (%s)\n",
                            untagged_pointer,
                            movinggc_semispace_name_of (untagged_pointer),
                            untagged_forwarding_pointer,
                            movinggc_semispace_name_of
                            (untagged_forwarding_pointer));
      return MOVINGGC_TAG_POINTER (untagged_forwarding_pointer);
    } // if

#ifdef MOVINGGC_DEBUG
  /* Check that the header has a valid tag: */
  if_unlikely (!MOVINGGC_IS_NONFORWARDING (tagged_header))
    {
      movinggc_verbose_log ("tagged_header is %p\n", tagged_header);
      movinggc_fatal ("tagged_header is both forwarding and non-forwarding");
    }
#endif // #ifdef MOVINGGC_DEBUG

  /* Do nothing if the pointer belongs to a different generation. */
  movinggc_generation_index_t pointer_generation_index =
    MOVINGGC_NONFORWARDING_HEADER_TO_GENERATION(tagged_header);
  if_unlikely (pointer_generation_index != fromg->generation_index)
    {
      movinggc_verbose_log
        ("* not scavenging pointer %p (generation %i, not %i)\n",
         untagged_pointer,
         (int)pointer_generation_index, (int)fromg->generation_index);
      return (void*)MOVINGGC_TAG_POINTER(untagged_pointer);
    }

  /* Ok, the parameter refers a fromspace object which is not a forwarding pointer;
     we have to copy it and install a forwarding pointer in the original pointer
     object: */
  const size_t size_in_chars =
    (movinggc_bitmask_t)
    MOVINGGC_NONFORWARDING_HEADER_TO_SIZE (tagged_header);
#ifdef MOVINGGC_DEBUG
  if_unlikely (size_in_chars <= 0)
    movinggc_fatal ("corrupted header: object size not positive");
  if_unlikely (size_in_chars % sizeof (void *) != 0)
    movinggc_fatal ("corrupted header: object size not a wordsize multiple");
#endif // #ifdef MOVINGGC_DEBUG

  const void **object_in_tospace =
    movinggc_allocate_from_semispace (tog->generation_index, tospace, size_in_chars);
    //fromg->allocate_chars_in_tospace (fromg, size_in_chars); // FIXME: why in the word is this faster?
  ((const void **) untagged_pointer)[-1] =
    MOVINGGC_FORWARDING_HEADER (object_in_tospace);
  movinggc_verbose_log ("* scavenging %p (%iB, %s) to %p (%s)\n",
                        untagged_pointer,
                        (int) size_in_chars,
                        movinggc_semispace_name_of (untagged_pointer),
                        object_in_tospace,
                        movinggc_semispace_name_of (object_in_tospace));

  /* Now we have to copy object fields into the new copy, and scavenge
     the new copy (or just push the pointers to the words to be changed
     onto the stack, to be scavenged later): */
#ifdef MOVINGGC_USE_MEMCPY
  memcpy (object_in_tospace, untagged_pointer, size_in_chars);
#else
  int i;
  const size_t size_in_words = size_in_chars / sizeof (void *);
  for (i = 0; i < size_in_words; i++)
    object_in_tospace[i] = ((const void **) untagged_pointer)[i];
#endif // #ifdef MOVINGGC_USE_MEMCPY

#ifdef MOVINGGC_DEBUG
  /* Clear the original object, so that we can't use it by mistake. */
  memset ((void *) untagged_pointer, -1, size_in_chars);
#endif // #ifdef MOVINGGC_DEBUG

  /* Return a tagged pointer to the new copy: */
  return MOVINGGC_TAG_POINTER (object_in_tospace);
}

static void
movinggc_scavenge_pointer_to_candidate_pointer (movinggc_generation_t fromg,
                                                movinggc_generation_t tog,
                                                movinggc_semispace_t tospace,
                                                void **pointer_to_candidate_pointer)
{
  /* Dereference the pointer to the candidate pointer; this is always
     safe if the parameter is, in fact, a pointer to something: */
  const void *tagged_candidate_pointer = *pointer_to_candidate_pointer;

  /* Is the candidate pointer really a pointer to an object of the
     generation we are interested in?  Scavenge it if it is, and update
     the pointer-to-pointer; otherwise we have nothing to do: */
  if (MOVINGGC_IS_POINTER (tagged_candidate_pointer))
    {
      const void *untagged_pointer =
        MOVINGGC_UNTAG_POINTER (tagged_candidate_pointer);
#ifdef MOVINGGC_DEBUG
      movinggc_semispace_t semispace =
        movinggc_semispace_of (untagged_pointer);
      if_unlikely (semispace == NULL)
        movinggc_fatal ("pointer %p (tagged %p) points out of the heap",
                        untagged_pointer, tagged_candidate_pointer);
#endif // #ifdef MOVINGGC_DEBUG

      *pointer_to_candidate_pointer =
        movinggc_scavenge_pointer (fromg, tog, tospace, untagged_pointer);
    }
  else
    movinggc_verbose_log
      ("* not scavenging non-pointer %li or %p (tagged %p)\n",
       (long int) (MOVINGGC_UNTAG_NONPOINTER (tagged_candidate_pointer)),
       MOVINGGC_UNTAG_NONPOINTER (tagged_candidate_pointer),
       tagged_candidate_pointer);
}

/* Cheney's two-finger algorithm.  Just to be clear about conventions,
   here the left finger moves from the beginning towards the end of
   tospace until it meets its allocation pointer, which is actually
   the right finger; the right fingers moves further right as new
   objects are scavenged from fromspace to tospace.  The left finger
   always points to tospace object headers, never to object fields. */
static void movinggc_two_fingers (movinggc_generation_t fromg,
                                  movinggc_generation_t tog,
                                  void **initial_left_finger,
                                  movinggc_semispace_t fromspace,
                                  movinggc_semispace_t tospace)
{
#ifdef MOVINGGC_VERBOSE
  long scavenged_pointer_no = 0;
#endif // #ifdef MOVINGGC_VERBOSE
  void **left_finger = initial_left_finger;
  while (left_finger < (void**)tospace->next_unallocated_word)
    {
      size_t object_size_in_bytes =
        MOVINGGC_NONFORWARDING_HEADER_TO_SIZE(*left_finger);
#ifdef MOVINGGC_DEBUG
      if_unlikely (! MOVINGGC_IS_NONFORWARDING(*left_finger))
        movinggc_fatal ("corrupted tospace scavenged header %p (forwarding)", *left_finger);
      if_unlikely (object_size_in_bytes <= 0
                   || object_size_in_bytes % sizeof (void*) != 0)
        movinggc_fatal ("corrupted tospace scavenged header %p (wrong size)", *left_finger);
#endif // #ifdef MOVINGGC_DEBUG
      size_t object_size_in_words = object_size_in_bytes / sizeof (void*);
      int i;
      for (i = 1; i <= object_size_in_words; i ++)
        {
          movinggc_scavenge_pointer_to_candidate_pointer (fromg, tog,
                                                          tospace, left_finger + i);
#ifdef MOVINGGC_VERBOSE
          scavenged_pointer_no ++;
#endif // #ifdef MOVINGGC_VERBOSE
        }
      left_finger += i;
#ifdef MOVINGGC_DEBUG
      if_unlikely (left_finger > (void**)tospace->next_unallocated_word)
        movinggc_fatal ("left finger crossed an object boundary");
#endif // #ifdef MOVINGGC_DEBUG
    } // while
#ifdef MOVINGGC_VERBOSE
  movinggc_log ("Scavenged %li candidate pointers\n", scavenged_pointer_no);
#endif // #ifdef MOVINGGC_VERBOSE
}

static void
movinggc_run_hook (movinggc_hook_t hook)
{
  if (hook == NULL)
    return;
  char *name __attribute__ ((unused))
    = (hook == movinggc_pre_hook) ? "pre" : "post";
  movinggc_verbose_log ("Entering %s hook  (roots are %i)...\n",
                        name, (int) movinggc_roots_no);
  hook (movinggc_hook_argument);
  movinggc_verbose_log ("...exited %s hook (roots are %i).\n",
                        name, (int) movinggc_roots_no);
}

static void
movinggc_scavenge_roots (movinggc_generation_t fromg,
                         movinggc_generation_t tog,
                         movinggc_semispace_t tospace)
{
#ifdef MOVINGGC_DEBUG
  size_t static_root_no = movinggc_roots_no;
#endif // #ifdef MOVINGGC_DEBUG

  movinggc_run_hook (movinggc_pre_hook);

#ifdef MOVINGGC_VERY_VERBOSE
  movinggc_dump_generations ();
#endif // #ifdef MOVINGGC_VERY_VERBOSE

#ifdef MOVINGGC_DEBUG
  // No longer correct with multiple generations.
  /* if_unlikely (tospace != NULL */
  /*              && movinggc_fill_ratio_of(tospace, 0) != 0.0) */
  /*   movinggc_fatal ("tospace (%s) is not empty", tospace->name); */
#endif // #ifdef MOVINGGC_DEBUG

  /* Scavenge static and dynamic roots. */
  int root_index;
#ifdef MOVINGGC_VERBOSE
  long scavenged_root_pointer_no = 0;
#endif // #ifdef MOVINGGC_VERBOSE
  for (root_index = 0; root_index < movinggc_roots_no; root_index++)
    {
      void **candidate_pointers = (void **)
        movinggc_roots[root_index].pointer_to_roots;
      const int word_no = movinggc_roots[root_index].size_in_words;
      int word_index;
      for (word_index = 0; word_index < word_no; word_index++)
        movinggc_scavenge_pointer_to_candidate_pointer (fromg, tog, tospace,
                                                        candidate_pointers + word_index);
#ifdef MOVINGGC_VERBOSE
      scavenged_root_pointer_no ++;
#endif // #ifdef MOVINGGC_VERBOSE
    } // for
#ifdef MOVINGGC_VERBOSE
  movinggc_log ("Scavenged %li candidate root pointers\n", scavenged_root_pointer_no);
#endif // #ifdef MOVINGGC_VERBOSE

  movinggc_run_hook (movinggc_post_hook);

#ifdef MOVINGGC_DEBUG
  if_unlikely (static_root_no != movinggc_roots_no)
    movinggc_fatal ("hooks disagree about dynamic root number");
#endif // #ifdef MOVINGGC_DEBUG

  /* Scavenge inter-generational pointers. */
  void ***roots = fromg->roots_from_older_generations.roots;
  const size_t root_no = fromg->roots_from_older_generations.root_no;
  int i;
  for (i = 0; i < root_no; i ++)
    movinggc_scavenge_pointer_to_candidate_pointer (fromg, tog, tospace, roots[i]);
}

size_t
movinggc_words_in_generation (movinggc_generation_t g)
{
  return g->words (g);
}

size_t
movinggc_free_words_in_generation (movinggc_generation_t g)
{
  return g->free_words (g);
}

size_t
movinggc_used_words_in_generation (movinggc_generation_t g)
{
  return movinggc_words_in_generation (g)
         - movinggc_free_words_in_generation (g);
}

void
movinggc_scan_previous_generations (movinggc_generation_t younger_than,
                                    movinggc_generation_t tog,
                                    movinggc_semispace_t tospace)
{
  //movinggc_generation_index_t index = younger_than->generation_index;
  movinggc_generation_t fromg;
  for (fromg = younger_than->younger_generation;
       fromg != NULL;
       fromg = fromg->younger_generation)
    {
      movinggc_log ("Scanning generation %i\n", (int)fromg->generation_index);
      movinggc_semispace_t fromspace = fromg->fromspace;
      void **p = fromspace->payload_beginning;
      while (p < fromspace->next_unallocated_word)
        {
          void *header = *p;
          size_t field_no =
            MOVINGGC_NONFORWARDING_HEADER_TO_SIZE(header) / sizeof (void*);
#ifdef MOVINGGC_DEBUG
          if_unlikely (MOVINGGC_IS_FORWARDING(header))
            movinggc_fatal ("forwarging header at %p scanning younger generations", p);
          if_unlikely (field_no <= 0)
            movinggc_fatal ("field_no is %i\n", (int)field_no);
#endif // #ifdef MOVINGGC_DEBUG
          int i;
          for (i = 1; i <= field_no; i ++)
            movinggc_scavenge_pointer_to_candidate_pointer (younger_than, tog, tospace, p + i);
          p += i;
        } // while
    } // for
}

void
movinggc_gc_generation (movinggc_generation_t g)
{
  /* Collecting this generation might promote objects into next older
     one, so we first have to make sure it has enough space. */
  movinggc_generation_t next_older = g->older_generation;
  if (next_older != NULL
      && movinggc_free_words_in_generation (next_older)
         < movinggc_used_words_in_generation (g))
    {
      movinggc_log ("Not enough room in generation %i to collect generation %i\n",
                    (int)next_older->generation_index, (int)g->generation_index);
      movinggc_gc_generation (next_older);
      if_unlikely (movinggc_free_words_in_generation (next_older)
                   < movinggc_used_words_in_generation (g))
        movinggc_fatal ("not enough space in next generation");
    }

  movinggc_semispace_t const fromspace = g->fromspace;
  movinggc_semispace_t const tospace
    = g->tospace ? g->tospace : g->older_generation->fromspace;
#ifdef MOVINGGC_VERBOSE
  movinggc_log ("[%i-GC %s->%s: BEGIN...\n", (int)g->generation_index,
                fromspace->name, tospace->name);
  /* movinggc_dump_generations (); */
  /* movinggc_log ("]\n"); */
#endif // #ifdef MOVINGGC_VERBOSE

  movinggc_generation_t tog = g->tospace ? g : g->older_generation;

  /* Remember where the part to scavenge in tospace begins, before
     touching roots. */
  void **initial_left_finger = tospace->next_unallocated_word;

  /* Any generation younger than the one we are collecting is
     effectively a set of roots.  Scan them.
     [FIXME: What shall I do about inter-generational pointers?
     I'm not sure the current solution is correct.] */
  movinggc_scan_previous_generations (g, tog, tospace);

  movinggc_scavenge_roots (g, tog, tospace);

  /* fprintf (stderr, "\n-----------------\nBEFORE TWO-FINGERS: BEGIN\n"); */
  /* movinggc_dump_generation_contents (); */
  /* fprintf (stderr, "BEFORE TWO-FINGERS: END\n-----------------\n\n"); */

  movinggc_two_fingers (g, tog, initial_left_finger, fromspace, tospace);

  if (g->tospace != NULL)
    movinggc_flip_spaces (g);
  else
    fromspace->next_unallocated_word = fromspace->payload_beginning;

  /* Clear inter-generational pointers for all generations younger
     than this one. */
  movinggc_generation_t younger_g;
  for (younger_g = g->younger_generation;
       younger_g != NULL;
       younger_g = younger_g->younger_generation)
    {
#ifdef MOVINGGC_DEBUG
      /* if_unlikely (movinggc_free_words_in_generation (younger_g) */
      /*              < movinggc_words_in_generation (younger_g)) */
      /*   movinggc_fatal ("generation %i is not empty after %i-GC: I fear it should be", */
      /*                   (int)younger_g->generation_index, (int)g->generation_index); */
#endif // #ifdef MOVINGGC_DEBUG
      movinggc_clear_roots (& younger_g->roots_from_older_generations);
    } // for

  g->gc_no ++;
#ifdef MOVINGGC_VERBOSE
  /* movinggc_log ("[\n"); */
  movinggc_dump_generations ();
  movinggc_log ("...%i-GC %s->%s: END (scavenged %.02fKiB)]\n", (int)g->generation_index,
                fromspace->name, tospace->name,
                (float)(tospace->next_unallocated_word - tospace->payload_beginning) * sizeof(void*) / 1024.0);
#endif // #ifdef MOVINGGC_VERBOSE
  /* movinggc_dump_generation_contents (); */
  /* fprintf (stderr, "-----------------\nAFTER GC]\n\n"); */
}

void
movinggc_full_gc (void)
{
  /* GC all generations, from the youngest to the oldest. */
  movinggc_generation_t g;
  for (g = & movinggc_generation_0; g != NULL; g = g->older_generation)
    g->gc (g);
}

void *
movinggc_allocate_words (size_t size_in_words)
{
  return movinggc_allocate_chars (size_in_words * sizeof (void *));
}

void *
movinggc_allocate_cons (void)
{
  return movinggc_allocate_words (2);
}

void *
movinggc_allocate_chars (size_t size_in_chars)
{
  return movinggc_generation_0.allocate_chars(& movinggc_generation_0,
                                              size_in_chars);
}

static size_t
movinggc_words_in_semispace_generation (movinggc_generation_t g)
{
  return g->fromspace->after_payload_end - g->fromspace->payload_beginning;
}

static size_t
movinggc_free_words_in_semispace_generation (movinggc_generation_t g)
{
  return g->fromspace->after_payload_end - g->fromspace->next_unallocated_word;
}

void movinggc_write_barrier (void **untagged_initial_pointer,
                             long offset_in_words)
{
  movinggc_generation_index_t pointer_generation =
    MOVINGGC_NONFORWARDING_HEADER_TO_GENERATION(untagged_initial_pointer[-1]);
  if (pointer_generation != 0)
    {
      movinggc_generation_index_t i;
      for (i = pointer_generation - 1; i >= 0; i --)
        movinggc_push_root (& movinggc_generations[i]->roots_from_older_generations,
                            untagged_initial_pointer + offset_in_words);
    } // if
}

void *
movinggc_allocate_chars_in_tospace_from_single_semispace_generation (movinggc_generation_t g,
                                                                     size_t size_in_chars)
{
#ifdef MOVINGGC_DEBUG
  if_unlikely (g->older_generation == NULL)
    movinggc_fatal ("there's no older generation to scavenge into");
#endif // #ifdef MOVINGGC_DEBUG
  return g->older_generation->allocate_chars(g->older_generation, size_in_chars);
}

void *
movinggc_allocate_chars_in_tospace_from_double_semispace_generation (movinggc_generation_t g,
                                                                     size_t size_in_chars)
{
  return movinggc_allocate_from_semispace (g->generation_index, g->tospace, size_in_chars);
}


static void *
movinggc_allocate_chars_from_semispace_generation_slow_path (movinggc_generation_t g,
                                                             size_t size_in_chars)
  __attribute__ ((cold, noinline));

static void *
movinggc_allocate_chars_from_semispace_generation_slow_path (movinggc_generation_t g,
                                                             size_t size_in_chars)
{
  /* There is not enough room in fromspace.  Notice that this collection might
     trigger collections in older generations as well. */
  movinggc_gc_generation (g);

  /* Do we have have enough room available in fromspace? */
  movinggc_semispace_t fromspace = g->fromspace;
  if_unlikely (((char *) fromspace->next_unallocated_word)
               + size_in_chars + sizeof (void *)
               > (char *) fromspace->after_payload_end)
    movinggc_fatal ("not enough room for allocating right after a collection");

  /* This time we are sure there is room. */
  return movinggc_allocate_from_semispace (g->generation_index, g->fromspace, size_in_chars);
}

/* Using char* instead of void** may save a few instructions (tested:
   one, on a better-written test, on both x86_64 and MIPS, gcc 4.9.2
   -Ofast, on moore).  Here it's important.  FIXME: rewrite looking at
   the generated assembly. */
void *
movinggc_allocate_chars_from_semispace_generation (movinggc_generation_t g,
                                                   size_t size_in_chars)
{
#ifdef MOVINGGC_DEBUG
  if_unlikely (size_in_chars <= 0)
    movinggc_fatal ("movinggc_allocate_chars: object size not positive");
  if_unlikely (size_in_chars % sizeof (void *) != 0)
    movinggc_fatal
    ("movinggc_allocate_chars: object size not a wordsize multiple");
#endif // #ifdef MOVINGGC_DEBUG

  movinggc_semispace_t const fromspace = g->fromspace;

  movinggc_verbose_log ("Attempting an allocation from %s...\n",
                        fromspace->name);

  /* Do we have have enough room available in fromspace? */
  if_unlikely (((char *) fromspace->next_unallocated_word)
               + size_in_chars + sizeof (void *)
               > (char *) fromspace->after_payload_end)
    return movinggc_allocate_chars_from_semispace_generation_slow_path (g, size_in_chars);

  /* Ok, now we can allocate. */
  void *res = movinggc_allocate_from_semispace (g->generation_index, fromspace, size_in_chars);

  movinggc_verbose_log ("...Allocated %p(%li) (%liB, %s)\n",
                        res, (long) res, size_in_chars,
                        movinggc_semispace_name_of (res));
#ifdef MOVINGGC_DEBUG
  if_unlikely (movinggc_semispace_of (res) != fromspace)
    movinggc_fatal ("%p allocated from %s instead of fromspace (%s)", res,
                    movinggc_semispace_name_of (res), fromspace->name);
#endif // #ifdef MOVINGGC_DEBUG
  return res;
}
