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
#include <sys/times.h>

#include "features.h"
#include "movinggc.h"

#ifdef EGC_USE_GLOBAL_POINTERS
#ifdef EGC_USE_REGISTER_POINTERS
// esi edi ebx
//register void **egc_fromspace_next_unallocated_word asm ("%esi");
//register void **egc_fromspace_after_payload_end asm ("%ebx");
#else // in the #else branch we have !defined(EGC_USE_REGISTER_POINTERS)
static void **egc_fromspace_next_unallocated_word = NULL;;
static void **egc_fromspace_after_payload_end = NULL;;
#endif // #ifdef EGC_USE_REGISTER_POINTERS
#endif // #ifdef EGC_USE_GLOBAL_POINTERS

#ifdef EGC_VERBOSE
#define egc_log(format, ...)                                            \
  do { fprintf (stderr, format, ## __VA_ARGS__); fflush(stderr); } while (0)
#else
#define egc_log(...)       /* do nothing */
#endif // #ifdef EGC_VERBOSE

#ifdef EGC_VERY_VERBOSE
#define egc_verbose_log(format, ...)                                    \
  do { fprintf (stderr, format, ## __VA_ARGS__); fflush(stderr); } while (0)
#else
#define egc_verbose_log(...)       /* do nothing */
#endif // #ifdef EGC_VERBOSE

/* FIXME: make names more explicit. */
#define EGC_INITIAL_ROOTS_ALLOCATED_SIZE 64
#define EGC_INITIAL_ALLOCATED_ROOT_NO  1 // FIXME: increase

#define if_likely(CONDITION)                    \
  if(__builtin_expect(CONDITION, true))
#define if_unlikely(CONDITION)                  \
  if(__builtin_expect(CONDITION, false))
#define while_likely(CONDITION)                 \
  while(__builtin_expect(CONDITION, true))
#define while_unlikely(CONDITION)               \
  while(__builtin_expect(CONDITION, false))

#define egc_fatal(message, ...)                                         \
  do {                                                                  \
    fprintf(stderr, "eGC fatal error: " message "\n", ## __VA_ARGS__);  \
    exit(EXIT_FAILURE);                                                 \
  } while (0)

static egc_hook_t egc_pre_hook;
static egc_hook_t egc_post_hook;
static void *egc_hook_argument;

struct egc_semispace
{
  void **next_unallocated_word;
  void **after_payload_end;
  void **payload_beginning;
  size_t payload_size_in_words;
  char name[10];
}; // struct
typedef struct egc_semispace *egc_semispace_t;

/* In a mark-sweep heap the allocation pointer, which is always
   untagged, is NULL when the heap is full, or points to the first
   word of the first free block otherwise.
   For each block:
   i)  the first word contains the block size in words (which includes
   the first two words), tagged as nonforwarding with generation 0;
   ii) the second word contains an untagged pointer to the next free
   block, or NULL. */
struct egc_marksweep_heap
{
  void **payload_beginning;
  size_t payload_size_in_words;
  void **allocation_pointer;
  //size_t allocated_word_no; // FIXME: no, this is inefficient to keep updated.
  char name[10];
}; // struct
typedef struct egc_marksweep_heap *egc_marksweep_heap_t;

#ifdef EGC_TIME
static double egc_ticks_per_second;
double
egc_get_current_time (void)
{
#ifdef HAS_CLOCK_GETTIME
  struct timespec t;
  clock_gettime (CLOCK_REALTIME
                 //CLOCK_MONOTONIC_RAW//CLOCK_REALTIME_COARSE//CLOCK_PROCESS_CPUTIME_ID
                 , &t);
  return t.tv_sec + t.tv_nsec / 1000000000.0;
#else
  struct tms t;
  if_unlikely ((int)times (& t) == -1)
    egc_fatal ("times failed");
  return (double)(t.tms_utime + t.tms_stime) / egc_ticks_per_second;
#endif // #ifdef HAS_CLOCK_GETTIME
}
static double egc_initialization_time;
#endif // #ifdef EGC_TIME

static void
egc_dump_semispace (egc_semispace_t semispace)
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

static void
egc_dump_marksweep_heap (egc_marksweep_heap_t h)
{
  long payload_words = h->payload_size_in_words;
  fprintf (stderr, "%s %.01fkiB [%p, %p)\n",
           h->name, (float)payload_words * sizeof (void*) / 1024.0,
           h->payload_beginning, h->payload_beginning + payload_words);
}

static void
egc_dump_marksweep_heap_content (egc_marksweep_heap_t h)
{
  egc_dump_marksweep_heap (h);
  fprintf (stderr, "(content dump unimplemented for mark-sweep heaps)\n");
}

struct egc_roots
{
  size_t root_no;
  size_t allocated_root_no;
  void ***roots;
};

typedef struct egc_roots*
egc_roots_t;

void
egc_initialize_roots (egc_roots_t roots)
{
  roots->allocated_root_no = EGC_INITIAL_ALLOCATED_ROOT_NO;
  roots->root_no = 0;
  roots->roots = (void***)
    malloc (sizeof (void**) * EGC_INITIAL_ALLOCATED_ROOT_NO);
  if_unlikely (roots->roots == NULL)
    egc_fatal ("couldn't initialize roots");
}

void
egc_finalize_roots (egc_roots_t roots)
{
  free (roots->roots);
  roots->root_no = 0;
  roots->allocated_root_no = 0;
  roots->roots = NULL;
}

void
egc_push_root (egc_roots_t roots, void **new_root)
{
  if_unlikely (roots->root_no == roots->allocated_root_no)
    {
      roots->allocated_root_no *= 2;
      roots->roots = (void***)
        realloc (roots->roots, sizeof (void**) * roots->allocated_root_no);
      if_unlikely (roots->roots == NULL)
        egc_fatal ("couldn't resize roots");
    }
  roots->roots[roots->root_no ++] = new_root;
}

void
egc_clear_roots (egc_roots_t roots)
{
  roots->root_no = 0;
}

typedef void* (*egc_allocate_chars_function_t)
  (egc_generation_t g, size_t char_no);

/* Allocating in tospace may mean allocating in the older generation. */
typedef void* (*egc_allocate_chars_in_tospace_function_t)
  (egc_generation_t g, size_t char_no);

typedef size_t (*egc_gc_words_function_t)
(egc_generation_t g);

typedef size_t (*egc_gc_free_words_function_t)
(egc_generation_t g);

typedef void (*egc_gc_generation_function_t)
(egc_generation_t g);

enum egc_generation_type
  {
    egc_generation_type_semispace,
    egc_generation_type_marksweep,
    egc_generation_type_large,
  };
typedef enum egc_generation_type
egc_generation_type_t;

struct egc_generation
{
  egc_generation_index_t generation_index;
  egc_generation_t next_younger;
  egc_generation_t next_older;

  egc_generation_type_t type;
  union
  {
    struct /* type == egc_generation_type_semispace */
    {
      egc_semispace_t fromspace;
      egc_semispace_t tospace; /* NULL if not used. */
    };
    struct /* type == egc_generation_type_marksweep */
    {
      egc_marksweep_heap_t marksweep_heap;
    };
  }; // union

  /* Statistics. */
  int gc_no;
  double scavenged_words;
  double gc_time;

  egc_allocate_chars_function_t allocate_chars;
  egc_allocate_chars_in_tospace_function_t allocate_chars_in_tospace;
  egc_gc_generation_function_t gc_generation;
  egc_gc_words_function_t words;
  egc_gc_free_words_function_t free_words;

  struct egc_roots roots_from_older_generations;
};

static void*
egc_allocate_chars_from_semispace_generation (egc_generation_t g,
                                              size_t size_in_chars)
  __attribute__ ((hot, malloc));

static void*
egc_allocate_chars_from_marksweep_generation (egc_generation_t g,
                                              size_t size_in_chars)
  __attribute__ ((hot, malloc));

static void*
egc_allocate_chars_in_tospace_from_single_semispace_generation (egc_generation_t g,
                                                                size_t size_in_chars)
  __attribute__ ((hot, malloc, unused));

static void*
egc_allocate_chars_in_tospace_from_double_semispace_generation (egc_generation_t g,
                                                                size_t size_in_chars)
  __attribute__ ((hot, malloc));

static void
egc_gc_semispace_generation (egc_generation_t g)
  __attribute__ ((cold, noinline));
static void
egc_gc_marksweep_generation (egc_generation_t g)
  __attribute__ ((cold, noinline));
static void
egc_gc_generation (egc_generation_t g)
  __attribute__ ((cold, noinline));

static size_t
egc_words_in_semispace_generation (egc_generation_t g);

static size_t
egc_free_words_in_semispace_generation (egc_generation_t g);

static size_t
egc_words_in_marksweep_generation (egc_generation_t g);

static size_t
egc_free_words_in_marksweep_generation (egc_generation_t g);

/* ************************************************************* */

static egc_generation_t egc_generations;

/* ************************************************************* */

static void
egc_dump_semispace_content (egc_semispace_t semispace)
{
  egc_dump_semispace (semispace);
  void **p;
  for (p = semispace->payload_beginning ;
       p < semispace->next_unallocated_word;//p < semispace->after_payload_end;
       p ++)
    {
      fprintf (stderr, "    %p: untagged %p or %li, tag %li (", p,
               EGC_UNTAG_POINTER(*p), (long)EGC_UNTAG_NONPOINTER(*p),
               (long)*p & 1);
      if (EGC_IS_POINTER(*p))
        fprintf (stderr, "pointer to %s)\n",
                 egc_heap_name_of(EGC_UNTAG_POINTER(*p)));
      else
        fprintf (stderr, "non-pointer)\n");
    } // for
  fprintf (stderr, "    semispace->next_unallocated_word: %p\n", semispace->next_unallocated_word);
  fprintf (stderr, "    semispace->after_payload_end: %p\n", semispace->after_payload_end);
}

static void egc_dump_older_generation_pointers (egc_generation_t g)
{
  fprintf (stderr, "  Older generation roots:\n");
  int i;
  for (i = 0; i < g->roots_from_older_generations.root_no; i ++)
    {
      void **p = g->roots_from_older_generations.roots[i];
      fprintf (stderr, "    %p: untagged %p or %li, tag %li (%s)\n", p,
               EGC_UNTAG_POINTER(*p), (long)EGC_UNTAG_NONPOINTER(*p),
               (long)*p & 1, EGC_IS_POINTER(*p) ? "pointer" : "non-pointer");
    } // for
}

typedef void (*egc_semispace_function_t) (egc_semispace_t s);
typedef void (*egc_marksweep_heap_function_t) (egc_marksweep_heap_t s);
typedef void (*egc_generation_function_t) (egc_generation_t s);

static void
egc_call_on_generation (egc_generation_t g,
                        egc_semispace_function_t sf,
                        egc_marksweep_heap_function_t mf,
                        egc_generation_function_t gf)
{
  while (g != NULL)
    {
      long word_no =
        g->fromspace->after_payload_end - g->fromspace->payload_beginning;
      fprintf (stderr, "Generation %i (collected %12i times",
               (int)g->generation_index, (int)g->gc_no);
      if (g->gc_no != 0)
        fprintf (stderr, ", %5.01f%% survival",
                 g->scavenged_words / word_no / g->gc_no * 100);
#ifdef EGC_TIME
      if (g->gc_no != 0)
        fprintf (stderr, ", %.9fs average", g->gc_time / g->gc_no);
#endif // #ifdef EGC_TIME
      fprintf (stderr, "):\n");
      switch (g->type)
        {
        case egc_generation_type_semispace:
          fprintf (stderr, "  Fromspace: ");
          sf (g->fromspace);
          if (g->tospace)
            {
              fprintf (stderr, "  Tospace:   ");
              sf (g->tospace);
            }
          break;
        case egc_generation_type_marksweep:
          fprintf (stderr, "  Marksweep: ");
          mf (g->marksweep_heap);
          break;
        default:
          egc_fatal ("unknown generation type %i", (int)g->type);
        }
      if (gf != NULL)
        gf (g);
      g = g->next_older;
    } // while
}

void
egc_dump_generations (void)
{
  egc_call_on_generation (egc_generations,
                          egc_dump_semispace,
                          egc_dump_marksweep_heap,
                          NULL);
}

void
egc_dump_generation_contents (void)
{
  egc_call_on_generation (egc_generations,
                          egc_dump_semispace_content,
                          egc_dump_marksweep_heap_content,
                          egc_dump_older_generation_pointers);
}

void
egc_dump_times (void)
{
#ifdef EGC_TIME
  double now = egc_get_current_time ();
  double total_time = now - egc_initialization_time;

  fprintf (stderr, "Total run time %.3fs:\n", total_time);
  double total_gc_time = 0.0;
  egc_generation_t g;
  for (g = egc_generations; g != NULL; g = g->next_older)
    total_gc_time += g->gc_time;
  double total_mutator_time = total_time - total_gc_time;
  fprintf (stderr, "  total mutator time %.3fs (%.1f%%)\n", total_mutator_time,
           total_mutator_time / total_time * 100);
  fprintf (stderr, "  total GC time      %.3fs (%.1f%%", total_gc_time,
           total_gc_time / total_time * 100);
  if (total_gc_time > 0.0)
    {
      fprintf (stderr, ", of which: ");
      bool first = true;
      for (g = egc_generations; g != NULL; g = g->next_older)
        {
          if (first)
            first = false;
          else
            fprintf (stderr, ", ");
          fprintf (stderr, "%.1f%% g%i",
                   g->gc_time / total_gc_time * 100, (int)g->generation_index);
        } // for
      first = true;
      fprintf (stderr, ")\n");
      fprintf (stderr, "                                    (absolute ");
      for (g = egc_generations; g != NULL; g = g->next_older)
        {
          if (first)
            first = false;
          else
            fprintf (stderr, ", ");
          fprintf (stderr, "%.1f%% g%i",
                   g->gc_time / total_time * 100, (int)g->generation_index);
        } // for
    } // if
  fprintf (stderr, ").\n");
#else
  fprintf (stderr, "(Timing not configured in.)\n");
#endif // #ifdef EGC_TIME
}

static bool
egc_is_in_semispace (const void * const untagged_pointer_as_void_star,
                     const egc_semispace_t const semispace)
{
  void **untagged_pointer = (void **) untagged_pointer_as_void_star;
  return untagged_pointer >= semispace->payload_beginning
    && untagged_pointer < semispace->after_payload_end;
}

static bool
egc_is_in_marksweep_heap (const void * const untagged_pointer_as_void_star,
                          const egc_marksweep_heap_t const marksweep_heap)
{
  void **untagged_pointer = (void **) untagged_pointer_as_void_star;
  return untagged_pointer >= marksweep_heap->payload_beginning
    && untagged_pointer < marksweep_heap->payload_beginning
                          + marksweep_heap->payload_size_in_words;
}

static egc_semispace_t
egc_semispace_of_untagged (const void *untagged_pointer)
{
  egc_generation_t g;
  for (g = egc_generations; g != NULL; g = g->next_older)
    {
      if (g->type != egc_generation_type_semispace)
        continue;
      if (egc_is_in_semispace (untagged_pointer, g->fromspace))
        return g->fromspace;
      else if (g->tospace != NULL
               && egc_is_in_semispace (untagged_pointer, g->tospace))
        return g->tospace;
    } // for
  return NULL;
}

static egc_marksweep_heap_t
egc_marksweep_heap_of_untagged (const void *untagged_pointer)
{
  egc_generation_t g;
  for (g = egc_generations; g != NULL; g = g->next_older)
    {
      if (g->type != egc_generation_type_marksweep)
        continue;
      if (egc_is_in_marksweep_heap (untagged_pointer, g->marksweep_heap))
        return g->marksweep_heap;
    } // for
  return NULL;
}

const char *
egc_heap_name_of (const void *untagged_pointer_as_void_star)
{
  egc_semispace_t semispace =
    egc_semispace_of_untagged (untagged_pointer_as_void_star);
  if (semispace != NULL)
    return semispace->name;
  egc_marksweep_heap_t marksweep_heap =
    egc_marksweep_heap_of_untagged (untagged_pointer_as_void_star);
  if (marksweep_heap != NULL)
    return marksweep_heap->name;
  else
    return "out-of-heap";
}

void
egc_finalize_semispace (egc_semispace_t semispace)
{
  free (semispace->payload_beginning);
}

static void
egc_initialize_semispace (egc_semispace_t s, size_t word_no, char *name);

static void
egc_destructively_grow_semispace (egc_semispace_t semispace,
                                  size_t new_payload_size_in_words)
  __attribute__ ((unused));
static void
egc_destructively_grow_semispace (egc_semispace_t semispace,
                                  size_t new_payload_size_in_words)
{
  char *name = semispace->name;
  egc_finalize_semispace (semispace);
  egc_initialize_semispace (semispace, new_payload_size_in_words, name);
}

/* This is supposed not to fail.  The function should only be called
   when there is sufficient room in the given semispace. */
static inline void *
egc_allocate_from_semispace (egc_generation_index_t gi,
                             egc_semispace_t semispace,
                             size_t size_in_chars)
  __attribute__ ((hot, always_inline, flatten));
static inline void *
egc_allocate_from_semispace (egc_generation_index_t gi,
                             egc_semispace_t semispace,
                             size_t size_in_chars)
{
#ifdef EGC_DEBUG
  if_unlikely (size_in_chars <= 0)
    egc_fatal ("object size not positive");
  if_unlikely (size_in_chars % sizeof (void *) != 0)
    egc_fatal ("object size not a wordsize multiple");
#endif // #ifdef EGC_DEBUG
  void **const next_unallocated_word = semispace->next_unallocated_word;
  void **const next_unallocated_word_after_the_new_objext = (void **)
    (((char *) next_unallocated_word) + size_in_chars + sizeof (void *)); // count the header word
#ifdef EGC_DEBUG
  if_unlikely (next_unallocated_word_after_the_new_objext >
               semispace->after_payload_end)
    {
      egc_verbose_log
        ("we were trying to allocate from %s\n",
         semispace->name);
      egc_fatal ("not enough space allocating %li chars from %s",
                 (long)size_in_chars, semispace->name);
    }                           // if_unlikely
#endif // #ifdef EGC_DEBUG

  /* Ok, there is space available; fill the header word, bump the pointer
     and return the next unallocated object: */
  (*next_unallocated_word) =
    EGC_NONFORWARDING_HEADER (size_in_chars, gi);

#ifdef EGC_VERBOSE
  /* fprintf (stderr, "size: %li  generation: %li\n", */
  /*          (long)EGC_NONFORWARDING_HEADER_TO_SIZE(*next_unallocated_word), */
  /*          (long)EGC_NONFORWARDING_HEADER_TO_GENERATION(*next_unallocated_word)); */
#endif // #ifdef EGC_VERBOSE

#ifdef EGC_DEBUG
  /* Initialize the object with invalid words, to make the program
     fail in case a collection is triggered before every field is
     initialized.  This should alert the user. */
  const size_t size_in_words = size_in_chars / sizeof (void*);
  int i;
  for (i = 0; i < size_in_words; i ++)
    next_unallocated_word[i + 1] = EGC_TAG_POINTER ((void*)0xbadbad0);
#endif // #ifdef EGC_DEBUG

  semispace->next_unallocated_word =
    next_unallocated_word_after_the_new_objext;
  return ((char *) next_unallocated_word) + sizeof (void *);
}

float
egc_fill_ratio_of (egc_semispace_t semispace,
                   size_t char_no_to_be_allocated)
{
  const size_t free_word_no =
    semispace->after_payload_end -
    semispace->next_unallocated_word - char_no_to_be_allocated;
  const size_t semispace_size_in_words = semispace->payload_size_in_words;
  return 1.0 - (float) free_word_no / (float) semispace_size_in_words;
}

static void
egc_initialize_generation (int generation_index,
                           egc_generation_t generation)
{
#ifdef EGC_DEBUG
  /* Intentionally fill every field with invalid data. */
  memset (generation, -1, sizeof (struct egc_generation));
#endif // #ifdef EGC_DEBUG
  generation->generation_index = generation_index;

  generation->roots_from_older_generations.root_no = 0;
  generation->roots_from_older_generations.allocated_root_no = 0;
  generation->roots_from_older_generations.roots = NULL;

  generation->gc_no = 0;
  generation->scavenged_words = 0;
  generation->gc_time = 0;
}

egc_generation_t
egc_make_generations (size_t generation_no)
{
  if_unlikely (generation_no > 1 << EGC_GENERATION_BIT_NO)
    egc_fatal ("not enough header generation bits to represent %i generations",
               (int)generation_no);
  egc_generation_t res = malloc (sizeof(struct egc_generation) * generation_no);
  if (res == NULL)
    egc_fatal ("couldn't allocate generations");
  int i;
  for (i = 0; i < generation_no; i ++)
    egc_initialize_generation (i, res + i);
  egc_generations = res;
  return res;
}

static void
egc_initialize_semispace (egc_semispace_t s, size_t word_no, char *name)
{
  void **semispace_payload = (void **) malloc (word_no * sizeof(void*));
  if_unlikely (semispace_payload == NULL)
    egc_fatal ("egc_initialize_semispace: couldn't allocate");
  s->next_unallocated_word = semispace_payload;
  s->payload_beginning = semispace_payload;
  s->after_payload_end = semispace_payload + word_no;
  s->payload_size_in_words = word_no;
  strcpy (s->name, name);
}

static egc_semispace_t
egc_make_semispace (size_t word_no, char *name)
{
  egc_semispace_t res = (egc_semispace_t)malloc (sizeof (struct egc_semispace));
  if_unlikely (res == NULL)
    egc_fatal ("couldn't allocate semispace");
  egc_initialize_semispace (res, word_no, name);
  return res;
}

void
egc_initialize_semispace_generation (egc_generation_t generation,
                                     int semispace_no, /* 1 or 2 */
                                     size_t word_no)
{
  generation->type = egc_generation_type_semispace;
  if (semispace_no < 0 || semispace_no > 2)
    egc_fatal ("a semispace generation can have only one or two semispaces");
  generation->words = egc_words_in_semispace_generation;
  generation->free_words = egc_free_words_in_semispace_generation;
  generation->allocate_chars = egc_allocate_chars_from_semispace_generation;
  if (semispace_no == 1)
    generation->allocate_chars_in_tospace = egc_allocate_chars_in_tospace_from_single_semispace_generation;
  else
    generation->allocate_chars_in_tospace = egc_allocate_chars_in_tospace_from_double_semispace_generation;
  generation->gc_generation = egc_gc_semispace_generation;
  char name[10];
  sprintf (name, "G%i%s", (int)generation->generation_index,
           (semispace_no == 1) ? "" : "-A");
  generation->fromspace = egc_make_semispace (word_no, name);
  if (semispace_no == 1)
    generation->tospace = NULL;
  else
    {
      sprintf (name, "G%i-B", (int)generation->generation_index);
      generation->tospace = egc_make_semispace (word_no, name);
    }
}

static void
egc_initialize_marksweep_heap (egc_marksweep_heap_t s, size_t word_no, char *name)
{
  void **marksweep_heap_payload = (void **) malloc (word_no * sizeof(void*));
  if_unlikely (marksweep_heap_payload == NULL)
    egc_fatal ("egc_initialize_marksweep_heap: couldn't allocate");
  s->payload_beginning = marksweep_heap_payload;
  s->payload_size_in_words = word_no;
  strcpy (s->name, name);
  s->allocation_pointer = marksweep_heap_payload;
  /* At the beginning there is only one big slot in the free list, of size
     word_no (in words). */
  s->allocation_pointer[0] = EGC_NONFORWARDING_HEADER(word_no, 0);
  s->allocation_pointer[1] = NULL;
}

static egc_marksweep_heap_t
egc_make_marksweep_heap (size_t word_no, char *name)
{
  egc_marksweep_heap_t res = (egc_marksweep_heap_t)malloc (sizeof (struct egc_marksweep_heap));
  if_unlikely (res == NULL)
    egc_fatal ("couldn't allocate marksweep_heap");
  egc_initialize_marksweep_heap (res, word_no, name);
  return res;
}

static size_t
egc_words_in_marksweep_generation (egc_generation_t g)
{
  egc_fatal ("egc_words_in_marksweep_generation: unimplemented");
}

static size_t
egc_free_words_in_marksweep_generation (egc_generation_t g)
{
  egc_fatal ("egc_free_words_in_marksweep_generation: unimplemented");
}

/* This is a simple first-fit. */
static void*
egc_allocate_words_from_marksweep_generation (egc_generation_t g,
                                              size_t size_in_words)
{
  bool did_we_gc = false;
  do
    {
      egc_marksweep_heap_t h = g->marksweep_heap;
      void **p;
      for (p = h->allocation_pointer; p != NULL; p = (void**)p[1])
        {
          size_t block_size_in_words = EGC_NONFORWARDING_HEADER_TO_SIZE(*p);
          if (size_in_words + 1 > block_size_in_words)
            {
              fprintf (stderr, "the block at %p is %i words long, and I need %i: trying next\n", p,
                       (int)block_size_in_words, (int)size_in_words + 1);
              continue;
            }
          size_t new_empty_block_size = block_size_in_words - size_in_words - 1;
          switch (new_empty_block_size)
            {
            case 0:
              /* We used the entire free block. */
              h->allocation_pointer = (void**)p[1];
              break;
            case 1:
              /* We used the entire free block minus one word.  That's
                 not enough space to make a new block, so we just
                 initialize the unused word to a non-pointer to avoid
                 confusing the GC, and then forget about it. */
              h->allocation_pointer = (void**)p[1];
              p[size_in_words + 1] = EGC_TAG_NONPOINTER(0xf00f000);
              break;
            default:
              {
                /* Make a smaller block from the block we partially used. */
#ifdef EGC_DEBUG
                if_unlikely (new_empty_block_size < 2)
                  egc_fatal ("wrong new empty block size");
#endif // #ifdef EGC_DEBUG
                void **new_empty_block = p + size_in_words + 1;
                h->allocation_pointer = new_empty_block;
                new_empty_block[1] = p[1];
                new_empty_block[0] = EGC_NONFORWARDING_HEADER (new_empty_block_size, 0);
              }
            } // switch
          //fprintf (stderr, "Allocated %p\n", p + 1);
          p[0] = EGC_NONFORWARDING_HEADER (size_in_words * sizeof(void*),
                                           g->generation_index);
          return p + 1;
        } // while
      if (did_we_gc)
        egc_fatal ("couldn't free up space after GC'ing");
      else
        {
          egc_gc_marksweep_generation (g);
          did_we_gc = true;
        }
    } while (true);
}

static void*
egc_allocate_chars_from_marksweep_generation (egc_generation_t g,
                                              size_t size_in_chars)
{
#ifdef EGC_DEBUG
  if_unlikely (size_in_chars <= 0)
    egc_fatal ("egc_allocate_chars_from_marksweep_generation: object size %li not positive",
               (long)size_in_chars);
  if_unlikely (size_in_chars % sizeof (void *) != 0)
    egc_fatal
    ("egc_allocate_chars_from_marksweep_generation: object size %li not a wordsize multiple",
     (long)size_in_chars);
#endif // #ifdef EGC_DEBUG
  return egc_allocate_words_from_marksweep_generation (g, size_in_chars / sizeof(void*));
}

static void
egc_gc_marksweep_generation (egc_generation_t g)
{
  egc_fatal ("egc_gc_marksweep_generation: unimplemented");
}

void
egc_initialize_marksweep_generation (egc_generation_t generation,
                                     size_t word_no)
{
  generation->type = egc_generation_type_marksweep;
  generation->words = egc_words_in_marksweep_generation;
  generation->free_words = egc_free_words_in_marksweep_generation;
  generation->allocate_chars = egc_allocate_chars_from_marksweep_generation;
  generation->allocate_chars_in_tospace = NULL; // FIXME: I might need this in the end.
  generation->gc_generation = egc_gc_marksweep_generation;
  char name[10];
  sprintf (name, "G%i", (int)generation->generation_index);
  generation->marksweep_heap = egc_make_marksweep_heap (word_no, name);
}

void egc_link_generations (egc_generation_t generations, size_t generation_no)
{
  int i; egc_generation_t g;
  for (i = 0, g = generations; i < generation_no; i ++, g ++)
    {
      if (i == 0)
        g->next_younger = NULL;
      else
        g->next_younger = g - 1;
      if (i != generation_no - 1)
        {
          g->next_older = g + 1;
          /* A semispace generation which is not the last one can only
             have one semispaces, since we do immediate promotion. */
          if (g->type == egc_generation_type_semispace
              && g->tospace != NULL)
            egc_fatal ("generation %i (of %i) has two semispaces",
                       i, (int)generation_no);
        }
      else
        {
          g->next_older = NULL;
          /* The last generation, if it's semispace, must have two
             semispaces; otherwise there's nowhere to promote to. */
          if (g->type == egc_generation_type_semispace
              && g->tospace == NULL)
            egc_fatal ("generation %i (of %i) has one semispace",
                       i, (int)generation_no);
        }
    } // for
  /* FIXME: check that the size of each generation is sufficient for
     worst-case promotion. */
}

void
egc_initialize (void)
{
#if 0
  int generation_no = 3, i;
  egc_generation_t generations = egc_make_generations (generation_no);
  for (i = 0; i < generation_no; i ++)
    egc_initialize_semispace_generation (generations + i,
                                         (i == generation_no - 1) ? 2 : 1,
                                         1024 * 4 * (1 << i)*(1 << i)*(1 << i)*(1 << i)*(1 << i));
#else
  int generation_no = 1, i;
  egc_generation_t generations = egc_make_generations (generation_no);
  for (i = 0; i < generation_no; i ++)
    egc_initialize_marksweep_generation (generations + i, 0.5 * 1000 * 1024 * 1024L);
#endif
  //egc_initialize_semispace_generation (generations + 0, 1, EGC_GENERATION_0_WORD_NO);
  //egc_initialize_semispace_generation (generations + 1, 1, EGC_GENERATION_1_WORD_NO);
  //egc_initialize_semispace_generation (generations + 2, 2, EGC_GENERATION_2_WORD_NO);
  egc_link_generations (generations, generation_no);

#ifdef EGC_TIME
  egc_ticks_per_second = (double)sysconf (_SC_CLK_TCK);
  egc_initialization_time = egc_get_current_time ();
#endif // #ifdef EGC_TIME

  egc_pre_hook = NULL;
  egc_post_hook = NULL;
  egc_hook_argument = NULL;

  egc_dump_generations ();
}

struct egc_root
{
  /* The address of the candidate pointer *must* be indirect, as we're
     gonna move it at collection time. */
  void **pointer_to_roots;
  size_t size_in_words;
};                              // struct

struct egc_root *egc_roots = NULL;
size_t egc_roots_allocated_size = 0;
size_t egc_roots_no = 0;
void
egc_register_roots (void **pointer_to_roots, size_t size_in_words)
{
  /* Grow the array of roots, if needed: */
  if_unlikely (egc_roots_no == egc_roots_allocated_size)
    {
      egc_verbose_log ("Enlarging the root array from %i ",
                       (int) egc_roots_allocated_size);
      if (egc_roots_allocated_size == 0)
        egc_roots_allocated_size = EGC_INITIAL_ROOTS_ALLOCATED_SIZE;
      else
        egc_roots_allocated_size *= 2;
      egc_verbose_log ("to %i\n", (int) egc_roots_allocated_size);
      egc_roots = (struct egc_root *)
        realloc (egc_roots,
                 sizeof (struct egc_root) * egc_roots_allocated_size);
      if_unlikely (egc_roots ==
                   NULL)
        egc_fatal ("egc_register_roots: couldn't enlerge the array");
    } // if

  /* Add the new root: */
  egc_roots[egc_roots_no].pointer_to_roots = pointer_to_roots;
  egc_roots[egc_roots_no].size_in_words = size_in_words;
  egc_roots_no++;
  /* egc_verbose_log("Registered the root %p, whose first word contains %p\n", */
  /*        pointer_to_roots, *pointer_to_roots); */
  /* egc_verbose_log("Roots are now %i\n", (int)egc_roots_no); */
}

void
egc_push_dynamic_root (void **root_pointer)
{
  egc_register_roots (root_pointer, 1);
}

void
egc_pop_dynamic_root (void)
{
  egc_roots_no--;
}

void
egc_pop_dynamic_roots (size_t how_many)
{
  egc_roots_no -= how_many;
}

void
egc_set_pre_hook (egc_hook_t hook)
{
  egc_pre_hook = hook;
}

void
egc_set_post_hook (egc_hook_t hook)
{
  egc_post_hook = hook;
}

void
egc_set_hook_argument (void *argument)
{
  egc_hook_argument = argument;
}

static void
egc_flip_spaces (egc_generation_t g)
{
  egc_semispace_t copy = g->fromspace;
  g->fromspace = g->tospace;
  g->tospace = copy;

  /* Reset the next_unallocated_word of what is now tospace, so that the
     next collection will start to fill it from the beginning: */
  g->tospace->next_unallocated_word = g->tospace->payload_beginning;

#ifdef EGC_DEBUG
  void **p;
  for (p = g->tospace->payload_beginning;
       p < g->tospace->after_payload_end; p++)
    *p = EGC_TAG_POINTER((void *) 0xdead20);
  for (p = g->fromspace->next_unallocated_word;
       p < g->fromspace->after_payload_end; p++)
    *p = EGC_TAG_POINTER((void *) 0xdead30);
#endif // #ifdef EGC_DEBUG
  egc_verbose_log ("Generation %i: flip: the new fromspace is %s\n",
                   (int)g->generation_index, g->fromspace->name);
#ifdef EGC_VERY_VERBOSE
  egc_dump_generations ();
#endif // #ifdef EGC_VERBOSE
}

static void
egc_scavenge_pointer_to_candidate_pointer (egc_generation_t fromg,
                                           egc_generation_t tog,
                                           egc_semispace_t tospace,
                                           void **pointer_to_candidate_pointer);

inline static void *
egc_scavenge_pointer (egc_generation_t fromg,
                      egc_generation_t tog,
                      egc_semispace_t tospace,
                      const void *untagged_pointer)
  __attribute__ ((always_inline));

/* Move the given fromspace object and return a tagged pointer to the new tospace
   copy, unless it the parameter points to a forwarding pointer; in that case just
   return a tagged pointer to the tospace copy: */
inline static void *
egc_scavenge_pointer (egc_generation_t fromg,
                      egc_generation_t tog,
                      egc_semispace_t tospace,
                      const void *untagged_pointer)
{
  /* If we arrived here then the parameter refers a valid tagged pointer pointing
     within fromspace. */
#ifdef EGC_DEBUG
  // This is now incorrect: a root can very well point to a semispace
  // not belonging to the 0-th generation.
  /* if_unlikely (egc_semispace_of_untagged (untagged_pointer) != fromspace) */
  /*   { */
  /*     egc_dump_generation_contents (); */
  /*     egc_fatal ("%p (%s) is not in fromspace (%s)", untagged_pointer, */
  /*                     egc_heap_name_of (untagged_pointer), */
  /*                     fromspace->name); */
  /*   } */
#endif // #ifdef EGC_DEBUG

  /* Check whether the parameter refers a forwarding pointer: */
  const void *tagged_header = ((const void **) untagged_pointer)[-1];
  if_unlikely (EGC_IS_FORWARDING (tagged_header))
    {
      void **untagged_forwarding_pointer =
        EGC_FORWARDING_HEADER_TO_DESTINATION (tagged_header);
      egc_verbose_log ("%p (%s) forwards to %p (%s)\n",
                       untagged_pointer,
                       egc_heap_name_of (untagged_pointer),
                       untagged_forwarding_pointer,
                       egc_heap_name_of
                       (untagged_forwarding_pointer));
      return EGC_TAG_POINTER (untagged_forwarding_pointer);
    } // if

#ifdef EGC_DEBUG
  /* Check that the header has a valid tag: */
  if_unlikely (!EGC_IS_NONFORWARDING (tagged_header))
    {
      egc_verbose_log ("tagged_header is %p\n", tagged_header);
      egc_fatal ("tagged_header is both forwarding and non-forwarding");
    }
#endif // #ifdef EGC_DEBUG

  /* Do nothing if the pointer belongs to a different generation. */
  egc_generation_index_t pointer_generation_index =
    EGC_NONFORWARDING_HEADER_TO_GENERATION(tagged_header);
  if_unlikely (pointer_generation_index != fromg->generation_index)
    {
      egc_verbose_log
        ("* not scavenging pointer %p (generation %i, not %i)\n",
         untagged_pointer,
         (int)pointer_generation_index, (int)fromg->generation_index);
      return (void*)EGC_TAG_POINTER(untagged_pointer);
    }

  /* Ok, the parameter refers a fromspace object which is not a forwarding pointer;
     we have to copy it and install a forwarding pointer in the original pointer
     object: */
  const size_t size_in_chars =
    (egc_bitmask_t)
    EGC_NONFORWARDING_HEADER_TO_SIZE (tagged_header);
#ifdef EGC_DEBUG
  if_unlikely (size_in_chars <= 0)
    egc_fatal ("corrupted header: object size not positive");
  if_unlikely (size_in_chars % sizeof (void *) != 0)
    egc_fatal ("corrupted header: object size not a wordsize multiple");
#endif // #ifdef EGC_DEBUG

  const void **object_in_tospace =
    egc_allocate_from_semispace (tog->generation_index, tospace, size_in_chars);
  //fromg->allocate_chars_in_tospace (fromg, size_in_chars); // FIXME: why in the word is this faster?
  ((const void **) untagged_pointer)[-1] =
    EGC_FORWARDING_HEADER (object_in_tospace);
  egc_verbose_log ("* scavenging %p (%iB, %s) to %p (%s)\n",
                   untagged_pointer,
                   (int) size_in_chars,
                   egc_heap_name_of (untagged_pointer),
                   object_in_tospace,
                   egc_heap_name_of (object_in_tospace));

  /* Now we have to copy object fields into the new copy, and scavenge
     the new copy (or just push the pointers to the words to be changed
     onto the stack, to be scavenged later): */
#ifdef EGC_USE_MEMCPY
  memcpy (object_in_tospace, untagged_pointer, size_in_chars);
#else
  int i;
  const size_t size_in_words = size_in_chars / sizeof (void *);
  for (i = 0; i < size_in_words; i++)
    object_in_tospace[i] = ((const void **) untagged_pointer)[i];
#endif // #ifdef EGC_USE_MEMCPY

#ifdef EGC_DEBUG
  /* Clear the original object, so that we can't use it by mistake. */
  memset ((void *) untagged_pointer, -1, size_in_chars);
#endif // #ifdef EGC_DEBUG

  /* Return a tagged pointer to the new copy: */
  return EGC_TAG_POINTER (object_in_tospace);
}

static void
egc_scavenge_pointer_to_candidate_pointer (egc_generation_t fromg,
                                           egc_generation_t tog,
                                           egc_semispace_t tospace,
                                           void **pointer_to_candidate_pointer)
{
  /* Dereference the pointer to the candidate pointer; this is always
     safe if the parameter is, in fact, a pointer to something: */
  const void *tagged_candidate_pointer = *pointer_to_candidate_pointer;

  /* Is the candidate pointer really a pointer to an object of the
     generation we are interested in?  Scavenge it if it is, and update
     the pointer-to-pointer; otherwise we have nothing to do: */
  if (EGC_IS_POINTER (tagged_candidate_pointer))
    {
      const void *untagged_pointer =
        EGC_UNTAG_POINTER (tagged_candidate_pointer);
#ifdef EGC_DEBUG
      egc_semispace_t semispace =
        egc_semispace_of_untagged (untagged_pointer);
      if_unlikely (semispace == NULL)
        egc_fatal ("pointer %p (tagged %p) points out of the heap",
                   untagged_pointer, tagged_candidate_pointer);
#endif // #ifdef EGC_DEBUG

      *pointer_to_candidate_pointer =
        egc_scavenge_pointer (fromg, tog, tospace, untagged_pointer);
    }
  else
    egc_verbose_log
      ("* not scavenging non-pointer %li or %p (tagged %p)\n",
       (long int) (EGC_UNTAG_NONPOINTER (tagged_candidate_pointer)),
       EGC_UNTAG_NONPOINTER (tagged_candidate_pointer),
       tagged_candidate_pointer);
}

/* Cheney's two-finger algorithm.  Just to be clear about conventions,
   here the left finger moves from the beginning towards the end of
   tospace until it meets its allocation pointer, which is actually
   the right finger; the right fingers moves further right as new
   objects are scavenged from fromspace to tospace.  The left finger
   always points to tospace object headers, never to object fields. */
static void egc_two_fingers (egc_generation_t fromg,
                             egc_generation_t tog,
                             void **initial_left_finger,
                             egc_semispace_t fromspace,
                             egc_semispace_t tospace)
{
#ifdef EGC_VERBOSE
  long scavenged_pointer_no = 0;
#endif // #ifdef EGC_VERBOSE
  void **left_finger = initial_left_finger;
  while (left_finger < (void**)tospace->next_unallocated_word)
    {
      size_t object_size_in_bytes =
        EGC_NONFORWARDING_HEADER_TO_SIZE(*left_finger);
#ifdef EGC_DEBUG
      if_unlikely (! EGC_IS_NONFORWARDING(*left_finger))
        egc_fatal ("corrupted tospace scavenged header %p (forwarding)", *left_finger);
      if_unlikely (object_size_in_bytes <= 0
                   || object_size_in_bytes % sizeof (void*) != 0)
        egc_fatal ("corrupted tospace scavenged header %p (wrong size %i)",
                   *left_finger, (int)object_size_in_bytes);
#endif // #ifdef EGC_DEBUG
      size_t object_size_in_words = object_size_in_bytes / sizeof (void*);
      int i;
      for (i = 1; i <= object_size_in_words; i ++)
        {
          egc_scavenge_pointer_to_candidate_pointer (fromg, tog,
                                                     tospace, left_finger + i);
#ifdef EGC_VERBOSE
          scavenged_pointer_no ++;
#endif // #ifdef EGC_VERBOSE
        }
      left_finger += i;
#ifdef EGC_DEBUG
      if_unlikely (left_finger > (void**)tospace->next_unallocated_word)
        egc_fatal ("left finger crossed an object boundary");
#endif // #ifdef EGC_DEBUG
    } // while
#ifdef EGC_VERBOSE
  egc_log ("Scavenged %li candidate pointers\n", scavenged_pointer_no);
#endif // #ifdef EGC_VERBOSE
}

static void
egc_run_hook (egc_hook_t hook)
{
  if (hook == NULL)
    return;
  char *name __attribute__ ((unused))
    = (hook == egc_pre_hook) ? "pre" : "post";
  egc_verbose_log ("Entering %s hook  (roots are %i)...\n",
                   name, (int) egc_roots_no);
  hook (egc_hook_argument);
  egc_verbose_log ("...exited %s hook (roots are %i).\n",
                   name, (int) egc_roots_no);
}

static void
egc_scavenge_roots (egc_generation_t fromg,
                    egc_generation_t tog,
                    egc_semispace_t tospace)
{
#ifdef EGC_DEBUG
  size_t static_root_no = egc_roots_no;
#endif // #ifdef EGC_DEBUG

  egc_run_hook (egc_pre_hook);

#ifdef EGC_VERY_VERBOSE
  egc_dump_generations ();
#endif // #ifdef EGC_VERY_VERBOSE

#ifdef EGC_DEBUG
  // No longer correct with multiple generations.
  /* if_unlikely (tospace != NULL */
  /*              && egc_fill_ratio_of(tospace, 0) != 0.0) */
  /*   egc_fatal ("tospace (%s) is not empty", tospace->name); */
#endif // #ifdef EGC_DEBUG

  /* Scavenge static and dynamic roots. */
  int root_index;
#ifdef EGC_VERBOSE
  long scavenged_root_pointer_no = 0;
#endif // #ifdef EGC_VERBOSE
  for (root_index = 0; root_index < egc_roots_no; root_index++)
    {
      void **candidate_pointers = (void **)
        egc_roots[root_index].pointer_to_roots;
      const int word_no = egc_roots[root_index].size_in_words;
      int word_index;
      for (word_index = 0; word_index < word_no; word_index++)
        egc_scavenge_pointer_to_candidate_pointer (fromg, tog, tospace,
                                                   candidate_pointers + word_index);
#ifdef EGC_VERBOSE
      scavenged_root_pointer_no ++;
#endif // #ifdef EGC_VERBOSE
    } // for
#ifdef EGC_VERBOSE
  egc_log ("Scavenged %li candidate root pointers\n", scavenged_root_pointer_no);
#endif // #ifdef EGC_VERBOSE

  egc_run_hook (egc_post_hook);

#ifdef EGC_DEBUG
  if_unlikely (static_root_no != egc_roots_no)
    egc_fatal ("hooks disagree about dynamic root number");
#endif // #ifdef EGC_DEBUG

  /* Scavenge inter-generational pointers. */
  void ***roots = fromg->roots_from_older_generations.roots;
  const size_t root_no = fromg->roots_from_older_generations.root_no;
  int i;
  for (i = 0; i < root_no; i ++)
    egc_scavenge_pointer_to_candidate_pointer (fromg, tog, tospace, roots[i]);
}

size_t
egc_words_in_generation (egc_generation_t g)
{
  return g->words (g);
}

size_t
egc_free_words_in_generation (egc_generation_t g)
{
  return g->free_words (g);
}

size_t
egc_used_words_in_generation (egc_generation_t g)
{
  return egc_words_in_generation (g)
    - egc_free_words_in_generation (g);
}

void
egc_scan_previous_generations (egc_generation_t younger_than,
                               egc_generation_t tog,
                               egc_semispace_t tospace)
{
  //egc_generation_index_t index = younger_than->generation_index;
  egc_generation_t fromg;
  for (fromg = younger_than->next_younger;
       fromg != NULL;
       fromg = fromg->next_younger)
    {
      egc_log ("Scanning generation %i\n", (int)fromg->generation_index);
      egc_semispace_t fromspace = fromg->fromspace;
      void **p = fromspace->payload_beginning;
      while (p < fromspace->next_unallocated_word)
        {
          void *header = *p;
          size_t field_no =
            EGC_NONFORWARDING_HEADER_TO_SIZE(header) / sizeof (void*);
#ifdef EGC_DEBUG
          if_unlikely (EGC_IS_FORWARDING(header))
            egc_fatal ("forwarging header at %p scanning younger generations", p);
          if_unlikely (field_no <= 0)
            egc_fatal ("field_no is %i\n", (int)field_no);
#endif // #ifdef EGC_DEBUG
          int i;
          for (i = 1; i <= field_no; i ++)
            egc_scavenge_pointer_to_candidate_pointer (younger_than, tog, tospace, p + i);
          p += i;
        } // while
    } // for
}

void
egc_gc_generation (egc_generation_t g)
{
  g->gc_generation (g);
}

void
egc_gc_semispace_generation (egc_generation_t g)
{
#ifdef EGC_TIME
  double time_before = egc_get_current_time ();
#endif // #ifdef EGC_TIME

  /* Collecting this generation might promote objects into the next
     older one, so we first have to make sure the older generation has
     enough room. */
  egc_generation_t next_older = g->next_older;
  if (next_older != NULL
      && egc_free_words_in_generation (next_older)
      < egc_used_words_in_generation (g))
    {
      egc_log ("Not enough room in generation %i to collect generation %i\n",
               (int)next_older->generation_index, (int)g->generation_index);
      egc_gc_generation (next_older);
      if_unlikely (egc_free_words_in_generation (next_older)
                   < egc_used_words_in_generation (g))
        egc_fatal ("Not enough room in generation %i to collect generation %i, after GC\n",
                   (int)next_older->generation_index, (int)g->generation_index);
    }

  egc_semispace_t const fromspace = g->fromspace;
  egc_semispace_t const tospace
    = g->tospace ? g->tospace : g->next_older->fromspace;
#ifdef EGC_VERBOSE
  egc_log ("[%i-GC %s->%s: BEGIN...\n", (int)g->generation_index,
           fromspace->name, tospace->name);
  /* egc_dump_generations (); */
  /* egc_log ("]\n"); */
#endif // #ifdef EGC_VERBOSE

  egc_generation_t tog = g->tospace ? g : g->next_older;

  /* Remember where the part to scavenge in tospace begins, before
     touching roots. */
  void **initial_left_finger = tospace->next_unallocated_word;

  /* Any generation younger than the one we are collecting is
     effectively a set of roots.  Scan them.
     [FIXME: What shall I do about inter-generational pointers?
     I'm not sure the current solution is correct.] */
  egc_scan_previous_generations (g, tog, tospace);

  egc_scavenge_roots (g, tog, tospace);

  /* fprintf (stderr, "\n-----------------\nBEFORE TWO-FINGERS: BEGIN\n"); */
  /* egc_dump_generation_contents (); */
  /* fprintf (stderr, "BEFORE TWO-FINGERS: END\n-----------------\n\n"); */

  egc_two_fingers (g, tog, initial_left_finger, fromspace, tospace);

  if (g->tospace != NULL)
    egc_flip_spaces (g);
  else
    fromspace->next_unallocated_word = fromspace->payload_beginning;

  /* Clear inter-generational pointers for all generations younger
     than this one. */
  egc_generation_t younger_g;
  for (younger_g = g->next_younger;
       younger_g != NULL;
       younger_g = younger_g->next_younger)
    {
#ifdef EGC_DEBUG
      /* if_unlikely (egc_free_words_in_generation (younger_g) */
      /*              < egc_words_in_generation (younger_g)) */
      /*   egc_fatal ("generation %i is not empty after %i-GC: I fear it should be", */
      /*                   (int)younger_g->generation_index, (int)g->generation_index); */
#endif // #ifdef EGC_DEBUG
      egc_clear_roots (& younger_g->roots_from_older_generations);
    } // for

  long scavenged_words = tospace->next_unallocated_word - initial_left_finger;

  g->gc_no ++;
  g->scavenged_words += scavenged_words;

#ifdef EGC_TIME
  double elapsed_time = egc_get_current_time () - time_before;
  g->gc_time += elapsed_time;
#endif // #ifdef EGC_TIME

#ifdef EGC_VERBOSE
  /* egc_log ("[\n"); */
  egc_dump_generations ();
  egc_log ("...%i-GC %s->%s: END (scavenged %.02fKiB",
           (int)g->generation_index,
           fromspace->name, tospace->name,
           (float)scavenged_words * sizeof(void*) / 1024.0);
#ifdef EGC_TIME
  egc_log (" in %.9fs", elapsed_time);
#endif // #ifdef EGC_TIME
  egc_log (")]\n");
#endif // #ifdef EGC_VERBOSE
  /* egc_dump_generation_contents (); */
  /* fprintf (stderr, "-----------------\nAFTER GC]\n\n"); */
}

void
egc_full_gc (void)
{
  /* GC all generations, from the youngest to the oldest. */
  egc_generation_t g;
  for (g = egc_generations; g != NULL; g = g->next_older)
    g->gc_generation (g);
}

void *
egc_allocate_words (size_t size_in_words)
{
  return egc_allocate_chars (size_in_words * sizeof (void *));
}

void *
egc_allocate_cons (void)
{
  return egc_allocate_words (2);
}

void *
egc_allocate_chars (size_t size_in_chars)
{
  return egc_generations[0].allocate_chars(egc_generations + 0,
                                           size_in_chars);
}

void *
egc_allocate_chars_inizializing (size_t size_in_chars)
{
  void *res = egc_allocate_chars (size_in_chars);
  memset (res, 0, size_in_chars);
  return res;
}

void *
egc_allocate_words_inizializing (size_t size_in_words)
{
  return egc_allocate_chars_inizializing (size_in_words * sizeof (void*));
}

static size_t
egc_words_in_semispace_generation (egc_generation_t g)
{
  return g->fromspace->after_payload_end - g->fromspace->payload_beginning;
}

static size_t
egc_free_words_in_semispace_generation (egc_generation_t g)
{
  return g->fromspace->after_payload_end - g->fromspace->next_unallocated_word;
}

void egc_write_barrier (void **untagged_initial_pointer,
                        long offset_in_words)
{
  // FIXME: this is ridiculously inefficient.  Use my new strategy instead.
  egc_generation_index_t pointer_generation =
    EGC_NONFORWARDING_HEADER_TO_GENERATION(untagged_initial_pointer[-1]);
  if (pointer_generation != 0)
    {
      egc_generation_index_t i;
      for (i = pointer_generation - 1; i >= 0; i --)
        egc_push_root (& egc_generations[i].roots_from_older_generations,
                       untagged_initial_pointer + offset_in_words);
    } // if
}

static void *
egc_allocate_chars_in_tospace_from_single_semispace_generation (egc_generation_t g,
                                                                size_t size_in_chars)
{
#ifdef EGC_DEBUG
  if_unlikely (g->next_older == NULL)
    egc_fatal ("there's no older generation to scavenge into");
#endif // #ifdef EGC_DEBUG
  return g->next_older->allocate_chars(g->next_older, size_in_chars);
}

void *
egc_allocate_chars_in_tospace_from_double_semispace_generation (egc_generation_t g,
                                                                size_t size_in_chars)
{
  return egc_allocate_from_semispace (g->generation_index, g->tospace, size_in_chars);
}


static void *
egc_allocate_chars_from_semispace_generation_slow_path (egc_generation_t g,
                                                        size_t size_in_chars)
  __attribute__ ((cold, noinline));

static void *
egc_allocate_chars_from_semispace_generation_slow_path (egc_generation_t g,
                                                        size_t size_in_chars)
{
  /* There is not enough room in fromspace.  Notice that this collection might
     trigger collections in older generations as well. */
  egc_gc_semispace_generation (g);

  /* Do we have have enough room available in fromspace? */
  egc_semispace_t fromspace = g->fromspace;
  if_unlikely (((char *) fromspace->next_unallocated_word)
               + size_in_chars + sizeof (void *)
               > (char *) fromspace->after_payload_end)
    egc_fatal ("not enough room for allocating right after a collection");

  /* This time we are sure there is room. */
  return egc_allocate_from_semispace (g->generation_index, g->fromspace, size_in_chars);
}

/* Using char* instead of void** may save a few instructions (tested:
   one, on a better-written test, on both x86_64 and MIPS, gcc 4.9.2
   -Ofast, on moore).  Here it's important.  FIXME: rewrite looking at
   the generated assembly. */
void *
egc_allocate_chars_from_semispace_generation (egc_generation_t g,
                                              size_t size_in_chars)
{
#ifdef EGC_DEBUG
  if_unlikely (size_in_chars <= 0)
    egc_fatal ("egc_allocate_chars_from_semispace_generation: object size %li not positive",
               (long)size_in_chars);
  if_unlikely (size_in_chars % sizeof (void *) != 0)
    egc_fatal
    ("egc_allocate_chars_from_semispace_generation: object size %li not a wordsize multiple",
     (long)size_in_chars);
#endif // #ifdef EGC_DEBUG

  egc_semispace_t const fromspace = g->fromspace;

  egc_verbose_log ("Attempting an allocation from %s...\n",
                   fromspace->name);

  /* Do we have have enough room available in fromspace? */
  if_unlikely (((char *) fromspace->next_unallocated_word)
               + size_in_chars + sizeof (void *)
               > (char *) fromspace->after_payload_end)
    return egc_allocate_chars_from_semispace_generation_slow_path (g, size_in_chars);

  /* Ok, now we can allocate. */
  void *res = egc_allocate_from_semispace (g->generation_index, fromspace, size_in_chars);

  egc_verbose_log ("...Allocated %p(%li) (%liB, %s)\n",
                   res, (long) res, size_in_chars,
                   egc_heap_name_of (res));
#ifdef EGC_DEBUG
  if_unlikely (egc_semispace_of_untagged (res) != fromspace)
    egc_fatal ("%p allocated from %s instead of fromspace (%s)", res,
               egc_heap_name_of_untagged (res), fromspace->name);
#endif // #ifdef EGC_DEBUG
  return res;
}
