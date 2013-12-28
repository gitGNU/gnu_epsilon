/* This file is part of GNU epsilon.

   Copyright (C) 2012 Université Paris 13
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


/* The high-level GC interface for initialization, finalization and explicit
   collection. */

#ifndef EPSILONGC_GARBAGE_COLLECTOR_H_
#define EPSILONGC_GARBAGE_COLLECTOR_H_

/* epsilongc_types.h should be included first, as it #define's some
   features macros which have effects on standard headers: */
#include "epsilongc_types.h"
#include "epsilongc_debug.h"
#include "epsilongc_debug.h"
#include "doubly_linked_list_macros.h"
#include <stdio.h>
#include <stdbool.h>
#include "epsilongc_threads.h"
#include "kind.h"
#include "allocator.h"
#include "pool.h"
#include "page.h"
#include "set_of_pages.h"
#include "trace.h"
#include "roots.h"
#include "run_time_settings.h"
#include "heuristics.h"
#include "time.h"

/* Initialize garbage collection support. This should be called once at the
   beginning (absolutely *not* once per thread): */
void epsilongc_initialize_garbage_collection(void);

/* Finalize garbage collection support. This should be called once after having
   used the collector (absolutely *not* once per thread), or never. After
   calling this no garbage collection functionality can be used, except after
   anothe initialization: */
void epsilongc_finalize_garbage_collection(void);

/* Initialize thread-local garbage collection support. This *must* be called once
   per thread (for all threads allocating or using memory from the GC), before
   using any GC functionality: */
void epsilongc_initialize_thread_local_garbage_collection(void);

/* Finalize thread-local garbage collection support. This *must* be called
   once before thread finalization or after the last use of GC functionality
   in a thread. Every thread which has called
   epsilongc_initialize_thread_local_garbage_collection() must also call this,
   unless *the whole process* is going to die without ever using GC
   functionalities any longer. */
void epsilongc_finalize_thread_local_garbage_collection(void);

/* Alignment and metadata can be set for kindless objects, *before* initialization.
   When the function name includes neither "conservative" nor "list" the setting
   applies to both: */
void epsilongc_set_implicit_kind_alignment(const epsilongc_integer_t alignment_);
void epsilongc_set_implicit_conservative_kind_tag(const epsilongc_integer_t tag_);
void epsilongc_set_implicit_leaf_kind_tag(const epsilongc_integer_t tag_);
void epsilongc_set_implicit_kind_tag(const epsilongc_integer_t tag_);
void epsilongc_set_implicit_conservative_kind_datum(const epsilongc_word_t datum_);
void epsilongc_set_implicit_leaf_kind_datum(const epsilongc_word_t datum_);
void epsilongc_set_implicit_kind_datum(const epsilongc_word_t datum_);


/* Request a garbage collection: */
void epsilongc_request_garbage_collection(void);

/* Print timing statistics about garbage collection time vs.
   mutation time. This must be called after initializtion, but
   may also be called after finalization; the printed statistics
   only reflect the time after the last initialization. */
void epsilongc_print_garbage_collection_timing_statistics(void);

/* /\* Acquire the global mutex: *\/ */
/* void epsilongc_lock_garbage_collector(void); */

/* /\* Release the global mutex: *\/ */
/* void epsilongc_unlock_garbage_collector(void); */

/* This is useful for debugging (in very small cases, of course): show all pages
   held by all pools and allocators and in all global lists, showing to which
   "container" each of them belongs: */
void epsilongc_dump_all_pages(void);

void epsilongc_make_thread_local_implicit_allocators(void);
void epsilongc_destroy_thread_local_implicit_allocators(void);

epsilongc_word_t epsilongc_allocate_words_conservative(const epsilongc_integer_t size_in_words)
  __attribute__((malloc));
epsilongc_word_t epsilongc_allocate_words_leaf(const epsilongc_integer_t size_in_words)
  __attribute__((malloc));
epsilongc_word_t epsilongc_allocate_bytes_conservative(const epsilongc_integer_t size_in_bytes)
  __attribute__((malloc));
epsilongc_word_t epsilongc_allocate_bytes_leaf(const epsilongc_integer_t size_in_bytes)
  __attribute__((malloc));

epsilongc_word_t epsilongc_allocate_large_object(const epsilongc_integer_t payload_size_in_words,
                                                 const epsilongc_integer_t alignment_in_words,
                                                 const epsilongc_kind_tag_t tag,
                                                 const epsilongc_kind_datum_t datum,
                                                 const epsilongc_tracer_t tracer,
                                                 const epsilongc_integer_t pointers_no_in_the_worst_case)
  __attribute__((malloc));
epsilongc_word_t epsilongc_allocate_large_object_conservative(const epsilongc_integer_t payload_size_in_words,
                                                              const epsilongc_integer_t alignment_in_words,
                                                              const epsilongc_kind_tag_t tag,
                                                              const epsilongc_kind_datum_t datum)
  __attribute__((malloc));
epsilongc_word_t epsilongc_allocate_large_object_leaf(const epsilongc_integer_t payload_size_in_words,
                                                      const epsilongc_integer_t alignment_in_words,
                                                      const epsilongc_kind_tag_t tag,
                                                      const epsilongc_kind_datum_t datum)
  __attribute__((malloc));

#endif // #ifndef EPSILONGC_GARBAGE_COLLECTOR_H_
