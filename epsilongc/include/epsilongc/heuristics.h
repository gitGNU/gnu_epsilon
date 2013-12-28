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


#ifndef EPSILONGC_HEURISTICS_H_
#define EPSILONGC_HEURISTICS_H_

/* epsilongc_types.h should be included first, as it #define's some features macros which
   have effects on standard headers: */
#include "epsilongc_types.h"
#include "config.h"
#include <stdbool.h>

/* The statistics computed by sweeping pages: */
struct epsilongc_sweep_statistics{
  /* How many pages were swept: */
  epsilongc_unsigned_integer_t swept_pages_no;
  
  /* How much payload memory was found alive, in bytes: */
  epsilongc_unsigned_integer_t alive_bytes;

  /* How much payload memory was found dead, in bytes: */
  epsilongc_unsigned_integer_t dead_bytes;
};
typedef struct epsilongc_sweep_statistics* epsilongc_sweep_statistics_t;

/* Initialize the given sweep statistics to zero: */
void epsilongc_initialize_sweep_statistics(epsilongc_sweep_statistics_t statistics);

/* Merge the two given statistics, storing the result into *result: */
void epsilongc_merge_sweep_statistics(epsilongc_sweep_statistics_t result,
                                      epsilongc_sweep_statistics_t statistics1,
                                      epsilongc_sweep_statistics_t statistics2);

/* Update heuristics for the next collection, based on the given global
   statistics: */
void epsilongc_update_heuristics(epsilongc_sweep_statistics_t statistics);

/* Return true iff we should collect now, according to some heuristic: */
bool epsilongc_should_we_garbage_collect(void);

/* This should be called once (not once per thread) at initialization time: */
void epsilongc_initialize_heuristics(void);

/* This should be called once (not once per thread) at finalization time: */
void epsilongc_finalize_heuristics(void);

/* Return true iff GC is currently enabled. It's enabled by default: */
bool epsilongc_is_garbage_collection_enabled(void)
  __attribute__((pure));

/* Disable garbage collection until the next call to the procedure
   epsilongc_enable_garbage_collection(): */
void epsilongc_disable_garbage_collection(void);

/* Re-enable garbage collection after it was disabled: */
void epsilongc_enable_garbage_collection(void);

#endif // #ifndef EPSILONGC_HEURISTICS_H_
