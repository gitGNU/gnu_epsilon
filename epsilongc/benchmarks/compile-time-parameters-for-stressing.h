/* This file is part of GNU epsilon.

Copyright (C) 2006  Luca Saiu
Copyright (C) 2012  Université Paris 13
Written by Luca Saiu

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. */


#ifndef EPSILONGC_COMPILE_TIME_PARAMETERS_H_
#define EPSILONGC_COMPILE_TIME_PARAMETERS_H_

#include <stdbool.h>
#include "config.h"
#include "epsilongc_types.h"

/* The number of bits used for the offset within a homogeneous page in a
   pointer. This is also the binary logarithm of the page size in bytes: */
//#define EPSILONGC_PAGE_OFFSET_WIDTH 28 // 256Mb pages
//#define EPSILONGC_PAGE_OFFSET_WIDTH 26 // 64Mb pages
//#define EPSILONGC_PAGE_OFFSET_WIDTH 24 // 16Mb pages

//#define EPSILONGC_PAGE_OFFSET_WIDTH 22 // 4Mb pages

//#define EPSILONGC_PAGE_OFFSET_WIDTH 20 // 1Mb pages
//#define EPSILONGC_PAGE_OFFSET_WIDTH 18 // 256Kb pages
//#define EPSILONGC_PAGE_OFFSET_WIDTH 16 // 64Kb pages
//#define EPSILONGC_PAGE_OFFSET_WIDTH 15 // 32Kb pages
//#define EPSILONGC_PAGE_OFFSET_WIDTH 10 // 1Kb pages

//#define EPSILONGC_PAGE_OFFSET_WIDTH 9 // 512 bytes pages
#define EPSILONGC_PAGE_OFFSET_WIDTH 8 // 256 bytes pages
//#define EPSILONGC_PAGE_OFFSET_WIDTH 7 // 128 bytes pages

////#define EPSILONGC_PAGE_OFFSET_WIDTH 6 // 64 bytes pages

// 2008-04-25: 32Kb (15) looks like the optimum on mccarthy, allocating conses (8 bytes)
// 2008-04-25: 256Kb (18) looks like the optimum on factotum, allocating conses (16 bytes)

/* These are automatically computed from other constants: */
#define EPSILONGC_PAGE_SIZE_IN_BYTES \
  (1 << EPSILONGC_PAGE_OFFSET_WIDTH)
#define EPSILONGC_PAGE_SIZE_IN_WORDS \
  (EPSILONGC_PAGE_SIZE_IN_BYTES / sizeof(word_t))

/* The fill ratio beyond which a hash table size is doubled: */
#define EPSILONGC_MAXIMUM_HASH_FILL_RATIO 0.75

/* This is a hash table size; it's better if it's a prime numbers,
   since this statistically minimizes collisions: */
#define EPSILONGC_INITIAL_SIZE_OF_SET_OF_PAGES 19997

/* To do: this is only for the temporary sequential solution: */
#define EPSILONGC_INITIAL_MARK_STACK_ALLOCATED_SIZE (8L * 1024L * 1024L * 1024L)

/* The ideal (alive-bytes / total-bytes) ratio, which the collector tries
   to approach when computing the maximum heap size at the end of each
   collection: */
#define EPSILONGC_IDEAL_ALIVE_RATIO 0.3//0.001

/* The minimum heap size; the heap is never resized to be smaller
   than this. In bytes: */
#define EPSILONGC_MINIMUM_HEAP_SIZE \
  1500//40960//((epsilongc_unsigned_integer_t)(0.01 * 1024L * 1024L))
  //((epsilongc_unsigned_integer_t)(16 * 1024L * 1024L))

///* The maximum heap size; the heap is never resized to be larger
//   than this. In bytes: */
//#define EPSILONGC_MAXIMUM_HEAP_SIZE ((epsilongc_unsigned_integer_t)(320 * 1024L * 1024L))
  //EPSILONGC_MINIMUM_HEAP_SIZE
  //((epsilongc_unsigned_integer_t)(5000 * 1024L * 1024L))
  //  ((epsilongc_unsigned_integer_t)(2560 * 1024L * 1024L))

/* The initial heap size. This should in general be failrly small, as
   it's automatically tuned after each collection: */
#define EPSILONGC_INITIAL_HEAP_SIZE_IN_BYTES \
  EPSILONGC_MINIMUM_HEAP_SIZE
  //(1500 * 1024 * 1024)
   //  
//  (16 * 1024 * 1024)


/* Page destruction is lazy; this allows to choose how many empty pages
   we should destroy each time we reuse a possibly-nonfull page from a
   pool. This is only used when ENABLE_DESTROY_EMPTY_PAGES is #define'd.
   2 looks like a reasonable value in my benchmarks. */
#define EPSILONGC_NUMBER_OF_PAGES_TO_DESTROY_PER_REUSED_PAGE 2

/* How many collector threads to use. To do: this should be automatically found. */
#define EPSILONGC_COLLECTOR_THREADS_NO 2

#endif //#ifndef EPSILONGC_COMPILE_TIME_PARAMETERS_H_
