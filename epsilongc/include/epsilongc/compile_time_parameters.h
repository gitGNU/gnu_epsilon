/* This file is part of GNU epsilon.

   Copyright (C) 2006 Luca Saiu
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

#define EPSILONGC_PAGE_OFFSET_WIDTH 20 // 1Mb pages

//#define EPSILONGC_PAGE_OFFSET_WIDTH 18 // 256Kb pages
//#define EPSILONGC_PAGE_OFFSET_WIDTH 16 // 64Kb pages
//#define EPSILONGC_PAGE_OFFSET_WIDTH 15 // 32Kb pages
//#define EPSILONGC_PAGE_OFFSET_WIDTH 14 // 16Kb pages
//#define EPSILONGC_PAGE_OFFSET_WIDTH 13 // 8Kb pages
//#define EPSILONGC_PAGE_OFFSET_WIDTH 12 // 4Kb pages
//#define EPSILONGC_PAGE_OFFSET_WIDTH 11 // 2Kb pages
//#define EPSILONGC_PAGE_OFFSET_WIDTH 10 // 1Kb pages

//#define EPSILONGC_PAGE_OFFSET_WIDTH 9 // 512 bytes pages
//#define EPSILONGC_PAGE_OFFSET_WIDTH 8 // 256 bytes pages
//#define EPSILONGC_PAGE_OFFSET_WIDTH 7 // 128 bytes pages

////#define EPSILONGC_PAGE_OFFSET_WIDTH 6 // 64 bytes pages

// 2008-04-25: 32Kb (15) looks like the optimum on mccarthy, allocating conses (8 bytes)
// 2008-04-25: 256Kb (18) looks like the optimum on factotum, allocating conses (16 bytes)

/* This is a hash table size; it's better if it's a prime numbers,
   since this statistically minimizes collisions: */
//#define EPSILONGC_INITIAL_SIZE_OF_SET_OF_PAGES 200087 //19997
#define EPSILONGC_INITIAL_SIZE_OF_SET_OF_PAGES 39997 // To do: find a reasonable constant
/* To do: this is only for the temporary sequential solution: */
//#define EPSILONGC_INITIAL_MARK_STACK_ALLOCATED_SIZE (8L * 1024L * 1024L * 1024L)

/* The ideal (alive-bytes / total-bytes) ratio, which the collector tries
   to approach when computing the maximum heap size at the end of each
   collection: */
#define EPSILONGC_IDEAL_ALIVE_RATIO 0.2//0.75

/* The minimum heap size; the heap is never resized to be smaller
   than this. In bytes: */
#define EPSILONGC_MINIMUM_HEAP_SIZE \
  ((epsilongc_unsigned_integer_t)(512 * 1024L * 1024L))//((epsilongc_unsigned_integer_t)(16 * 1024L * 1024L))
//  ((epsilongc_unsigned_integer_t)(2048 * 1024L * 1024L))//((epsilongc_unsigned_integer_t)(16 * 1024L * 1024L))
//((epsilongc_unsigned_integer_t)(2512 * 1024L * 1024L))//((epsilongc_unsigned_integer_t)(16 * 1024L * 1024L))
  //18784256
  //4096
  //((epsilongc_unsigned_integer_t)(2560 * 1024L * 1024L))
  //1//((epsilongc_unsigned_integer_t)(16 * 1024L * 1024L))
  //((epsilongc_unsigned_integer_t)(640 * 1024L * 1024L))
//((epsilongc_unsigned_integer_t)(1280 * 1024L * 1024L))
   //1500//40960//((epsilongc_unsigned_integer_t)(0.01 * 1024L * 1024L))
  //((epsilongc_unsigned_integer_t)(16 * 1024L * 1024L))

///* The maximum heap size; the heap is never resized to be larger
//   than this. In bytes: */
//#define EPSILONGC_MAXIMUM_HEAP_SIZE ((epsilongc_unsigned_integer_t)(320 * 1024L * 1024L))
  //EPSILONGC_MINIMUM_HEAP_SIZE
  //((epsilongc_unsigned_integer_t)(5000 * 1024L * 1024L))
  //  ((epsilongc_unsigned_integer_t)(2560 * 1024L * 1024L))

/* The initial heap size. This should in general be failrly small, as the
   heap size is automatically tuned anyway, at the end of each collection: */
#define EPSILONGC_INITIAL_HEAP_SIZE_IN_BYTES \
  EPSILONGC_MINIMUM_HEAP_SIZE
  //(1500 * 1024 * 1024)
   //  
//  (16 * 1024 * 1024)


/* We don't want to destroy unused empty pages too often, as we
   wish to avoid deallocaing memory and then re-allocating again,
   which is expensive and is hard to parallelize becasue it requires
   kernel support. We prefer the allocated memory to decrease *slowly*
   when the working set shrinks. When considering whether to destroy
   an empty page, we do it only once every
   EPSILONGC_EMPTY_PAGE_DESTRUCTION_INVERSE_FREQUENCY times. 
   Setting this to 1 means to always destroy when it's possible. */
#define EPSILONGC_EMPTY_PAGE_DESTRUCTION_INVERSE_FREQUENCY 25//10

/* When marking in parallel rebalancing is considered once every
   EPSILONGC_POLLING_INTERVAL_CYCLE_LENGTH stack pops: */
#define EPSILONGC_POLLING_INTERVAL_CYCLE_LENGTH 1000

/* Kind, pools and allocators are automatically created for objects
   of size:
   EPSILONGC_MINIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS,
   EPSILONGC_MINIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS * EPSILONGC_IMPLICIT_KIND_FACTOR,
   EPSILONGC_MINIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS * 2 * EPSILONGC_IMPLICIT_KIND_FACTOR,
   EPSILONGC_MINIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS * 3 * EPSILONGC_IMPLICIT_KIND_FACTOR,
   ...,
   for sizes up to EPSILONGC_MAXIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS, *included*.
   Each kind exists in a fully-conservative-pointer-finding version and in a leaf version. */
#define EPSILONGC_MINIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS 4//1// 4
#define EPSILONGC_MAXIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS 256//(8192 * 32)//512
#define EPSILONGC_IMPLICIT_KIND_FACTOR                       4//2//4

#ifdef ENABLE_STRESS_TEST
#undef EPSILONGC_PAGE_OFFSET_WIDTH 
#define EPSILONGC_PAGE_OFFSET_WIDTH 10 // 1Kb pages
#undef EPSILONGC_MINIMUM_HEAP_SIZE
#define EPSILONGC_MINIMUM_HEAP_SIZE 1
#undef EPSILONGC_IDEAL_ALIVE_RATIO
#define EPSILONGC_IDEAL_ALIVE_RATIO 1.0
#endif // #ifdef ENABLE_STRESS_TEST

/* These are automatically computed from other constants: */
#define EPSILONGC_PAGE_SIZE_IN_BYTES \
  (1 << EPSILONGC_PAGE_OFFSET_WIDTH)
#define EPSILONGC_PAGE_SIZE_IN_WORDS \
  (EPSILONGC_PAGE_SIZE_IN_BYTES / sizeof(word_t))

/* The fill ratio beyond which a hash table size is doubled: */
#define EPSILONGC_MAXIMUM_HASH_FILL_RATIO 0.75

/* Size of a stack block, in bytes, including the header: */
#define EPSILONGC_STACK_BLOCK_SIZE_IN_BYTES \
  (epsilongc_unsigned_integer_t)(1 * 1024 * 1024) // 8Mb looked like the best value on my big machine, in a very old test I should not trust
  //(sizeof(struct epsilongc_stack_block_header) + sizeof(epsilongc_word_t) * 1024 * 1024)
  //64//(epsilongc_unsigned_integer_t)(40 * 1024 * 1024) // 8Mb looked like the best value on my big machine, in a very old test I should not trust
//8192

#endif //#ifndef EPSILONGC_COMPILE_TIME_PARAMETERS_H_
