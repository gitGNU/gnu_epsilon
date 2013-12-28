/* This file is part of GNU epsilon.

   Copyright (C) 2012 Université Paris 13
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


#include "epsilongc_types.h"
#include "heuristics.h"
#include "compile_time_parameters.h"
#include "pool.h"

/* The current heap size; if the heap has at least this size and there are
   no free pages of a kind we currently need, a collection is triggered: */
static epsilongc_unsigned_integer_t epsilongc_heap_size_in_bytes;

/* Is GC currently enabled? */
static volatile bool epsilongc_is_garbage_collection_enabled_variable;

bool epsilongc_is_garbage_collection_enabled(void){
  return epsilongc_is_garbage_collection_enabled_variable;
}

void epsilongc_initialize_heuristics(void){
  /* If the user asked for a fixed heap size then use that, right from the
     start: */
  if(epsilongc_fixed_heap_size_in_bytes == 0)
    epsilongc_heap_size_in_bytes = EPSILONGC_INITIAL_HEAP_SIZE_IN_BYTES;
  else
    epsilongc_heap_size_in_bytes = epsilongc_fixed_heap_size_in_bytes;
  epsilongc_is_garbage_collection_enabled_variable = true;
}

void epsilongc_finalize_heuristics(void){
  // Nothing yet.
}

static void epsilongc_print_heap_size(void){
  printf("The heap size is now %.2fMb\n",
         epsilongc_heap_size_in_bytes / 1024.0 / 1024.0);
}

static void epsilongc_cut_heap_size(epsilongc_sweep_statistics_t statistics){
  /* Ask the system how much we can still allocate before
     trashing: */
  const epsilongc_unsigned_integer_t system_page_size =
    (epsilongc_unsigned_integer_t)getpagesize();
  const epsilongc_unsigned_integer_t available_system_pages_no =
    (epsilongc_unsigned_integer_t)sysconf(_SC_AVPHYS_PAGES);
  const epsilongc_unsigned_integer_t still_available_size_in_bytes =
    (epsilongc_unsigned_integer_t)
    (available_system_pages_no * system_page_size /
     sizeof(epsilongc_word_t));
  const epsilongc_unsigned_integer_t current_pages_no =
    epsilongc_pages_no();
  const epsilongc_unsigned_integer_t current_heap_size_in_bytes =
    current_pages_no * EPSILONGC_PAGE_SIZE_IN_BYTES;
  
  if(epsilongc_get_verbose_collection()){
    printf("We currently have %i pages (%.2fMb).\n",
           (int)current_pages_no,
           current_heap_size_in_bytes / 1024.0 / 1024.0);
    printf("We can still allocate %.2fMb without trashing.\n",
           still_available_size_in_bytes / 1024.0 / 1024.0);
    printf("The new tentative heap size is %.2fMb\n",
           epsilongc_heap_size_in_bytes / 1024.0 / 1024.0);
  } // if
  epsilongc_unsigned_integer_t minimum_limit =
    EPSILONGC_MINIMUM_HEAP_SIZE;
  epsilongc_unsigned_integer_t maximum_limit =
    current_heap_size_in_bytes + still_available_size_in_bytes;
  if(epsilongc_fixed_heap_size_in_bytes != 0){
    minimum_limit = maximum_limit = epsilongc_fixed_heap_size_in_bytes;
  }
  if(epsilongc_get_verbose_collection())
    printf("The maximum heap size is %.2fMb.\n",
           maximum_limit / 1024.0 / 1024.0);
  //epsilongc_assert_on_debug(maximum_limit >= minimum_limit);
  if(epsilongc_heap_size_in_bytes < minimum_limit){
    if(epsilongc_get_verbose_collection())
      printf("The tentative heap size was too small: cutting...\n");
    epsilongc_heap_size_in_bytes = minimum_limit;
  }
  else if(epsilongc_heap_size_in_bytes > maximum_limit){
    if(epsilongc_get_verbose_collection())
      printf("The tentative heap size was too large: cutting...\n");
    epsilongc_heap_size_in_bytes = maximum_limit;
  }
  if(epsilongc_get_verbose_collection())
    epsilongc_print_heap_size();
}

void epsilongc_update_heuristics(epsilongc_sweep_statistics_t statistics){
  /* How many alive and dead bytes are there: */
  const double alive_bytes = statistics->alive_bytes;
  const double dead_bytes = statistics->dead_bytes;
  const double total_bytes = alive_bytes + dead_bytes;
  
  /* Which proportion of bytes is alive? */
  const double alive_ratio =
    (total_bytes != 0) ? alive_bytes / total_bytes : 0.0;
    
  if(epsilongc_get_verbose_collection()){
    printf("=======================================\n");
    printf("Alive bytes were %lu (%.2fMb) [over %lu bytes (%.2fMb)]\n",
           (unsigned long)alive_bytes,
           alive_bytes / 1024. / 1024.,
           (unsigned long)total_bytes,
           total_bytes / 1024. / 1024.);
    printf("Alive ratio was %.2f%%\n", alive_ratio * 100.0);
  } // if
  
  epsilongc_heap_size_in_bytes =
    (epsilongc_unsigned_integer_t)((double)alive_bytes / EPSILONGC_IDEAL_ALIVE_RATIO);
  epsilongc_cut_heap_size(statistics);
  if(epsilongc_get_verbose_collection())
    printf("=======================================\n");
}

bool epsilongc_should_we_garbage_collect(void){
  /* If GC is currently disabled then we should *not* collect: */
  if(! epsilongc_is_garbage_collection_enabled_variable)
    return false;
  
  const epsilongc_unsigned_integer_t current_heap_size_in_bytes =
    epsilongc_current_heap_size_in_bytes();
  if(current_heap_size_in_bytes > epsilongc_heap_size_in_bytes)
    return true;
  else
    return false;
}

void epsilongc_initialize_sweep_statistics(epsilongc_sweep_statistics_t statistics){
  epsilongc_assert_on_debug(statistics != NULL);
  statistics->swept_pages_no = 0;
  statistics->alive_bytes = 0;
  statistics->dead_bytes = 0;
}

void epsilongc_merge_sweep_statistics(epsilongc_sweep_statistics_t result,
                                      epsilongc_sweep_statistics_t statistics1,
                                      epsilongc_sweep_statistics_t statistics2){
  epsilongc_assert_on_debug(result != NULL);
  epsilongc_assert_on_debug(statistics1 != NULL);
  epsilongc_assert_on_debug(statistics2 != NULL);
  result->swept_pages_no = statistics1->swept_pages_no + statistics2->swept_pages_no;
  result->alive_bytes = statistics1->alive_bytes + statistics2->alive_bytes;
  result->dead_bytes = statistics1->dead_bytes + statistics2->dead_bytes;
}

void epsilongc_disable_garbage_collection(void){
  epsilongc_is_garbage_collection_enabled_variable = false;
}

void epsilongc_enable_garbage_collection(void){
  epsilongc_is_garbage_collection_enabled_variable = true;
}
