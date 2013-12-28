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


#include "epsilongc.h"
#include "time.h"
#include "heuristics.h"
#include "run_time_settings.h"
#include "epsilongc_threads.h"
#include "roots.h"
#include "large.h"
#include <unistd.h>
#include <time.h>
#include <signal.h>
#include <pthread.h>
#include "doubly_linked_list_macros.h"
#include "global_structures.h"

// To do: we need a mutex to synchronize thread initialization and finalization
// with the rest of the system

/* The number of collections performed till now. This can safely wrap around,
   hence we use an unsigned type: */
volatile epsilongc_unsigned_integer_t epsilongc_last_collection_index = 0;

void epsilongc_increment_last_collection_index(void){
  epsilongc_last_collection_index ++;
}

/* Absolute time at initialization: */
static double epsilongc_initialization_time;

/* Time at the end of the last collection phase: */
static double epsilongc_last_collection_end_time;

/* Time at the end of the last mutation phase: */
static double epsilongc_last_mutation_end_time;

/* Total mutation time, in seconds: */
static double epsilongc_total_mutation_time;

/* Total collection time, in seconds: */
static double epsilongc_total_collection_time;

/* Total root-mark time, in seconds: */
static double epsilongc_total_root_mark_time;
/* Total mark time, in seconds: */
static double epsilongc_total_mark_time;
/* Total sweep time, in seconds: */
static double epsilongc_total_sweep_time;

/* The length of the longest GC pause, in seconds: */
static double epsilongc_longest_collection_pause;

/* The length of the longest mark and sweep pauses, in seconds: */
static double epsilongc_longest_root_mark_pause;
static double epsilongc_longest_mark_pause;
static double epsilongc_longest_sweep_pause;

/* Are we collecting in this moment? This is only used for measuring
   how much time is spent in collection with respect to mutation: */
static bool epsilongc_are_we_collecting;

/* This atexit() cleanup is only used to print timing statistics
   at exit time, if the GC is in verbose mode (at exit time): */
static bool did_we_install_the_atexit_cleanup_function = false;
static void epsilongc_atexit_cleanup_function(void){
  /* Print time statistics if we're in verbose mode: */ 
  //if(epsilongc_get_verbose_collection())
    epsilongc_print_garbage_collection_timing_statistics();
}

/* The global sweep statistics. It's particularly important that this is
   global (and opportunely synchronized) in the case of deferred sweeping,
   as pages are swept on-demand at different times, hence statistics are
   not computed for all pages in the same phase, like with non-deferred
   sweeping: */
struct epsilongc_sweep_statistics epsilongc_sweep_statistics;

/* We want to create collector threads exactly once: */
static bool were_collector_threads_created = false;

static epsilongc_semaphore_t epsilongc_sweepers_who_have_not_completed_yet = NULL;

void epsilongc_initialize_thread_local_garbage_collection(void){
  set_current_c_stack_top_as_c_stack_bottom(); 
  epsilongc_initialize_thread_local_allocator_support();
  //if(epsilongc_get_verbose_collection())
  //  printf("%s: Initialized thread-local garbage collection support.\n", epsilongc_calling_thread_name());
}

void epsilongc_finalize_thread_local_garbage_collection(void){
  epsilongc_unregister_user_defined_thread_local_roots();
  epsilongc_destroy_thread_local_implicit_allocators();
  epsilongc_finalize_thread_local_allocator_support();
  //if(epsilongc_get_verbose_collection())
  //  printf("%s: Finalized thread-local garbage collection support.\n", epsilongc_calling_thread_name());
}

void epsilongc_print_garbage_collection_timing_statistics(void){
  const double current_time =
    epsilongc_get_current_time();
  double total_collection_time_as_of_now =
    epsilongc_total_collection_time;
  double total_mutation_time_as_of_now =
    epsilongc_total_mutation_time;
  if(epsilongc_are_we_collecting){
    const double length_of_this_collection =
      current_time - epsilongc_last_mutation_end_time;
    total_collection_time_as_of_now += length_of_this_collection;
  }
  else{
    const double length_of_this_mutation =
      current_time - epsilongc_last_collection_end_time;
    total_mutation_time_as_of_now += length_of_this_mutation;
  } // else
  
  const epsilongc_unsigned_integer_t collections_no = epsilongc_last_collection_index;
  printf("---- epsilon garbage collection statistics (collected with %i processors) ----\n",
         (int)epsilongc_processors_no);
  const double total_run_time =
    total_collection_time_as_of_now + total_mutation_time_as_of_now;
  const double collection_time_ratio =
    total_collection_time_as_of_now / total_run_time;
  const double average_root_mark_time = epsilongc_total_root_mark_time / collections_no;
  const double average_mark_time = epsilongc_total_mark_time / collections_no;
  const double average_sweep_time = epsilongc_total_sweep_time / collections_no;
  const char *non_deferred_or_nothing =
#ifdef ENABLE_DEFERRED_SWEEP
    " (non-deferred only)";
#else
    "";
#endif // #ifdef ENABLE_DEFERRED_SWEEP
  printf("Collections:     %8li\n", (long)collections_no);
  printf("Collection time: %8.2f seconds (%6.2f%% )\n", total_collection_time_as_of_now, 100.0 * collection_time_ratio);
  if(collections_no > 0){
    printf("  * Root mark time:  %.2f seconds globally ( %.2f%% runtime )\n",
           average_root_mark_time * collections_no,
           collections_no * average_root_mark_time / total_run_time * 100.0);
    printf("  * Mark time:       %.2f seconds globally ( %.2f%% runtime )\n",
           average_mark_time * collections_no,
           collections_no * average_mark_time / total_run_time * 100.0);
    printf("  * Sweep time:      %.2f seconds globally ( %.2f%% runtime )%s\n",
           average_sweep_time * collections_no,
           collections_no * average_sweep_time / total_run_time * 100.0,
           non_deferred_or_nothing);
  } // if
  printf("Mutation time:   %8.2f seconds (%6.2f%% )\n", total_mutation_time_as_of_now, 100.0 * (1.0 - collection_time_ratio));
  printf("Total runtime:   %8.2f seconds\n", total_mutation_time_as_of_now + total_collection_time_as_of_now);
  
  const int pages_no = (int)epsilongc_pages_no();
  printf(" Pages are %i (%4.1fMb)\n", pages_no,
         pages_no / 1024. / 1024. * EPSILONGC_PAGE_SIZE_IN_BYTES);
  printf(" Stack blocks are %i (%4.1fMb)\n", (int)epsilongc_stack_blocks_no,
         epsilongc_stack_blocks_no / 1024.0 / 1024.0 * EPSILONGC_STACK_BLOCK_SIZE_IN_BYTES);
  //printf("-------------------------------------------------------------------------------------\n");
}

static void epsilongc_request_thread_local_roots(epsilongc_thread_t thread){
  pthread_kill(thread->thread, SIGUSR1);
}

static void epsilongc_say_to_go_on(epsilongc_thread_t thread){
  pthread_kill(thread->thread, SIGUSR2);
}

static void epsilongc_react_to_the_go_on_request(void){
  //printf("Thread %p (%s): reacting to the go-on request: setting can_i_continue to true\n", (void*)pthread_self(), epsilongc_calling_thread_name()); fflush(stdout);
  volatile epsilongc_thread_t calling_thread = epsilongc_calling_mutator_thread();
  calling_thread->collection_state = epsilongc_collection_ended_collection_state;
}

static void epsilongc_react_to_the_go_on_request_signal(
   int signal_number __attribute__((unused))){
  if(epsilongc_get_verbose_collection()){
    //printf("%s: Got a SIGUSR2: %s\n", epsilongc_calling_thread_name(), strsignal(signal_number)); fflush(stdout);
  }
  if(! epsilongc_is_the_caller_thread_a_mutator()){
    //printf("!!!!!!!! WARNING: the non-mutator thread %p received the second signal\n", (void*)pthread_self());
    fflush(stdout);
    return;
  } // if
  
  epsilongc_react_to_the_go_on_request();
    
  /* Now return to the handler for the *first* signal, which will test
     can_i_continue and finally return in its turn: */
  //printf("%s: Returning to the first handler\n", epsilongc_calling_thread_name()); fflush(stdout);
}

void epsilongc_actually_garbage_collect(void);

static void epsilongc_react_to_thread_local_root_information_request(void){
  //printf("%s: epsilongc_react_to_thread_local_root_information_request(): begin\n", epsilongc_calling_thread_name()); fflush(stdout);
  volatile epsilongc_thread_t volatile calling_thread = epsilongc_calling_mutator_thread();
  epsilongc_assert_on_debug(calling_thread != NULL);
  
#ifndef ENABLE_DEFERRED_SWEEP
  /* Add all pages held by my allocators to the list of pages to be swept: */
  epsilongc_allocator_t allocator;
  for(allocator = EPSILONGC_FIRST_ELEMENT_OF_LIST(allocators, &epsilongc_thread_local_allocators_with_a_page);
      allocator != NULL;
      allocator = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(thread_local_allocators_with_a_page, allocator)){
    epsilongc_page_t page = allocator->current_page;
    epsilongc_assert_on_debug(page != NULL);
    epsilongc_lock_mutex(epsilongc_within_collection_mutex);
    //EPSILONGC_INITIALIZE_LIST_ELEMENT(list, page);// To do: this should not be needed
    EPSILONGC_APPEND_OBJECT_TO_LIST(pages, list, epsilongc_page_t, page, &epsilongc_pages_to_be_swept);
    //printf("%s [MAYBE FROM THE SIGNAL HANDLER]: adding the page %p (from the allocator %p) to the list of pages to be swept\n", epsilongc_calling_thread_name(), page, allocator); fflush(stdout);
    epsilongc_unlock_mutex(epsilongc_within_collection_mutex);
  } // for
#endif // #ifndef ENABLE_DEFERRED_SWEEP
  
  /* Fill the root structure: SIGUSR2 will not arrive until this
     thread, and all other mutators, have done this: */
  epsilongc_fill_thread_roots((volatile epsilongc_thread_roots_t)&calling_thread->roots);
  calling_thread->last_collection_no_when_root_information_was_updated =
    epsilongc_last_collection_index;
  calling_thread->collection_state =
    epsilongc_root_information_is_up_to_date_collection_state;
  
  //printf("%s: epsilongc_react_to_thread_local_root_information_request(): end\n", epsilongc_calling_thread_name()); fflush(stdout);
}

/* The signal handler called by mutator threads when requested to
   update their thread-local root information: */
static void epsilongc_react_to_thread_local_root_information_request_signal(
   int signal_number __attribute__((unused))){
  //printf("%s: epsilongc_react_to_thread_local_root_information_request_signal(): begin\n", epsilongc_calling_thread_name()); fflush(stdout);
  if(epsilongc_get_verbose_collection()){
    //printf("%s: Got a SIGUSR1: %s\n", epsilongc_calling_thread_name(), strsignal(signal_number)); fflush(stdout);
  }
  //printf("Thread %s %p: got a SIGUSR1\n", epsilongc_calling_thread_name(), (void*)pthread_self()); fflush(stdout);
  if(! epsilongc_is_the_caller_thread_a_mutator()){
    //printf("!!!!!!!! WARNING: the non-mutator thread %p received the first signal\n", (void*)pthread_self());
    fflush(stdout);
    return;
  } // if
  
  //printf("%s: epsilongc_react_to_thread_local_root_information_request_signal(): Ok-1\n", epsilongc_calling_thread_name()); fflush(stdout);
  // (*)
  epsilongc_react_to_thread_local_root_information_request();
  
  //printf("%s: epsilongc_react_to_thread_local_root_information_request_signal(): Ok-2\n", epsilongc_calling_thread_name()); fflush(stdout);
  //printf("%s: Waiting for SIGUSR2...\n", epsilongc_calling_thread_name()); fflush(stdout);
  /* Passively wait for the second signal: */
  epsilongc_thread_t calling_thread = epsilongc_calling_mutator_thread();
  while(calling_thread->collection_state != epsilongc_collection_ended_collection_state){
    sleep(30); // this will be interrupted by the signal, of course
    //printf("Thread %s: %p: (waiting for SIGUSR2): can I continue? %s\n", epsilongc_calling_thread_name(), (void*)pthread_self(), (calling_thread->collection_state == epsilongc_collection_ended_collection_state)?"yes":"no"); fflush(stdout);
  }; // while
  
  //printf("%s: Returning from the first handler: the collection is over, I can go on.\n", epsilongc_calling_thread_name()); fflush(stdout);
  //printf("%s: epsilongc_react_to_thread_local_root_information_request_signal(): end\n", epsilongc_calling_thread_name()); fflush(stdout);
};

/* This also returns the current time: */
static double epsilongc_set_current_time_as_collection_begin_time(void){
  const double current_time = epsilongc_get_current_time();
  /* We're collecting now: */
  const double time_for_the_last_mutation =
    current_time - epsilongc_last_collection_end_time;
  epsilongc_last_mutation_end_time = current_time;
  epsilongc_total_mutation_time += time_for_the_last_mutation;
  epsilongc_are_we_collecting = true;
  return current_time;
}

/* This also returns the current time: */
static double epsilongc_set_current_time_as_collection_end_time(void){
  const double current_time = epsilongc_get_current_time();
  epsilongc_last_collection_end_time = current_time;
  epsilongc_are_we_collecting = false;
  const double time_for_this_collection = epsilongc_last_collection_end_time - epsilongc_last_mutation_end_time;
  if(time_for_this_collection > epsilongc_longest_collection_pause)
    epsilongc_longest_collection_pause = time_for_this_collection;
  epsilongc_total_collection_time += time_for_this_collection;
  return current_time;
}

static void epsilongc_set_pause_time_for_this_root_mark(const double time){
  epsilongc_total_root_mark_time += time;
  if(time > epsilongc_longest_root_mark_pause)
    epsilongc_longest_root_mark_pause = time;
}

static void epsilongc_set_pause_time_for_this_mark(const double time){
  epsilongc_total_mark_time += time;
  if(time > epsilongc_longest_mark_pause)
    epsilongc_longest_mark_pause = time;
}

static void epsilongc_set_pause_time_for_this_sweep(const double time){
  epsilongc_total_sweep_time += time;
  if(time > epsilongc_longest_sweep_pause)
    epsilongc_longest_sweep_pause = time;
}

static pthread_t *epsilongc_marker_threads = NULL;
static pthread_t *epsilongc_sweeper_threads = NULL;
static volatile bool epsilongc_was_a_collection_requested;
static epsilongc_mutex_t epsilongc_was_a_collection_requested_mutex = NULL;

static void epsilongc_garbage_collect(void){
  /* Lock the read/write lock for writing: a maximum of *one* collection can
     occur at any given time: */
  epsilongc_lock_read_write_lock_for_writing(epsilongc_global_read_write_lock);
  
  /* Ok, we really need to collect if we arrived here. This collection has just begun: */
  const double collection_begin_time =
    epsilongc_set_current_time_as_collection_begin_time();
  
  /* We're starting a new collection: increment the collection
     counter: */
  epsilongc_increment_last_collection_index();
  if(epsilongc_get_verbose_collection())
    printf("==== HERE BEGINS COLLECTION %lu ==== (triggered by %s; there are %i empty pages now)\n",
           (unsigned long)epsilongc_last_collection_index,
           epsilongc_calling_thread_name(),
           (int)EPSILONGC_LENGTH_OF_LIST(pages, &epsilongc_empty_pages)); fflush(stdout);
  
  //printf("%s: Ok-F 0: (I'm a mutator? %s)\n", epsilongc_calling_thread_name(), epsilongc_is_the_caller_thread_a_mutator() ? "yes" : "no"); fflush(stdout);
  /* Ask all mutators threads (except myself: I don't want to react to a signal
     now) to update their roots: */
  //printf("++++++++++ %s: signaling all threads except for myself\n", epsilongc_calling_thread_name()); fflush(stdout);
  epsilongc_call_on_all_mutator_threads_except_myself(epsilongc_request_thread_local_roots);
  //printf("++++++++++ %s: Still alive\n", epsilongc_calling_thread_name()); fflush(stdout);
  
  //printf("%s: Ok-F 1\n", epsilongc_calling_thread_name()); fflush(stdout);
  /* *This* thread should also update its roots, if it's a mutator: */
  if(epsilongc_is_the_caller_thread_a_mutator())
    epsilongc_react_to_thread_local_root_information_request();
  else{
    printf("%s: ***** WARNING ***** The thread which triggered the collection is not a mutator. Strange.\n", epsilongc_calling_thread_name()); fflush(stdout);
  }
  //printf("%s: Ok-F 2\n", epsilongc_calling_thread_name()); fflush(stdout);
  
  /* Actively wait till all mutators have updated their root information: */
  while(! epsilongc_are_all_mutator_roots_up_to_date()){
    //printf("%s: Still waiting for a thread to update its roots (collection %lu)...\n", epsilongc_calling_thread_name(), (unsigned long)epsilongc_last_collection_index); fflush(stdout);
    /* Do nothing */;
  } // while
  
  //printf("%s: Ok-F 3\n", epsilongc_calling_thread_name()); fflush(stdout);

  /* Great, now we're ready to collect: */
  epsilongc_actually_garbage_collect();
  //printf("@@ This COLLECTION WORK has ENDED.\n"); fflush(stdout);
  
  /* Clear the 'was-a-collection-requested' flag, which prevented another
     collection request to be satisfied right after the end of the collection
     which we have just performed: */
  epsilongc_was_a_collection_requested = false;
  
  /* Ok, we've collected: tell everybody (except myself) that they can
     go on. This makes them set their roots as not up-to-date; I also
     have to do it for myself if I'm a mutator:  */
  epsilongc_call_on_all_mutator_threads_except_myself(epsilongc_say_to_go_on);
  if(epsilongc_is_the_caller_thread_a_mutator())
    epsilongc_react_to_the_go_on_request();
  
  /* Ok, of course collection is ending also for this thread, if it's a mutator (this
     will be read at the *next* collection: */
  if(epsilongc_is_the_caller_thread_a_mutator())
    epsilongc_react_to_the_go_on_request();
  
  // To do: maybe this is needed for correctness...? I don't think so.
#ifdef ENABLE_ACTIVELY_WAIT_FOR_THE_SECOND_TIME
  /* Actively wait till all mutators have understood they can return: */
  while(! epsilongc_have_all_mutator_ended_collection()){
    //printf("%s: Still waiting for a thread to understand it can go on...\n", epsilongc_calling_thread_name()); fflush(stdout);
    /* Do nothing */;
  }  // while
#endif // #ifdef ENABLE_ACTIVELY_WAIT_FOR_THE_SECOND_TIME
  
  /* This collection has just ended: */
  const double collection_end_time =
    epsilongc_set_current_time_as_collection_end_time();
  //printf("%s: The collection is over. Returning\n", epsilongc_calling_thread_name()); fflush(stdout);
  
  //epsilongc_print_garbage_collection_timing_statistics();
  if(epsilongc_get_verbose_collection()){
    printf("Garbage collecting: end (%.3f seconds)\n\n",
           collection_end_time - collection_begin_time);
    fflush(stdout);
  } // if
  
  /* Unlock the read/write lock we acquired for writing, and we're done: */
  epsilongc_unlock_read_write_lock(epsilongc_global_read_write_lock);
}

void epsilongc_request_garbage_collection(void){
  //static epsilongc_unsigned_integer_t epsilongc_index_of_the_last_requested_collection;
  epsilongc_lock_mutex(epsilongc_was_a_collection_requested_mutex);
  if(epsilongc_was_a_collection_requested){
    epsilongc_unlock_mutex(epsilongc_was_a_collection_requested_mutex);
    //printf("%s: I don't want to make two collection in a row happen. Spinning...\n", epsilongc_calling_thread_name()); fflush(stdout);
    /* Actively wait to be signaled: */
    //const double time_before_spin = epsilongc_get_current_time();
    while(epsilongc_was_a_collection_requested)
      /* Do nothing */;
    //const double time_after_spin = epsilongc_get_current_time();
    //printf("%s: Finished spinning (spinned for %.5f seconds).\n", epsilongc_calling_thread_name(), time_after_spin - time_before_spin); fflush(stdout);
    return;
  }
  epsilongc_was_a_collection_requested = true;
  epsilongc_unlock_mutex(epsilongc_was_a_collection_requested_mutex);
  epsilongc_garbage_collect();
  /* After the collection proper is over, we can destroy all
     condemned large objects; ask the large-object-destroyer
     thread to do it, if we have condemned at least one large
     object: */
  if(EPSILONGC_FIRST_ELEMENT_OF_LIST(large_object_headers,
                                     &epsilongc_condemned_large_objects) != NULL)
    epsilongc_request_condemned_large_object_destruction();
}

/* Just for debugging: show GC statistics even when the program
   is killed with a signal: To do: change this for production */
static void epsilongc_show_timing_statistics_before_dying(int signal_number){
  printf("\n==============================================\n");
  printf("Got a signal: %s\n", strsignal(signal_number));
  printf("==============================================\n\n");
  fflush(stdout);
  /* Print timing statistics: */
  epsilongc_print_garbage_collection_timing_statistics();
  
  /* Die: */
  signal(signal_number, SIG_DFL);
  printf("Dying.\n");
  raise(signal_number);
}

/* Perform the work of a marker thread: this is the function running in
   the thread, as it's visible from its signature: */
static void* epsilongc_marker_thread_work(void *useless){
  /* This thread will need to trace, so we initialize the thread-local
     support for working with mark stack blocks: */
  epsilongc_initialize_thread_local_tracing_support();
  
  /* Trace from the available stack blocks, possibly releasing other full blocks
     in the process, for the other tracing threads: this contains an infinite
     loop: */
  epsilongc_trace();
  
  /* This should be unreachable: */
  epsilongc_finalize_thread_local_tracing_support();  
  return NULL;
}

/* Perform the work of a marker thread: this is the function running in
   the thread, as it's visible from its signature: */
static void* epsilongc_sweeper_thread_work(void *useless){
  /* Sweep statistics only used for pages swept by this thread in one cycle: */
  struct epsilongc_sweep_statistics thread_sweep_statistics;
  
  /* A local list we use to collect empty pages; this will be destructively
     appended to the global list at the end, to save synchronizations: */
  struct epsilongc_page_list temporary_empty_pages_list;
  EPSILONGC_INITIALIZE_LIST(pages, &temporary_empty_pages_list);
  
  /* Pointer to the page we're working on at the moment, and sweeping
     statistics for it: */
  epsilongc_page_t page;
  struct epsilongc_sweep_statistics page_sweep_statistics;
  
  /* It only makes sense to lock if we do parallel collection: */
#ifdef ENABLE_PARALLEL_COLLECTION
#define EPSILONGC_LOCK_ON_PARALLEL_COLLECTION() epsilongc_lock_mutex(epsilongc_within_collection_mutex)
#define EPSILONGC_UNLOCK_ON_PARALLEL_COLLECTION() epsilongc_unlock_mutex(epsilongc_within_collection_mutex)
#else
#define EPSILONGC_LOCK_ON_PARALLEL_COLLECTION()   /* do nothing */
#define EPSILONGC_UNLOCK_ON_PARALLEL_COLLECTION() /* do nothing */
#endif // #ifdef ENABLE_PARALLEL_COLLECTION
  
  /* Do this for ever: */
  while(true){
    /* Reset the sweep statistics for this thread: */
    epsilongc_initialize_sweep_statistics(&thread_sweep_statistics);
    
    /* Do our part in waking up the main collector thread, then wait for the next
       cycle to begin. It's important to do these two things here in the same
       critical section, so that when the main collector thread broadcasts the
       condition after having acquired the same mutex, we can be sure all sweeper
       threads are effectively waiting on the condition: */
    epsilongc_lock_mutex__(epsilongc_begin_to_sweep_mutex);
    epsilongc_v_semaphore(epsilongc_sweeping_phase_semaphore);
    epsilongc_wait_condition(epsilongc_begin_to_sweep_condition, epsilongc_begin_to_sweep_mutex);
    epsilongc_unlock_mutex__(epsilongc_begin_to_sweep_mutex);
    
    /* For each page in the list of the pages to be swept, sweep it and return
       it to the correct list: */
    while(true){
      EPSILONGC_LOCK_ON_PARALLEL_COLLECTION();
      page = EPSILONGC_FIRST_ELEMENT_OF_LIST(pages, &epsilongc_pages_to_be_swept);
      if(page == NULL){ // there are no more pages to sweep
        EPSILONGC_UNLOCK_ON_PARALLEL_COLLECTION();
        break;
      } // if
      EPSILONGC_DETACH_ELEMENT_FROM_LIST(pages, list, epsilongc_page_t, page,
                                         &epsilongc_pages_to_be_swept);
      EPSILONGC_UNLOCK_ON_PARALLEL_COLLECTION();
      
      /* Sweeping is performed here, out of critical sections. The opportunity for
         parallelism is here, and this should also be the slowest part by far: */
      epsilongc_initialize_sweep_statistics(&page_sweep_statistics);
      epsilongc_sweep_page(page, &page_sweep_statistics);
      epsilongc_merge_sweep_statistics(&thread_sweep_statistics,
                                       &thread_sweep_statistics, &page_sweep_statistics);
      
      /* Great, we've swept a page: where shall we put it? */
      if(page->current_allocator != NULL)
        page->current_allocator->current_page = page; // this is safe without synchronization
      else if(page_sweep_statistics.alive_bytes == 0){
        page->belongs_to_the_empty_pages_list = true;
        EPSILONGC_APPEND_OBJECT_TO_LIST(pages, list, epsilongc_page_t, page, &temporary_empty_pages_list);
      }
      else{
        epsilongc_pool_t pool = page->pool;
        epsilongc_assert_on_debug(pool != NULL);
        EPSILONGC_LOCK_ON_PARALLEL_COLLECTION();
        EPSILONGC_APPEND_OBJECT_TO_LIST(possibly_nonfull_pages, list, epsilongc_page_t, page, pool);
        EPSILONGC_UNLOCK_ON_PARALLEL_COLLECTION();
      } // else
    } // inner while
    
    /* We've finised sweeping. Add the empty pages we have put in our local list to the global
       list, and merge the statistics for this thread into global statistics. This only requires
       one synchronization: */
    EPSILONGC_LOCK_ON_PARALLEL_COLLECTION();
    EPSILONGC_APPEND_SECOND_LIST_TO_FIRST_LIST(pages, pages, list, epsilongc_page_t,
                                               &epsilongc_empty_pages, &temporary_empty_pages_list);
    epsilongc_merge_sweep_statistics(&epsilongc_sweep_statistics,
                                     &epsilongc_sweep_statistics,
                                     &thread_sweep_statistics);
    EPSILONGC_UNLOCK_ON_PARALLEL_COLLECTION();
  } // while
  
  /* This should be unreachable: */
  return NULL;
}

/* Alignment and metadata can be set for kindless objects, *before* initialization: */
static epsilongc_integer_t epsilongc_implicit_kind_alignment = 1;
static epsilongc_integer_t epsilongc_implicit_conservative_kind_tag = 42;
static epsilongc_integer_t epsilongc_implicit_leaf_kind_tag = 42;
static epsilongc_word_t epsilongc_implicit_conservative_kind_datum = NULL;
static epsilongc_word_t epsilongc_implicit_leaf_kind_datum = NULL;

static epsilongc_integer_t epsilongc_implicit_pools_no; // the size of *each* array, not the sum!
static epsilongc_pool_t *epsilongc_implicit_conservative_pools = NULL;
static epsilongc_pool_t *epsilongc_implicit_leaf_pools = NULL;
static __thread epsilongc_allocator_t *epsilongc_thread_local_implicit_conservative_allocators;
static __thread epsilongc_allocator_t *epsilongc_thread_local_implicit_leaf_allocators;
/* Looking up this array with a size in words as the index returns the index for
   the correct element to be looked up in pool or allocator arrays; of course the
   index for this array must be less than
   EPSILONGC_MAXIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS: */
static epsilongc_integer_t *epsilongc_size_in_words_to_array_index;
static __thread epsilongc_allocator_t *epsilongc_size_to_implicit_conservative_allocator_array;
static __thread epsilongc_allocator_t *epsilongc_size_to_implicit_leaf_allocator_array;

static bool is_the_collector_initialized = false;

static void epsilongc_fail_if_epsilongc_is_already_initialized(void){
  if(is_the_collector_initialized)
    epsilongc_fatal("FATAL ERROR: epsilongc: you cannot do this *after* initialization\n");
}

void epsilongc_set_implicit_kind_alignment(const epsilongc_integer_t epsilongc_implicit_kind_alignment_){
  epsilongc_fail_if_epsilongc_is_already_initialized();
  epsilongc_implicit_kind_alignment = epsilongc_implicit_kind_alignment_;
}

void epsilongc_set_implicit_conservative_kind_tag(const epsilongc_integer_t tag_){
  epsilongc_fail_if_epsilongc_is_already_initialized();
  epsilongc_implicit_conservative_kind_tag = tag_;
}

void epsilongc_set_implicit_leaf_kind_tag(const epsilongc_integer_t tag_){
  epsilongc_fail_if_epsilongc_is_already_initialized();
  epsilongc_implicit_leaf_kind_tag = tag_;
}

void epsilongc_set_implicit_kind_tag(const epsilongc_integer_t tag_){
  epsilongc_fail_if_epsilongc_is_already_initialized();
  epsilongc_set_implicit_conservative_kind_tag(tag_);
  epsilongc_set_implicit_leaf_kind_tag(tag_);
}

void epsilongc_set_implicit_conservative_kind_datum(const epsilongc_word_t datum_){
  epsilongc_fail_if_epsilongc_is_already_initialized();
  epsilongc_implicit_conservative_kind_datum = datum_;
}

void epsilongc_set_implicit_leaf_kind_datum(const epsilongc_word_t datum_){
  epsilongc_fail_if_epsilongc_is_already_initialized();
  epsilongc_implicit_leaf_kind_datum = datum_;
}

void epsilongc_set_implicit_kind_datum(const epsilongc_word_t datum_){
  epsilongc_fail_if_epsilongc_is_already_initialized();
  epsilongc_set_implicit_conservative_kind_datum(datum_);
  epsilongc_set_implicit_leaf_kind_datum(datum_);
}

/* Automatically create implicit kinds and pools of some reasonable sizes, both
   in fully-conservative-pointer-finding and in leaf version: */
static void epsilongc_make_implicit_kinds_and_pools(void){
#ifndef ENABLE_IMPLICIT_KINDS
  /* Do nothing in this function if we don't want implicit kinds: */
  return;
#endif // #ifndef ENABLE_IMPLICIT_KINDS
  
  /* Do nothing if we already have arrays: */
  if(epsilongc_implicit_conservative_pools != NULL)
    return;
  
  /* Perform some cheap sanity checks: */
  if(EPSILONGC_IMPLICIT_KIND_FACTOR < 2)
    epsilongc_fatal("EPSILONGC_IMPLICIT_KIND_FACTOR must be at least 2");
  if(EPSILONGC_MINIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS >
     EPSILONGC_MAXIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS)
    epsilongc_fatal("EPSILONGC_MINIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS is greater than the maximum");
  
  /* Make arrays: */
  epsilongc_integer_t size_in_words;
  epsilongc_implicit_pools_no = 0;
  for(size_in_words = EPSILONGC_MINIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS;
      size_in_words <= EPSILONGC_MAXIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS;
      size_in_words *= EPSILONGC_IMPLICIT_KIND_FACTOR)
    epsilongc_implicit_pools_no ++;
  if(size_in_words != (EPSILONGC_MAXIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS * EPSILONGC_IMPLICIT_KIND_FACTOR))
    epsilongc_fatal("EPSILONGC_MAXIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS can not be exactly reached");
  epsilongc_implicit_conservative_pools =
    epsilongc_xmalloc(sizeof(epsilongc_pool_t) * epsilongc_implicit_pools_no);
  epsilongc_implicit_leaf_pools =
    epsilongc_xmalloc(sizeof(epsilongc_pool_t) * epsilongc_implicit_pools_no);
  
  /* Make kinds and pools: */
  int i;
  for(size_in_words = EPSILONGC_MINIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS, i = 0;
      size_in_words <= EPSILONGC_MAXIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS;
      size_in_words *= EPSILONGC_IMPLICIT_KIND_FACTOR, i ++){
    //printf("Making kinds of size %i words\n", (int)size_in_words);
    epsilongc_kind_t implicit_conservative_kind =
      epsilongc_make_kind(size_in_words,
                          size_in_words, // all words can be heap pointers
                          epsilongc_implicit_kind_alignment,
                          epsilongc_implicit_conservative_kind_tag,
                          epsilongc_implicit_conservative_kind_datum,
                          epsilongc_conservative_tracer,
                          NULL); // no finalizer
    epsilongc_kind_t implicit_leaf_kind =
      epsilongc_make_kind(size_in_words,
                          0, // these objects can't contain any heap pointer
                          epsilongc_implicit_kind_alignment,
                          epsilongc_implicit_leaf_kind_tag,
                          epsilongc_implicit_leaf_kind_datum,
                          epsilongc_leaf_tracer,
                          NULL); // no finalizer
    epsilongc_pool_t implicit_conservative_pool =
      epsilongc_make_pool("implicit-conservative", implicit_conservative_kind);
    epsilongc_pool_t implicit_leaf_pool =
      epsilongc_make_pool("implicit-leaf", implicit_leaf_kind);
    /* Add our just-created pools to global arrays: */
    epsilongc_implicit_conservative_pools[i] = implicit_conservative_pool;
    epsilongc_implicit_leaf_pools[i] = implicit_leaf_pool;
  } // for
  
  /* Allocate epsilongc_size_in_words_to_array_index and fill it with reasonable
     values: */
  epsilongc_size_in_words_to_array_index =
    epsilongc_xmalloc(sizeof(epsilongc_integer_t) * (EPSILONGC_MAXIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS + 1));
  //printf("epsilongc_size_in_words_to_array_index has %i elements\n", (int)(EPSILONGC_MAXIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS + 1));
  epsilongc_integer_t correct_referred_array_index = 0;
  for(i = 0; i <= EPSILONGC_MAXIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS; i ++){
    if(i > epsilongc_implicit_conservative_pools[correct_referred_array_index]->kind->object_size_in_words)
      correct_referred_array_index ++;
    if(correct_referred_array_index >= epsilongc_implicit_pools_no)
      epsilongc_fatal("EPSILONGC_MAXIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS can not be exactly reached");
    epsilongc_size_in_words_to_array_index[i] = correct_referred_array_index;
    //printf("I've set the array for i = %i as %i\n",(int)i, (int)epsilongc_size_in_words_to_array_index[i]);
  } // for
  /* for(i = 0; i <= EPSILONGC_MAXIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS; i ++) */
  /*   printf("%i -> %i (size %i)\n", (int)i, (int)epsilongc_size_in_words_to_array_index[i], */
  /*          (int)(epsilongc_implicit_conservative_pools[epsilongc_size_in_words_to_array_index[i]]->kind->object_size_in_words)); */
  /* printf("\n"); */
}

/* Make thread-local allocators for all implicit pools, and set the appropriate
   thread-local variables: */
void epsilongc_make_thread_local_implicit_allocators(void){
  /* Make allocator arrays: */
  epsilongc_thread_local_implicit_conservative_allocators =
    epsilongc_xmalloc(sizeof(epsilongc_allocator_t) * epsilongc_implicit_pools_no);
  epsilongc_thread_local_implicit_leaf_allocators =
    epsilongc_xmalloc(sizeof(epsilongc_allocator_t) * epsilongc_implicit_pools_no);
  
  /* Fill them by making and initializing allocators: */
  epsilongc_integer_t i;
  for(i = 0; i < epsilongc_implicit_pools_no; i++){
    epsilongc_thread_local_implicit_conservative_allocators[i] =
      epsilongc_xmalloc(sizeof(struct epsilongc_allocator));
    epsilongc_initialize_allocator(epsilongc_thread_local_implicit_conservative_allocators[i],
                                   epsilongc_implicit_conservative_pools[i]);
    epsilongc_thread_local_implicit_leaf_allocators[i] =
      epsilongc_xmalloc(sizeof(struct epsilongc_allocator));
    epsilongc_initialize_allocator(epsilongc_thread_local_implicit_leaf_allocators[i],
                                   epsilongc_implicit_leaf_pools[i]);
  } // for
  
  /* Make and fill "size to allocator" arrays: */
  epsilongc_size_to_implicit_conservative_allocator_array =
    epsilongc_xmalloc(sizeof(epsilongc_allocator_t) * (EPSILONGC_MAXIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS + 1));
  epsilongc_size_to_implicit_leaf_allocator_array =
    epsilongc_xmalloc(sizeof(epsilongc_allocator_t) * (EPSILONGC_MAXIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS + 1));
  for(i = 0; i <= EPSILONGC_MAXIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS; i ++){
    epsilongc_size_to_implicit_conservative_allocator_array[i] =
      epsilongc_thread_local_implicit_conservative_allocators[epsilongc_size_in_words_to_array_index[i]];
    epsilongc_size_to_implicit_leaf_allocator_array[i] =
      epsilongc_thread_local_implicit_leaf_allocators[epsilongc_size_in_words_to_array_index[i]];
    //printf("i is %i (%p)\n", (int)i, epsilongc_size_to_implicit_leaf_allocator_array[i]);
  } // for
}

/* Destroy all thread-local implicit allocators. This is automatically called
   at worker thread exit time: */
void epsilongc_destroy_thread_local_implicit_allocators(void){
  /* Finalize and destroy all thread-local allocators: */
  epsilongc_integer_t i;
  for(i = 0; i < epsilongc_implicit_pools_no; i++){
    epsilongc_finalize_allocator(epsilongc_thread_local_implicit_conservative_allocators[i]);
    epsilongc_finalize_allocator(epsilongc_thread_local_implicit_leaf_allocators[i]);
    free(epsilongc_thread_local_implicit_conservative_allocators[i]);
    free(epsilongc_thread_local_implicit_leaf_allocators[i]);
  } // for
  
  /* Destroy arrays: */
  free(epsilongc_thread_local_implicit_conservative_allocators);
  free(epsilongc_thread_local_implicit_leaf_allocators);
  free(epsilongc_size_to_implicit_conservative_allocator_array);
  free(epsilongc_size_to_implicit_leaf_allocator_array);
}

void epsilongc_print_garbage_collection_timing_statistics_on_signal(int unused_signal){
  epsilongc_print_garbage_collection_timing_statistics();
}

void epsilongc_initialize_garbage_collection(void){
  epsilongc_fail_if_epsilongc_is_already_initialized();
  //printf("The page header size is %li bytes\n",
  //       (long)sizeof(struct epsilongc_page_header));
  epsilongc_initialize_debugging();
  epsilongc_initialize_global_structures();
  epsilongc_initialize_threads();
  epsilongc_initialize_tracing_support();
  epsilongc_initialize_run_time_settings();
  epsilongc_initialize_set_of_pages();
  epsilongc_initialize_page_support();
  epsilongc_initialize_large_object_support();
  epsilongc_initialize_allocator_support();
  epsilongc_initialize_pool_support();
  epsilongc_initialize_heuristics();

  assert(epsilongc_sweepers_who_have_not_completed_yet == NULL);
  epsilongc_sweepers_who_have_not_completed_yet =
    epsilongc_make_semaphore("sweeper-who-have-not-completed-yet", 0, 0);
  
  epsilongc_was_a_collection_requested_mutex = epsilongc_make_mutex("was-a-collection-requested", 0);
  epsilongc_was_a_collection_requested = false;

  /* Make collector threads: */
  if(! were_collector_threads_created){
    /* epsilongc_integer_t i; */
    /* for(i = 0; i < EPSILONGC_MARKER_THREADS_NO; i++) */
    /*   epsilongc_make_thread(); */
    were_collector_threads_created = true;
  } // if

  /* Establish a signal handler to handle requests for
     thread-local root information. It must be performed here,
     as signal handler are shared by all threads: */
  signal(SIGUSR1,
         epsilongc_react_to_thread_local_root_information_request_signal);
  signal(SIGUSR2,
         epsilongc_react_to_the_go_on_request_signal);

  /* Print debugging statistics on SIGCONT: */
#ifdef ENABLE_ASSERTIONS
  signal(SIGCONT,
         epsilongc_print_garbage_collection_timing_statistics_on_signal);
#endif // #ifdef ENABLE_ASSERTIONS
  
  /* Just for debugging: show GC statistics even when the program
     is killed with SIGINT: To do: change this for production */
  signal(SIGINT, epsilongc_show_timing_statistics_before_dying);
  signal(SIGTERM, epsilongc_show_timing_statistics_before_dying);

  /* We've just started, and we' re not collecting: */
  const double current_time = epsilongc_get_current_time();
  epsilongc_are_we_collecting = false;
  epsilongc_initialization_time = current_time;
  
  /* We've just started mutating: */
  epsilongc_last_collection_end_time = current_time;
  epsilongc_total_mutation_time = 0;
  epsilongc_total_collection_time = 0;
  epsilongc_total_root_mark_time = 0;
  epsilongc_total_mark_time = 0;
  epsilongc_total_sweep_time = 0;
  epsilongc_longest_collection_pause = 0;
  epsilongc_longest_root_mark_pause = 0;
  epsilongc_longest_mark_pause = 0;
  epsilongc_longest_sweep_pause = 0;

  /* Install the atexit() handler if we haven't already
     done it: */
  if(! did_we_install_the_atexit_cleanup_function){
    atexit(epsilongc_atexit_cleanup_function); // ignore failure
    did_we_install_the_atexit_cleanup_function = true;
  } // if
  
  /* Make marker threads and sweeper threads; they are created
     once and never joined or cancelled: */
  if(epsilongc_marker_threads == NULL){
    epsilongc_marker_threads =
      epsilongc_xmalloc(sizeof(pthread_t) * epsilongc_processors_no);
    epsilongc_sweeper_threads =
      epsilongc_xmalloc(sizeof(pthread_t) * epsilongc_processors_no);
    epsilongc_integer_t i;
    for(i = 0; i < epsilongc_processors_no; i++)
      if(pthread_create(&epsilongc_marker_threads[i],
                        NULL,
                        epsilongc_marker_thread_work,
                        NULL) != 0)
        epsilongc_fatal("could not pthread_create() a marker thread");
    for(i = 0; i < epsilongc_processors_no; i++)
      if(pthread_create(&epsilongc_sweeper_threads[i],
                        NULL,
                        epsilongc_sweeper_thread_work,
                        NULL) != 0)
        epsilongc_fatal("could not pthread_create() a sweeper thread");
    /* We want epsilongc_sweeping_phase_semaphore to have a zero value
       at initialization time: each sweeper thread performs a V on it: */
    for(i = 0; i < epsilongc_processors_no; i++)
      epsilongc_p_semaphore(epsilongc_sweeping_phase_semaphore);
  } // if
  
  /* Make the large-object destroyer thread: */
  pthread_t large_object_destroyer_thread;
  if(pthread_create(&large_object_destroyer_thread,
                    NULL,
                    epsilongc_destroy_large_objects_thread,
                    NULL) != 0)
    epsilongc_fatal("could not pthread_create() the large object destroyer thread");

  /* Make implicit kinds and pools: */  
  epsilongc_make_implicit_kinds_and_pools();

  /* Remember that the collector is now initialized: */
  is_the_collector_initialized = true;

  if(epsilongc_get_verbose_collection())
    printf("Initialized global garbage collection support.\n");
}

void epsilongc_finalize_garbage_collection(void){
  /* We *don't* destroy implicit kinds and pools: they stay alive for ever. */
  
  ///* Print time statistics if we're in verbose mode: */ 
  //if(epsilongc_get_verbose_collection())
  //  epsilongc_print_garbage_collection_timing_statistics();
  
  epsilongc_destroy_mutex(epsilongc_was_a_collection_requested_mutex);
  epsilongc_was_a_collection_requested_mutex = NULL;
  assert(epsilongc_sweepers_who_have_not_completed_yet != NULL);
  epsilongc_destroy_semaphore(epsilongc_sweepers_who_have_not_completed_yet);
  epsilongc_sweepers_who_have_not_completed_yet = NULL;
  
  epsilongc_finalize_run_time_settings();
  epsilongc_finalize_tracing_support();
  epsilongc_finalize_heuristics();
  epsilongc_finalize_pool_support();
  epsilongc_finalize_set_of_pages();
  epsilongc_finalize_allocator_support();
  epsilongc_finalize_page_support();
  epsilongc_finalize_large_object_support();
  epsilongc_finalize_threads();
  
  epsilongc_finalize_global_structures();
  epsilongc_finalize_debugging();
  
  if(epsilongc_get_verbose_collection())
    printf("Finalized global garbage collection support.\n");
}

static void epsilongc_sweep(epsilongc_sweep_statistics_t sweep_statistics){  
  /* Tell sweeper threads to go: */
  epsilongc_lock_mutex__(epsilongc_begin_to_sweep_mutex);
  epsilongc_broadcast_condition(epsilongc_begin_to_sweep_condition);
  epsilongc_unlock_mutex__(epsilongc_begin_to_sweep_mutex);

  /* Sleep until all sweeper threads are done: */
  epsilongc_integer_t i;
  for(i = 0; i < epsilongc_processors_no; i++)
    epsilongc_p_semaphore(epsilongc_sweeping_phase_semaphore);
}

/* Dump a page pointer, for debugging, prefixed by an asterisk if some objects in
   the page are marked: */
static void epsilongc_dump_page(epsilongc_page_t page)
  __attribute__((unused));
static void epsilongc_dump_page(epsilongc_page_t page){
  printf(" %s%p", epsilongc_is_page_completely_unmarked(page) ? "" : "*", page);
  fflush(stdout);
}

void epsilongc_dump_all_pages(void){
#ifdef ENABLE_DUMP_PAGE_INFORMATION
  epsilongc_pool_t pool;
  epsilongc_allocator_t allocator;
  epsilongc_page_t page;
  printf("  ======================================================================\n");
  printf("  Possibly non-full: ");
  for(pool = EPSILONGC_FIRST_ELEMENT_OF_LIST(pools, &epsilongc_pools);
      pool != NULL;
      pool = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(pools, pool)){
    epsilongc_page_t page;
    for(page = EPSILONGC_FIRST_ELEMENT_OF_LIST(possibly_nonfull_pages, pool);
        page != NULL;
        page = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(list, page))
      epsilongc_dump_page(page);
  } // for
  printf("\n");
#ifdef ENABLE_DEFERRED_SWEEP
  printf("  Full list: ");
  for(pool = EPSILONGC_FIRST_ELEMENT_OF_LIST(pools, &epsilongc_pools);
      pool != NULL;
      pool = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(pools, pool)){
    for(page = EPSILONGC_FIRST_ELEMENT_OF_LIST(full_pages, pool);
        page != NULL;
        page = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(list, page))
      epsilongc_dump_page(page);
  } // for
  printf("\n");
  printf("  Non-full not-to-be-swept list: ");
  for(pool = EPSILONGC_FIRST_ELEMENT_OF_LIST(pools, &epsilongc_pools);
      pool != NULL;
      pool = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(pools, pool)){
    for(page = EPSILONGC_FIRST_ELEMENT_OF_LIST(nonfull_not_to_be_swept_pages, pool);
        page != NULL;
        page = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(list, page))
      epsilongc_dump_page(page);
  } // for
  printf("\n");
#endif // #ifdef ENABLE_DEFERRED_SWEEP
  printf("  Allocators (1):");
  for(allocator = EPSILONGC_FIRST_ELEMENT_OF_LIST(allocators, &epsilongc_all_allocators);
      allocator != NULL;
      allocator = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(all_allocators, allocator)){
    if(allocator->current_page != NULL){
      epsilongc_dump_page(allocator->current_page);
      printf("(allocator %p)", allocator);
    }
  } // for
  printf("\n");
  printf("  Allocators (2):");
  for(allocator = EPSILONGC_FIRST_ELEMENT_OF_LIST(allocators, &epsilongc_all_allocators_with_a_page);
      allocator != NULL;
      allocator = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(all_allocators_with_a_page, allocator)){
    if(allocator->current_page != NULL)
      epsilongc_dump_page(allocator->current_page);
    else
      printf(" NO-PAGE(allocator %p)!!!", allocator);
  } // for
  printf("\n");
  printf("  Empty:");
  for(page = EPSILONGC_FIRST_ELEMENT_OF_LIST(pages, &epsilongc_empty_pages);
      page != NULL;
      page = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(list, page))
    epsilongc_dump_page(page);
  printf("\n");
  printf("  To be swept:");
  for(page = EPSILONGC_FIRST_ELEMENT_OF_LIST(pages, &epsilongc_pages_to_be_swept);
      page != NULL;
      page = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(list, page))
    epsilongc_dump_page(page);
  printf("\n");
#ifdef ENABLE_ASSERTIONS
  printf("  All pages:");
  for(page = EPSILONGC_FIRST_ELEMENT_OF_LIST(pages, &epsilongc_all_pages);
      page != NULL;
      page = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(all_pages, page))
    epsilongc_dump_page(page);
  printf("\n");
#endif // #ifdef ENABLE_ASSERTIONS
  printf("  ======================================================================\n");
#endif // #ifdef ENABLE_DUMP_PAGE_INFORMATION
}

/* Actually perform collection. This is called with the global collector mutex
   locked, when all mutator threads have already been stopped with the first
   signal: */
void epsilongc_actually_garbage_collect(void){
  epsilongc_dump_all_pages();
  if(epsilongc_get_verbose_collection()){
    const epsilongc_unsigned_integer_t current_pages_no =
      epsilongc_pages_no();
    const epsilongc_unsigned_integer_t current_heap_size_in_bytes =
      current_pages_no * EPSILONGC_PAGE_SIZE_IN_BYTES;
    if(epsilongc_get_verbose_collection()){
      printf("We have %i pages (%.2fMb) at the beginning of this collection.\n",
             (int)current_pages_no,
             current_heap_size_in_bytes / 1024.0 / 1024.0); fflush(stdout);
    } // if
  } // if
  
  /* The total time for the sweep phase of this collection, (deferred or
     non-deferred) + large objects: */
  double pause_time_for_this_sweep = 0.0;
  
#ifdef ENABLE_DEFERRED_SWEEP
  if(epsilongc_get_verbose_collection()){
    printf("* Sweeping (non-deferred part): begin...\n"); fflush(stdout);
  }
  
  /* There should be no pages to be swept left in the list at the beginning of a
     collection when we adopt deferred sweeping: */
  epsilongc_assert_on_debug(EPSILONGC_LENGTH_OF_LIST(pages, &epsilongc_pages_to_be_swept) == 0);
  
  /* Sweep the remaining possibly-nonfull pages which haven't been used: */
  const double time_before_sweeping = epsilongc_get_current_time();
  epsilongc_pool_t pool;
  for(pool = EPSILONGC_FIRST_ELEMENT_OF_LIST(pools, &epsilongc_pools);
      pool != NULL;
      pool = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(pools, pool)){
    EPSILONGC_APPEND_SECOND_LIST_TO_FIRST_LIST(pages, possibly_nonfull_pages, list,
                                               epsilongc_page_t,
                                               &epsilongc_pages_to_be_swept, pool);
  } // for
  epsilongc_sweep(&epsilongc_sweep_statistics);
  const double time_after_sweeping = epsilongc_get_current_time();
  const double time_for_this_nondeferred_sweeping =
    time_after_sweeping - time_before_sweeping;
  if(epsilongc_get_verbose_collection()){
    printf("  Sweeping (non-deferred part): end (%.3f seconds).\n",
           time_for_this_nondeferred_sweeping); fflush(stdout);
  }
  pause_time_for_this_sweep += time_for_this_nondeferred_sweeping;
  epsilongc_dump_all_pages();
  /* If we do deferred sweeping then we can be sure all pages are unmarked now. Right? */ // To do: is this true? I hadn't written this the first time...
  epsilongc_assert_on_debug(epsilongc_are_all_pages_completely_unmarked());
#else
  epsilongc_dump_all_pages();
  /* If we *don't* do deferred sweeping then we can be sure all pages are
     unmarked now. Right? */
  epsilongc_assert_on_debug(epsilongc_are_all_pages_completely_unmarked());
  
  /* Possibly-non-full pages will have to be swept later: */
  epsilongc_pool_t pool;
  for(pool = EPSILONGC_FIRST_ELEMENT_OF_LIST(pools, &epsilongc_pools);
      pool != NULL;
      pool = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(pools, pool)){
    EPSILONGC_APPEND_SECOND_LIST_TO_FIRST_LIST(pages, possibly_nonfull_pages, list,
                                               epsilongc_page_t,
                                               &epsilongc_pages_to_be_swept, pool);
  } // for
#endif // #ifdef ENABLE_DEFERRED_SWEEP
  
  /* Trace thread-local roots for all threads: */
  if(epsilongc_get_verbose_collection()){
    printf("* Marking roots: begin...\n"); fflush(stdout);
  }
  const double time_before_root_marking = epsilongc_get_current_time();

  // To do: this is temporary! // To do: [early 2009] Or not? I think it makes sense now
  epsilongc_acquire_a_nonfull_block_if_needed();
  
  //const double time0 = epsilongc_get_current_time();
  //printf("  !0: %.3f seconds\n", time0 - time_before_root_marking); fflush(stdout);

  /* Actually trace roots: */
  epsilongc_call_on_all_mutator_threads(epsilongc_trace_thread_roots);
  
  //const double time2 = epsilongc_get_current_time();
  //printf("  !2: %.3f seconds\n", time2 - time_before_root_marking); fflush(stdout);

  /* Mark allocator edge pointers, like additional roots: */
  //printf("Marking allocator edge objects: begin...\n"); fflush(stdout);
  epsilongc_allocator_t allocator;
  for(allocator = EPSILONGC_FIRST_ELEMENT_OF_LIST(allocators, &epsilongc_all_allocators_with_a_page);
      allocator != NULL;
      allocator = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(all_allocators_with_a_page, allocator)){
    //printf("About to mark the edge objects of the allocator %p\n", allocator); fflush(stdout);
    epsilongc_page_t page = allocator->current_page;
    epsilongc_assert_on_debug(page != NULL);
    epsilongc_word_t next_object_to_allocate =
      EPSILONGC_UNCONCEAL_POINTER(page->next_free_object);
    epsilongc_trace_pointer_if_valid(next_object_to_allocate);
    if(epsilongc_get_verbose_collection()){
      //printf("The edge pointer is %p\n", next_object_to_allocate); fflush(stdout);
    }
  } // for

  //printf("Marking allocator edge objects: end.\n"); fflush(stdout);
  const double time_after_root_marking = epsilongc_get_current_time();
  if(epsilongc_get_verbose_collection()){
    printf("  Marking roots: end (%.3f seconds)\n", time_after_root_marking - time_before_root_marking); fflush(stdout);
  } // if
  epsilongc_set_pause_time_for_this_root_mark(time_after_root_marking - time_before_root_marking);
  
  /* Trace reachable objects: */
  if(epsilongc_get_verbose_collection()){
    printf("* Marking: begin...\n"); fflush(stdout);
  } // if
  const double time_before_marking = epsilongc_get_current_time();
  /* Release the stack block we currently own (although we may have already
     released some blocks, if roots didn't fit in a single block): */
  epsilongc_release_stack_block();
  
  /* Sleep until the marking phase is over: */
  epsilongc_p_semaphore(epsilongc_marking_phase_semaphore);
  const double time_after_marking = epsilongc_get_current_time();
  if(epsilongc_get_verbose_collection()){
    printf("  Marking: end (%.3f seconds).\n", time_after_marking - time_before_marking); fflush(stdout);
  } // if
  epsilongc_set_pause_time_for_this_mark(time_after_marking - time_before_marking);
  
  epsilongc_dump_all_pages();
  
#ifdef ENABLE_DEFERRED_SWEEP
  // To do: this should be done lazily by each pool:
  /* For each pool... */
  for(pool = EPSILONGC_FIRST_ELEMENT_OF_LIST(pools, &epsilongc_pools);
      pool != NULL;
      pool = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(pools, pool)){
    /* ...move all the pages in the full list into the possibly-nonfull list: */
    // To do: maybe PREpending would be better for cache locality?
    EPSILONGC_APPEND_SECOND_LIST_TO_FIRST_LIST(possibly_nonfull_pages, full_pages, list,
                                               epsilongc_page_t,
                                               pool, pool);
    
    /* The pages which were kept as non-full and not-to-be-swept have now been marked;
       they can be reused but they will need to be swept: */
    EPSILONGC_APPEND_SECOND_LIST_TO_FIRST_LIST(possibly_nonfull_pages, nonfull_not_to_be_swept_pages, list,
                                               epsilongc_page_t,
                                               pool, pool);
  } // for each pool
  /* For each allocator with a page: */
  for(allocator = EPSILONGC_FIRST_ELEMENT_OF_LIST(allocators, &epsilongc_all_allocators_with_a_page);
      allocator != NULL;
      allocator = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(all_allocators_with_a_page, allocator)){
    epsilongc_assert_on_debug(allocator->current_page != NULL);
    epsilongc_completely_unmark_page(allocator->current_page);
  } // for each allocator with a page
#else
  /* Sweep: */
  const double time_before_sweeping = epsilongc_get_current_time();
  epsilongc_initialize_sweep_statistics(&epsilongc_sweep_statistics);
  if(epsilongc_get_verbose_collection()){
    printf("* Sweeping: begin...\n"); fflush(stdout);
  }
  epsilongc_sweep(&epsilongc_sweep_statistics);
  const double time_after_sweeping = epsilongc_get_current_time();
  const double time_for_this_complete_sweeping =
    (time_after_sweeping - time_before_sweeping);
  if(epsilongc_get_verbose_collection()){
    printf("  Sweeping: end (%.3f seconds).\n", time_for_this_complete_sweeping); fflush(stdout);
  }
  pause_time_for_this_sweep += time_for_this_complete_sweeping;
  
  /* We *don't* do deferred sweeping if we're here, so we can be sure that
     all pages are unmarked now. Right? */
  epsilongc_assert_on_debug(epsilongc_are_all_pages_completely_unmarked());
#endif // #ifdef ENABLE_DEFERRED_SWEEP
  
  /* Sweep large objects; notice that this condemns but doesn't
     destroy dead large objects, as we can't destroy them yet (we
     can't use free() here). Large object sweeping is currently
     always sequential and non-deferred: */
  const double time_before_large_object_sweeping = epsilongc_get_current_time();
  if(epsilongc_get_verbose_collection()){
    printf("* Sweeping large objects: begin...\n"); fflush(stdout);
  }
  epsilongc_sweep_large_objects_unlocked(&epsilongc_sweep_statistics);
  const double time_after_large_object_sweeping = epsilongc_get_current_time();
  const double time_for_this_large_sweeping =
    time_after_large_object_sweeping - time_before_large_object_sweeping;
  if(epsilongc_get_verbose_collection()){
    printf("  Sweeping large objects: end (%.3f seconds).\n",
           time_for_this_large_sweeping); fflush(stdout);
  }
  pause_time_for_this_sweep += time_for_this_large_sweeping;
  epsilongc_set_pause_time_for_this_sweep(pause_time_for_this_sweep);

  /* We've swept all pages, so we have collected statistics to resize the heap
     for the next collection: */
  epsilongc_update_heuristics(&epsilongc_sweep_statistics);
  
#ifdef ENABLE_DEFERRED_SWEEP
  /* Reset statistics for the next time. With deferred sweeping we need to
     update statistics at each page sweeping, so the variable must be zeroed
     now, before we start the new cycle: */
  epsilongc_initialize_sweep_statistics(&epsilongc_sweep_statistics);
#endif // #ifdef ENABLE_DEFERRED_SWEEP
  
  if(epsilongc_get_verbose_collection()){
    const epsilongc_unsigned_integer_t current_pages_no =
      epsilongc_pages_no();
    const epsilongc_unsigned_integer_t current_heap_size_in_bytes =
      current_pages_no * EPSILONGC_PAGE_SIZE_IN_BYTES;
    if(epsilongc_get_verbose_collection()){
      printf("We have %i pages (%.2fMb) at the end of this collection.\n",
             (int)current_pages_no,
             current_heap_size_in_bytes / 1024.0 / 1024.0); fflush(stdout);
    } // if
  } // if
  epsilongc_dump_all_pages();
}

epsilongc_word_t epsilongc_allocate_words_conservative(const epsilongc_integer_t size_in_words){
  /* If the required object is larger than allowed by our implicit kinds make a large
     object; otherwise use an implicit allocator: */
  epsilongc_assert_on_debug(size_in_words >= 0);
  if(size_in_words > EPSILONGC_MAXIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS)
    return epsilongc_allocate_large_object_conservative(size_in_words, 1, 0, NULL);
  else
    return epsilongc_allocate_from(epsilongc_size_to_implicit_conservative_allocator_array[size_in_words]);
}

epsilongc_word_t epsilongc_allocate_words_leaf(const epsilongc_integer_t size_in_words){
  /* If the required object is larger than allowed by our implicit kinds make a large
     object; otherwise use an implicit allocator: */
  epsilongc_assert_on_debug(size_in_words >= 0);
  if(size_in_words > EPSILONGC_MAXIMUM_IMPLICIT_KIND_OBJECT_SIZE_IN_WORDS)
    return epsilongc_allocate_large_object_leaf(size_in_words, 1, 0, NULL);
  else
    return epsilongc_allocate_from(epsilongc_size_to_implicit_leaf_allocator_array[size_in_words]);
}

inline epsilongc_word_t epsilongc_allocate_bytes_conservative(const epsilongc_integer_t size_in_bytes){
  const epsilongc_integer_t size_in_words =
    ((size_in_bytes % sizeof(epsilongc_word_t)) == 0) ?
    size_in_bytes / sizeof(epsilongc_word_t) :
    size_in_bytes / sizeof(epsilongc_word_t) + 1;
  return epsilongc_allocate_words_conservative(size_in_words);
}

inline epsilongc_word_t epsilongc_allocate_bytes_leaf(const epsilongc_integer_t size_in_bytes){
  const epsilongc_integer_t size_in_words =
    ((size_in_bytes % sizeof(epsilongc_word_t)) == 0) ?
    size_in_bytes / sizeof(epsilongc_word_t) :
    size_in_bytes / sizeof(epsilongc_word_t) + 1;
  return epsilongc_allocate_words_leaf(size_in_words);
}

epsilongc_word_t epsilongc_allocate_large_object(const epsilongc_integer_t payload_size_in_words,
                                                 const epsilongc_integer_t alignment_in_words,
                                                 const epsilongc_kind_tag_t tag,
                                                 const epsilongc_kind_datum_t datum,
                                                 const epsilongc_tracer_t tracer,
                                                 const epsilongc_integer_t pointers_no_in_the_worst_case){
  const epsilongc_large_object_header_t header =
    epsilongc_make_large_object_and_return_its_header(payload_size_in_words, alignment_in_words,
                                                      tag, datum, tracer,
                                                      pointers_no_in_the_worst_case);
  return header->payload_beginning;
}

epsilongc_word_t epsilongc_allocate_large_object_conservative(const epsilongc_integer_t payload_size_in_words,
                                                              const epsilongc_integer_t alignment_in_words,
                                                              const epsilongc_kind_tag_t tag,
                                                              const epsilongc_kind_datum_t datum){
  return epsilongc_allocate_large_object(payload_size_in_words, alignment_in_words, tag, datum,
                                         epsilongc_conservative_tracer,
                                         payload_size_in_words);

}

epsilongc_word_t epsilongc_allocate_large_object_leaf(const epsilongc_integer_t payload_size_in_words,
                                                      const epsilongc_integer_t alignment_in_words,
                                                      const epsilongc_kind_tag_t tag,
                                                      const epsilongc_kind_datum_t datum){
  return epsilongc_allocate_large_object(payload_size_in_words, alignment_in_words, tag, datum,
                                         epsilongc_leaf_tracer,
                                         0);
}
