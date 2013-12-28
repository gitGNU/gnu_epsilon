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


#ifndef EPSILONGC_THREADS_H_
#define EPSILONGC_THREADS_H_

#include "epsilongc_types.h"
//#include "roots.h"
//#include "doubly_linked_list.h"
#include "doubly_linked_list_macros.h"
#include <stdbool.h>
#include <pthread.h>
#include <setjmp.h>

/* All data structures defined in this module are "named", which means
   that, *only when debugging is enabled*, they contain a human-readable
   name which is shown in debug prints. The name (also including an index
   is not stored at all when not debugging.
   
   Althought it would be reasonable to turn this into a portable wrapper
   for several thread libraries, semantics are closely modeled on POSIX
   threads as of now.
   I've not investigated how easy to generalize this is. */

/* Initialize thread support. This should be called at initialization
   (*not* once per thread): */
void epsilongc_initialize_threads(void);

/* Finalize thread support: */
void epsilongc_finalize_threads(void);

/* ***** Mutexes ******** */

/* A mutex is a pointer to an opaque data structure: */
typedef struct epsilongc_mutex_* epsilongc_mutex_t_;

/* Make a new mutex: */
epsilongc_mutex_t_ epsilongc_make_mutex_(const char *name,
                                         const epsilongc_integer_t index);

/* Destroy a mutex: */
void epsilongc_destroy_mutex_(epsilongc_mutex_t_ mutex);

/* Lock a mutex: */
void epsilongc_lock_mutex__(epsilongc_mutex_t_ mutex);

/* Unlock a mutex: */
void epsilongc_unlock_mutex__(epsilongc_mutex_t_ mutex);

/* Return true iff the given mutex is currently locked: */
bool epsilongc_is_mutex_locked_(epsilongc_mutex_t_ mutex);

/* Return the mutex name, if available. The result should not be
   free()d or modified by the caller. */
const char* epsilongc_mutex_name_(const epsilongc_mutex_t_ mutex);

/* ***** Spinlocks ******** */

/* A spinlock is a pointer to an opaque data structure: */
typedef struct epsilongc_spinlock_* epsilongc_spinlock_t_;

/* Make a new spinlock: */
epsilongc_spinlock_t_ epsilongc_make_spinlock_(const char *name,
                                               const epsilongc_integer_t index);

/* Destroy a spinlock: */
void epsilongc_destroy_spinlock_(epsilongc_spinlock_t_ spinlock);

/* Lock a spinlock: */
void epsilongc_lock_spinlock__(epsilongc_spinlock_t_ spinlock);

/* Unlock a spinlock: */
void epsilongc_unlock_spinlock__(epsilongc_spinlock_t_ spinlock);

/* Return true iff the given spinlock is currently locked: */
bool epsilongc_is_spinlock_locked_(epsilongc_spinlock_t_ spinlock);

/* Return the spinlock name, if available. The result should not be
   free()d or modified by the caller. */
const char* epsilongc_spinlock_name_(const epsilongc_spinlock_t_ spinlock);

/* ***** Read/Write Locks ******** */

/* A read/write lock is a pointer to an opaque data structure: */
typedef struct epsilongc_read_write_lock* epsilongc_read_write_lock_t;

/* Make a new read/write lock: */
epsilongc_read_write_lock_t epsilongc_make_read_write_lock(const char *name,
                                                           const epsilongc_integer_t index);

/* Destroy a read/write lock: */
void epsilongc_destroy_read_write_lock(epsilongc_read_write_lock_t read_write_lock);

/* Lock a read/write lock for reading: */
void epsilongc_lock_read_write_lock_for_reading_(epsilongc_read_write_lock_t read_write_lock);

/* Lock a read/write lock for writing: */
void epsilongc_lock_read_write_lock_for_writing_(epsilongc_read_write_lock_t read_write_lock);

/* Unlock a read/write lock: */
void epsilongc_unlock_read_write_lock_(epsilongc_read_write_lock_t read_write_lock);

/* Return the read/write lock name, if available. The result should not be
   free()d or modified by the caller. */
const char* epsilongc_read_write_lock_name(const epsilongc_read_write_lock_t read_write_lock);

/* ***** Conditions ******** */

/* A condition is a pointer to an opaque data structure: */
typedef struct epsilongc_condition* epsilongc_condition_t;

/* Make a new condition: */
epsilongc_condition_t epsilongc_make_condition(const char *name,
                                               const epsilongc_integer_t index);

/* Destroy a condition: */
void epsilongc_destroy_condition(epsilongc_condition_t condition);

/* Wait on a condition: */
void epsilongc_wait_condition(epsilongc_condition_t condition,
                              epsilongc_mutex_t_ mutex);

/* Signal a condition: */
void epsilongc_signal_condition(epsilongc_condition_t condition);

/* Broadcast a condition: */
void epsilongc_broadcast_condition(epsilongc_condition_t condition);

/* ***** Semaphores ******** */

/* A semaphore is a pointer to an opaque data structure: */
typedef struct epsilongc_semaphore* epsilongc_semaphore_t;

/* Make a new semaphore: */
epsilongc_semaphore_t epsilongc_make_semaphore(const char *name,
                                               const epsilongc_integer_t index,
                                               const epsilongc_unsigned_integer_t value);

/* Destroy a semaphore: */
void epsilongc_destroy_semaphore(epsilongc_semaphore_t semaphore);

/* Perform a P on a semaphore: */
void epsilongc_p_semaphore(epsilongc_semaphore_t semaphore);

/* Perform a V on a semaphore: */
void epsilongc_v_semaphore(epsilongc_semaphore_t semaphore);

/* ***** Threads ******** */

/////////////// To do: move this somewhere else, but it may be nontrivial: begin
/* A user-defined buffer to be traced as a root: */
typedef struct epsilongc_user_defined_root* epsilongc_user_defined_root_t;
struct epsilongc_user_defined_root{
  /* Beginning of the buffer: */
  epsilongc_pointer_t buffer;
  
  /* Length of the buffer: */
  epsilongc_unsigned_integer_t length_in_bytes;
  
  /* Next root, or NULL: */
  epsilongc_user_defined_root_t next;
}; // struct

/* This information is enough to be able to trace from the roots of a
   given thread: */
struct epsilongc_thread_roots{
  /* The list of user-defined thread-local roots, which may be empty: */
  epsilongc_user_defined_root_t user_defined_thread_local_roots;
  
  /* Register image (plus something more we don't care about): */
  jmp_buf context_including_registers;
  
  /* Pointers to both ends of the thread-local C stack: */
  epsilongc_pointer_t stack_bottom;
  epsilongc_pointer_t stack_top;
}; // struct
typedef struct epsilongc_thread_roots* epsilongc_thread_roots_t;
/////////////// To do: move this somewhere else, but it may be nontrivial: end

/* The possible state of the garbage collection, from the point of view
   of a worker: */
enum epsilongc_collection_state{
  epsilongc_root_information_is_up_to_date_collection_state,
  epsilongc_collection_ended_collection_state,
}; // enum
typedef enum epsilongc_collection_state epsilongc_collection_state_t;

/* A thread is a pointer to this data structure. Mutator threads can
   not be joined or cancelled (To do: possibly reconsider this). */
typedef struct epsilongc_thread* epsilongc_thread_t;
struct epsilongc_thread{
  /* The actual PThread thread: */
  pthread_t thread;
  
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* The thread name: */
  char *name;
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
  
  ///* The element referring this structure in the global list: this is useful
  //   to be able to efficienly remove it at thread exit time: */
  //epsilongc_doubly_linked_list_element_t element_in_the_global_list;
  
  /* Mutator threads are linked in a global list: */
  EPSILONGC_LIST_ELEMENT_FIELDS(threads, epsilongc_thread_t)
  
  /* Thread roots, updated by the thread at each request. I need 'volatile'
     as this field is accessed by signal handlers. */
  volatile struct epsilongc_thread_roots roots;
  
  /* A flag used for synchronizing collections. I need 'volatile' as this
     field is accessed by signal handlers. */
  volatile epsilongc_collection_state_t collection_state;
  volatile epsilongc_unsigned_integer_t last_collection_no_when_root_information_was_updated;
}; // struct

/* Make a new (non-joinable, non-cancellable) mutator thread. The returned
   pointer refers a structure which is automatically destroyed at thread exit
   time (epsilongc_unregister_mutator_thread() is automatically called).
   make_allocators_or_NULL, if not NULL, is called before the given routine,
   and can be used to create thread-local allocators. They are automatically
   destroyed at thread exit time, so there's no need for the user to supply
   a finalization function for this purpose. Note in particular that the function
   epsilongc_unregister_the_calling_thread_as_a_mutator() will be automatically
   called, and must *not* be called by the user-supplied routine.
   The thread should *not* call pthread_exit(). */
epsilongc_thread_t epsilongc_make_mutator_thread(const char *name,
                                                 const epsilongc_integer_t index,
                                                 void(*make_allocators_or_NULL)(void),
                                                 epsilongc_word_t (*routine)(epsilongc_word_t),
                                                 epsilongc_word_t argument);

/* Make a non-mutator thread (non-joinable, cancellable): */
void epsilongc_make_thread(epsilongc_word_t (*routine)(epsilongc_word_t),
                           epsilongc_word_t argument);

/* Register the calling thread as a mutator, using the current stack top as
   the thread-local stack *bottom*, and making thread-local allocators with
   the given function, if not NULL. Thread-local garbage collection is
   initialized here. After having called this function the calling thread
   *must* also call epsilongc_unregister_the_calling_thread_as_a_mutator()
   before exiting. The thread is made non-cancellable and non-joinable
   (To do: as of now unregistering the thread as a mutator does *not*
   restore the previous state of this; that's not hard to change).
   The thread should *not* call pthread_exit(). */
void epsilongc_register_the_calling_thread_as_a_mutator(const char *name,
                                                        const epsilongc_integer_t index,
                                                        void(*make_allocators_or_NULL)(void));

/* Unregister the calling thread as a mutator thread, removing it from
   the global structure and finalizing thread-local garbage collection
   support. After this function has been called the calling thread
   must not allocate or refer potentially-dead heap objects, and its
   thread descriptor can not be used any more. */
void epsilongc_unregister_the_calling_thread_as_a_mutator(void);

/* Return the calling thread, if it's a mutator thread; otherwise fail
   with a fatal error: */
epsilongc_thread_t epsilongc_calling_mutator_thread(void);

/* Return true iff the caller thread is a mutator thread: */
bool epsilongc_is_the_caller_thread_a_mutator(void);

/* Call the given function once per mutator thread, passing the thread as
   parameter: */
void epsilongc_call_on_all_mutator_threads(void (*function)(epsilongc_thread_t thread));

/* Call the given function once per mutator thread, except the calling
   thread: */
void epsilongc_call_on_all_mutator_threads_except_myself(
   void (*function)(epsilongc_thread_t thread));

/* Return true iff the given predicate returns true on all mutator threads.
   If it returns false for any thread, don't check the remaining ones: */
bool epsilongc_does_hold_for_all_mutator_threads(bool(*predicate)(epsilongc_thread_t thread));

/* Return true iff all mutators have flagged their collection state as roots-up-to-date: */
bool epsilongc_are_all_mutator_roots_up_to_date(void);

/* Return true iff all mutators have flagged their collection state as collection-ended: */
bool epsilongc_have_all_mutator_ended_collection(void);

/* Return the number of currently existing mutator threads: */
epsilongc_unsigned_integer_t epsilongc_mutator_thread_no(void);

/* Return the name of the given thread: */
const char* epsilongc_thread_name(const epsilongc_thread_t thread);

/* Return the name of the calling thread: */
const char* epsilongc_calling_thread_name(void);

/* If we use spinlocks instead of mutexes we just need a handful of
   different #defines: */
#ifdef ENABLE_SPINLOCKS
/* The things we call mutexes in the code are in fact spinlocks: */
#define epsilongc_mutex         epsilongc_spinlock_
#define epsilongc_mutex_t       epsilongc_spinlock_t_
#define epsilongc_make_mutex    epsilongc_make_spinlock_
#define epsilongc_destroy_mutex epsilongc_destroy_spinlock_
#define epsilongc_lock_mutex_   epsilongc_lock_spinlock__
#define epsilongc_unlock_mutex_ epsilongc_unlock_spinlock__
#define epsilongc_mutex_name    epsilongc_spinlock_name_
#else
/* The things we call mutexes in the code are mutexes: */
#define epsilongc_mutex         epsilongc_mutex_
#define epsilongc_mutex_t       epsilongc_mutex_t_
#define epsilongc_make_mutex    epsilongc_make_mutex_
#define epsilongc_destroy_mutex epsilongc_destroy_mutex_
#define epsilongc_lock_mutex_   epsilongc_lock_mutex__
#define epsilongc_unlock_mutex_ epsilongc_unlock_mutex__
#define epsilongc_mutex_name    epsilongc_mutex_name_
#endif // #ifdef ENABLE_SPINLOCKS

#define fflush_on_debug(X) // nothing
#define fprintf_on_debug(...) // nothing
//#define fflush_on_debug fflush
//#define fprintf_on_debug fprintf
//#define stderr_or_stdout stdout

#define epsilongc_lock_mutex(MUTEX_) \
  { fprintf_on_debug(stderr_or_stdout, "..L Trying to lock %s (from %s %s:%i %s())\n", epsilongc_mutex_name(MUTEX_), epsilongc_calling_thread_name(), __FILE__, __LINE__, __FUNCTION__); fflush_on_debug(stdout); fflush_on_debug(stderr_or_stdout); \
    epsilongc_lock_mutex_(MUTEX_); \
    fprintf_on_debug(stderr_or_stdout, "+L Locked %s (from %s %s:%i %s())\n", epsilongc_mutex_name(MUTEX_), epsilongc_calling_thread_name(), __FILE__, __LINE__, __FUNCTION__); fflush_on_debug(stdout); fflush_on_debug(stderr_or_stdout); }

#define epsilongc_unlock_mutex(MUTEX_) \
  { fprintf_on_debug(stderr_or_stdout, "-L Unlocking %s (from %s %s:%i %s())\n", epsilongc_mutex_name(MUTEX_), epsilongc_calling_thread_name(), __FILE__, __LINE__, __FUNCTION__); fflush_on_debug(stdout); fflush_on_debug(stderr_or_stdout);  \
    epsilongc_unlock_mutex_(MUTEX_); }

#define epsilongc_lock_spinlock(SPINLOCK_) \
  { fprintf_on_debug(stderr_or_stdout, "..L Trying to lock %s (from %s %s:%i %s())\n", epsilongc_spinlock_name_(SPINLOCK_), epsilongc_calling_thread_name(), __FILE__, __LINE__, __FUNCTION__); fflush_on_debug(stdout); fflush_on_debug(stderr_or_stdout); \
    epsilongc_lock_spinlock_(SPINLOCK_); \
    fprintf_on_debug(stderr_or_stdout, "+L Locked %s (from %s %s:%i %s())\n", epsilongc_spinlock_name_(SPINLOCK_), epsilongc_calling_thread_name(), __FILE__, __LINE__, __FUNCTION__); fflush_on_debug(stdout); fflush_on_debug(stderr_or_stdout); }

#define epsilongc_unlock_spinlock(SPINLOCK_) \
  { fprintf_on_debug(stderr_or_stdout, "-L Unlocking %s (from %s %s:%i %s())\n", epsilongc_spinlock_name_(SPINLOCK_), epsilongc_calling_thread_name(), __FILE__, __LINE__, __FUNCTION__); fflush_on_debug(stdout); fflush_on_debug(stderr_or_stdout);  \
    epsilongc_unlock_spinlock_(SPINLOCK_); }

#define epsilongc_lock_read_write_lock_for_reading(READ_WRITE_LOCK_) \
  { fprintf_on_debug(stderr_or_stdout, "..R Trying to lock %s for reading (from %s %s:%i %s())\n", epsilongc_read_write_lock_name(READ_WRITE_LOCK_), epsilongc_calling_thread_name(), __FILE__, __LINE__, __FUNCTION__); fflush_on_debug(stdout); fflush_on_debug(stderr_or_stdout); \
    epsilongc_lock_read_write_lock_for_reading_(READ_WRITE_LOCK_); \
    fprintf_on_debug(stderr_or_stdout, "+R Locked %s for reading (from %s %s:%i %s())\n", epsilongc_read_write_lock_name(READ_WRITE_LOCK_), epsilongc_calling_thread_name(), __FILE__, __LINE__, __FUNCTION__); fflush_on_debug(stdout); fflush_on_debug(stderr_or_stdout); }

#define epsilongc_lock_read_write_lock_for_writing(READ_WRITE_LOCK_) \
  { fprintf_on_debug(stderr_or_stdout, "..W Trying to lock %s for WRITING (from %s %s:%i %s())\n", epsilongc_read_write_lock_name(READ_WRITE_LOCK_), epsilongc_calling_thread_name(), __FILE__, __LINE__, __FUNCTION__); fflush_on_debug(stdout); fflush_on_debug(stderr_or_stdout); \
    epsilongc_lock_read_write_lock_for_writing_(READ_WRITE_LOCK_); \
    fprintf_on_debug(stderr_or_stdout, "+W Locked %s for WRITING (from %s %s:%i %s())\n", epsilongc_read_write_lock_name(READ_WRITE_LOCK_), epsilongc_calling_thread_name(), __FILE__, __LINE__, __FUNCTION__); fflush_on_debug(stdout); fflush_on_debug(stderr_or_stdout); }

#define epsilongc_unlock_read_write_lock(READ_WRITE_LOCK_) \
  { fprintf_on_debug(stderr_or_stdout, "-? Unlocking %s (from %s %s:%i %s())\n", epsilongc_read_write_lock_name(READ_WRITE_LOCK_), epsilongc_calling_thread_name(), __FILE__, __LINE__, __FUNCTION__); fflush_on_debug(stdout); fflush_on_debug(stderr_or_stdout); \
    epsilongc_unlock_read_write_lock_(READ_WRITE_LOCK_); }

#endif // #ifndef EPSILONGC_THREADS_H_
