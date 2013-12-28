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
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include "epsilongc_threads.h"
#include "compile_time_parameters.h"
#include <pthread.h>
#include <semaphore.h>
#include <assert.h>
#include <string.h>
//#include <doubly_linked_list.h>
#include "doubly_linked_list_macros.h"
#include "fatal.h"
#include "malloc.h"

/* Return a new malloc()ed string containing the given strings
   concatenated to a text representation of the given number,
   making something easy-to-read for humans: */
static char* epsilongc_make_name(const char* object_sort,
                                 const char* name,
                                 const epsilongc_integer_t index)
  __attribute__((unused, malloc));
static char* epsilongc_make_name(const char* object_sort,
                                 const char* name,
                                 const epsilongc_integer_t index){
  char* result = (char*)
    epsilongc_xmalloc(strlen(object_sort) + strlen(name) + 200);
  sprintf(result, "%s-%s-%li",
          (name != NULL) ? name : "<anonymous>",
          (object_sort != NULL) ? object_sort : "<something>",
          (long)index);
  return result;
}

/* A mutex contains a PThread mutex and, only when debugging, its
   user-readable name: */
struct epsilongc_mutex_{
  /* The actual PThread mutex: */
  pthread_mutex_t mutex;
  
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* The mutex name: */
  char *name;
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
}; // struct

epsilongc_mutex_t_ epsilongc_make_mutex_(const char *name __attribute__((unused)),
                                        const epsilongc_integer_t index __attribute__((unused))){
  /* Allocate the buffer: */
  epsilongc_mutex_t_ result = (epsilongc_mutex_t_)
    epsilongc_xmalloc(sizeof(struct epsilongc_mutex_));

  /* Initialize the PThreads mutex, with the appropriate attributes: */
  pthread_mutexattr_t mutex_attributes;
  pthread_mutexattr_init(&mutex_attributes);
#ifdef ENABLE_ASSERTIONS
  pthread_mutexattr_settype(&mutex_attributes, PTHREAD_MUTEX_ERRORCHECK_NP);
  //pthread_mutexattr_settype(&mutex_attributes, PTHREAD_MUTEX_NORMAL);
#else
  //pthread_mutexattr_settype(&mutex_attributes, PTHREAD_MUTEX_RECURSIVE_NP/* PTHREAD_MUTEX_ADAPTIVE_NP */);
  //pthread_mutexattr_settype(&mutex_attributes, PTHREAD_MUTEX_ADAPTIVE_NP);
  pthread_mutexattr_settype(&mutex_attributes, PTHREAD_MUTEX_NORMAL);
#endif // #ifdef ENABLE_ASSERTIONS
  pthread_mutex_init(&result->mutex, &mutex_attributes);
  pthread_mutexattr_destroy(&mutex_attributes);

#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* Store the name into a new buffer: */
  result->name = epsilongc_make_name("mutex", name, index);
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
  
  /* Return the buffer we filled: */
  return result;
}

void epsilongc_destroy_mutex_(epsilongc_mutex_t_ mutex){
  /* Finalize the Posix mutex: */
  pthread_mutex_destroy(&mutex->mutex);  
  
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* Destroy the name: */
  free(mutex->name);
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
  
  /* Destroy the buffer: */
  free(mutex);
}

const char* epsilongc_mutex_name_(const epsilongc_mutex_t_ mutex){
#ifdef ENABLE_THREAD_OBJECT_NAMES
  return mutex->name;
#else
  return "<mutex name not available>";
#endif // #ifdef ENABLE_MUTEX_OBJECT_NAMES
}

inline void epsilongc_lock_mutex__(epsilongc_mutex_t_ mutex){
  //return;
  /* Lock the PThreads mutex: */
  pthread_mutex_lock(&mutex->mutex);
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* printf("Thread %s: LOCKED the mutex \"%s\".\n", */
  /*        epsilongc_calling_thread_name(), */
  /*        mutex->name); */
  /* fflush(stdout); */
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
}

inline void epsilongc_unlock_mutex__(epsilongc_mutex_t_ mutex){
  //return;
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* printf("Thread %s: UNLOCKING the mutex \"%s\".\n", */
  /*        epsilongc_calling_thread_name(), */
  /*        mutex->name); */
  /* fflush(stdout); */
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
  /* Unlock the PThreads mutex: */
  pthread_mutex_unlock(&mutex->mutex);
}

inline bool epsilongc_is_mutex_locked_(epsilongc_mutex_t_ mutex){
  //printf("Is %p locked?\n", &mutex->mutex);
  /* Try to acquire the lock: */
  const int result = pthread_mutex_trylock(&mutex->mutex);
  
  /* Did we fail? */
  if(result == EBUSY)
    return true; /* Yes, we failed: the mutex is locked. */
  else{
    /* No, we succeeded: hence the mutex has been locked *now*, by us: unlock
       it and say it wasn't locked: */
    pthread_mutex_unlock(&mutex->mutex);
    return false;
  } // else
}

/* A spinlock contains a PThread spinlock and, only when debugging, its
   user-readable name: */
struct epsilongc_spinlock_{
  /* The actual PThread spinlock: */
  pthread_spinlock_t spinlock;
  
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* The spinlock name: */
  char *name;
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
}; // struct

epsilongc_spinlock_t_ epsilongc_make_spinlock_(const char *name __attribute__((unused)),
                                               const epsilongc_integer_t index __attribute__((unused))){
  /* Allocate the buffer: */
  epsilongc_spinlock_t_ result = (epsilongc_spinlock_t_)
    epsilongc_xmalloc(sizeof(struct epsilongc_spinlock_));

  /* Initialize the PThreads spinlock: */
  pthread_spin_init(&result->spinlock, 0 /* not pshared */);

#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* Store the name into a new buffer: */
  result->name = epsilongc_make_name("spinlock", name, index);
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
  
  /* Return the buffer we filled: */
  return result;
}

void epsilongc_destroy_spinlock_(epsilongc_spinlock_t_ spinlock){
  /* Finalize the Posix spinlock: */
  pthread_spin_destroy(&spinlock->spinlock);  
  
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* Destroy the name: */
  free(spinlock->name);
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
  
  /* Destroy the buffer: */
  free(spinlock);
}

const char* epsilongc_spinlock_name_(const epsilongc_spinlock_t_ spinlock){
#ifdef ENABLE_THREAD_OBJECT_NAMES
  return spinlock->name;
#else
  return "<spinlock name not available>";
#endif // #ifdef ENABLE_SPINLOCK_OBJECT_NAMES
}

inline void epsilongc_lock_spinlock__(epsilongc_spinlock_t_ spinlock){
  //return;
  /* Lock the PThreads spinlock: */
  pthread_spin_lock(&spinlock->spinlock);
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* printf("Thread %s: LOCKED the spinlock \"%s\".\n", */
  /*        epsilongc_calling_thread_name(), */
  /*        spinlock->name); */
  /* fflush(stdout); */
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
}

inline void epsilongc_unlock_spinlock__(epsilongc_spinlock_t_ spinlock){
  //return;
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* printf("Thread %s: UNLOCKING the spinlock \"%s\".\n", */
  /*        epsilongc_calling_thread_name(), */
  /*        spinlock->name); */
  /* fflush(stdout); */
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
  /* Unlock the PThreads spinlock: */
  pthread_spin_unlock(&spinlock->spinlock);
}

inline bool epsilongc_is_spinlock_locked_(epsilongc_spinlock_t_ spinlock){
  //printf("Is %p locked?\n", &spinlock->spinlock);
  /* Try to acquire the lock: */
  const int result = pthread_spin_trylock(&spinlock->spinlock);
  
  /* Did we fail? */
  if(result == EBUSY)
    return true; /* Yes, we failed: the spinlock is locked. */
  else{
    /* No, we succeeded: hence the spinlock has been locked *now*, by us: unlock
       it and say it wasn't locked: */
    pthread_spin_unlock(&spinlock->spinlock);
    return false;
  } // else
}

///////////
/* A read_write_lock contains a PThread read_write_lock and, only when debugging, its
   user-readable name: */
struct epsilongc_read_write_lock{
  /* The actual PThread read_write_lock: */
  pthread_rwlock_t read_write_lock;
  
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* The read_write_lock name: */
  char *name;
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
}; // struct

epsilongc_read_write_lock_t epsilongc_make_read_write_lock(const char *name __attribute__((unused)),
                                                           const epsilongc_integer_t index __attribute__((unused))){
  /* Allocate the buffer: */
  epsilongc_read_write_lock_t result = (epsilongc_read_write_lock_t)
    epsilongc_xmalloc(sizeof(struct epsilongc_read_write_lock));

  /* Initialize the PThreads read/write lock, with the appropriate attributes: */
  pthread_rwlockattr_t read_write_lock_attributes;
  pthread_rwlockattr_init(&read_write_lock_attributes);
#ifdef ENABLE_ASSERTIONS
#else
  //pthread_rwlockattr_setkind_np(&read_write_lock_attributes, PTHREAD_RWLOCK_PREFER_WRITER_NP);
#endif // #ifdef ENABLE_ASSERTIONS
  pthread_rwlock_init(&result->read_write_lock, &read_write_lock_attributes);
  pthread_rwlockattr_destroy(&read_write_lock_attributes);

#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* Store the name into a new buffer: */
  result->name = epsilongc_make_name("read-write-lock", name, index);
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
  
  /* Return the buffer we filled: */
  return result;
}

void epsilongc_destroy_read_write_lock(epsilongc_read_write_lock_t read_write_lock){
  /* Finalize the Posix read_write_lock: */
  pthread_rwlock_destroy(&read_write_lock->read_write_lock);  
  
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* Destroy the name: */
  free(read_write_lock->name);
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
  
  /* Destroy the buffer: */
  free(read_write_lock);
}

const char* epsilongc_read_write_lock_name(const epsilongc_read_write_lock_t read_write_lock){
#ifdef ENABLE_THREAD_OBJECT_NAMES
  return read_write_lock->name;
#else
  return "<read_write_lock name not available>";
#endif // #ifdef ENABLE_READ_WRITE_LOCK_OBJECT_NAMES
}

inline void epsilongc_lock_read_write_lock_for_reading_(epsilongc_read_write_lock_t read_write_lock){
  //return;
  /* Lock the PThreads read_write_lock for reading: */
  pthread_rwlock_rdlock(&read_write_lock->read_write_lock);
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* printf("Thread %s: LOCKED the read_write_lock \"%s\".\n", */
  /*        epsilongc_calling_thread_name(), */
  /*        read_write_lock->name); */
  /* fflush(stdout); */
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
}

inline void epsilongc_lock_read_write_lock_for_writing_(epsilongc_read_write_lock_t read_write_lock){
  //return;
  /* Lock the PThreads read_write_lock for writing: */
  pthread_rwlock_wrlock(&read_write_lock->read_write_lock);
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* printf("Thread %s: LOCKED the read_write_lock \"%s\".\n", */
  /*        epsilongc_calling_thread_name(), */
  /*        read_write_lock->name); */
  /* fflush(stdout); */
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
}

inline void epsilongc_unlock_read_write_lock_(epsilongc_read_write_lock_t read_write_lock){
  //return;
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* printf("Thread %s: UNLOCKING the read_write_lock \"%s\".\n", */
  /*        epsilongc_calling_thread_name(), */
  /*        read_write_lock->name); */
  /* fflush(stdout); */
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
  pthread_rwlock_unlock(&read_write_lock->read_write_lock);
}
///////////

/* A condition contains a PThread condition and, only when debugging, its
   user-readable name: */
struct epsilongc_condition{
  /* The actual PThread condition: */
  pthread_cond_t condition;
  
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* The condition name: */
  char *name;
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
}; // struct

epsilongc_condition_t epsilongc_make_condition(const char *name __attribute__((unused)),
                                               const epsilongc_integer_t index __attribute__((unused))){
  /* Allocate the buffer: */
  epsilongc_condition_t result = (epsilongc_condition_t)
    epsilongc_xmalloc(sizeof(struct epsilongc_condition));

  /* Initialize the PThreads condition, with the appropriate attributes: */
  pthread_condattr_t condition_attributes;
  pthread_condattr_init(&condition_attributes);
  // No attributes are currently supported in NPTL (To do: is this still true?)
#ifdef ENABLE_THREAD_OBJECT_NAMES
#else
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
  pthread_cond_init(&result->condition, &condition_attributes);
  pthread_condattr_destroy(&condition_attributes);
  
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* Store the name into a new buffer: */
  result->name = epsilongc_make_name("condition", name, index);
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
  
  /* Return the buffer we filled: */
  return result;
}

void epsilongc_destroy_condition(epsilongc_condition_t condition){
  /* Finalize the Posix condition: */
  pthread_cond_destroy(&condition->condition);  
  
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* Destroy the name: */
  free(condition->name);
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
  
  /* Destroy the buffer: */
  free(condition);
}

inline void epsilongc_wait_condition(epsilongc_condition_t condition,
                                     epsilongc_mutex_t_ mutex){
  epsilongc_assert_on_debug(condition != NULL);
  epsilongc_assert_on_debug(mutex != NULL);
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* printf("Thread %p: waiting on \"%s\" with \"%s\".\n", */
  /*        (void*)pthread_self(), */
  /*        condition->name, */
  /*        mutex->name); */
  /* fflush(stdout); */
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
  pthread_cond_wait(&condition->condition,
                    &mutex->mutex);
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* printf("Thread %p: \"%s\" was signaled.\n", */
  /*        (void*)pthread_self(), */
  /*        condition->name); */
  /* fflush(stdout); */
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
}

inline void epsilongc_signal_condition(epsilongc_condition_t condition){
  epsilongc_assert_on_debug(condition != NULL);
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* printf("Thread %p: signaling \"%s\".\n", */
  /*        (void*)pthread_self(), */
  /*        condition->name); */
  /* fflush(stdout); */
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
  pthread_cond_signal(&condition->condition);
}

inline void epsilongc_broadcast_condition(epsilongc_condition_t condition){
  epsilongc_assert_on_debug(condition != NULL);
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* printf("Thread %p: broadcasting \"%s\".\n", */
  /*        (void*)pthread_self(), */
  /*        condition->name); */
  /* fflush(stdout); */
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
  pthread_cond_broadcast(&condition->condition);
}

///////////

/* A semaphore contains a PThread semaphore and, only when debugging, its
   user-readable name: */
struct epsilongc_semaphore{
  /* The actual PThread semaphore: */
  sem_t semaphore;
  
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* The semaphore name: */
  char *name;
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
}; // struct

epsilongc_semaphore_t epsilongc_make_semaphore(const char *name,
                                               const epsilongc_integer_t index,
                                               const epsilongc_unsigned_integer_t value){
  /* Allocate the buffer: */
  epsilongc_semaphore_t result = (epsilongc_semaphore_t)
    epsilongc_xmalloc(sizeof(struct epsilongc_semaphore));

  /* Initialize the PThreads semaphore: */
  sem_init(&result->semaphore, 0, (unsigned int)value);
  
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* Store the name into a new buffer: */
  result->name = epsilongc_make_name("semaphore", name, index);
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
  
  /* Return the buffer we filled: */
  return result;
}

/* Destroy a semaphore: */
void epsilongc_destroy_semaphore(epsilongc_semaphore_t semaphore){
  /* Finalize the Posix semaphore: */
  sem_destroy(&semaphore->semaphore);  
  
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* Destroy the name: */
  free(semaphore->name);
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
  
  /* Destroy the buffer: */
  free(semaphore);
}

/* P on a semaphore: */
inline void epsilongc_p_semaphore(epsilongc_semaphore_t semaphore){
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* printf("Thread %s: P on \"%s\".\n", */
  /*        epsilongc_calling_thread_name(), */
  /*        semaphore->name); */
  /* fflush(stdout); */
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
  sem_wait(&semaphore->semaphore);
}

/* V on a semaphore: */
inline void epsilongc_v_semaphore(epsilongc_semaphore_t semaphore){
  if(sem_post(&semaphore->semaphore) != 0)
    epsilongc_fatal("V on %s failed",
#ifdef ENABLE_THREAD_OBJECT_NAMES
                    semaphore->name
#else
                    "a semaphore"
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
                    );
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* printf("Thread %s: V on \"%s\".\n", */
  /*        epsilongc_calling_thread_name(), */
  /*        semaphore->name); */
  /* fflush(stdout); */
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
}
///////////

/* A thread list type: */
typedef struct epsilongc_thread_list* epsilongc_thread_list_t;
struct epsilongc_thread_list{
  EPSILONGC_LIST_FIELDS(threads, epsilongc_thread_t)
}; // struct

/* The global list of mutator threads: */
//static epsilongc_doubly_linked_list_t epsilongc_mutator_thread_list = NULL;
static struct epsilongc_thread_list epsilongc_mutator_thread_list;

///* The mutex protecting the list from concurrent accesses: */
//static epsilongc_mutex_t epsilongc_mutator_thread_list_mutex = NULL;

/* See the comment in epsilongc_make_mutator_thread() for information about
   why we need this: */
struct epsilongc_mutator_thread_parameters{
  /* The mutator thread: */
  epsilongc_thread_t thread;
  
  /* The routine to make thread-local allocators, or NULL if this is done in
     the main routine: */
  void (*make_allocators_or_NULL)(void);
  
  /* The actual routine to call, which must not be NULL: */
  epsilongc_word_t (*actual_routine)(epsilongc_word_t);
  
  /* The argument to the actual routine: */
  epsilongc_word_t argument;
}; // struct
typedef struct epsilongc_mutator_thread_parameters* epsilongc_mutator_thread_parameters_t;

/* The caller thread as an epsilongc mutator thread: */
static __thread epsilongc_thread_t epsilongc_myself_as_a_mutator_thread = NULL;

/* The actual code executed by a mutator thread: it requires an initialization
   and a finalization around the code supplied by the user... */
static void* epsilongc_mutator_thread_routine(void* parameters_as_void_star){
  epsilongc_mutator_thread_parameters_t parameters =
    (epsilongc_mutator_thread_parameters_t)parameters_as_void_star;
  //printf("%s: epsilongc_mutator_thread_routine(): begin\n", epsilongc_calling_thread_name());
  
  /* Make the thread not cancellable: */
  pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, NULL);
  
  /* Set the thread-local variable establishing the thread's identity: */
  epsilongc_thread_t this_thread = parameters->thread;
  epsilongc_myself_as_a_mutator_thread = this_thread;
  
  /* Initialize global structures and make the thread visible to the external
     world: */
  //printf("LOCKING the GC from the created thread %s.\n", epsilongc_calling_thread_name()); fflush(stdout);
  epsilongc_lock_read_write_lock_for_reading(epsilongc_global_read_write_lock);
  epsilongc_lock_mutex(epsilongc_global_mutex);
  //epsilongc_append_to_doubly_linked_list(this_thread, epsilongc_mutator_thread_list);
  EPSILONGC_INITIALIZE_LIST_ELEMENT(threads, this_thread);
  EPSILONGC_APPEND_OBJECT_TO_LIST(threads, threads, epsilongc_thread_t, this_thread, &epsilongc_mutator_thread_list);
  
  /* Initialize thread-local GC support and make allocators, which are
     always thread-local: */
  epsilongc_initialize_thread_local_garbage_collection();
  if(parameters->make_allocators_or_NULL != NULL)
    parameters->make_allocators_or_NULL();
  epsilongc_make_thread_local_implicit_allocators();
  
  /* Ok, now we can release the lock and actually perform this thread's
     work: */
  //printf("UNLocking the GC from the created thread %s.\n", epsilongc_calling_thread_name()); fflush(stdout);
  epsilongc_unlock_mutex(epsilongc_global_mutex);
  epsilongc_unlock_read_write_lock(epsilongc_global_read_write_lock);
  
  /* Call the user-defined routine with the user-defined argument: */
  //printf("%s: epsilongc_mutator_thread_routine(): Calling the user routine...\n", epsilongc_calling_thread_name());
  epsilongc_assert_on_debug(parameters->actual_routine != NULL);
  parameters->actual_routine(parameters->argument);
  //printf("%s: epsilongc_mutator_thread_routine(): Returned from the user routine...\n", epsilongc_calling_thread_name());
  
  // This free() is safe from reentrancy problems, as it can't happen during collection:
  //printf("%s: epsilongc_mutator_thread_routine(): about to free()...\n", epsilongc_calling_thread_name());
  free(parameters_as_void_star);
  //printf("%s: epsilongc_mutator_thread_routine(): free()ed.\n", epsilongc_calling_thread_name());

  /* Unregister the thread, which includes destroying thread-local allocators in a
     crictical section: */
  //printf("%s: epsilongc_mutator_thread_routine(): unregistering...\n", epsilongc_calling_thread_name());
  epsilongc_unregister_the_calling_thread_as_a_mutator();
  //printf("%s: epsilongc_mutator_thread_routine(): end\n", epsilongc_calling_thread_name());
  return NULL;
}

/* The PThread attributes we use to initialize every mutator thread: */
static pthread_attr_t epsilongc_mutator_thread_attributes;

void epsilongc_make_thread(epsilongc_word_t (*routine)(epsilongc_word_t),
                           epsilongc_word_t argument){
  pthread_t thread;
  if(pthread_create(&thread,
                    &epsilongc_mutator_thread_attributes,
                    routine,
                    argument) != 0)
    epsilongc_fatal("pthread_create() failed when creating a non-mutator thread");
}

epsilongc_thread_t epsilongc_make_mutator_thread(const char *name,
                                                 const epsilongc_integer_t index,
                                                 void(*make_allocators_or_NULL)(void),
                                                 epsilongc_word_t (*routine)(epsilongc_word_t),
                                                 epsilongc_word_t argument){
  /* Make the parameters structure; we have to pass it to a POSIX thread,
     which executes a function taking only a void* as its parameter; so we
     malloc() a structure here, we pass the thread a pointer to it, and the
     thread itself will eventually free() the buffer: */
  epsilongc_mutator_thread_parameters_t parameters = (epsilongc_mutator_thread_parameters_t)
    epsilongc_xmalloc(sizeof(struct epsilongc_mutator_thread_parameters));
  parameters->make_allocators_or_NULL = make_allocators_or_NULL;
  parameters->actual_routine = routine;
  parameters->argument = argument;

  /* Make the thread structure and fill it. It will be added to the global list
     with the appropriate synchronization *by the new thread itself*; note
     that the struct passed to the thread as parameter must also contain a
     pointer to the thread struct. Note that the field 
     'element_in_the_global_list' will be also filled from the thread, hence
     it will not be immediately available; but it will be available to the
     collector at the next collection (and collector threads are the only ones
     which actually use the field). */
  epsilongc_thread_t result = (epsilongc_thread_t)
    epsilongc_xmalloc(sizeof(struct epsilongc_thread));
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* Set the name field: */
  result->name = epsilongc_make_name("thread", name, index);
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
  parameters->thread = result;
  result->collection_state = epsilongc_collection_ended_collection_state;
  result->last_collection_no_when_root_information_was_updated = 0;
  if(pthread_create(&result->thread,
                    &epsilongc_mutator_thread_attributes,
                    epsilongc_mutator_thread_routine,
                    (void*)parameters) != 0)
    epsilongc_fatal("pthread_create() failed");
  
  /* Return a pointer to the thread structure we made: */
  return result;
}

void epsilongc_register_the_calling_thread_as_a_mutator(const char *name __attribute__((unused)),
                                                        const epsilongc_integer_t index __attribute__((unused)),
                                                        void(*make_allocators_or_NULL)(void)){
  /* Make the thread not cancellable: */
  pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, NULL);
  
  /* Make the thread structure, and start to fill it; we don't require any
     synchronization yet as we're not making this structure known to any other
     thread. */
  epsilongc_thread_t thread = (epsilongc_thread_t)
    epsilongc_xmalloc(sizeof(struct epsilongc_thread));
  thread->thread = pthread_self();
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* Set the name field: */
  thread->name = epsilongc_make_name("thread", name, index);
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
  thread->collection_state = epsilongc_collection_ended_collection_state;
  thread->last_collection_no_when_root_information_was_updated = 0;
  
  /* Here begins the crictial section: we have to add our thread to global
     structures: */
  epsilongc_lock_read_write_lock_for_reading(epsilongc_global_read_write_lock);
  epsilongc_lock_mutex(epsilongc_global_mutex);
  //epsilongc_append_to_doubly_linked_list(thread, epsilongc_mutator_thread_list);
  EPSILONGC_INITIALIZE_LIST_ELEMENT(threads, thread);
  EPSILONGC_APPEND_OBJECT_TO_LIST(threads, threads, epsilongc_thread_t, thread, &epsilongc_mutator_thread_list);
  //epsilongc_doubly_linked_list_element_t list_element = epsilongc_last_element(epsilongc_mutator_thread_list);
  //thread->element_in_the_global_list = list_element;
  
  /* Initialize thread local garbage collection support and make
     allocators. We are still in the crictical section, and for good
     reason: */
  epsilongc_initialize_thread_local_garbage_collection();
  if(make_allocators_or_NULL != NULL)
    make_allocators_or_NULL();
  epsilongc_make_thread_local_implicit_allocators();
  epsilongc_myself_as_a_mutator_thread = thread;
  
  /* End of the crictical section: */
  epsilongc_unlock_mutex(epsilongc_global_mutex);
  epsilongc_unlock_read_write_lock(epsilongc_global_read_write_lock);
}

void epsilongc_unregister_the_calling_thread_as_a_mutator(void){
  //printf("epsilongc_unregister_the_calling_thread_as_a_mutator(): %s is about to terminate\n", epsilongc_calling_thread_name()); fflush(stdout);
  if(epsilongc_myself_as_a_mutator_thread == NULL)
    epsilongc_fatal("attempting to unregister a non-mutator thread");
  epsilongc_thread_t calling_thread = epsilongc_myself_as_a_mutator_thread;
  
  /* Remove the list element (which free()s the element), and finalize
     thread-local garbage collection support (which destroys thread-local
     allocators). Also set to NULL the epsilongc_myself_as_a_mutator_thread variable,
     so that any future attempt to refer the calling thread as a mutator
     fails: */
  epsilongc_lock_read_write_lock_for_reading(epsilongc_global_read_write_lock);
  epsilongc_lock_mutex(epsilongc_global_mutex);
  //epsilongc_remove_from_doubly_linked_list(calling_thread->element_in_the_global_list, epsilongc_mutator_thread_list);
  EPSILONGC_DETACH_ELEMENT_FROM_LIST(threads, threads, epsilongc_thread_t, calling_thread, &epsilongc_mutator_thread_list);

  /* Finalize thread-local GC support, which includes destroying thread-local
     allocators: */
  epsilongc_finalize_thread_local_garbage_collection();
  
  //printf("epsilongc_unregister_the_calling_thread_as_a_mutator(): %s will exit now.\n", epsilongc_calling_thread_name()); fflush(stdout);
  //printf("epsilongc_unregister_the_calling_thread_as_a_mutator(): %s: calling free().\n", epsilongc_calling_thread_name()); fflush(stdout);
#ifdef ENABLE_THREAD_OBJECT_NAMES
  /* Destroy the name: */
  free(calling_thread->name);
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
  /* Destroy the structure itself: */
  free(calling_thread);
  //printf("epsilongc_unregister_the_calling_thread_as_a_mutator(): called free(), still alive.\n"); fflush(stdout);
  epsilongc_myself_as_a_mutator_thread = NULL;
  
  /* Now that we've free()d we can exit the critical section: */
  epsilongc_unlock_mutex(epsilongc_global_mutex);
  epsilongc_unlock_read_write_lock(epsilongc_global_read_write_lock);
}

epsilongc_unsigned_integer_t epsilongc_mutator_thread_no(void){
  epsilongc_lock_read_write_lock_for_reading(epsilongc_global_read_write_lock);
  epsilongc_lock_mutex(epsilongc_global_mutex);
  const epsilongc_unsigned_integer_t result = 
    //epsilongc_doubly_linked_list_length(epsilongc_mutator_thread_list);
    EPSILONGC_LENGTH_OF_LIST(threads, &epsilongc_mutator_thread_list);
  epsilongc_unlock_mutex(epsilongc_global_mutex);
  epsilongc_unlock_read_write_lock(epsilongc_global_read_write_lock);
  return result;
}

const char* epsilongc_thread_name(const epsilongc_thread_t thread){
#ifdef ENABLE_THREAD_OBJECT_NAMES
  return thread->name;
#else
  return "<thread name not available>";
#endif // #ifdef ENABLE_THREAD_OBJECT_NAMES
}

const char* epsilongc_calling_thread_name(void){
  if(epsilongc_myself_as_a_mutator_thread != NULL)
    return epsilongc_thread_name(epsilongc_myself_as_a_mutator_thread);
  else
    return "<non-mutator-thread>";
}

epsilongc_thread_t epsilongc_calling_mutator_thread(void){
  if(epsilongc_myself_as_a_mutator_thread != NULL)
    return epsilongc_myself_as_a_mutator_thread;
  else
    epsilongc_fatal("the current thread (%p) is not a mutator thread",
                    (void*)pthread_self());
}

bool epsilongc_is_the_caller_thread_a_mutator(void){
  return epsilongc_myself_as_a_mutator_thread != NULL;
}

/* Call the given function on all mutator threads except for the given one
   (which may be NULL): */
static void epsilongc_call_on_all_mutator_threads_except(
   void (*function)(epsilongc_thread_t thread),
   epsilongc_thread_t thread_on_which_not_to_call){
  //printf("Ok-G 0\n"); fflush(stdout);
  epsilongc_thread_t thread;
  for(thread = EPSILONGC_FIRST_ELEMENT_OF_LIST(threads, &epsilongc_mutator_thread_list);
      thread != NULL;
      thread = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(threads, thread)){
    //printf("Ok-G 1a\n"); fflush(stdout);
    if(thread != thread_on_which_not_to_call){
      //printf("Ok-G 1b: CALLING %p on %s\n", function, epsilongc_thread_name(thread)); fflush(stdout);
      function(thread);
      //printf("Ok-G 1c: OK, returned from the call.\n"); fflush(stdout);
    } // if
    else{
      //printf("Ok-G 1d: NOT calling %p on %s\n", function, epsilongc_thread_name(thread)); fflush(stdout);
    }
    //printf("Ok-G 1e\n"); fflush(stdout);
  }
  //printf("Ok-G 2: end of the for loop\n"); fflush(stdout);
}

void epsilongc_call_on_all_mutator_threads(void (*function)(epsilongc_thread_t thread)){
  epsilongc_call_on_all_mutator_threads_except(function, NULL);
}

void epsilongc_call_on_all_mutator_threads_except_myself(void (*function)(epsilongc_thread_t thread)){
  epsilongc_call_on_all_mutator_threads_except(function,
                                               epsilongc_myself_as_a_mutator_thread);
}

bool epsilongc_does_hold_for_all_mutator_threads(bool(*predicate)(epsilongc_thread_t thread)){
  volatile epsilongc_thread_t thread;
  for(thread = EPSILONGC_FIRST_ELEMENT_OF_LIST(threads, &epsilongc_mutator_thread_list);
      thread != NULL;
      thread = EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(threads, thread)){ 
    /* printf("%p: Checking if a condition holds for %p\n", (void*)pthread_self(), (void*)thread->thread); fflush(stdout); */
    /* printf("%p:                      (also called %s): %s\n", (void*)pthread_self(), epsilongc_thread_name(thread), predicate(thread)? "yes": "no"); fflush(stdout); */
    /* if(EPSILONGC_NEXT_ELEMENT_OF_ELEMENT(threads, thread) == NULL) */
    /*   printf("%p: This is the last thread\n", (void*)pthread_self()); */
    if(! predicate(thread)){
#ifdef ENABLE_VERBOSE_DEBUG
      printf("The condition does not hold for %s\n", epsilongc_thread_name(thread)); fflush(stdout);
#endif // #ifdef ENABLE_VERBOSE_DEBUG
      //printf("%p: The answer is FALSE\n", (void*)pthread_self());
      return false;
    } // if
  } // for
  //printf("%p: The answer is TRUE\n", (void*)pthread_self());
  return true;
}

static bool epsilongc_has_ended_collection(volatile epsilongc_thread_t thread){
  return thread->collection_state == epsilongc_collection_ended_collection_state;
}

static bool epsilongc_has_up_to_date_roots(volatile epsilongc_thread_t thread){
  /* printf("Thread %p: Are the roots of the thread %p (%s) up-to-date? %s\n", */
  /*        (void*)pthread_self(), */
  /*        (void*)thread->thread, thread->name, */
  /*        thread->are_roots_up_to_date ? "yes" : "no" */
  /*        ); */
  if((thread->collection_state == epsilongc_root_information_is_up_to_date_collection_state) &&
     (thread->last_collection_no_when_root_information_was_updated != epsilongc_last_collection_index)){
    //#ifdef ENABLE_ASSERTIONS
    printf("\n===============================================================================================\n;");
    printf("A second SIGUSR1 arrived during collection %lu while %s was still in the\n", (unsigned long)epsilongc_last_collection_index, epsilongc_calling_thread_name());
    printf("handler for the previous SIGUSR1 (after handling its SIGUSR2).\n");
    printf("EVERYTHING WILL WORK NONETHELESS, AS THE COLLECTION NUMBER IS DETECTED AS WRONG.\n");
    printf("=================================================================================================\n\n");
    fflush(stdout);
    //#endif //#ifdef ENABLE_ASSERTIONS
  } // if
  return
    (thread->collection_state == epsilongc_root_information_is_up_to_date_collection_state)
    && (thread->last_collection_no_when_root_information_was_updated == epsilongc_last_collection_index);
}

bool epsilongc_are_all_mutator_roots_up_to_date(void){
  return epsilongc_does_hold_for_all_mutator_threads(epsilongc_has_up_to_date_roots);
}

bool epsilongc_have_all_mutator_ended_collection(void){
  return epsilongc_does_hold_for_all_mutator_threads(epsilongc_has_ended_collection);
}
////////////////////////////////

void epsilongc_initialize_threads(void){
  //printf("epsilongc: initializing thread support: begin\n");
  //assert(epsilongc_mutator_thread_list == NULL);
  EPSILONGC_INITIALIZE_LIST(threads, &epsilongc_mutator_thread_list);
  //assert(epsilongc_mutator_thread_list_mutex == NULL);
  //epsilongc_mutator_thread_list = epsilongc_make_doubly_linked_list();
  //epsilongc_mutator_thread_list_mutex = epsilongc_make_mutex("mutator-thread-list", 0);
  pthread_attr_init(&epsilongc_mutator_thread_attributes); 
  pthread_attr_setdetachstate(&epsilongc_mutator_thread_attributes, PTHREAD_CREATE_DETACHED);
  //printf("epsilongc: initializing thread support: end\n");
}

void epsilongc_finalize_threads(void){
  epsilongc_lock_read_write_lock_for_reading(epsilongc_global_read_write_lock);
  //printf("epsilongc: finalizing thread support: begin\n");
  //assert(epsilongc_mutator_thread_list != NULL);
  //assert(epsilongc_mutator_thread_list_mutex != NULL);
  if(EPSILONGC_LENGTH_OF_LIST(threads, &epsilongc_mutator_thread_list) != 0)
    epsilongc_fatal("attempting to destroy a non-empty mutator thread list");
  //epsilongc_destroy_mutex(epsilongc_mutator_thread_list_mutex);
  //epsilongc_mutator_thread_list = NULL;
  EPSILONGC_FINALIZE_LIST(threads, &epsilongc_mutator_thread_list);
  //epsilongc_mutator_thread_list_mutex = NULL;
  pthread_attr_destroy(&epsilongc_mutator_thread_attributes);
  //printf("epsilongc: finalizing thread support: end\n");
  epsilongc_unlock_read_write_lock(epsilongc_global_read_write_lock);
}
