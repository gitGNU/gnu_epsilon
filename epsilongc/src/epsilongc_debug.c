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


#include "epsilongc_debug.h"
#include "epsilongc_threads.h"
#include <pthread.h>
#include <signal.h>
/* See the comment in epsilongc_types.h about the reason for this mess. */

/* /\* These macros should *not* be expanded in this file: *\/ */
#undef printf
#undef fprintf
#undef fflush

static epsilongc_mutex_t epsilongc_stdio_mutex = NULL;

/* A sigmask with all the signals in: */
static sigset_t epsilongc_all_signals;


void epsilongc_initialize_debugging(void){
  assert(epsilongc_stdio_mutex == NULL);
  epsilongc_stdio_mutex = epsilongc_make_mutex("stdio", 0);
  sigfillset(&epsilongc_all_signals);
}

void epsilongc_finalize_debugging(void){
  assert(epsilongc_stdio_mutex != NULL);
  epsilongc_destroy_mutex(epsilongc_stdio_mutex);
  epsilongc_stdio_mutex = NULL;
}

int epsilongc_signal_safe_printf(const char *format, ...){
  va_list args;
  va_start(args, format);
  sigset_t current_sigmask;
  pthread_sigmask(SIG_BLOCK, &epsilongc_all_signals, &current_sigmask);
  epsilongc_lock_mutex(epsilongc_stdio_mutex);
  //printf("[THIS-IS-SIGNAL-SAFE]");
  const int result = vfprintf(stdout, format, args);
  fflush(stdout);
  epsilongc_unlock_mutex(epsilongc_stdio_mutex);
  pthread_sigmask(SIG_SETMASK, &current_sigmask, NULL);
  va_end(args);
  return result;
}

int epsilongc_signal_safe_fprintf(FILE *stream, const char *format, ...){
  va_list args;
  va_start(args, format);
  sigset_t current_sigmask;
  pthread_sigmask(SIG_BLOCK, &epsilongc_all_signals, &current_sigmask);
  epsilongc_lock_mutex(epsilongc_stdio_mutex);
  //printf("[THIS-IS-SIGNAL-SAFE]");
  const int result = vfprintf(stream, format, args);
  fflush(stream);
  epsilongc_unlock_mutex(epsilongc_stdio_mutex);
  pthread_sigmask(SIG_SETMASK, &current_sigmask, NULL);
  va_end(args);
  return result;
}

int epsilongc_signal_safe_fflush(FILE *stream){
  sigset_t current_sigmask;
  pthread_sigmask(SIG_BLOCK, &epsilongc_all_signals, &current_sigmask);
  epsilongc_lock_mutex(epsilongc_stdio_mutex);
  const int result = fflush(stream);
  epsilongc_unlock_mutex(epsilongc_stdio_mutex);
  pthread_sigmask(SIG_SETMASK, &current_sigmask, NULL);
  return result;
}

/* Re-enable our hackish macros: */
#include "epsilongc_debugging_hack.h"
