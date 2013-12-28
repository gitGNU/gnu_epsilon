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


/* Just #include all source files in an appropriate order. This allows
   to emulate the effect of --combine, while using AutoMake. */

#include "epsilongc.h"
#include "epsilongc_types.h"
#include "epsilongc_debug.h"

//#include "epsilongc_threads.h"
//#include <pthread.h>

#include "epsilongc_threads.c"
#include "doubly_linked_list_macros.h"
#include "global_structures.c" 
#include "garbage_collector.c" 
#include "run_time_settings.c"
#include "allocator.c" 
#include "kind.c" 
#include "page.c" 
#include "pool.c" 
#include "heuristics.c" 
#include "set_of_pages.c" 
#include "trace.c" 
#include "roots.c" 
#include "fatal.c" 
#include "time.c" 
#include "malloc.c" 
#include "epsilongc_debug.c" 
