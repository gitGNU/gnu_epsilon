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


#ifndef EPSILONGC_RUN_TIME_SETTINGS_H_
#define EPSILONGC_RUN_TIME_SETTINGS_H_

#include "epsilongc_types.h"
#include <stdbool.h>

/* Initialize support for runtime settings: */
void epsilongc_initialize_run_time_settings(void);

/* Finalize support for runtime settings: */
void epsilongc_finalize_run_time_settings(void);

/* Make garbage collection verbose: */
void epsilongc_set_verbose_collection(bool value);
bool epsilongc_get_verbose_collection(void) __attribute__((pure));

#endif // #ifndef EPSILONGC_RUN_TIME_SETTINGS_H_
