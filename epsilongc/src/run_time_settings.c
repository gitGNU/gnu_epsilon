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


#include "run_time_settings.h"

static bool epsilongc_verbose_collection_flag;
void epsilongc_set_verbose_collection(bool value){
  epsilongc_verbose_collection_flag = value;
}
bool epsilongc_get_verbose_collection(void){
  return epsilongc_verbose_collection_flag;
}

void epsilongc_initialize_run_time_settings(void){
  /* By default we are not verbose: */
  epsilongc_verbose_collection_flag = false;

  char *value_of_epsilongc_verbose_as_string = getenv("EPSILONGC_VERBOSE");
  if(value_of_epsilongc_verbose_as_string != NULL){
    printf("Setting epsilongc's verbosity to true\n");
    epsilongc_verbose_collection_flag = true;
  } // if EPSILONGC_VERBOSE is set
}

void epsilongc_finalize_run_time_settings(void){
  // Nothing yet
}
