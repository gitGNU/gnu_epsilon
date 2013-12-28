/* [the file purpose in one line ???]

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


#include "interface-with-c.h"
#include "../utility/utility.h"
#include <dlfcn.h>
#include <string.h>

/* Map modules into handles, as returned by dlopen(): */
static epsilon_string_hash_t epsilon_module_to_handle = NULL;

/* Map modules into other maps mapping symbols into pointers, as returned
   by dlsym(): */
static epsilon_string_hash_t epsilon_module_to_symbol_to_pointer = NULL;

static void epsilon_initialize_hashes_if_needed(void){
  EPSILON_IF_UNLIKELY(epsilon_module_to_handle == NULL){
    epsilon_module_to_handle = epsilon_make_string_hash();
    epsilon_module_to_symbol_to_pointer = epsilon_make_string_hash();
  } // if-unlikely
}

epsilon_word epsilon_lookup_c_global_from(char *symbol, char *module_or_NULL){
  /* Make hashes, if they don't exist yet: */
  epsilon_initialize_hashes_if_needed();
  char *module = (module_or_NULL == NULL) ? "<self>" : module_or_NULL;
  
  /* Lookup the handle for the module, or create it it we don't have it yet.  Also set
     symbol_to_pointer_for_module to the map from symbol to pointer for the given module
     (which may be a new map if we didn't know the module before): */
  void *module_handle = epsilon_lookup_string_hash(epsilon_module_to_handle, module);
  epsilon_string_hash_t symbol_to_pointer_for_module;
  if(module_handle == NULL){
    module_handle = dlopen(module_or_NULL, RTLD_NOW);
    EPSILON_IF_UNLIKELY(module_handle == NULL)
      epsilon_fatal("epsilon_lookup_c_global_from(): couldn't find the module %s", module);
    epsilon_add_to_string_hash(epsilon_module_to_handle, module, module_handle);
    symbol_to_pointer_for_module = epsilon_make_string_hash();
    epsilon_add_to_string_hash(epsilon_module_to_symbol_to_pointer,
                               module,
                               symbol_to_pointer_for_module);
  }
  else // we have already loaded the module
    symbol_to_pointer_for_module =
      epsilon_lookup_string_hash(epsilon_module_to_symbol_to_pointer, module);
  
  /* Now we have a handle, and the correct symbol->pointer hash: lookup the symbol there,
     or add it if we don't have it yet: */
  epsilon_word result = epsilon_lookup_string_hash(symbol_to_pointer_for_module, symbol);
  if(result == NULL){
    result = dlsym(module_handle, symbol);
    EPSILON_IF_UNLIKELY(result == NULL)
      epsilon_fatal("epsilon_lookup_c_global_from(): found the module %s but not its symbol %s", module, symbol);
    epsilon_add_to_string_hash(symbol_to_pointer_for_module, symbol, result);
  } // if
  return result;
}

epsilon_word epsilon_lookup_c_global(char *module_dollar_symbol){
  /* Separate the string into module and symbol: */
  char *pointer_to_dollar = strchr(module_dollar_symbol, '$');
  const int dollar_index = pointer_to_dollar - module_dollar_symbol;
  EPSILON_CHECK_ON_DEBUG(pointer_to_dollar != NULL);
  char *module = alloca(dollar_index);
  strncpy(module, module_dollar_symbol, dollar_index);
  module[dollar_index] = '\0';
  char *symbol = pointer_to_dollar + 1; // the trailing string is shared
  
  /* Particular case: an empty module should be passed as a NULL pointer,
     meaning the current executable: */
  if(module[0] == '\0')
    module = NULL;
  
  /* Ok, now we have the two components: */
  //printf("epsilon_lookup_c_global: module:>%s< symbol:>%s<\n", module, symbol);
  return epsilon_lookup_c_global_from(symbol, module);
}
