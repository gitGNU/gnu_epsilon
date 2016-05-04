/* Backend-specific runtime: C backend.

   Copyright (C) 2013 Luca Saiu
   Updated in 2016 by Luca Saiu

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


#include "utility/utility.h"
#include "runtime/runtime.h"
#include "config.h"

void
epsilon_run_thread_context (epsilon_thread_context_t thread_context,
                            void *compiled_procedure_address)
{
  void (*compiled_procedure_as_function_pointer) (epsilon_value*)
    = compiled_procedure_address;
  compiled_procedure_as_function_pointer (thread_context->stack);
}
