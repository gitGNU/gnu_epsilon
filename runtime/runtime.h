/* The main runtime header #include'ing the others.

   Copyright (C) 2012 Université Paris 13
   Copyright (C) 2012 Luca Saiu [written during his few weeks with no employment]
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


#ifndef EPSILON_RUNTIME_H_
#define EPSILON_RUNTIME_H_

/* #include all runtime headers: */
#include "interface-with-c.h"
#include "c-wrapper.h"
#include "backend-specific.h" // the interface is always the same
#include "data.h"
#include "marshal.h"
#include "c-primitives.h"
#include "epsilon0-interpreter.h"
#include "thread-context.h"
#include "jit.h"


/* Initialize all runtime subsystems: */
void epsilon_runtime_initialize(void);

typedef void (*epsilon_compiled_c_function)(epsilon_value*);

#endif // #ifndef EPSILON_RUNTIME_H_
