/* Textual dumping and binary marshalling and unmarshalling.

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


#ifndef __EPSILON_MARSHAL_H_
#define __EPSILON_MARSHAL_H_

#include <stdio.h>
#include <stdint.h>
#include "data.h"

/* Textual dumping: */
void epsilon_dump_value(epsilon_value value, FILE *file); // fails with an untagged backend
extern epsilon_int epsilon_dump_maximum_depth; // set it to -1 for unbounded depth

/* Binary marshalling and unmarshalling.  FIXME: add error checking. */
void epsilon_marshal_value(epsilon_value value, FILE *file); // fails with an untagged backend
epsilon_value epsilon_unmarshal_value(FILE *file); // ALSO WORKS with an untagged backend

/* Binary marshalling file formats are structured into 32-bit
   big-endian words.  This functionality may also be useful in other
   contexts, so we export it: */
void write_32bit_bigendian(FILE *f, int32_t value);
int32_t read_32bit_bigendian(FILE *f);

#endif // #ifndef __EPSILON_MARSHAL_H_
