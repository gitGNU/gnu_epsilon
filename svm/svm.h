/* SVM generic header.

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


#ifndef SLOW_VIRTUAL_MACHINE_H_
#define SLOW_VIRTUAL_MACHINE_H_

#include "../utility/utility.h"

typedef epsilon_word svm_word;
typedef epsilon_int svm_int;
typedef epsilon_float svm_float;
typedef epsilon_unsigned svm_unsigned;

typedef void* svm_label;

union epsilon_word_or_float{
  svm_word non_float_value;
  epsilon_float float_value;
}; // union
typedef union epsilon_word_or_float epsilon_word_or_float;

#endif // #ifndef SLOW_VIRTUAL_MACHINE_H_
