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


static __thread unsigned int epsilongc_previous_number;

inline static void epsilongc_update_seed(void){
  const unsigned int p1 = 16807;
  const unsigned int p2 = 0;
  epsilongc_previous_number = (p1 * epsilongc_previous_number + p2) % 2147483647u;
}

void epsilongc_seed(void){
  epsilongc_previous_number = 79;
  const int q = rand() % 100;
  int i;
  for(i = 0; i < q; i++)
    epsilongc_update_seed();
}

int epsilongc_rand(void){
  epsilongc_update_seed();
  //printf("%u\n", epsilongc_previous_number % 4u);
  return (int)epsilongc_previous_number;
}
