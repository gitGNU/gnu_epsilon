/* Utility test program.

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


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include "utility.h"
//#include "../runtime/interface-with-c.h"

void test_dynamic_array(void){
  epsilon_dynamic_array_t array = epsilon_make_dynamic_array();
  EPSILON_PUSH_ONTO_DYNAMIC_ARRAY(array, 111);
  EPSILON_PUSH_ONTO_DYNAMIC_ARRAY(array, 222L);
  EPSILON_PUSH_ONTO_DYNAMIC_ARRAY(array, (long long)333);
  epsilon_reserve_from_dynamic_array(array, 13);
  epsilon_pop_from_dynamic_array(array, 13);
  printf("%i\n", ((int*)epsilon_dynamic_array_to_array(array))[0]);
  printf("%i\n", (int)((long*)((char*)epsilon_dynamic_array_to_array(array) + sizeof(int)))[0]);
  printf("%i\n", (int)((long long*)((char*)epsilon_dynamic_array_to_array(array) + sizeof(int) + sizeof(long)))[0]);
  //  EPSILON_PUSH_ONTO_DYNAMIC_ARRAY(array, y);
  printf("allocated size: %i\n", (int)epsilon_dynamic_array_allocated_size(array));
  printf("used size: %i\n", (int)epsilon_dynamic_array_used_size(array));
  epsilon_destroy_dynamic_array(array);
}

void test_unboxed_hash(void){
  //while(1){
    epsilon_unboxed_hash_t h = epsilon_make_unboxed_hash();
    epsilon_word x = (epsilon_word)(epsilon_int)3455;
    epsilon_word y = (epsilon_word)(epsilon_int)647458;
    epsilon_add_to_unboxed_hash(h, (epsilon_word)(epsilon_int)3454, x);
    epsilon_add_to_unboxed_hash(h, (epsilon_word)(epsilon_int)647457, y);
    printf("%i\n", (int)(epsilon_int)epsilon_lookup_unboxed_hash(h, (epsilon_word)(epsilon_int)3454));
    printf("%i\n", (int)(epsilon_int)epsilon_lookup_unboxed_hash(h, (epsilon_word)(epsilon_int)647457));
    printf("%i\n", (int)(epsilon_int)epsilon_lookup_unboxed_hash(h, (epsilon_word)(epsilon_int)600));
    epsilon_remove_from_unboxed_hash(h, (epsilon_word)(epsilon_int)3454);
    epsilon_remove_from_unboxed_hash(h, (epsilon_word)(epsilon_int)647457);
    epsilon_destroy_unboxed_hash(h);
  //}
}

void test_string_hash(void){
  //while(1){
    epsilon_string_hash_t h = epsilon_make_string_hash();
    epsilon_word x = (epsilon_word)(epsilon_int)1;
    epsilon_word y = (epsilon_word)(epsilon_int)42;
    epsilon_add_to_string_hash(h, "one", x);
    epsilon_add_to_string_hash(h, "fourty-two", y);
    printf("%i\n", (int)(epsilon_int)epsilon_lookup_string_hash(h, "one"));
    printf("%i\n", (int)(epsilon_int)epsilon_lookup_string_hash(h, "unbound"));
    printf("%i\n", (int)(epsilon_int)epsilon_lookup_string_hash(h, "fourty-two"));
    epsilon_remove_from_string_hash(h, "one");
    epsilon_remove_from_string_hash(h, "fourty-two");
    epsilon_destroy_string_hash(h);
  //}
}

/* void test_interface_with_c(void){ */
/*   void *mysinf = epsilon_lookup_c_global("libm.so$sinf"); */
/*   void *mycosf1 = epsilon_lookup_c_global("libm.so$cosf"); */
/*   void *mycosf2 = epsilon_lookup_c_global("libm.so$cosf"); */
/*   void *myacosf = epsilon_lookup_c_global("libm.so$acosf"); */
/*   printf("sinf(0) = %f\n", */
/*          EPSILON_CALL_C1(f, f, mysinf, 0.0f)); */
/*   printf("cosf(0) = %f (1)\n", */
/*          EPSILON_CALL_C1(f, f, mycosf1, 0.0f)); */
/*   printf("cosf(0) = %f (2)\n", */
/*          EPSILON_CALL_C1(f, f, mycosf2, 0.0f)); */
/*   printf("sinf(pi/2) = %f\n", */
/*          EPSILON_CALL_C1(f, f, mysinf, M_PI/2)); */
/*   printf("sinf(pi) = %f\n", */
/*          EPSILON_CALL_C1(f, f, mysinf, M_PI)); */
/*   printf("cosf(pi) = %f (1)\n", */
/*          EPSILON_CALL_C1(f, f, mycosf1, M_PI)); */
/*   printf("cosf(pi) = %f (2)\n", */
/*          EPSILON_CALL_C1(f, f, mycosf2, M_PI)); */
/*   printf("acosf(-1) = %f\n", */
/*          EPSILON_CALL_C1(f, f, myacosf, -1.0f)); */
/*   printf("acosf(0) = %f\n", */
/*          EPSILON_CALL_C1(f, f, myacosf, 0.0f)); */
/*   printf("acosf(1) = %f\n", */
/*          EPSILON_CALL_C1(f, f, myacosf, 1.0f)); */

/*   void *mysleep = epsilon_lookup_c_global("$sleep"); */
/*   const int delay = 2; */
/*   printf("sleeping for %i seconds...\n", delay); */
/*   EPSILON_CALL_C1(i, i, mysleep, delay); */
/*   printf("Done.\n"); */
/*   //EPSILON_CALL_C1(i, i, epsilon_lookup_c_global("$exit"), 42); */
/* } */

int main(void){
  test_dynamic_array();
  printf("------------\n");
  test_unboxed_hash();
  printf("------------\n");
  test_string_hash();
  printf("------------\n");
  //test_interface_with_c();
  //printf("------------\n");
  return 0;
}
