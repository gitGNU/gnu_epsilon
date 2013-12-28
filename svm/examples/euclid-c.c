#include <stdio.h>

unsigned euclid(unsigned a, unsigned b){
  if(a == b)
    return a;
  else if(a > b)
    return euclid(a - b, b);
  else
    return euclid(a, b - a);
}

int main(void){
  printf("%u\n", euclid(3333333333u, 12u));
  return 0;
}
