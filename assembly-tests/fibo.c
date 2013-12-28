/* Written by Luca Saiu in 2013
   I hereby release this file into the public domain, up to the extent
   permitted by the law. */

#include <stdio.h>

long fibo(long n) __attribute__((noinline));
long fibo(long n){
  if((n == 0) || (n == 1))
    return n;
  else
    return fibo(n - 2) + fibo(n - 1);
}


int main(void){
  printf("%i\n", (int)fibo(40));
  return 0;
}
