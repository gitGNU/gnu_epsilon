/* Written by Luca Saiu in 2013
   I hereby release this file into the public domain, up to the extent
   permitted by law. */
#include "../runtime/runtime.h"

typedef void (*function)(int);

extern function *functions;
function myfunctions[] = {exit, exit, exit};

extern long another_global;
long global = 57;
void set_global_to_sum(int a, int b){
  global = a + b;
}

long* return_pointer_to_global(void){
  return &another_global;
}

void set_global(int a){
  global = a;
}

long get_global(void){
  return global;
}

long* get_pointer_to_global(void){
  return &global;
}

char* get_pointer_to_global_plus_offset(int offset_in_bytes){
  return ((char*)&global) + offset_in_bytes;
}

int lookup_pointer_to_global_plus_offset(int offset_in_elements){
  return *((&global) + offset_in_elements);
}

int lookup_pointer_to_another_global_plus_offset(int offset_in_elements){
  return *((&another_global) + offset_in_elements);
}

void call_function(int function_no, int parameter){
  (functions[function_no])(parameter);
}

void g(int a, int b) __attribute__((noinline));
void g(int a, int b){
  global = a + b;
}

void f(epsilon_value *stack){
  //  epsilon_call_c_primitive_by_index(13, stack + 4);
  asm("### before the call");
  g(42, 57);
  asm("### after the call");
  stack[0] = NULL;
}

int fact(int n){
  if(n == 0)
    return 1;
  else
    return n * fact(n - 1);
}

int fibo(int n){
  if(n < 2)
    return n;
  else
    return fibo(n - 2) + fibo(n - 1);
}

void* apply(void*(*function)(void*), void *parameter){
  return function(parameter);
}

void say_hello(void){
  puts("Hello!\n");
}

void* apply_and_say_hello(void(*function)(void*), void *parameter){
  function(parameter);
  say_hello();
}

struct mystruct{
  long a;
  long b[2];
  long *p;
  long c;
}; // struct

long somelong;

struct mystruct s = {100, 10000, 20000, &somelong, 200};

int main(void){
  return 0;
}
