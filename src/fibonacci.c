#include <stdio.h>

int fibonacci(int n) {
   int a = 0;
   int b = 1;
   int t = 0;
   for (int i = 0; i < n; i++) {
      t = b;
      b += a;
      a = t;
   }
   return a;
}

int main(void) {
   printf("The 10th fibonacci number is %d \n", fibonacci(10));
}