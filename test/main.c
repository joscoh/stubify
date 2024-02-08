#include "b.h"
#include <stdio.h>

int main() {
  //Create vector v=(3, 4) and calculate ||v||^2
  Aa *vec = createA(3, 4);
  int norm = normA(vec);
  printf("Norm is %d\n", norm);
}