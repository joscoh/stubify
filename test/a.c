#include "a.h"
#include <stdlib.h>

//Implement as 2-D vectors and dot product

struct _p_A {
  int x;
  int y;
};

Aa* createA(int x, int y) {
  Aa *vec = malloc(sizeof(struct _p_A));
  vec->x = x;
  vec->y = y;
  return vec;
}

int composeA(Aa *a, Aa *b) {
  return (a->x * b->x) + (a->y * b->y);
}

void sumA(Aa *a, Aa *b) {
  //Comment to remove
  a->x += b->x;
  a->y += b->y;
}

