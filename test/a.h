#ifndef A_H
#define A_H

typedef struct _p_A Aa;

Aa* createA(int x, int y);

int composeA(Aa *a, Aa *b);

//function we don't need: a = a + b
void sumA(Aa *a, Aa *b);

#endif