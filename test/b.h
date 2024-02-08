#ifndef B_H
#define B_H

#include "a.h"

//Test macro in stubbed function
#define RETURN(x) return(x)

#define SILLY(x) \
do { \
	if(0) { \
		x; \
	} \
} while(0)

//A function we want
int normA(Aa *a);

//Another function (here, check for orthog) we will not use
int orthogA(Aa *a, Aa *b);

#endif