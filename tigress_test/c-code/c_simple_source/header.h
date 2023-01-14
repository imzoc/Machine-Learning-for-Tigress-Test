#include "tigress.h"

#define STR_EXPAND(tok) #tok
#define STR(tok) STR_EXPAND(tok)

#include<time.h>
#include<stdio.h>
#include<stdlib.h>
#include<limits.h>
#include<math.h>
#include<pthread.h>

#ifdef JIT
#ifdef __x86_64__
# include "jitter-amd64.c"
#elif __ARM_ARCH_6__
# include "jitter-arm32.c"
#endif
#endif

