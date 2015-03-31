#ifndef MOVINGAVERAGEOLD_H
#define MOVINGAVERAGEOLD_H

#include "feldspar_c99.h"
#include "feldspar_array.h"
#include "feldspar_future.h"
#include "ivar.h"
#include "taskpool.h"
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>
#include <complex.h>


struct s_3_unsignedS32_arr_double_arr_double
{
  uint32_t member1;
  struct array * member2;
  struct array * member3;
};

void movingAvg(struct array * v0, struct array * * out);

#endif // MOVINGAVERAGEOLD_H
