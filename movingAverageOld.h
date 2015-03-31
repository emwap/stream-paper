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


struct s_2_narr_unsignedS32_narr_double
{
  uint32_t member1[4294967295];
  double member2[4294967295];
};

struct s_3_unsignedS32_s_2_narr_unsignedS32_narr_double_s_2_narr_unsignedS32_narr_double
{
  uint32_t member1;
  struct s_2_narr_unsignedS32_narr_double member2;
  struct s_2_narr_unsignedS32_narr_double member3;
};

void movingAvg(double v0[4294967295], double * out);

#endif // MOVINGAVERAGEOLD_H
