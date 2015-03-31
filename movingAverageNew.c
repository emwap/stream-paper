#include "movingAverageNew.h"


double v47[] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};

void movingAvg(double v0[4294967295], double * out)
{
  uint32_t v5;
  double v7[4294967295];
  uint32_t v14;
  double v16[4294967295];
  uint32_t v23;
  uint32_t v25;
  double v48;
  uint32_t v27;
  uint32_t v31;
  uint32_t v49;
  double v38;
  double v50;
  double e51;
  double e52;
  double e53;
  uint32_t v39;
  
  v5 = 0;
  for (uint32_t v9 = 0; v9 < 8; v9 += 1)
  {
    v7[v9] = v47[v9];
  }
  v14 = 0;
  v16[0] = 0.0;
  v23 = 0;
  for (uint32_t v24 = 0; v24 < 32; v24 += 1)
  {
    v25 = v5;
    v48 = v0[v25];
    v5 = (v25 + 1);
    v27 = v14;
    v14 = ((v27 + 1) % 8);
    v7[v27] = v48;
    v31 = v14;
    v49 = (v31 + 8);
    e51 = 0.0;
    for (uint32_t v35 = 0; v35 < 8; v35 += 1)
    {
      e51 = (e51 + v7[(((v49 - v35) - 1) % 8)]);
    }
    v50 = (e51 / 8.0);
    e53 = v50;
    e52 = e53;
    v38 = e52;
    v39 = v23;
    v23 = 0;
    v16[v39] = v38;
    *out[v24] = v38;
  }
}
