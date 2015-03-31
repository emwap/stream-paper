#include "rememberNew.h"


void rememberCycle(uint32_t v0, uint32_t v1[4294967295], uint32_t * out)
{
  uint32_t v9;
  uint32_t v3;
  uint32_t v5;
  
  v9 = 4294967295;
  v3 = 0;
  for (uint32_t v4 = 0; v4 < v0; v4 += 1)
  {
    v5 = v3;
    v3 = ((v5 + 1) % v9);
    *out[v4] = v1[v5];
  }
}
