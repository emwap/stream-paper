#include "rememberIx.h"


void rememberCycle(uint32_t v0, uint32_t v1[4294967295], uint32_t * out)
{
  uint32_t v6;
  
  v6 = 4294967295;
  for (uint32_t v3 = 0; v3 < v0; v3 += 1)
  {
    *out[v3] = v1[(v3 % v6)];
  }
}
