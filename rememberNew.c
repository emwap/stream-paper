#include "rememberNew.h"


void rememberCycle(uint32_t v0, struct array * v1, struct array * * out)
{
  uint32_t v9;
  uint32_t v3;
  uint32_t v5;
  
  v9 = getLength(v1);
  *out = setLength(*out, sizeof(uint32_t), v0);
  v3 = 0;
  for (uint32_t v4 = 0; v4 < v0; v4 += 1)
  {
    v5 = v3;
    v3 = ((v5 + 1) % v9);
    at(uint32_t,*out,v4) = at(uint32_t,v1,v5);
  }
}
