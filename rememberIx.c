#include "rememberIx.h"


void rememberCycle(uint32_t v0, struct array * v1, struct array * * out)
{
  uint32_t v6;
  
  v6 = getLength(v1);
  *out = setLength(*out, sizeof(uint32_t), v0);
  for (uint32_t v3 = 0; v3 < v0; v3 += 1)
  {
    at(uint32_t,*out,v3) = at(uint32_t,v1,(v3 % v6));
  }
}
