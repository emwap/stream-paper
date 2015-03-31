#include "movingAverageOld.h"


void movingAvg(double v0[4294967295], double * out)
{
  struct s_3_unsignedS32_s_2_narr_unsignedS32_narr_double_s_2_narr_unsignedS32_narr_double v66 = { .member1 = 0, .member2 = { .member1, .member2 }, .member3 = { .member1, .member2 } };
  struct s_3_unsignedS32_s_2_narr_unsignedS32_narr_double_s_2_narr_unsignedS32_narr_double v68 = { .member1 = 0, .member2 = { .member1, .member2 }, .member3 = { .member1, .member2 } };
  uint32_t v189;
  double v190;
  uint32_t v191;
  double v179;
  uint32_t v174;
  double v178[4294967295];
  uint32_t v177;
  double e192;
  uint32_t v181;
  uint32_t v184;
  uint32_t v186;
  uint32_t v188;
  
  (v66).member1 = 0;
  ((v66).member2).member1[0] = 8;
  ((v66).member2).member2[0] = 0.0;
  ((v66).member2).member2[1] = 0.0;
  ((v66).member2).member2[2] = 0.0;
  ((v66).member2).member2[3] = 0.0;
  ((v66).member2).member2[4] = 0.0;
  ((v66).member2).member2[5] = 0.0;
  ((v66).member2).member2[6] = 0.0;
  ((v66).member2).member2[7] = 0.0;
  ((v66).member3).member1[0] = 0;
  for (uint32_t v67 = 0; v67 < 32; v67 += 1)
  {
    (v68).member1 = (v66).member1;
    ((v68).member2).member1 = ((v66).member2).member1;
    ((v68).member2).member2 = ((v66).member2).member2;
    ((v68).member3).member1 = ((v66).member3).member1;
    ((v68).member3).member2 = ((v66).member3).member2;
    v189 = ((v68).member2).member1[0];
    v190 = v0[(v68).member1];
    v191 = ((v68).member3).member1[0];
    v174 = (min((v189 - 1), v189) + 1);
    v177 = min((v189 - 1), v189);
    v178[0] = v190;
    for (uint32_t v77 = 0; v77 < v177; v77 += 1)
    {
      v178[(v77 + 1)] = ((v68).member2).member2[v77];
    }
    e192 = 0.0;
    for (uint32_t v71 = 0; v71 < v174; v71 += 1)
    {
      e192 = (e192 + v178[v71]);
    }
    v179 = (e192 / 8.0);
    v181 = (min((v189 - 1), v189) + 1);
    v184 = min((v189 - 1), v189);
    v186 = (min((v191 - 1), v191) + 1);
    v188 = min((v191 - 1), v191);
    *out[v67] = v179;
    (v66).member1 = ((v68).member1 + 1);
    ((v66).member2).member1[0] = v181;
    ((v66).member2).member2[0] = v190;
    for (uint32_t v89 = 0; v89 < v184; v89 += 1)
    {
      ((v66).member2).member2[(v89 + 1)] = ((v68).member2).member2[v89];
    }
    ((v66).member3).member1[0] = v186;
    ((v66).member3).member2[0] = v179;
    for (uint32_t v111 = 0; v111 < v188; v111 += 1)
    {
      ((v66).member3).member2[(v111 + 1)] = ((v68).member3).member2[v111];
    }
  }
}
