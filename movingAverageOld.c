#include "movingAverageOld.h"


void movingAvg(struct array * v0, struct array * * out)
{
  struct s_3_unsignedS32_arr_double_arr_double v46 = { .member1 = 0, .member2 = NULL, .member3 = NULL };
  struct s_3_unsignedS32_arr_double_arr_double v48 = { .member1 = 0, .member2 = NULL, .member3 = NULL };
  uint32_t v137;
  double v138;
  double v131;
  uint32_t v126;
  struct array * v130 = NULL;
  uint32_t v129;
  double e139;
  uint32_t v133;
  uint32_t v136;
  uint32_t v135;
  
  *out = setLength(*out, sizeof(double), 32);
  (v46).member1 = 0;
  (v46).member2 = initArray((v46).member2, sizeof(double), 8);
  at(double,(v46).member2,0) = 0.0;
  at(double,(v46).member2,1) = 0.0;
  at(double,(v46).member2,2) = 0.0;
  at(double,(v46).member2,3) = 0.0;
  at(double,(v46).member2,4) = 0.0;
  at(double,(v46).member2,5) = 0.0;
  at(double,(v46).member2,6) = 0.0;
  at(double,(v46).member2,7) = 0.0;
  (v46).member3 = initArray((v46).member3, sizeof(double), 1);
  at(double,(v46).member3,0) = 0.0;
  for (uint32_t v47 = 0; v47 < 32; v47 += 1)
  {
    (v48).member1 = (v46).member1;
    (v48).member2 = initArray((v48).member2, sizeof(double), getLength((v46).member2));
    copyArray((v48).member2, (v46).member2);
    (v48).member3 = initArray((v48).member3, sizeof(double), getLength((v46).member3));
    copyArray((v48).member3, (v46).member3);
    v137 = getLength((v48).member2);
    v138 = at(double,v0,(v48).member1);
    v126 = (min((v137 - 1), v137) + 1);
    v129 = min((v137 - 1), v137);
    v130 = setLength(v130, sizeof(double), v126);
    at(double,v130,0) = v138;
    for (uint32_t v55 = 0; v55 < v129; v55 += 1)
    {
      at(double,v130,(v55 + 1)) = at(double,(v48).member2,v55);
    }
    e139 = 0.0;
    for (uint32_t v49 = 0; v49 < v126; v49 += 1)
    {
      e139 = (e139 + at(double,v130,v49));
    }
    v131 = (e139 / 8.0);
    v133 = min((v137 - 1), v137);
    v135 = getLength((v48).member3);
    v136 = min((v135 - 1), v135);
    at(double,*out,v47) = v131;
    (v46).member1 = ((v48).member1 + 1);
    (v46).member2 = setLength((v46).member2, sizeof(double), (v133 + 1));
    at(double,(v46).member2,0) = v138;
    for (uint32_t v63 = 0; v63 < v133; v63 += 1)
    {
      at(double,(v46).member2,(v63 + 1)) = at(double,(v48).member2,v63);
    }
    (v46).member3 = setLength((v46).member3, sizeof(double), (v136 + 1));
    at(double,(v46).member3,0) = v131;
    for (uint32_t v79 = 0; v79 < v136; v79 += 1)
    {
      at(double,(v46).member3,(v79 + 1)) = at(double,(v48).member3,v79);
    }
  }
}
