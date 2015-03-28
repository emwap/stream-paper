#include "movingAverageOld.h"


void movingAvg(struct array * v0, struct array * * out)
{
  struct s_3_unsignedS32_s_2_arr_unsignedS32_arr_double_s_2_arr_unsignedS32_arr_double v66 = { .member1 = 0, .member2 = { .member1 = NULL, .member2 = NULL }, .member3 = { .member1 = NULL, .member2 = NULL } };
  struct s_3_unsignedS32_s_2_arr_unsignedS32_arr_double_s_2_arr_unsignedS32_arr_double v68 = { .member1 = 0, .member2 = { .member1 = NULL, .member2 = NULL }, .member3 = { .member1 = NULL, .member2 = NULL } };
  uint32_t v189;
  double v190;
  uint32_t v191;
  double v179;
  uint32_t v174;
  struct array * v178 = NULL;
  uint32_t v177;
  double e192;
  uint32_t v181;
  uint32_t v184;
  uint32_t v186;
  uint32_t v188;
  
  *out = setLength(*out, sizeof(double), 32);
  (v66).member1 = 0;
  ((v66).member2).member1 = initArray(((v66).member2).member1, sizeof(uint32_t), 1);
  at(uint32_t,((v66).member2).member1,0) = 8;
  ((v66).member2).member2 = initArray(((v66).member2).member2, sizeof(double), 8);
  at(double,((v66).member2).member2,0) = 0.0;
  at(double,((v66).member2).member2,1) = 0.0;
  at(double,((v66).member2).member2,2) = 0.0;
  at(double,((v66).member2).member2,3) = 0.0;
  at(double,((v66).member2).member2,4) = 0.0;
  at(double,((v66).member2).member2,5) = 0.0;
  at(double,((v66).member2).member2,6) = 0.0;
  at(double,((v66).member2).member2,7) = 0.0;
  ((v66).member3).member1 = initArray(((v66).member3).member1, sizeof(uint32_t), 1);
  at(uint32_t,((v66).member3).member1,0) = 1;
  ((v66).member3).member2 = initArray(((v66).member3).member2, sizeof(double), 1);
  at(double,((v66).member3).member2,0) = 0.0;
  for (uint32_t v67 = 0; v67 < 32; v67 += 1)
  {
    (v68).member1 = (v66).member1;
    ((v68).member2).member1 = initArray(((v68).member2).member1, sizeof(uint32_t), getLength(((v66).member2).member1));
    copyArray(((v68).member2).member1, ((v66).member2).member1);
    ((v68).member2).member2 = initArray(((v68).member2).member2, sizeof(double), getLength(((v66).member2).member2));
    copyArray(((v68).member2).member2, ((v66).member2).member2);
    ((v68).member3).member1 = initArray(((v68).member3).member1, sizeof(uint32_t), getLength(((v66).member3).member1));
    copyArray(((v68).member3).member1, ((v66).member3).member1);
    ((v68).member3).member2 = initArray(((v68).member3).member2, sizeof(double), getLength(((v66).member3).member2));
    copyArray(((v68).member3).member2, ((v66).member3).member2);
    v189 = at(uint32_t,((v68).member2).member1,0);
    v190 = at(double,v0,(v68).member1);
    v191 = at(uint32_t,((v68).member3).member1,0);
    v174 = (min((v189 - 1), v189) + 1);
    v177 = min((v189 - 1), v189);
    v178 = setLength(v178, sizeof(double), v174);
    at(double,v178,0) = v190;
    for (uint32_t v77 = 0; v77 < v177; v77 += 1)
    {
      at(double,v178,(v77 + 1)) = at(double,((v68).member2).member2,v77);
    }
    e192 = 0.0;
    for (uint32_t v71 = 0; v71 < v174; v71 += 1)
    {
      e192 = (e192 + at(double,v178,v71));
    }
    v179 = (e192 / 8.0);
    v181 = (min((v189 - 1), v189) + 1);
    v184 = min((v189 - 1), v189);
    v186 = (min((v191 - 1), v191) + 1);
    v188 = min((v191 - 1), v191);
    at(double,*out,v67) = v179;
    (v66).member1 = ((v68).member1 + 1);
    ((v66).member2).member1 = setLength(((v66).member2).member1, sizeof(uint32_t), 1);
    at(uint32_t,((v66).member2).member1,0) = v181;
    ((v66).member2).member2 = setLength(((v66).member2).member2, sizeof(double), v181);
    at(double,((v66).member2).member2,0) = v190;
    for (uint32_t v89 = 0; v89 < v184; v89 += 1)
    {
      at(double,((v66).member2).member2,(v89 + 1)) = at(double,((v68).member2).member2,v89);
    }
    ((v66).member3).member1 = setLength(((v66).member3).member1, sizeof(uint32_t), 1);
    at(uint32_t,((v66).member3).member1,0) = v186;
    ((v66).member3).member2 = setLength(((v66).member3).member2, sizeof(double), v186);
    at(double,((v66).member3).member2,0) = v179;
    for (uint32_t v111 = 0; v111 < v188; v111 += 1)
    {
      at(double,((v66).member3).member2,(v111 + 1)) = at(double,((v68).member3).member2,v111);
    }
  }
}
