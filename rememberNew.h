#ifndef REMEMBERNEW_H
#define REMEMBERNEW_H

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


void rememberCycle(uint32_t v0, uint32_t v1[4294967295], uint32_t * out);

#endif // REMEMBERNEW_H
