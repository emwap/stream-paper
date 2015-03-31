#ifndef REMEMBERIX_H
#define REMEMBERIX_H

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


void rememberCycle(uint32_t v0, struct array * v1, struct array * * out);

#endif // REMEMBERIX_H
