#include "fir_ref.h"
#include <stdint.h>

////////////////////////////////////////////////////////////
// Parameters
////////////////////////////////////////////////////////////

#define NUMTAPS 4



void fir_ref(uint32_t N, struct array * Signal, struct array ** FilteredSignal)
{
    double const FirCoeff[NUMTAPS] = { 1.0, 0.5, 0.25, 0.125 };

    double Reg[NUMTAPS];

    // This loop body is the inlined FIR filter function from:
    // http://www.iowahills.com/A7ExampleCodePage.html
    int j, k, n, Top = 0;
    double y;

    for(j=0; j<NUMTAPS; j++) Reg[j] = 0.0;

    for(j=0; j<N; j++)
    {
      Reg[Top] = at(double, Signal, j);
      y = 0.0;
      n = 0;

      // The FirCoeff index increases while the Reg index decreases.
      for(k=Top; k>=0; k--)
      {
        y += FirCoeff[n++] * Reg[k];
      }
      for(k=NUMTAPS-1; k>Top; k--)
      {
        y += FirCoeff[n++] * Reg[k];
      }
      at(double,*FilteredSignal,j) = y;

      Top++;
      if(Top >= NUMTAPS)Top = 0;
    }
}
