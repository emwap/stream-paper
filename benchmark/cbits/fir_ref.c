#include "fir_ref.h"
#include <stdint.h>

////////////////////////////////////////////////////////////
// Parameters
////////////////////////////////////////////////////////////

void fir_ref(struct array * FirCoeff, struct array * Signal, struct array ** FilteredSignal)
{
    int N = getLength(FirCoeff);
    double Reg[N];

    // This loop body is the inlined FIR filter function from:
    // http://www.iowahills.com/A7ExampleCodePage.html
    int j, k, n, Top = 0;
    double y;
    int len = getLength(Signal);

    for(j=0; j<N; j++) Reg[j] = 0.0;

    for(j=0; j<len; j++)
    {
      Reg[Top] = at(double, Signal, j);
      y = 0.0;
      n = 0;

      // The FirCoeff index increases while the Reg index decreases.
      for(k=Top; k>=0; k--)
      {
        y += at(double,FirCoeff,n++) * Reg[k];
      }
      for(k=N-1; k>Top; k--)
      {
        y += at(double,FirCoeff,n++) * Reg[k];
      }
      at(double,*FilteredSignal,j) = y;

      Top++;
      if(Top >= N)Top = 0;
    }
}
