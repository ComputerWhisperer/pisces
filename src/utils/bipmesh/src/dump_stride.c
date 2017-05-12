#include <stdio.h>
#include "struct.h"

#ifdef ANSI_FUNC

int 
dump_stride (stride_dopstr *ptr)
#else

dump_stride(ptr)
stride_dopstr *ptr;
#endif
{
    printf("\tpeak doping is %g\n", ptr->peak_dop);
    printf("\txdev is %g\n", ptr->xdev);
    printf("\tydev is %g\n", ptr->ydev);
    printf("\tzdev is %g\n", ptr->zdev);
    printf("\txlo is %g\n", ptr->xlo);
    printf("\txhi is %g\n", ptr->xhi);
    printf("\tylo is %g\n", ptr->ylo);
    printf("\tyhi is %g\n", ptr->yhi);
    printf("\tzlo is %g\n", ptr->zlo);
    printf("\tzhi is %g\n", ptr->zhi);
    printf("\tierfcx is %d\n", ptr->ierfcx);
    printf("\tierfcy is %d\n", ptr->ierfcy);
    printf("\tierfcz is %d\n", ptr->ierfcz);
    printf("\tnum_planes is %d\n", ptr->num_planes);
}
