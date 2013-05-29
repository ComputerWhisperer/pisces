/*----------------------------------------------------------------------
**  Copyright 1988 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/



#include <math.h>


calc_junc( peak_loc, peak_dop, std_dev, substrate_dop, junction )
double peak_loc;
double peak_dop;
double std_dev;
double substrate_dop;
double *junction;
{
    if ( peak_dop != 0.0 )  
        *junction = std_dev * sqrt( log(peak_dop/ substrate_dop) )
		+ peak_loc;
    else
       *junction = 0.0;
}
