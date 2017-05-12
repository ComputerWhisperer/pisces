/*----------------------------------------------------------------------
**  Copyright 1988 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/*
 * doublenode.c
 *	compute info needed if spacings for both ends are specified
 */

#include <math.h>

extern double dmin();
extern double dmax();
extern double calcratio();
extern calcnode();

#ifdef ANSI_FUNC

int 
doublenode (double ratio, double init, double final, double distance, int *num1, double *x1, double *rat1, int *num2, double *x2, double *rat2, int *num3, double *x3, double *rat3)
#else

doublenode( ratio, init, final, distance, num1, x1, rat1, 
	num2, x2, rat2,  num3, x3, rat3 )
double ratio;
double init;
double final;
double distance;
int *num1;
double *x1;
double *rat1;
int *num2;
double *x2;
double *rat2;
int *num3;
double *x3;
double *rat3;
#endif
{
    double spacing1 = 0.0;
    double spacing2 = 0.0;
    double distance1 = 0.0;
    double distance2 = 0.0;
    int n1;
    int n2;
    int n3;

    if ( init > final )  {
    /*  in this case slowly grate back from back end until we get
     *    the same spacing as init
     */
        spacing1 = init;
        spacing2 = 0.0;
        n3 = 0;
        while ( spacing2 < spacing1 )  {
	    n3++; 
	    spacing2 = final * pow( ratio, (double)n3 );
	    distance2 = distance2 + spacing2;
        }
	/*  need to check case where we can't grade.  */
	if ( distance2 >= distance )  {
	    *rat1 = calcratio( init, final, distance, &n1 );
	    *rat2 = 0.0;
	    *rat3 = 0.0;
	    *x1 = distance;
	    *x2 = 0.0;
	    *x3 = 0.0;
	    n2 = 0.0;
	    n3 = 0.0;
   	}
	else  {
	    *rat1 = ratio;
	    *rat2 = 1.0/ratio;
	    *rat3 = 1.0/ratio;
	    *x1 = (distance - distance2)/2.0;
	    *x2 = (distance - distance2)/2.0;
	    *x3 = distance2;
            n1 = calcnode( (distance - distance2)/2.0, ratio, init, 0.0 );
            n2 = calcnode( (distance - distance2)/2.0, 1.0/ratio, 0.0, init );
	}
    }
    else if ( init < final )
    /*  in this case slowly from front until the spacing is 
     * the same as final 
     */
    {
        spacing2 = final;
        spacing1 = 0.0;
        n1 = 0;
        while ( spacing1 < spacing2 )  {
	    n1++;
	    spacing1 = init * pow( ratio, (double)n1 );
	    distance1 = distance1 + spacing1;
        }
	/*  need to check case where we can't grade.  */
	if ( distance1 >= distance )  {
	    *rat1 = calcratio( init, final, distance, &n1 );
	    *rat2 = 0.0;
	    *rat3 = 0.0;
	    *x1 = distance;
	    *x2 = 0.0;
	    *x3 = 0.0;
	    n2 = 0.0;
	    n3 = 0.0;
   	}
	else  {
	    *rat1 = ratio;
	    *rat2 = ratio;
	    *rat3 = 1.0/ratio;
	    *x1 = distance1;
	    *x2 = (distance - distance1)/2.0;
	    *x3 = (distance - distance1)/2.0;
            n2 = calcnode( (distance - distance1)/2.0, ratio, final, 0.0 );
            n3 = calcnode( (distance - distance1)/2.0, 1.0/ratio, 0.0, final );
	}
    }
    else  {  /* init = final */
	n1 = floor( distance/init + 0.5 );
	n2 = 0;
	n3 = 0;
        *rat1 = ratio;
        *rat2 = 0.0;
        *rat3 = 0.0;
        *x1 = distance;
        *x2 = 0.0;
        *x3 = 0.0;
    }
    *num1 = n1;
    *num2 = n2;
    *num3 = n3;
}
