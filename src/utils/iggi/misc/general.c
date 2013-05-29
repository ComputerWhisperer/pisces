static char rcsid[]="$Header: /users/suprem/ig2/misc/RCS/general.c,v 1.1 85/05/17 14:32:26 conor Exp $";

/*----------------------------------------------------------------------
 *
 * general.c - general routines, used in most C programs.
 *
 * Copyright c 1985 The board of trustees of the Leland Stanford
 *                  Junior University. All rights reserved.
 * This subroutine may not be used outside of the SUPREM4 computer
 * program without the prior written consent of Stanford University.
 *
 * Original: CSR Nov 84
 *---------------------------------------------------------------------*/
#include "general.h"

double dmax (x,y)
    double x,y;
{
    if (x >= y) return(x);
    else        return(y);
}
double dmin (x,y)
    double x,y;
{
    if (x <= y) return(x);
    else        return(y);
}
double dabs (x)
    double x;
{
    if (x >= 0) return(x);
    else        return(-x);
}




int imax (x,y)
    int x,y;
{
    if (x >= y) return(x);
    else        return(y);
}
int imin (x,y)
    int x,y;
{
    if (x <= y) return(x);
    else        return(y);
}
int iabs (x)
    int x;
{
    if (x >= 0) return(x);
    else        return(-x);
}

int iflor (x)
    double x;
{
    if (x >= 0) return ((int) x);
    else        return ((int) (x-(1-PREC)));
}

int iceil (x)
    double x;
{
    if (x >= 0) return ((int) (x+(1-PREC)));
    else        return ((int) x);
}

#if 0
int round (x)
    double x;
{
    if (x > 0) return ((int) (x+0.5));
    else       return ((int) (x-0.5));
}
#endif

/* String equality. */
strequ(s, t)
   char *s,*t;
{
   return(strcmp(s,t) == 0);
}

/* Mod function, because the % operator was botched. */
int mod(i,n)
    int i,n;
{
    if (i>=0) return(i%n);
    else return(n-1 - (-1-i)%n);
}
