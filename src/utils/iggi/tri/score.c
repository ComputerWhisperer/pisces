/*static char []="$Header: score.c,v 1.3 86/05/23 17:36:20 conor Exp $";*/
/***********************************************************************
 *                                                                     *
 * score.c - Evaluate a triangulation.                                 *
 *                                                                     *
 * Copyright c 1985 The board of trustees of the Leland Stanford       *
 *                  Junior University. All rights reserved.            *
 * This subroutine may not be used outside of the SUPREM4 computer     *
 * program without the prior written consent of Stanford University.   *
 *                                                                     *
 * Original: CSR Nov.84                                                *
 *                                                                     *
 ***********************************************************************/
#include "general.h"
#include "dbase.h"
#include <math.h>
#include <stdio.h>

/*-----------------SCORE------------------------------------------------
 * A word about geometry.
 *----------------------------------------------------------------------*/
char * score(blurb)
    char *blurb;
{
    int t, nob, n[3], j;
    double ag, mg, ma, Ma, a, g, geom(), intang();

    if (!blurb) 	/* Just do the header line */
	printf("Nodes Triangles       Obtuse Quality MinAng MaxAng\n");
    
    else {		/* Check out the mesh */
	nob = 0;
	ag = Ma = 0;
	mg = ma = MAXFLOAT;
	FOR (t, 1, ntri) {
	    n[0] = tri[t]->n[0];
	    n[1] = tri[t]->n[1];
	    n[2] = tri[t]->n[2];
	    g = geom(n[0], n[1], n[2]);
	    if (g < mg) mg = g;
	    ag += g;
	    FOR (j, 0, 2) {
		a = intang (n[j], n[(j+1)%3], n[(j+2)%3]);
		if (a < ma) ma = a;
		if (a > Ma) Ma = a;
		if (a > PI/2+EPS) nob++;
		}
	    }
	ag /= ntri;
	printf("%5d %9d %4d(%5.1f%%) %7.5f %6.2f %6.2f %s\n",
	    nnode, ntri, nob, 100.0*nob/ntri, ag, ma*180/PI, Ma*180/PI, blurb);
	return(0);
    }
    return(0);
}
