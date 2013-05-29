static char rcsid[]="$Header: /users/suprem/ig2/tri/RCS/gn_asm.c,v 1.2 85/10/18 13:43:32 conor Exp $";
/***********************************************************************
 *                                                                     *
 * assjtj.c - Assembly routines for Gauss-Newton optimization.         *
 *                                                                     *
 * Copyright c 1985 The board of trustees of the Leland Stanford       *
 *                  Junior University. All rights reserved.            *
 * This subroutine may not be used outside of the SUPREM4 computer     *
 * program without the prior written consent of Stanford University.   *
 *                                                                     *
 * Original: CSR Nov.84 (Levenberg-Marquardt optimization)             *
 * Modified: CSR Dec.84 (Added Newton derivatives)                     *
 * Modified: CSR Jan.85 (Simplified back to Gauss-Newton)              *
 *                                                                     *
 ***********************************************************************/
#include "general.h"
#include "thyme.h"		/* Timing functions. */
#include "dbase.h"         	/* For the triangle connectivity. */
#include <math.h>		/* For the sqrt in the 2-norm. */
#include <stdio.h>

extern int ntfunc;		/* Number of functions per triangle */

/*-----------------ASSJTJ-----------------------------------------------
 *            A      = JtJ
 *            a[j,k] = J[l,j]*J[l,k] (summation convention)
 *----------------------------------------------------------------------*/
char * assjtj (ia, ja, a, func, jacobi)
    int ia[];		/* Matrix map. */
    short ja[];
    double a[];		/* Matrix. */
    double func[];      /* Function values */
    double jacobi[];	/* Jacobian. */
{
    int it, ie, nj,nk, vj,vk, row,col, asiz, ax, in, jbase;
    int r2, c2, amap();
    int knode, jnode;
    double elap = log_cpu (0.0, "");


    /*...Clear the matrix. */
    asiz = ia[2*nnode]-1;
    for (ax = 1; ax <= asiz; ax++) a[ax] = 0.0;

    /*...Assemble JtJ by element. */
    /*...Storage scheme : 
     *   (1,1,1) (1,1,2) ... where the first variable is which triangle,
     *   the second is which function for that triangle and the third is
     *   1-6 for x1,y1,x2,y2,x3,y3 
     *...Ugly but functional.
     */
    jbase = 0;
    FOR (it, 1, ntri) {
	FOR (nj, 0, 2) {
	    jnode = tri[it]->n[nj];
	    if (ELEC(jnode)) continue;
	    FOR (vj, 1, 2) {
		FOR (nk, 0, 2) {
		    knode = tri[it]->n[nk];
		    if (ELEC(knode)) continue;
		    FOR (vk, 1, 2) {
			row = vj + 2*(jnode-1);
			col = vk + 2*(knode-1);
			ax = amap(ia, ja, row, col);
			if (ax < 0) return("Bad index in assjtj");
			r2 = 2*nj + vj;
			c2 = 2*nk + vk;
			FOR (ie, 0, ntfunc-1) {
			    a[ax] += jacobi[jbase+r2] * jacobi[jbase+c2];
			    r2 += 6;
			    c2 += 6;
			    }
			}
		    }
		}
	    }
	jbase += 6*ntfunc;
	}

    /*...Put diagonal 1's in for boundary conditions. */
    for (in = 1; in <= 2*nnode; in++) {
	ax = amap (ia, ja, in, in);
	if (ELEC((in-1)/2+1)) a[ax] = 1.0;
    }
    
    log_cpu (elap, "JtJ assembly");
    return(0);
}

/*-----------------ASSB-------------------------------------------------
 * Assemble the G-N right hand side 
 *	b = -Jtf
 *	b[i] = -J[l,i]f[l]
 *----------------------------------------------------------------------*/
char * assb (jacobi, func, rhs)
    double jacobi[];	/* Jacobian at current location. */
    double func[];	/* Function at current location. */
    double rhs[];	/* Result. */
{    
    int row, it, in, j, iv, frow, jrow, ie;
    double accum, elap = log_cpu (0.0, "");

    /*...Clear rhs. */
    FOR (row, 1, 2*nnode) rhs[row] = 0;

    /*...Accumulate element contributions. */
    FOR (it, 1, ntri) {
	FOR (ie, 0, ntfunc-1) {
	    FOR (j, 0, 2) {
		in = tri[it]->n[j];
		if (!ELEC(in)) {
		    FOR (iv, 1, 2) {
			row  = 2*(in - 1) + iv;
			frow = ntfunc*(it - 1) + ie;
			jrow = 6*frow + 2*j + iv;
			rhs[row] -= jacobi[jrow] * func[frow+1];
			}
		    }
		}
	    }
	}
    if (debug2) {
	for (accum = 0, row = 1; row <= 2*nnode; row++) 
	    accum += rhs[row]*rhs[row];
	accum = sqrt(accum);
	printf("N2= %g	",accum);
	}

    log_cpu (elap, "Right hand side assembly");
    return(0);
}
