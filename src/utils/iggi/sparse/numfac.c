static char rcsid[]="$Header: /users/suprem/ig2/sparse/RCS/numfac.c,v 1.1 85/05/17 14:42:22 conor Exp $";
#include "thyme.h"
#include <stdio.h>
char * numfac(nv, ia, ja, a, il, jl, l, iu, ju, u, di, x, ipri, ipc)
    int nv; 			/* Number of variables. */
    int ia[], il[], iu[];	/* Column start pointers to ja/l/u.*/
    short ja[], jl[], ju[];	/* Row numbers of a,l,u */
    double a[], l[], di[], u[];	/* Matrix and its LDU factorization. */
    double x[];			/* Scratch vector. */
    int ipri[],ipc[];		/* Inverse pivot rows, pivot cols. */

/* 
 * Sparse matrix factorization with general pivoting.
 * Uncompressed storage version.
 * This version patched to add to diagonal if singular.
 *
 * CSR Nov. 84.
 */
{

    int iv, ci, ri, alo, ahi, ulo, uhi, llo, lhi, rci, 
	lx, ux, ax;
    double aux, alph, elap = log_cpu (0.0, "");

    alph = 0;

RESTART:

    /*...Clear workspace ; only do it once. */
    /*...It is cleared on the fly during factorization */
    for (iv = 1; iv <= nv; iv++)
	x[iv] = 0;

    /*...Main loop. For each column of A...*/
    for (ci = 1; ci <= nv; ci++) {
	
	alo = ia[ipc[ci]-1];
	ahi = ia[ipc[ci]];

	/*...Copy column ci of a into the workspace, using pivoting. */
	for (ax = alo; ax < ahi; ax++) {
	    ri = ipri[ja[ax]];
	    x[ri] = a[ax];
	    if (ri==ci) x[ri] *= (1+alph);
            if (x[ri] /*"still"*/ == 0) x[ri] = alph;
        }


	/*...Compute upper triangle. */
	ulo = iu[ci-1];
	uhi = iu[ci];
	for (ux = ulo; ux < uhi; ux++) {
	    
	    rci = ju[ux];
	    aux = u[ux] = x[rci];	/* Store in a local variable */
	    x[rci] = 0;			/* during multiply. */
	    
	    llo = il[rci-1];		/* Subtract L vector from X */
	    lhi = il[rci];
	    for (lx = llo; lx < lhi; lx++) {
		ri = jl[lx];
		x[ri] -= aux * l[lx];
		}

	    }

	/*...Check pivot. */
	if (x[ci] <= 0) {
	    printf("Warning - factorization restart\n");
	    if (alph==0) alph  = 1e-3;
	    else         alph *= 4;
	    goto RESTART;
	    }
	di[ci] = aux = 1.0/x[ci];
	x[ci] = 0;

	/*...Compute lower triangle. */
	llo = il[ci-1];
	lhi = il[ci];
	for (lx = llo; lx < lhi; lx++) {
	    ri = jl[lx];
	    l[lx] = aux * x[ri];
	    x[ri] = 0;
	    }

	} /* Next column */

    log_cpu (elap, "sparse factorization");
    return(0);
}



