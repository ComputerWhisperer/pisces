#include <stdio.h>
static char rcsid[]="$Header: /users/suprem/ig2/sparse/RCS/symfac.c,v 1.1 85/05/17 14:43:06 conor Exp $";
char * symfac(nv, ia, ja, il, jl, iu, ju, ipri, ipc, maxlusiz, x)
    int nv;			/* Number of variables. */
    int ia[],il[],iu[];		/* Symbolic maps of a,l,u */
    short ja[],jl[],ju[];
    int ipri[], ipc[];		/* Inverse pivot row order, piv. col. order */
    int x[];			/* Workspace. */
    int maxlusiz;		/* Maximum lu sizes. */

/*
 * Sparse matrix symbolic factorization with general pivoting.
 * Uncompressed storage version.
 *
 * CSR Nov. 84.
 */
{
    int ri,ci,rx,jx,clo,chi,wlo,whi,rj,lsiz,usiz;
    static char err[80];
    
    /*...Initialize. */
    for (ri=1; ri<=nv; ri++) 			
	x[ri] = iu[ri] = ju[ri] = 0;
    
    iu[0] = il[0] = 1;

    lsiz = usiz = 0;

    /*...Main loop. Factorization is column ordered. */

    for (ci=1; ci<=nv; ci++) {			/* For each column */

	iu[ci] = iu[ci-1];
	il[ci] = il[ci-1];
	
	clo = ia[ipc[ci]-1];
	chi = ia[ipc[ci]]; 	
			if (clo >= chi) {
			    sprintf(err,"Column %d of A is empty", ipc[ci]);
			    return(err);
			    }
	for (rx=clo; rx < chi; rx++)		/* Copy column pointers. */
	    x[ipri[ja[rx]]] = 1;

	for (ri = 1; ri <= ci-1; ri++) {	/* Examine upper column. */
	    if (x[ri] == 0) continue;		/* No element. */
	    ju[++usiz] = ri; 	
			if (usiz >= maxlusiz) {
			    sprintf(err,"U overflowed in column %d",ci);
			    return(err);
			    }
	    iu[ci]++;
	    x[ri] = 0;

	    wlo = il[ri-1];			/* Subtract partial l vec. */
	    whi = il[ri]-1;
	    for (jx = wlo; jx <= whi; jx++) {
		rj = jl[jx];
		x[rj] = 1;			/* Fill-in. */
		}
	    }
			if (x[ci] == 0) {
			    sprintf(err,"No pivot in column %d.",ci);
			    return(err);
			    }
	x[ci] = 0;

	for (ri = ci+1; ri <= nv; ri++) {	/* Examine lower column. */
	    if (x[ri] == 0) continue;
	    jl[++lsiz] = ri; 	
			if (lsiz >= maxlusiz) {
			    sprintf(err,"L overflowed in column %d.",ci);
			    return(err);
			    }
	    il[ci]++;
	    x[ri] = 0;
	    }
	}

    return(0);

}
