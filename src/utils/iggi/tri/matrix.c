static char rcsid[]="$Header: /users/suprem/ig2/tri/RCS/matrix.c,v 1.5 85/06/02 02:03:33 conor Exp $";
/***********************************************************************
 *                                                                     *
 * matrix.c - Matrix hacking routines of optimizer.                    *
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
#include "sparse.h"
#include "thyme.h"
#include <stdlib.h>


/*-----------------MX_INIT----------------------------------------------
 * Create symbolic maps of a, l, u.
 * First estimates sizes, then calls iaja for the a pointers, then
 * min_fill and symfac for the l/u pointers.
 *----------------------------------------------------------------------*/
char * mx_init (nvar)
    int nvar;           /* Number of variables per node */
/*
 * Create symbolic maps of a, l, u.
 */
{
    char *err , *iaja(), *symfac(), *min_fill();
    int  asize, lusize, iv, in, nv, ilv;
    double elap = log_cpu (0.0, "");
    int  *reo ;		/* Temporary for setting up reordering */

    nv     = nvar*nnode;
    asize  = 7*nvar*nvar*nnode;   /* 7 neighbours and 2 variables/node. */

    lusize = 5.0*asize;             /* Empirical constant */

    if (!(
	 (ia 	= (int *) 	calloc (nv+1, 	sizeof(int)) ) &&
	 (ja	= (short *) 	calloc (asize+1,sizeof(short)) ) &&
	 (reo   = (int *)	calloc (nnode+1,sizeof(int)))
	))
    return("Insufficient memory for A pointers.");

    /*
     *...Allocate the remaining storage.
     */
     if (!(
	 (ipri	= (int *) 	calloc (nv+1,	    sizeof(int)) ) &&
	 (ipc	= (int *) 	calloc (nv+1,	    sizeof(int)) ) &&
	 (il 	= (int *) 	calloc (nv+1, 	    sizeof(int)) ) &&
	 (iu 	= (int *) 	calloc (nv+1, 	    sizeof(int)) ) &&
	 (jl	= (short *) 	calloc (lusize+1,   sizeof(short)) ) &&
	 (ju	= (short *) 	calloc (lusize+1,   sizeof(short)) ) 
	))
    return("Insufficient memory for LU pointers.");

     if (!(
	 (a	= (double *) 	calloc (asize+1,    sizeof(double)) ) &&
	 (l	= (double *) 	calloc (lusize+1,   sizeof(double)) ) &&
	 (u	= (double *) 	calloc (lusize+1,   sizeof(double)) ) &&
	 (di	= (double *) 	calloc (nv+1,	    sizeof(double)) ) &&
	 (x	= (double *) 	calloc (nv+1,	    sizeof(double)) ) 
	))
    return("Insufficient memory for numeric ALU storage.");
    elap = log_cpu (elap, "Matrix memory allocation");

    /*
     *...Symbolic factorization. 
     *...First map A pretending there is one var/node, then do minfill.
     *...Minimum fill algorithm returns reo, 
     *...which is ipri for one variable per node, need to adjust.
     *...Extra complication from reo being 0..nnode-1, ipri being 1..nnode.
     *...Finally remap A, allowing for nvar var/node.
     *...(All to avoid doing min_fill with many variables/node)
     */
    err = iaja (1, ia, ja, asize);		if (err) return(err);

    if (err = min_fill (nnode, ia, ja, reo))
	return(err);
    for (iv=1, in = 0; in < nnode; in++) 
	for (ilv=1; ilv <= nvar; ilv++) 
	    ipri[iv++] = nvar*reo[in] + ilv;
    for (iv=1; iv <= nv; iv++) 
	ipc[ipri[iv]] = iv;

    err = iaja (nvar, ia, ja, asize);		if (err) return(err);

    free(reo);
    elap = log_cpu (elap, "matrix map and symbolic reordering");

    err = symfac(nv, ia, ja, il, jl, iu, ju, ipri, ipc, lusize, x);
    elap = log_cpu (elap, "symbolic factorization");
    if (err) return(err);

    return(0);
}


/*-----------------MX_FREE----------------------------------------------
 * Free all that beautiful storage we seized.
 *----------------------------------------------------------------------*/
mx_free()
{
    free(ia); 	free(il); 	free(iu);	
    free(ja);	free(jl);	free(ju);
    free(a);	free(l);	free(u);
    free(ipri);	free(ipc);
    free(di);	free(x);
    ia = il = iu = ipri = ipc = 0;
    ja = jl = ju = 0;
    a = l = u = di = x = 0;

}

/*-----------------AMAP-------------------------------------------------
 * Find the location of (i,j) in the sparse matrix.
 *----------------------------------------------------------------------*/
int amap (ia, ja, i, j)
    int ia[];		/* Sparse map */
    short ja[];
    int i,j;		/* Location to find. */
{
    int clo, chi, rx;

    /*...Simple linear search of column. */
    clo = ia[j-1];
    chi = ia[j];
    for (rx = clo; rx < chi; rx++)
	if (ja[rx] == i) return(rx);
    
    /*...Uh Oh */
    return(-1);
}

/*-----------------IAJA-------------------------------------------------
 * Set up sparse matrix pointers for a triangular grid.
 * Number of variables/node is a parameter.
 *----------------------------------------------------------------------*/
char * iaja (nvar, ia, ja, maxasize)
    int nvar;           /* Number of variables per node */
    int ia[];		/* Symbolic map of a */
    short ja[];		
    int maxasize;	/* Storage allocation for a */
{
    int in, jn, iv, jv, col, row;
    struct Slink *iw;

    ia[0] = 1;
    col   = 0;

    /* For each node, each variable */
    for (in = 1; in <= nnode; in++) 
	for (iv = 1; iv <= nvar; iv++) {
	    col++;
	    ia[col] = ia[col-1];

	    /* 
	     * For each neighbor node, each variable. 
	     * Note that each node is considered a neighbor of itself in p2p.
	     */
	    for (iw = node[in]->p2p; iw != 0; iw=iw->next) {
		jn = iw->i;
		row = nvar*(jn-1) + 1;
		for (jv = 1; jv <= nvar; jv++) {
		    ja [ia [col]] = row++;
		    if (ia [col]++ >= maxasize) return("A pointers overflowed");
		}
	    }
    }
	    
    return(0);
}

