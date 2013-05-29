/*-----------------ICCG-------------------------------------------------
 * Includge-free iccg routine.
 * Right hand side is used to return the result.
 *----------------------------------------------------------------------*/
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

iccg (neq, ia, ja, a, il, jl, l, iu, ju, u, di, x, rhs, ipri, ipc, tol, itl, v)
      int neq;				/* Number of equations. */
      int ia[], il[], iu[];		/* Column starts */
      short ja[], jl[], ju[];		/* Row pointers */
      int ipri[], ipc[];		/* Pivot order */
      double a[], l[], u[], di[], x[];	/* A and its LU factorization */
      double rhs[];			/* Right hand side/solution */
      double tol;			/* Relative tolerance */
      int itl;				/* Iteration limit */
      int v;				/* Verbose */
{
    double 
	*r = (double *) malloc ((1+neq) * sizeof (double)),
	*p = (double *) malloc ((1+neq) * sizeof (double)),
	*ap = (double *) malloc ((1+neq) * sizeof (double)),
	*qinvr = (double *) malloc ((1+neq) * sizeof (double)),
	*aqinvr = (double *) malloc ((1+neq) * sizeof (double)),
	norm2(), log_cpu(), elap = log_cpu (0.0, ""), tloop = 0,
	n0 = norm2 (neq, rhs), n2, normap, rap, alpha, beta, numer;
    int
	i, itc, nmore;

    /*
     * Initialize `old' vectors
     * Guess = 0, residual (r), direction(p), a*direction (ap)
     */
     for (i = 1; i <= neq; i++) {
	p[i] = r[i] = rhs[i];
	rhs[i] = 0;
    }
    mxv (neq, ia, ja, a, p, ap);
    elap = log_cpu (elap, "Loop set up");

    /*
     * Main loop
     */
    for (itc = 1; itc < itl; itc++) {
	
	/* Compute alpha */
	normap = rap = 0;
	for (i = 1; i <= neq; i++) {
	    normap += ap[i]*ap[i];
	    rap += r[i]*ap[i];
	}
	if (normap == 0) return (0);
	alpha = rap / normap;

	/* Update solution and residual vectors */
	for (i = 1; i <= neq; i++) {
	    rhs[i] += alpha * p[i];
	    r[i]   -= alpha * ap[i];
	}

	/* See are we done */
	n2 = norm2 (neq, r)/n0; 
	if (n2 != 0.0) 
	    {nmore = 1+log(tol/n2)/log(n2)*itc; if (nmore < 0) nmore = 0;}
	else nmore = 0;
	if (v) printf ("Loop %3d error %10.2e proj %3d time %6.1f\n", 
			itc, n2, itc+nmore, nmore*tloop);

	if (n2 < tol) goto bye;

	/* Calculate beta and loop */
	for (i=1; i <= neq; i++)
	    qinvr[i] = r[i];
	numbac (neq, il, jl, l, iu, ju, u, di, x, qinvr, ipri, ipc);
	mxv (neq, ia, ja, a, qinvr, aqinvr);

	for (numer = 0, i=1; i <= neq; i++)
	    numer += aqinvr[i] * ap[i];
	
	beta = - numer / normap;

	for (i = 1; i <= neq; i++) {
	    p[i]  = qinvr[i]  + beta *  p[i];
	    ap[i] = aqinvr[i] + beta * ap[i];
	}
	tloop = elap;
	elap = log_cpu (elap, "Loop time");
	tloop = elap - tloop;
    }

bye:
    free(r); free(p); free (ap); free (qinvr); free (aqinvr);
    return(0);
}
