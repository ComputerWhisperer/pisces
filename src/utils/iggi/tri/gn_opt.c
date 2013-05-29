static char rcsid[]="$Header: /users/suprem/ig2/tri/RCS/gn_opt.c,v 1.6 85/10/18 13:43:36 conor Exp $";
/***********************************************************************
 *                                                                     *
 * gn_opt.c - The Gauss-Newton optimizer.                              *
 *            Shifts the points around to try to reduce the big bad    *
 *            angles. Usually successful, but horribly slow.           *
 *                                                                     *
 * Copyright c 1985 The board of trustees of the Leland Stanford       *
 *                  Junior University. All rights reserved.            *
 * This subroutine may not be used outside of the SUPREM4 computer     *
 * program without the prior written consent of Stanford University.   *
 *                                                                     *
 * Original: CSR Nov.84 (Levenberg-Marquardt)                          *
 * Modified: CSR Dec.84 (Newton-trust region approach)                 *
 * Modified: CSR Jan.85 (Return to simple Gauss-Newton approach)       *
 *                                                                     *
 ***********************************************************************/
#include "general.h"
#include "thyme.h"
#include "sparse.h"
#include <stdlib.h>
#include <stdio.h>

/*-----------------GN_OPT-----------------------------------------------
 * Gauss-Newton optimization routine. 
 *
 * Notes:
 * -    This routine implements the outer loops of the damped 
 *      Gauss-Newton method.
 *
 * -    The data base isn't included. The idea is that this routine
 *      knows nothing about the particular problem being solved.
 *      The main problem dependency then is the use of a sparse direct
 *      solver for solving the Gauss-Newton direction.
 *      The solver is assumed to be initialized elsewhere.
 *      The current location is not explicitly defined anywhere, since
 *      it is used only in the user-supplied function and jacobian
 *      routines, and modified by the update routine.
 *
 * -    All the functions are weighted equally in the sum of squares.
 *----------------------------------------------------------------------*/
char * gn_opt (nv, nf, ndep, step_tol, in_tol, itmax, debug2, scale)
    int nv, nf;				/* number of variables, functions. */
    int ndep;				/* number of dependent vars/func.  */
    double step_tol, in_tol;		/* solution tolerance is on step. */
    int itmax;				/* maximum number of iterations */
    int debug2;
    double *scale;			/* local length scales */


{
/* DECLARE */
    double *fval, 		/* f(current location) */
	   *jval,		/* Jacobi at current location */
           *step,		/* dx. */
	   func(), jacobi();	/* The function to minimise and its jacobian */
    double max_step, fmin, test_step(), lsearch(), tdamp, tstep, elap;
    int ifunc, iv, ij, itc, nj;
    char *err, *assjtj(), *numfac(), *numbac(), *assb();

    /*...Allocate storage. */
    nj   = nf * ndep;           /* Total number of Jacobian terms */
    if (!(
         (fval  = (double *) 	calloc(nf+1, 		sizeof(double)) ) &&
	 (jval  = (double *)	calloc(nj+1,	        sizeof(double)) ) &&
	 (step  = (double *) 	calloc(nv+1, 		sizeof(double)) ) 
       ))
    return("Insufficient memory for G-N variables.");


    /*...Compute initial function values and initial error. */
    for (fmin=0, ifunc=1; ifunc <=nf; ifunc++) {
	fval[ifunc] = func(ifunc);
	fmin += fval[ifunc]*fval[ifunc];
	}

    if (debug2) printf("Initial norm %g\n",fmin);

    /*
     *...Outer loop - step from one guess to another.
     *   Function values are recomputed at bottom of loop.
     *...Termination criterion is after computing step.
     */
    
    for (itc = 0; itc < itmax; itc++) {

	if (debug2) printf("Iter %d	",itc);

	/*...Compute Jacobian - all dependencies together. */
	elap = log_cpu (0.0, "");
	for (ij=1, ifunc=1; ifunc <= nf; ifunc++, ij += ndep){
	    jacobi(ifunc, &jval[ij]);
	    }
	log_cpu (elap, "Jacobian calculation");

	/*...Compute step : assemble JtJ*/
        err = assjtj (ia, ja, a, fval, jval);	        if (err) return(err); 

        /* 
         * Sparse LU decomposition. 
         * This version "fixes" small pivots.
         */
        err = numfac (nv, ia, ja, a, il, jl, l, iu, ju, u, di, x, ipri, ipc);
        if (err) return(err);

        /* Compute right hand side of system to be solved. */
        err = assb (jval, fval, step);		        if (err) return(err);

        /* Backsolve */
        err = numbac (nv, il, jl, l, iu, ju, u, di, x, step, ipri, ipc);
        if (err) return(err);


	/*
	 *...Inner loop - damp the Gauss-Newton step.
	 *...Termination criterion is after computing size of step.
	 */

	tdamp = lsearch(0.0, 2.0, step_tol, in_tol, test_step, step, scale);

	do_step(step, tdamp);
        for (max_step=0, iv=1; iv <= nv; iv++)
            if ((tstep=dabs(step[iv])*tdamp/scale[iv]) > max_step)
                max_step = tstep;
	if (debug2) printf("Step %g\ttdamp %g\n",max_step,tdamp);
        if (max_step < step_tol) goto DONE;

	/*...Recompute function. */
	for (fmin=0, ifunc=1; ifunc <=nf; ifunc++) {
	    fval[ifunc] = (*func)(ifunc);
	    fmin += fval[ifunc]*fval[ifunc];
	    }

    } /* Outer loop */

DONE:
    return(0);
}

/*-----------------LSEARCH----------------------------------------------
 * Line search for the minimum along the G-N direction.
 * Combined golden ratio/parabolic search, from Brent.
 *----------------------------------------------------------------------*/
double lsearch (a, b, ex_tol, in_tol, f, zow, scale)
    double a,b;		/* Ends of segment */
    double ex_tol, in_tol;  /* Outer tolerance, inner tolerance. */
    double (*f)();	/* Function */
    double *zow, *scale;/* Must be passed to function */
{

    int itc, itG, itP;
    double d,e,m,p,q,r,tol,t2,u,v,w,fu,fv,fw,fx,x,
	   c = 0.381966, /* Golden ratio 0.5*(3-sqrt(5)) */
           elap = log_cpu (0.0, "");
    
    v = w = x = a + c*(b-a);
    e = 0;
    fv = fw = fx = (*f) (zow, x, ex_tol, scale);
    itc = 1; itG = itP = 0;

    for (;;) {
	m = 0.5*(a+b);
	tol = in_tol;           /* Just use in_tol - no range problem */
	t2 = 2*tol;
	if (dabs(x-m) < t2 - 0.5*(b-a)) break;
	p = q = r = 0;
	if (dabs(e)>tol) {	/* Fit parabola */
	    r = (x-w)*(fx-fv);
	    q = (x-v)*(fx-fw);
	    p = (x-v)*q-(x-w)*r;
	    q = 2*(q-r);
	    if (q>0) p = -p; else q = -q;
	    r = e;
	    e = d;
	    }
	if (dabs(p) < dabs(0.5*q*r) && p > q*(a-x) && p < q*(b-x)) {
	   /*...Parabolic interpolation */
            itP++;
	    d = p/q;
	    u = x+d;
	    if (u-a < t2 || b-u < t2) d = (x<m)?tol:-tol;
	    }
	else {
	   /*...Golden section */
            itG++;
	    e = ((x < m)?b:a) - x;
	    d = c*e;
	    }
	if (dabs(d) > tol) u = x + d;
	else u = x + ((d>0)?tol:-tol);
	fu = (*f) (zow, u, ex_tol, scale); 		itc++;
       /*...Update a,b,v,w,x */
	if (fu <= fx) {
	    if (u<x) b = x;
	    else     a = x;
	    v = w;	fv = fw;
	    w = x;	fw = fx;
	    x = u;	fx = fu;
	    }
	else {
	    if (u < x) a = u;
	else b = u;
	    if (fu<=fw || w==x) {
		v = w;	fv = fw;
		w = u;	fw = fu;
		}
	    else if (fu <= fv || v==x || v == w) {
		v = u;	fv = fu;
		}
	    }
	}

    log_cpu (elap, "linear search");
    return(x);
}
