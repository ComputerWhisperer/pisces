static char rcsid[]="$Header: /users/suprem/ig2/sparse/RCS/numbac.c,v 1.1 85/05/17 14:42:03 conor Exp $";
char * numbac(nv, il, jl, l, iu, ju, u, di, x, b, ipri, ipc)
    int nv; 			/* Number of variables. */
    int il[], iu[];		/* Column start pointers to jl/ju.*/
    short jl[], ju[];		/* Row numbers of l,u */
    double l[], di[], u[];	/* LU factorization */
    double x[];			/* Scratch vector. */
    double b[];			/* Right hand side. */
    int ipri[],ipc[];		/* Inverse pivot rows, pivot cols. */

/* 
 * Sparse matrix backsolve with general pivoting. 
 * Uncompressed storage version.
 *
 * CSR Nov. 84.
 */
{

    int iv, ci, ri, llo, lhi, ulo, uhi, lx, ux;
    double aux;

    /*...Copy right hand side to scratch, with permutation. */
    for (iv = 1; iv <= nv; iv++)
	x[ipri[iv]] = b[iv];

    /*...Forward elimination; scan columns. */
    for (ci = 1; ci < nv; ci++) {
	llo = il[ci-1];
	lhi = il[ci];
	aux = x[ci];

	for (lx = llo; lx < lhi; lx++) {
	    ri = jl[lx];
	    x[ri] -= aux * l[lx];
	    }
	}

    /*...Back substitution. */
    x[nv] *= di[nv];
    for (ci = nv; ci > 1; ci--) {
	aux = x[ci];
	uhi = iu[ci];
	ulo = iu[ci-1];
	for (ux = ulo; ux < uhi; ux++) {
	    ri = ju[ux];
	    x[ri] -= aux * u[ux];
	    }
	x[ci-1] *= di[ci-1];
	}

    /*...Deconvolve the result, overwriting the right hand side. */
    for (iv = 1; iv <= nv; iv++) 
	b[ipc[iv]] = x[iv];
	
    return(0);
}
