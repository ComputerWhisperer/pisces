
/*-----------------MXV--------------------------------------------------
 * Multiply a sparse matrix by a vector.
 * Uses column ordered sparsity.
 *----------------------------------------------------------------------*/
mxv (neq, ia, ja, a, v, av)
    int neq, ia[];
    double a[], v[], av[];
    short ja[];
{
    int ri, ci, clo, chi, rx;
    double vci;

    for (ri = 1; ri <= neq; ri++)
	av[ri] = 0;

    for (ci = 1; ci <= neq; ci++) {
	clo = ia[ci-1];
	chi = ia[ci];
	vci = v[ci];
	for (rx = clo; rx < chi; rx++) 
	    av[ja[rx]] += vci * a[rx];
    }
}
    
