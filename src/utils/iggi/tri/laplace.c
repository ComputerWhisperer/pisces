static char rcsid[]="$Header: /users/suprem/ig2/tri/RCS/laplace.c,v 1.4 85/05/27 23:42:13 conor Exp $";
/***********************************************************************
 *                                                                     *
 * laplace.c - Smooth triangles by averaging.                          *
 *                                                                     *
 * Copyright c 1985 The board of trustees of the Leland Stanford       *
 *                  Junior University. All rights reserved.            *
 * This subroutine may not be used outside of the SUPREM4 computer     *
 * program without the prior written consent of Stanford University.   *
 *                                                                     *
 * Original: CSR Jan.84                                                *
 *                                                                     *
 ***********************************************************************/
#include "general.h"
#include "dbase.h"
#include "thyme.h"
#include "sparse.h"
#include <stdlib.h>

/*-----------------LAPLACE----------------------------------------------
 * Smooth triangles by averaging - equivalent to Laplace equation.
 * Driver routine which calls subordinates.
 *----------------------------------------------------------------------*/
char * laplace()
{
/* DECLARE */
    char *asslpl(), *numfac(), *asslpb(), *numbac(), *mx_init(), 
         *mx_free(), *err;
    double *rhs;    /* Right hand side of Laplace equation */
    int inode;


  /*...Allocate storage. */
    if (!
         (rhs   = (double *)    calloc (nnode+1,    sizeof(double)))
       )
    return("Insufficient memory for Laplace solver");
       
  /*...Assemble matrix - same for x or y */
    err = asslpl (ia, ja, a);	                    	if (err) return(err); 

  /* 
   * Sparse LU decomposition. 
   * This version "fixes" small pivots.
   */
    err = numfac (nnode, ia, ja, a, il, jl, l, iu, ju, u, di, x, ipri, ipc);
    if (err) return(err);


  /*...Average x coordinates. */
    err = asslpb (rhs, 1);			        if (err) return(err);

    err = numbac (nnode, il, jl, l, iu, ju, u, di, x, rhs, ipri, ipc);
    if (err) return(err);

    FOR (inode, 1, nnode)
	if (!ELEC(inode)) node[inode]->x = rhs[inode];

  /*...Average y coordinates. */
    err = asslpb (rhs, 0);			        if (err) return(err);

    err = numbac (nnode, il, jl, l, iu, ju, u, di, x, rhs, ipri, ipc);
    if (err) return(err);

    FOR (inode, 1, nnode)
	if (!ELEC(inode)) node[inode]->y = rhs[inode];

    return(0);
}
    


/*-----------------ASSLPL-----------------------------------------------
 * Assemble Laplace's equation.
 *----------------------------------------------------------------------*/
char * asslpl (ia, ja, a)
    int ia[];
    short ja[];             /* Matrix map */
    double a[];                 /* Matrix */
{
    int itri, in, jn, amap(), asiz, ax, inode, jnode, apos;
    double elap = log_cpu (0.0, "");

  /*...Clear matrix */
    asiz = ia[nnode]-1;
    for (ax = 1; ax <= asiz; ax++) a[ax] = 0;

  /*...Assembly by element contributions. */
    FOR (itri, 1, ntri) {
        FOR (in, 0, 2) {
            inode = tri[itri]->n[in]; 		if (ELEC(inode)) continue;
            FOR (jn, 0, 2) {
                jnode = tri[itri]->n[jn]; 	if (ELEC(jnode)) continue;
                apos = amap (ia, ja, inode, jnode);
                a[apos] += (in==jn)? 2.0 : -1.0;
            }
        }
    }                        

  /*...BC's by putting 1's on the diagonal. (Blast of hate from TJRH) */
    FOR (inode, 1, nnode) {
	if (!ELEC(inode)) continue;
	apos = amap (ia, ja, inode, inode);
	a[apos] = 1.0;
	}

  /*...Adios. */
    log_cpu (elap, "laplacian assembly");
    return(0);
}

	

/*-----------------ASSLPB-----------------------------------------------
 * Assemble rhs of Laplace's equation, i.e. zero - except for
 * boundary conditions.
 *----------------------------------------------------------------------*/
char * asslpb (rhs, do_x)
    double rhs[];   /*...RHS of system to be solved. */
    int do_x;       /*...Do x or y? */
{
    int it, in, jn, inode, jnode;

    FOR (inode, 1, nnode) rhs[inode] = 0;

    FOR (it, 1, ntri) {
        FOR (in, 0, 2) {
	    inode = tri[it]->n[in];
	    if (ELEC(inode)) continue;	/* Ignore fixed nodes. */
            FOR (jn, 0, 2) {
		jnode = tri[it]->n[jn];
                if (ELEC(jnode)) {
                    rhs[inode] += (do_x)? node[jnode]->x
                                        : node[jnode]->y;
                } 
            }
        }
    }
    return(0);
}
