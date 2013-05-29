

static char SccsID[] = "@(#)dispose.c	1.3\t3/30/90";

/************************************************************************
 *									*
 *   Original : MEL         Stanford University        Sept, 1984	*
 *									*
 *	Primitive routines for allocating, deallocating and		*
 *      manipulating geometric objects					*
 *									*
 *     Copyright c 1984 The board of trustees of the Leland Stanford 	*
 *                      Junior University. All rights reserved.		*
 *     This subroutine may not be used outside of the SUPREM4 computer	*
 *     program without the prior written consent of Stanford University.*
 *									*
 ************************************************************************/
/*   dispose.c                Version 3.3     */
/*   Last Modification : 6/23/88  15:39:06 */

#include <stdio.h>
#include "global.h"
#include "constant.h"
#include "geom.h"
#include "material.h"	/* So we can set nmat to 0 - want this? */
#include "impurity.h"	/* So we can set n_imp to 0 */
#include "more_imp.h"
#include "diffuse.h"	/* for the time of creation */

/*
 * The next four utilities exist because we haven't imported
 * the regrid.h macros into geom.h yet, so some files still
 * do these by subroutine call. To be removed soon.
 */

/*-----------------TWHICH, PWHICH---------------------------------------
 * Find which neighbor/node/pt of a triangle is the given neighbor/node/pt.
 * Would be macros were C perfect. Returns 3 if anything went wrong.
 *----------------------------------------------------------------------*/
/*
twhich (in, wanted)
    int in, wanted;
{
    int j;
    for (j=0; j<3; j++)
	if (tri[in]->nb[j] == wanted)
	    break;
    return(j);
}

nwhich (in, wanted)
    int in, wanted;
{
    int j;
    for (j=0; j<3; j++)
	if (tri[in]->nd[j] == wanted)
	    break;
    return(j);
}

pwhich (in, wanted)
    int in, wanted;
{
    int j;
    for (j=0; j<3; j++)
	if (nd[tri[in]->nd[j]]->pt == wanted)
	    break;
    return(j);
}
*/
/*-----------------NODE_MAT---------------------------------------------
 * Find the node under a particular node which is the right material.
 * -1 for bad.
 *----------------------------------------------------------------------*/
/*
node_mat (in, wanted)
    int in, wanted;
{
    int ip, *nx;
    ip = nd[in]->pt;

    for (nx = pt[ip]->nd; nx < pt[ip]->nd + pt[ip]->nn; nx++)
	if (nd[*nx]->mater == wanted)
	    return (*nx);
    return(-1);
}
*/
/*-----------------ALLOC_ND, ALLOC_PT-----------------------------------
 * Dumb routines to allocate storage and bump np, nn.
 * A good place to put malloc smarts.
 *---------------------------------------------------------------------*/
char *alloc_pt()
{
    int j;

    if (np+1 > MAXPNT) return ("Too many points generated");
    pt[np] = (pt_typ *) malloc (sizeof (pt_typ));
    if (!pt[np]) return ("Out of storage in alloc_pt");

    pt[ np ]->cord[ 0 ] = MAXFLOAT;
    pt[ np ]->cord[ 1 ] = MAXFLOAT;
    pt[ np ]->flags = 0;
    pt[ np ]->nn = 0;

    for (j = 0; j < MAXMAT; j++)
	pt[ np]->nd[ j] = -1;

    np++;
    return(0);
}

char *alloc_nd()
{
    int j;

    if (nn + 1 >= MAXPNT) return("Too many nodes generated");
    nd[nn] = (nd_typ *) malloc (sizeof (nd_typ));
    if (!nd[nn]) return ("Out of storage in alloc_nd");

    nd[ nn ]->pt = -1;
    nd[ nn ]->mater = -1;
    nd[ nn ]->ne = 0;
    nd[ nn ]->tri = 0;
    nd[ nn ]->step = process_step;
    nd[ nn ]->time = total;

    for (j = 0; j < MAXIMP; j++) {
	switch ( soltoimp[j] ) {
	case As:
	case B :
	case P :
	case Sb:
	case I :
	case V :
	case O2:
	case H2O:
	case T :
	case Au:
	case Cs:
	    nd[nn]->sol[j] = 1.0e5;
	    break;
	default:
	    nd[nn]->sol[j] = 0.0;
	    break;
	}
    }

    nn++;
    return(0);
}

char *alloc_tri()
{
    int j;

    if (ne + 1 >= MAXTRI) return("Too many triangles generated");
    tri[ne] = (tri_typ *) calloc (1, sizeof (tri_typ));
    if (!tri[ne]) return ("Out of storage in alloc_tri");

    for (j = 0; j < 3; j++) {
	tri[ ne ]->nd [ j ] = tri[ ne ]->nd0 [ j ] = -1;
	tri[ ne ]->nb [ j ] = tri[ ne ]->nb0 [ j ] = -1;
	tri[ ne ]->ehed[j] = tri[ ne ]->d[j] = tri[ ne ]->earea[j] = MAXFLOAT;
	tri[ ne ]->sig[ j ] = 0;
    }

    tri[ne]->fath  = NO_TRIANGLE;
    tri[ne]->son   = NO_TRIANGLE;
    tri[ne]->level = 0;

    ne++;
    return(0);
}

/************************************************************************
 *									*
 *	dis_all() - this routine frees all the currently used 		*
 *  memory for the mesh information.					*
 *									*
 ************************************************************************/
dis_all()
{
    int i;
    dis_pt(pt); np=0;
    dis_nd(nd); nn=0;
    dis_tri(tri); ne=0;
    nmat = 0;
    for (i = 0; i < MAXMAT; i++) mattyp[ i] = -1;
    n_imp = 0;
    for (i = 0; i < MAXIMP; i++) soltoimp[i] = imptosol[i] = -1;
    MeshInvalidate();
}

dis_pt (p)
    struct pt_str **p;
{
    for (; *p != NULL; p++)
	dis_1pt (p);
}

dis_1pt (p)
    struct pt_str **p;
{
	/*free the rest of the structure*/
	free(p[0]);

	/*null the pointer so that we have no future problems*/
	p[0] = NULL;
}

dis_tri (t)
    struct tri_str **t;
{
    /*free the triangle information*/
    for (; *t != NULL; t++) 
	dis_1tri (t);
}

dis_1tri (t)
    struct tri_str **t;
{
    /*free the structure*/
    free(t[0]);
    /*null the pointer so that we have no future problems*/
    t[0] = NULL;
}


dis_nd (n)
    struct nd_str **n;
{
    /*free the node information*/
    for (; *n != NULL; n++)
	dis_1nd (n);
}

dis_1nd (n)
    struct nd_str **n;
{
    /*free the structure*/
    if (n[0]->tri) free(n[0]->tri);
    free(n[0]);
    /*null the pointer so that we have no future problems*/
    n[0] = NULL;
}

#include "regrid.h"	/* Until we move the macros from regrid.h to geom.h */

/*-----------------WASTE------------------------------------------------
 * To standardize global grid removals
 *----------------------------------------------------------------------*/
int waste()
{
    int ie, save_ne = ne, new_tri[MAXTRI],
	in, save_nn = nn, new_nd[MAXPNT],
	ip, save_np = np, new_pt[MAXPNT],
	j;
    tri_typ *tswap; nd_typ *nswap; pt_typ *pswap;

    /* 
     * Remove dead wood - all removal is done here.
     * First we actually remove the objects, then fix pointers to them. 
     * EVERYTHING THAT IS INDEXED BY NODE POINT OR TRIANGLE CHANGES
     * HERE
     */
    for (ne = 0, ie = 0; ie < save_ne; ie++)
	if (dead_tri (ie)) {
	    new_tri[ie] = -1; dis_1tri (&tri[ie]);
	    }
	else {
	    new_tri [ie] = ne;
	    tswap = tri[ ie]; tri[ie] = 0; tri[ ne++] = tswap;
	    }

    for (nn = 0, in = 0; in < save_nn; in++)
	if (dead_nd (in)) {
	    new_nd[in] = -1; dis_1nd (&nd[in]);
	    }
	else {
	    new_nd[in] = nn;
	    nswap = nd[ in]; nd[ in] = 0; nd[ nn++] = nswap;
	    }

    for (np = 0, ip = 0; ip < save_np; ip++)
	if (dead_pt (ip)) {
	    new_pt[ip] = -1; dis_1pt (&pt[ip]);
	    }
	else {
	    new_pt[ip] = np;
	    pswap = pt[ ip]; pt[ ip] = 0; pt[np++] = pswap;
	    }

    for (ie = 0; ie < ne; ie++)
	for (j=0; j < 3; j++) {
	    if ((tri[ie]->nd[j] = new_nd[ tri[ie]->nd[j] ]) < 0)
		panic("pointer to zombie node");
	    if (tri[ie]->nb[j] >= 0)
		if ((tri[ie]->nb[j] = new_tri[ tri[ie]->nb[j] ]) < 0)
		    panic("pointer to zombie triangle");
	}

    for (in = 0; in < nn; in++)
	if ((nd[in]->pt = new_pt[ nd[in]->pt ]) < 0)
	    panic("pointer to zombie point");
    
    for (ip = 0; ip < np; ip++)
	for (j=0; j < pt[ip]->nn; j++)
	if ((pt[ip]->nd[j]= new_nd[ pt[ip]->nd[j] ]) < 0)
	    panic("pointer to zombie node");

    init_tri0();
    
    return (ne != save_ne || nn != save_nn || np != save_np);
}

/*-----------------ADD_IMPURITY-----------------------------------------
 * Adds a new impurity to the solution set...
 *----------------------------------------------------------------------*/
add_impurity( imp, background, mat)
     int imp;			/* The impurity number */
     float background;		/* A default value to give nodes */
     int mat;			/* If default is only to be given to some */
{
    int i, sol;
    
    if ( imptosol[ imp ] == -1) {
	soltoimp[ n_imp ] = imp;
	imptosol[ imp ] = n_imp;
	sol = n_imp++;
	for( i = 0; i < nn; i++ )
	    if( mat < 0 || nd[ i ]->mater == mat)
		nd[ i ] -> sol[ sol ] = background;
    }
}

lose_impurity( imp)
     int imp;
{
    int i, j, sol = imptosol[ imp ];
    
    if( sol != -1 ) {
	for( j = sol; j < n_imp-1; j++) {
	    for (i = 0; i < nn; i++)
		nd[ i ]->sol[ j ] = nd[ i ]->sol[ j+1 ];
	    soltoimp[ j ] = soltoimp[ j+1 ];
	    imptosol[ soltoimp[ j ] ] = j;
	}
	n_imp--;
	imptosol[imp] = -1;
	soltoimp[n_imp] = -1;
    }
}


/*-----------------MeshValid--------------------------------------------
 * Try not to crash due to absence of mesh.
 *----------------------------------------------------------------------*/
MeshValid()
{
    /* This is as good a guess as any. */
    /* Maybe someday we'll keep a global flag */
    return( ne != 0 && np != 0 && nn != 0);
}

MeshInvalidate()
{
    ne = 0;
    np = 0;
    nn = 0;
}

InvalidMeshCheck()
{
    if( !MeshValid()) {
	fprintf( stderr, "No mesh defined!\n");
	return(-1);
    }
    else
	return(0);
}

