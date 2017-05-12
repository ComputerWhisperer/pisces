/*************************************************************************
 *									 *
 *     Copyright c 1984 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the SUPREM4 computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/
/*   make_db.c                Version 3.1     */
/*   Last Modification : 9/14/87  10:49:20 */

#include <stdio.h>
#include <gplot.h>
#include "global.h"
#include "constant.h"
#include "geom.h"
#include "material.h"	/* For nmat mattyp GAS */

/************************************************************************
 *									*
 *	make_db() - This routine sets up the data structures used by	*
 *  SUPREM IV.  It assumes on input that points and triangles have been	*
 *  created.  It assumes that the neighbor list in tri is accurate, and *
 *  it assumes that the triangles are pointing to the points, rather	*
 *  than the nodes.							*
 *									*
 *  Original:	MEL	10/85						*
 *									*
 ************************************************************************/
#ifdef ANSI_FUNC

int 
make_db (void)
#else

make_db()
#endif
{
    /*if we have no nodes read in*/
    if (nn == 0)
	/*first order of business is to create the nodes*/
	make_nodes();
    else
	/*get the point to node lists set up*/
	pt_to_node();

    /*next, change the triangle to point data to triangle to nodes*/
    tri_to_node();

    /*this is to make regrid happy*/
    init_tri0();

    /*finally, generate the node to triangle lists*/
    node_to_tri();
}


/************************************************************************
 *									*
 *	make_nodes() - This routine generates the node list in a two 	*
 *  step method.  The first step is determine the number of nodes and 	*
 *  materials that are at each point in the mesh.  This step makes use	*
 *  of the nd array in point as a temp list.  The second pass is	*
 *  through the points to create as many nodes as are needed for the	*
 *  point.								*
 *									*
 *  Original:	MEL	10/85		(Based on the old set_sol for	*
 *					 history buffs)			*
 *									*
 ************************************************************************/
#ifdef ANSI_FUNC

int 
make_nodes (void)
#else

make_nodes()
#endif
{
    register int i;	/*every routine needs an i for indexing*/
    register int p;	/*point count*/
    register int t;	/*triangle count*/
    register int mat;	/*material type of the current triangle*/
    char *err, *alloc_nd();
#   define PC(ARG) if (err=ARG) panic(err)

    /*pass 0: clear out the pt->nd arrays (slightly redundant but safe) */
    for( p = 0; p < np; p++)
	for (i = 0; i < MAXMAT; i++)
	    pt[ p]->nd[ i] = FALSE;

    /*pass 1*/
    for(t = 0; t < ne; t++) {
	mat = mattyp[ tri[t]->regnum ];

	/*for each vertex and side*/
	for(i = 0; i < 3; i++) {

	    /*each point needs an node for this material*/
	    pt[ tri[t]->nd[i] ]->nd[ mat ] = TRUE;

	    /*if this side is a free surface*/
	    if ( (tri[t]->nb[i] < 0) && (tri[t]->nb[i] != BC_OFFSET) ) {
		pt[ tri[t]->nd[ (i+1)%3 ] ]->nd[ GAS ] = TRUE;
		pt[ tri[t]->nd[ (i+2)%3 ] ]->nd[ GAS ] = TRUE;
	    }
	}
    }

    /*pass 2*/
    for( nn = p = 0; p < np; p++ ) {
	/*for each material marked true, add a node*/
	for(pt[p]->nn = i = 0; i < MAXMAT; i++) {

	    if ( pt[p]->nd[i] ) {

		/*allocate space for the node*/
		PC( alloc_nd() );

		/*initialize it*/
		nd[nn-1]->mater = i;
		nd[nn-1]->pt = p;

		/*set up the parents stuff*/
		pt[p]->nd[ pt[p]->nn++ ] = nn-1;
	    }
	}
    }
    /*that was easier than I expected, how about you?*/
}

/************************************************************************
 *									*
 *	pt_to_node() - This routine assumes the nodes have been created *
 *  using a structure read and then generates the point to node list 	*
 *									*
 *  Original:	MEL	4/86						*
 *									*
 ************************************************************************/
#ifdef ANSI_FUNC

int 
pt_to_node (void)
#else

pt_to_node()
#endif
{
    int i, p;

    /*clear the number of nodes list*/
    for(i = 0; i < np; i++) 
	pt[i]->nn = 0;

    /*step through the node list and build the pt to node stuff*/
    for(i = 0; i < nn; i++) {
	p = nd[i]->pt;
	pt[p]->nd[ pt[p]->nn++ ] = i;
    }
}
	


/************************************************************************
 *									*
 *	tri_to_nod() - This routine is entered with the triangle vertex	*
 *  list being points rather than nodes.  Foreach triangle, the point's	*
 *  node list is checked to find the matching material node.		*
 *  This works out to be a bit of a thrash...				*
 *									*
 *  Original:	MEL	10/85						*
 *									*
 ************************************************************************/
#ifdef ANSI_FUNC

int 
tri_to_node (void)
#else

tri_to_node()
#endif
{
    int t;	/*triangle counter*/
    int i;	/*vertex counter*/
    int p;	/*point number*/
    int n;	/*node counter*/
    int mat;	/*material of the triangle in question*/

    /*foreach triangle*/
    for(t = 0; t < ne; t++) {
	
	/*get the material in a local*/
	mat = mattyp[ tri[t]->regnum ];
	/*foreach vertex*/
	for(i = 0; i < 3; i++) {
	    
	    /*get the point number in a local*/
	    p = tri[t]->nd[i];
	    for(n = 0; n < pt[p]->nn; n++) {
		if ( mat == nd[ pt[p]->nd[n] ]->mater )
		    tri[t]->nd[i] = pt[p]->nd[n];
	    }
	}
    }
    /*whew!*/
}

/*-----------------INIT_TRI0--------------------------------------------*
 * A dumb routine to make sure nd0 and nb0 are initialized for regrid
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

int 
init_tri0 (void)
#else

init_tri0()
#endif
{
    int t, i;

    /*
     * Remember the original nodes and neighbors for posterity.
     * This is sort of crucial for regrid, maybe it should be
     * somewhere more obvious. But it can't be in mesh_end()
     * because that gets called from regrid().
     */
    for (t = 0; t < ne; t++)
	for (i = 0; i < 3; i++) {
	    tri[t]->nd0[i] = tri[t]->nd[i];
	    tri[t]->nb0[i] = tri[t]->nb[i];
	}
}



/************************************************************************
 *									*
 *	node_to_tri() - This routine generates the node to triangle	*
 *  list.  It is extremely inefficient due to the (>=) nn malloc calls. *
 *									*
 *  Original:	CSR	(Somewhere in Pisces/Iggi)			*
 *  Revised:	MEL	(Made it work for the latest greatest data base)*
 *									*
 ************************************************************************/
#ifdef ANSI_FUNC

int 
node_to_tri (void)
#else

node_to_tri()
#endif
{
    struct pt_str  *p;
    int i, j;
    int t;
    int nx;
    int avail[MAXPNT];		/*array of amount of space for each tri list*/

    /*malloc off space for 6 triangle in the point list*/
    for(i = 0; i < nn; i++) {
	if (nd[i]->tri != NULL)
	    free(nd[i]->tri);
	avail[i] = 6;
	nd[i]->tri = (int *)malloc(avail[i] * sizeof(int));
	nd[i]->ne = 0;
    }

    /*set up the node to triangle array*/
    for(t = 0; t < ne; t++)  {
	if (!leaf (tri[t])) continue;
	for(i = 0; i < 3; i++) {
	    nx = tri[t]->nd[i];

	    /*check for space consideration*/
	    if ( nd[nx]->ne >= avail[nx] ) {
		avail[nx] += 6;
		nd[nx]->tri = (int *)realloc(nd[nx]->tri,avail[nx]*sizeof(int));
	    }

	    /*store the triangle number away*/
	    nd[nx]->tri[ nd[nx]->ne ] = t;

	    /*increment the number of triangles counter*/
	    nd[nx]->ne += 1;
	}
    }
}


/*debug routine...*/
/*
debugset_sol()
{
    float xmin, xmax, ymin, ymax;
    float pxmax, pymax;
    int i;
    int gp_iv[4]; float gp_fv[4];

    dev_lmts(&xmin, &xmax, &ymin, &ymax);

    get device limits

    gpmisc(G_PSIZE, 0, gp_iv, gp_fv, 0);
    pxmax = gp_fv[0]; pymax = gp_fv[1];

    gplot2(G_RESET, G_TRANS, 0.0, 0.0);
    gplot2(G_RESET, G_SCALE, 0.0, 0.0);
    gplot2(G_RESET, G_ROTATE, 0.0, 0.0);
    gplot2(G_CLIPL, G_RESET, 0.0, 0.0);
    gplot2(G_CLIPH, G_RESET, 0.0, 0.0);
    gclear();

    if ((xmax-xmin) > (ymax-ymin))
	ymax = (xmax - xmin) + ymin;
    else 
	xmax = (ymax - ymin) + xmin;

    draw around screen

    if (pxmax > pymax)
	pxmax = pymax;
    else
	pymax = pxmax;
    gscale(pxmax / (xmax - xmin), pymax / (ymax - ymin));
    gtrans(-xmin, -ymin);

    set up clipping to the current scaled values

    gplot2(G_CLIPL, G_ONLOG, xmin, ymin);
    gplot2(G_CLIPH, G_ONLOG, xmax, ymax);

    draw each point

    for(i = 0; i < np; i++) {
	gnline(pt[i]->nn);
	gmove(pt[i]->cord[0], pt[i]->cord[1]);
	gdraw(pt[i]->cord[0], pt[i]->cord[1]);
    }
    gnline(0);
    gpost();
}
*/
