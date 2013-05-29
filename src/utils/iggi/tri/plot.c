/*	%M%		Version %I%		*/
/*	Last Modification:	%G% %U%		*/


/*static char rcsid[]="$Header: /users/suprem/ig2/tri/RCS/plot.c,v 1.6 85/10/18 13:43:49 conor Exp $";*/
/*************************************************************************
 *                                                                       *
 * plot.c - plot support for tri.                                        *
 *                                                                       *
 *     Copyright c 1985 The board of trustees of the Leland Stanford     *
 *                      Junior University. All rights reserved.          *
 *     This subroutine may not be used outside of the IGGI-2 computer    *
 *     program without the prior written consent of Stanford University. *
 *                                                                       *
 * Original : CSR Nov84                                                  *
 * Modified : CSR Mar85 To borrow plotting support from skel             *
 *                                                                       *
 *************************************************************************/
#include "general.h"
#include "dbase.h"	/* So we can do draw_node etc. */
#include "skelp.h"	/* For the window stuff */
#include "gplot.h"
/*  #include <tplot.h> */
#include <stdio.h>

void edge_pl(struct Sedge *i, int color);
void tri_pl(struct Stri *i, int color);
void node_pl(struct Snode *i, int color);

char *init_screen()
{
    char *err, *tty_parm();
    double xmin, xmax, ymin, ymax; int ino;

    err = tty_parm();				if (err) return(err);

   /*...Create the mesh window */
    wmesh = cr_window();       if (!wmesh) return("Out of memory in cr_window");

    err = set_viewport (wmesh, 0.0, ScrWd, 0.0, ScrHt);	if (err) return(err);

   /*...Find how big the grid is. */
    xmin = ymin = MAXFLOAT;
    xmax = ymax = -MAXFLOAT;
    FOR (ino, 1, nnode) {
	xmin = dmin (xmin, node[ino]->x);
	xmax = dmax (xmax, node[ino]->x);
	ymin = dmin (ymin, node[ino]->y);
	ymax = dmax (ymax, node[ino]->y);
	}
    err = set_window (wmesh, xmin, xmax, ymin, ymax);	if (err) return(err);

  /*...And initialize the graphics screen */
/*
    ginit();
*/
    gatog(); 
/*
gpost();
*/

    return(0);
}

char *term_screen()
{
    gpend();
    ggtoa();
    return 0;
}

/*-----------------DRAW_MESH--------------------------------------------
 * Draw the nodes and edges of the mesh.
 *----------------------------------------------------------------------*/
void draw_mesh()
{
    int i; 

    gclear(); 
    FOR (i, 1, nnode) node_pl(node[i],1);
    FOR (i, 1, nedge) edge_pl(edge[i],1);
    FOR (i, 1, ntri)  tri_pl (tri[i],1);
/*
    gpost();
*/
}

void node_pl(i,color)
    struct Snode *i;
    int color;
{
    if (!DsNode) return;
    dotwin (wmesh, i->x, i->y, MEDIUM, color);
    if (debug2) ggtoa();
}

void edge_pl (i,color)
    struct Sedge *i;
    int color;
{
    int j;
    if (!DsEdge) return;
    gnline(color);
    j = i->n[0]; mwin (wmesh, node[j]->x, node[j]->y);
    j = i->n[1]; lwin (wmesh, node[j]->x, node[j]->y);
    if (debug2) ggtoa();
}

void reg_pl (i,color)
    struct Sreg *i;
    int color;
{
    struct LLedge *f, *b;

    if (!DsReg) return;
    WALK (i->bnd, f, b)
	edge_pl (edge[f->edge],color);
    if (debug2) ggtoa();
}

void tri_pl (i,color)
    struct Stri *i;
    int color;
{
    int j;
    if (!DsTri) return;
    gnline(color);
    j = i->n[0]; mwin (wmesh, node[j]->x, node[j]->y);
    j = i->n[1]; lwin (wmesh, node[j]->x, node[j]->y);
    j = i->n[2]; lwin (wmesh, node[j]->x, node[j]->y);
    j = i->n[0]; lwin (wmesh, node[j]->x, node[j]->y);
}
    

/*-----------------DUMMY------------------------------------------------
 * Some dummy routines to resolve references in skelp.c
 *----------------------------------------------------------------------*/
double xpts, ypts;

double dread_topl(s) char *s; { return(0.0); }

void uerr(s) 
    char *s; 
{ 
    fprintf (stderr, "%s\n", s);
}

void something_happened() {}

void wait_more() {}

void skel_screen() {}
