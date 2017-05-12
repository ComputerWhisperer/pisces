/*-----------------------------------------------------------------------
 *
 *     user.c - user interface routines.
 *              Mostly a `pick', an error check, and a database call.
 *
 *     Copyright c 1984 The board of trustees of the Leland Stanford
 *                      Junior University. All rights reserved.
 *     This subroutine may not be used outside of the SUPREM4 computer
 *     program without the prior written consent of Stanford University.
 *
 * Original: CSR Feb85
 *-----------------------------------------------------------------------*/

#include "general.h"
#include "dbase.h"
#include "skelp.h" 		/* Window sizes */
#include "griph.h"		/* Pick functions */
#include "alpha.h"		/* Reading stuff from the top line */
#include "gplot.h"		/* Graphics library. */

#include <stdlib.h>
#include <stdio.h>
 
/*-----------------SOMETHING_HAPPENED-----------------------------------
 *Do all the the things that should be done every time the user moves.
 *For now, we just check on the the top-line buffer.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

void 
something_happened (void)
#else

void something_happened()
#endif
{
    topl_upd();
}

/*-----------------URMESH-----------------------------------------------
 * Prompt for the name of a file, read it, update the screen.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
urmesh (void)
#else

char *urmesh()
#endif
{
    char *name, *err, *rumesh();

    name = sread_topl("File name: ");
    err = rumesh (name); 	if (err) uerr(err);
    wreset();			/*...Should we let @ decide? */
    skel_screen();
    return(0);
}

/*-----------------UWMESH-----------------------------------------------
 * Prompt for a filename, then write the mesh.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
uwmesh (void)
#else

char *uwmesh()
#endif
{
    char *name, *err, *wumesh();

    name = sread_topl("File name: ");
    err = wumesh (name); 	if (err) uerr(err);
    return(0);
}
/*-----------------UGMESH-----------------------------------------------
 * Prompt for a filename, then make a dplot copy of the edges.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
ugmesh (void)
#else

char *ugmesh()
#endif
{
    char *name, *err, *wgmesh();

    name = sread_topl("File name: ");
    err = wgmesh (name); 	if (err) uerr(err);
    return(0);
}

/*-----------------UCR_NODE---------------------------------------------*/
#ifdef ANSI_FUNC

char *
ucr_node (void)
#else

char *ucr_node()
#endif
{
    double x, y; int p; node *new; char *err;

    while (wget_grin (wmesh, &x, &y, &p, "Where", TRUE)) {
	err = cr_node (&new, x, y);			if (err) return(err);
	draw_node (new, -1);
	}
    return(0);
}

/*-----------------UDS_NODE---------------------------------------------*/
#ifdef ANSI_FUNC

char *
uds_node (void)
#else

char *uds_node()
#endif
{
    node *n; char *err;
    while (n = npick("Node")) {
	err = ds_node (n);				if (err) return(err);
	draw_node (n, 0);
	}
    return(0);
}

/*-----------------UDS_ALL----------------------------------------------
 * Clear the data base.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
uds_all (void)
#else

char *uds_all()
#endif
{
    char *err, *really = sread_topl ("Really destroy everything?");
    if (really[0] == 'y' || really[0] == 'Y') {

	/*...Just keep removing the roots.*/
	while (root.reg)
	    if (err = ds_reg (root.reg)) return(err);
	while (root.edge)
	    if (err = ds_edge (root.edge)) return(err);
	while (root.node)
	    if (err = ds_node (root.node)) return(err);
    }
    skel_screen();
    return(0);
}


/*-----------------UMV_NODE---------------------------------------------
 * Move a node, updating all edges connected to it. 
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
umv_node (void)
#else

char *umv_node()
#endif
{
    node *n; double x,y; int p; char *do_mv_node(), *prox(), *err;

    while (n = npick("Node")) {
	if (!wget_grin (wmesh, &x, &y, &p, "Where", TRUE)) return(0);
	if (check && (err=prox (x, y))) return(err);
	do_mv_node (n, x - n->x, y - n->y);
	}
    return (0);
}

#ifdef ANSI_FUNC

char *
do_mv_node (
    node *n,
    double dx,
    double dy	/* Increment. */
)
#else

char *do_mv_node (n, dx, dy)
    node *n;
    double dx, dy;	/* Increment. */
#endif
{
    edge *ef, *eb; triangle *tf, *tb; region *rf, *rb; lledge *lf, *lb;

  /*...This isn't as awful as you might expect. */
    WALK (root.edge, ef, eb) 
	if (ef->n[0] == n || ef->n[1] == n) draw_edge (ef, 0);
    WALK (root.tri, tf, tb)
	if (tf->n[0] == n || tf->n[1] == n || tf->n[2] == n) draw_tri (tf, 0);
    WALK (root.reg, rf, rb)
	WALK (rf->bnd, lf, lb)
	    if (lf->edge->n[0] == n || lf->edge->n[1] == n) {
		draw_reg (rf, 0); break;
		}
	
    draw_node (n, 0);
    n->x += dx;
    n->y += dy;
    draw_node (n, 1);
    WALK (root.edge, ef, eb)
	if (ef->n[0] == n || ef->n[1] == n) draw_edge (ef, -1);
    WALK (root.tri, tf, tb)
	if (tf->n[0] == n || tf->n[1] == n || tf->n[2] == n) draw_tri (tf, -1);
    WALK (root.reg, rf, rb)
	WALK (rf->bnd, lf, lb)
	    if (lf->edge->n[0] == n || lf->edge->n[1] == n) {
		draw_reg (rf, -1); break;
		}
    return(0);
}
    
	
/*-----------------UMV_EDGE---------------------------------------------
 * Move an edge, updating adjacent edges. 
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
umv_edge (void)
#else

char *umv_edge()
#endif
{
    edge *e; double sx, sy, ex, ey; int p; char *err, *prox();

    while (e=epick("Edge")) {
	if (! (wget_grin (wmesh, &sx, &sy, &p, "Start", 1) &&
	       wget_grin (wmesh, &ex, &ey, &p, "End", 1))) return("Aborted");
	if (check && (err=prox (e->n[0]->x + ex-sx, e->n[0]->y + ey-sy)) ||
		     (err=prox (e->n[1]->x + ex-sx, e->n[1]->y + ey-sy)))
	    return (err);
      /*...Is this a cop-out or what? */
	do_mv_node (e->n[0], ex-sx, ey-sy);
	do_mv_node (e->n[1], ex-sx, ey-sy);
	}
    return (0);
}

/*-----------------UMV_REG----------------------------------------------
 * Move a region, updating half the world.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
umv_reg (void)
#else

char *umv_reg()
#endif
{
    region *r; double sx, sy, ex, ey; int p; node *nf, *nb; lledge *bf, *bb;
    char *err, *prox();

    while (r = rpick("Region")) {
	if (! (wget_grin (wmesh, &sx, &sy, &p, "Start", 1) &&
	       wget_grin (wmesh, &ex, &ey, &p, "End", 1))) return("Aborted");
	WALK (r->bnd, bf, bb) 
	if (check && 
	    (err=prox (bf->edge->n[0]->x + ex-sx, bf->edge->n[0]->y + ey-sy)) ||
	    (err=prox (bf->edge->n[1]->x + ex-sx, bf->edge->n[1]->y + ey-sy)))
	    return (err);

      /*...CAREFUL. We can't move each edge because then some nodes 
       *...would get shifted twice. So we mark all the nodes, then mv 'em.
       */
	WALK (root.node, nf, nb)
	    nf->iocode = FALSE;
	WALK (r->bnd, bf, bb) {
	    bf->edge->n[0]->iocode = TRUE;
	    bf->edge->n[1]->iocode = TRUE;
	    }
	WALK (root.node, nf, nb)
	    if (nf->iocode) do_mv_node (nf, ex-sx, ey-sy);
    }
    return(0);
}

/*-----------------UMV_ORG----------------------------------------------
 * Move the origin to one of the nodes of the grid.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
umv_org (void)
#else

char *umv_org()
#endif
{
    node *n, *fn, *bn; double x,y; window t;

    if (! (n = npick("Node"))) return ("Aborted");
    x = n->x;	
    y = n->y;

  /*...OK everything that has an x or a y in it gets modified. */
    WALK (root.node, fn, bn) {
	fn->x -= x;
	fn->y -= y;
	}

    for (t = wmesh; t != 0; t = t->pw) 
	set_window (t, wmesh->wxmin - x, wmesh->wxmax - x, 
		       wmesh->wymin - y, wmesh->wymax - y);

    return (0);
}
	

/*-----------------UMV_BLOCK --------------------------------------------
 *This stuff isn't consistent with anything else in the program, but 
 *I'm including it for compatibility with Iggi-I. Besides, it's fun.
 *The idea is that everything lying between start and end is shifted
 *by a fraction of new-end.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
umv_block (void)
#else

char *umv_block()
#endif
{
    double sx, sy, ex, ey, nx, ny; int p; 
    char *err, *do_mv_block();

    if (!( wget_grin (wmesh, &sx, &sy, &p, "Start", TRUE) &&
	   wget_grin (wmesh, &ex, &ey, &p, "End", TRUE)   &&
	   wget_grin (wmesh, &nx, &ny, &p, "New", TRUE))) return ("Aborted");
    
  /*...Need a dry run to check for errors. */
    if (err=do_mv_block(sx, sy, ex, ey, nx, ny, FALSE)) return(err);
    if (err=do_mv_block(sx, sy, ex, ey, nx, ny, TRUE))  return(err);

    wreset();
    skel_screen();
    return(0);
}

/*...This used to be a nice piece of code, until these error-checks moved in */

#ifdef ANSI_FUNC

char *
do_mv_block (double sx, double sy, double ex, double ey, double nx, double ny, int for_real)
#else

char *do_mv_block (sx, sy, ex, ey, nx, ny, for_real)
    double sx, sy, ex, ey, nx, ny;
    int for_real;
#endif
{
    node *n, *b; double ap, aq; char *err, *prox();

    WALK (root.node, n, b) {
      /*...Compute coordinate of node n along line (sx,sy)->(ex,ey) */
	lil (sx, sy, ex-sx, ey-sy, n->x, n->y, ey-sy, sx-ex, &ap, &aq);

	if (ap < 0) 	/* Off the "left" end */ continue;

	if (ap > 1) { 	/* Off the "right" end */ 
	    if (for_real) {n->x += nx-ex; n->y += ny-ey;continue;} 
	    else 
		if (err = prox (n->x + nx-ex, n->y + ny-ey)) return(err);
	}
	
	else { 		/* In the segment. */
	    if (for_real) {n->x += ap*(nx-ex); n->y += ap*(ny-ey); continue;}
	    else 
		if (err=prox (n->x + ap*(nx-ex), n->y + ap*(ny-ey)))return(err);
	}
    }
    return(0);
}

/*-----------------UAL_NODE---------------------------------------------
 *Print node stats and let @ change h.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
ual_node (void)
#else

char *ual_node()
#endif
{
    static char obuf[80]; node *n; double newh;
    while (n = npick("Node")) {
	sprintf (obuf,"x = %g y = %g h = %g links = %d", 
		       n->x, n->y, n->h, n->link);
	topl (obuf);
	newh = dread_topl ("New h (return for no change) ->");
	if (newh != MAXFLOAT) n->h = newh;
	gatog();
	}
    return(0);
}
/*-----------------UAL_EDGE---------------------------------------------
 *Print edge stats and let @ change boundary code.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
ual_edge (void)
#else

char *ual_edge()
#endif
{
    static char obuf[80]; edge *e; double d; triangle *t, *bt; int j;

    while (e = epick("Edge")) {
	sprintf (obuf,"links = %d electrode = %d", e->link, e->elec);
	topl (obuf);
	d = dread_topl ("New electrode number (return for no change) ->");
	gatog();
	if (d != MAXFLOAT && d != e->elec) {
	    e->elec = d;
	    draw_edge (e, -1);

	    /* Triangles also need to know this. */
	    WALK (root.tri, t, bt) 
		for (j=0; j<3; j++)
		    if (ined (t->n[(j+1)%3], e) && ined (t->n[(j+2)%3], e))
			t->e[j] = OFFSET_ELEC + d;
	    }
	}
    return(0);
}
/*-----------------UAL_REG----------------------------------------------
 *Print region stats and let @ change material number
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
ual_reg (void)
#else

char *ual_reg()
#endif
{
    static char obuf[80]; region *r;  int len; lledge *f, *b; double d;
    while (r = rpick("Region")) {
	len=0; WALK (r->bnd, f, b) len++;	
	sprintf (obuf,"edges = %d material = %d", len, r->mat);
	topl(obuf);
	d = dread_topl ("New material number (return for no change) ->");
	if (d != MAXFLOAT)
	    r->mat = (int) d;
	gatog();
	}
    return(0);
}

/*-----------------UWI_REFR---------------------------------------------
 * Refresh window.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
uwi_refr (void)
#else

char *uwi_refr()
#endif
{
    skel_screen(); 	return(0);
}

/*-----------------UWI_ZIN----------------------------------------------
 * Zoom in. Maintains a stack.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
uwi_zin (void)
#else

char *uwi_zin()
#endif
{
    double xlo, xhi, ylo, yhi; int inp;
    window t; char *err;

  /*...Create a new window and push it on the stack. */
    if (wget_grin (wmesh, &xlo, &ylo, &inp, "Lower left", TRUE) &&
	wget_grin (wmesh, &xhi, &yhi, &inp, "Upper right", TRUE))
	{
	t = (window) malloc (sizeof (struct WINDOW)); 
	if (!t) return("Out of memory");
	*t = *wmesh;
	err = set_window (t, xlo, xhi, ylo, yhi);
	if (err) {free(t); return(err);}
	t->pw = wmesh; wmesh = t;
	}
    else return ("Aborted");

  /*...Redisplay mesh at new scale. */
    skel_screen();
    return(0);
}

/*-----------------UWI_ZOUT---------------------------------------------
 *Window out. If we have have a stack, just pop, else get new coords.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
uwi_zout (void)
#else

char *uwi_zout()
#endif
{
    double xlo, xhi, ylo, yhi; 
    window t; char *err;

    if (wmesh->pw == 0) { 
	xlo = dread_topl ("Xmin ->");	if (xlo == MAXFLOAT) return("Aborted");
	xhi = dread_topl ("Xmax ->");	if (xhi == MAXFLOAT) return("Aborted");
	ylo = dread_topl ("Ymin ->");	if (ylo == MAXFLOAT) return("Aborted");	
	yhi = dread_topl ("Ymax ->");	if (yhi == MAXFLOAT) return("Aborted");
	err = set_window (wmesh, xlo, xhi, ylo, yhi); 
	if (err) return(err);
	}
    else {
      /*...Already have a window in stack so just pop to window out */
	t = wmesh;
	wmesh = wmesh->pw;
	free (t);
	}

  /*...Redisplay mesh at new scale. */
    skel_screen();
    return(0);
}

/*-----------------UWI_PAN----------------------------------------------
 * Pan window.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
uwi_pan (void)
#else

char *uwi_pan()
#endif
{
    double xs,ys, xe,ye; int inp;
    if (!(wget_grin (wmesh, &xs, &ys, &inp, "Start", TRUE) &&
	  wget_grin (wmesh, &xe, &ye, &inp, "End", TRUE)     )) 
	return ("Out of window");

    set_window (wmesh, wmesh->wxmin + xe-xs, wmesh->wxmax + xe-xs,
		       wmesh->wymin + ye-ys, wmesh->wymax + ye-ys);

    skel_screen();
    return(0);
}

/*-----------------UWI_POP----------------------------------------------
 * Pop to initial window, recalculate coords.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
uwi_pop (void)
#else

char *uwi_pop()
#endif
{
    window t; char *err;
    while (wmesh->pw) {
	t = wmesh;
	wmesh = wmesh->pw;
	free(t);
	}
    err = wreset();	if(err) uerr(err);
    skel_screen();
    return(0);
}
    
/*-----------------UOP_XXXX---------------------------------------------
 *Toggle display options.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
uop_node (void)
#else

char *uop_node()
#endif
 {DsNode = !DsNode; skel_screen(); return(0);}
#ifdef ANSI_FUNC

char *
uop_reg (void)
#else

char *uop_reg()
#endif
  {DsReg  = !DsReg ; skel_screen(); return(0);}
#ifdef ANSI_FUNC

char *
uop_edge (void)
#else

char *uop_edge()
#endif
 {DsEdge = !DsEdge; skel_screen(); return(0);}
#ifdef ANSI_FUNC

char *
uop_tri (void)
#else

char *uop_tri()
#endif
  {DsTri  = !DsTri;  skel_screen(); return(0);}
#ifdef ANSI_FUNC

char *
uop_obt (void)
#else

char *uop_obt()
#endif
  {DsObt = !DsObt; if (DsTri) skel_screen(); return(0);}
#ifdef ANSI_FUNC

char *
uop_axis (void)
#else

char *uop_axis()
#endif
 {DsAxis = !DsAxis; if (DsAxis) skel_screen(); return(0);}

#ifdef ANSI_FUNC

char *
uop_fill (void)
#else

char *uop_fill()
#endif
 
{
    DsFill = !DsFill; 
    set_window (wmesh, wmesh->wxmin, wmesh->wxmax, wmesh->wymin, wmesh->wymax);
    skel_screen(); return(0);
} 

#ifdef ANSI_FUNC

char *
uop_bgrid (void)
#else

char *uop_bgrid()
#endif
{
    double dx = 0, dy = 0;

    if ((dx = dread_topl("x grid space->")) == MAXFLOAT) 
	return("Aborted");

    if (dx != 0)
	if ((dy = dread_topl("y grid space->")) == MAXFLOAT)
	    return("Aborted");
    
    if (dx == 0 || dy == 0)
	xback = yback = 0;
    else
	{xback = dx; yback = dy;}

    polkadot (wmesh);
    return(0);
}
	
