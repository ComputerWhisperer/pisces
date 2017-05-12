/*----------------------------------------------------------------------
 *
 * griph.c - Grid graphics functions.
 * Plot/pick nodes, elements, regions, edges
 *
 * Copyright c 1984 The board of trustees of the Leland Stanford
 *                  Junior University. All rights reserved.
 * This subroutine may not be used outside of the SUPREM4 computer
 * program without the prior written consent of Stanford University.
 *
 * Original : CSR Feb85
 *---------------------------------------------------------------------*/


#include "dbase.h"
#include "skelp.h"
#include "general.h"
#include "gplot.h"
#include "griph.h"


/*-----------------WRESET-----------------------------------------------
 * Reset the mesh window to just cover the device.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
wreset (void)
#else

char *wreset()
#endif
{
    node *n, *b; double xlo, xhi, ylo, yhi; char *err;
    
    xlo = ylo =  MAXFLOAT;
    xhi = yhi = -MAXFLOAT;
    WALK (root.node, n, b) {
	if (n->x < xlo) xlo = n->x;
	if (n->x > xhi) xhi = n->x;
	if (n->y < ylo) ylo = n->y;
	if (n->y > yhi) yhi = n->y;
	}
    err = set_window (wmesh, xlo, xhi, ylo, yhi);  
    return(err);
}
	
/*-----------------DRAW_NODE--------------------------------------------
 * Draw a node. If the background grid is on, or if the node is on
 * an edge, add a little diamond so it can be distinguished.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

void 
draw_node (node *n, int color)
#else

void draw_node (n,color)
    node *n;
    int color;
#endif
{
    if (!DsNode) return;

    if (color < 0) color = 1;
    {gnline(color);}
    dotwin (wmesh, n->x, n->y, MEDIUM, color);
}


/*---------------------------SHADE_TRI----------------------------------
 * In the absence of area fill, this routine shades a triangle.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

void 
shade_tri (triangle *t)
#else

void shade_tri (t)
    triangle *t;
#endif
{
    double  x1, x2, x3, y1, y2, y3, delx31, delx32, dely31, dely32;
    int i ;
      
    x1 = t->n[0]->x; y1 = t->n[0]->y;
    x2 = t->n[1]->x; y2 = t->n[1]->y;
    x3 = t->n[2]->x; y3 = t->n[2]->y;
    x3 = x3 + 0.1*((x1+x2)/2 - x3);
    y3 = y3 + 0.1*((y1+y2)/2 - y3);
    x2 = x2 + 0.1*((x1+x3)/2 - x2);
    y2 = y2 + 0.1*((y1+y3)/2 - y2);
    x1 = x1 + 0.1*((x2+x3)/2 - x1);
    y1 = y1 + 0.1*((y2+y3)/2 - y1);
    delx31 = (x3 - x1)/20;
    dely31 = (y3 - y1)/20;
    delx32 = (x3 - x2)/20;
    dely32 = (y3 - y2)/20;
    mwin (wmesh, x1, y1);
    for (i =0; i < 20; i++)
	{
	lwin (wmesh, x2, y2);
	x1 = x1 + delx31; y1 = y1 + dely31;
	lwin (wmesh, x1, y1);
	x2 = x2 + delx32; y2 = y2 + dely32;
	}
}

/*-----------------DRAW_TRI---------------------------------------------
 * Draw a triangle some color. 0 is erase, -1 is default.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

void 
draw_tri (triangle *t, int color)
#else

void draw_tri (t, color)
    triangle *t;
    int color;
#endif
{
	int obtuse();

	if (!DsTri) return;

	if (color <0) {gnline (ClrTri);} else {gnline (color);}
	mwin (wmesh, t->n[0]->x, t->n[0]->y);
	lwin (wmesh, t->n[1]->x, t->n[1]->y);
	lwin (wmesh, t->n[2]->x, t->n[2]->y);
	lwin (wmesh, t->n[0]->x, t->n[0]->y);
	if (DsObt && obtuse (t)) shade_tri (t);
}


/*-----------------DRAW_EDGE--------------------------------------------
 * Draw an edge some color. 0 is erase, -1 is default.
 * Someday I'll work out what the electrode convention is and adjust.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

void 
draw_edge (edge *e, int color)
#else

void  draw_edge (e, color)
    edge *e;
    int color;
#endif
{
    if (!DsEdge) return;

  /*...Color is `edge' color if its electrode 0, else its `elec' color */
    if (color >=0) {gnline (color);}
    else
	if (e->elec) 
	    {gnline (ClrElec);}
	else 
	    {gnline (ClrEdge);}

    mwin (wmesh, e->n[0]->x, e->n[0]->y);
    lwin (wmesh, e->n[1]->x, e->n[1]->y);

}


/*-----------------DRAW_REG---------------------------------------------
 * Draw a region. If DsRegFull, draw the region warts'n'all.
 * Otherwise just draw the edges which belong to the region.
 * For terminals with area fill, we let the hardware do the work. -not yet
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

void 
draw_reg (region *r, int color)
#else

void draw_reg (r, color)
    region *r;
    int color;
#endif
{
#ifdef BOGUS /*This is not here any more.*/
lledge *b, *f;
    int i;
    node *n;

    if (!DsReg) return;
    if (!r->bnd || !r->bnd->prev) {uerr ("Short region!"); return;}

    if (color < 0) color = ClrReg; 
    {gnline (color);}

 /* Link EACH node in EACH edge. For closed regions, every
  * node gets duplicated. For any region, the order is usually
  * not what the user had in mind, but that's OK, all they need is
  * a non-empty area to put the cursor in so ptnreg() can find it.
  */

 /* Start by moving to the "forward" node of the last edge */
    n = renode (r, r->bnd->prev, 1);     
    mwin (wmesh, n->x, n->y);

 /* Walk around the remainder of the boundary. */
    WALK (r->bnd, f, b) {
	    n = renode (r, f, 0);
	    lwin (wmesh, n->x, n->y);
	    n = renode (r, f, 1);
	    lwin (wmesh, n->x, n->y);
	    }
#endif /*BOGUS*/
}
    

/*-----------------WLABEL-----------------------------------------------
 *Put axes on a window.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

void 
wlabel (window w)
#else

void wlabel (w)
    window w;
#endif
{
    double xlo, ylo, xhi, yhi; 
    float tlo, thi, tdel; 
    float hgt;  /* label height */
    int ntic;

  /*...First thing is to get the viewport corners in problem coords */
    vw_xform (w, w->vxmin, w->vymin, &xlo, &ylo);
    vw_xform (w, w->vxmax, w->vymax, &xhi, &yhi);
    hgt = 0.02 * (float)( (0.95 * w->vymax + 0.05 * w->vymin) -
		(0.92 * w->vymin - 0.08 * w->vymax) );
  /*...Y axis : compute ticks, draw */
    {gnline(1);}
    axtcs2 ((float)ylo, (float)yhi, 0, 0, &tlo, &thi, &tdel, &ntic);
    axplt2 ((float) w->vxmax, (float) w->vymin, 	/* Start x,y */
	    (float) w->vymax - w->vymin,		/* Length */
	    (float) 90.0, 				/* Angle */
	    (float) ylo, (float) yhi,			/* First, last values */
	    (float) tlo, (float) thi,			/* First, last labels */
	    (float) tdel,				/* Label spacing */
	    (int)   ntic,				
/*	    (float) DefCh, */  hgt,			/* Size of labels */
	    (float) 0.0,				/* Angle wrt axis */
	    (int)   0,					/* Below axis */
	    (float) 90.0,				/* Angle of tic */
/*	    (float) 0.66*DefCh,	*/  hgt,		/* Size of tics */
	    (float) 0.0,				/* Depth of tic */
	    "",						/* no title, thanks*/
	    (float) 0.0,				/* title height */
	    (int) 0,					/* title location*/
	    "g");					/* label format */

    axplt2 ((float) w->vxmin, (float) w->vymin, 	/* Start x,y */
	    (float) w->vymax - w->vymin,		/* Length */
	    (float) 90.0, 				/* Angle */
	    (float) ylo, (float) yhi,			/* First, last values */
	    (float) tlo, (float) thi,			/* First, last labels */
	    (float) tdel,				/* Label spacing */
	    (int)   ntic,				
	    (float) 0.0,				/* Size of labels */
	    (float) 0.0,				/* Angle wrt axis */
	    (int)   0,					/* Below axis */
	    (float) 90.0,				/* Angle of tic */
/*	    (float) 0.66*DefCh, */  hgt, 		/* Size of tics */
	    (float) 0.0,				/* Depth of tic */
	    "",						/* no title, thanks*/
	    (float) 0.0,				/* title height */
	    (int) 0,					/* title location*/
	    "g");					/* label format */

  /*...X axis : compute ticks, draw */
/*
    hgt = 0.02 * (float)( (0.97 * w->vxmax + 0.03 * w->vxmin) -
		(0.80 * w->vxmin + 0.20 * w->vxmax) );
*/
    {gnline(1);}
    axtcs2 ((float)xlo, (float)xhi, 0, 0, &tlo, &thi, &tdel, &ntic);
    axplt2 ((float) w->vxmin, (float) w->vymin, 	/* Start x,y */
	    (float) w->vxmax - w->vxmin,		/* Length */
	    (float) 0.0, 				/* Angle */
	    (float) xlo, (float) xhi,			/* First, last values */
	    (float) tlo, (float) thi,			/* First, last labels */
	    (float) tdel,				/* Label spacing */
	    (int)   ntic,				
/*	    (float) DefCh,	*/ hgt,			/* Size of labels */
	    (float) 0.0,				/* Angle wrt axis */
	    (int)   0,					/* Below axis */
	    (float) 90.0,				/* Angle of tic */
/*	    (float) 0.66*DefCh,	*/  hgt,		/* Size of tics */
	    (float) 0.0,				/* Depth of tic */
	    "",						/* no title, thanks*/
	    (float) 0.0,				/* title height */
	    (int) 0,					/* title location*/
	    "g");					/* label format */

    axplt2 ((float) w->vxmin, (float) w->vymax, 	/* Start x,y */
	    (float) w->vxmax - w->vxmin,		/* Length */
	    (float) 0.0, 				/* Angle */
	    (float) xlo, (float) xhi,			/* First, last values */
	    (float) tlo, (float) thi,			/* First, last labels */
	    (float) tdel,				/* Label spacing */
	    (int)   ntic,				
	    (float) 0.0,				/* Size of labels */
	    (float) 0.0,				/* Angle wrt axis */
	    (int)   0,					/* Below axis */
	    (float) 90.0,				/* Angle of tic */
/*	    (float) 0.66*DefCh,	 */ hgt, 		/* Size of tics */
	    (float) 0.0,				/* Depth of tic */
	    "",						/* no title, thanks*/
	    (float) 0.0,				/* title height */
	    (int) 0,					/* title location*/
	    "g");					/* label format */


}

/*-----------------SKEL_SCREEN------------------------------------------
 *Draw a new screen for skel.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

void 
skel_screen (void)
#else

void skel_screen()
#endif
{
    gclear();
    ggtoa();
    refresh_menu();
    draw_mesh();
}

/*---------------------------DRAW_MESH----------------------------------
 * Redraw the screen with the current options, using window wmesh();
 * The display options are listed in skelp.h.
 *
 * NOTES
 * -	Because we can't clear-graphics-area, the menu has to be
 *	refreshed (UGH).
 * - 	Badly need an update routine instead of redrawing all the time.
 *----------------------------------------------------------------------*/
#define DRAW 1
#ifdef ANSI_FUNC

void 
draw_mesh (void)
#else

void draw_mesh ()
#endif
{ 
    node *n, *bn; triangle *t, *bt; region *r, *br; edge *e,*be;

    gnline(1);	
    draw_viewport (wmesh, DRAW);   
    ggtoa();  

  /*...Axis (wince) */
    if (DsAxis) 
	wlabel (wmesh);
	

  /*...Nodes. */
    if (DsNode) 
	WALK (root.node, n, bn) draw_node (n, -1);

  /*...Triangles */
    if (DsTri) 
	WALK (root.tri, t, bt) draw_tri (t, -1);

  /*...Edges */
    if (DsEdge) 
	WALK (root.edge, e, be) draw_edge (e, -1);

  /*...Regions */
    if (DsReg) 
	WALK (root.reg, r, br) draw_reg (r, -1);

  /*...Background grid */
    if (DsGrid)
	polkadot (wmesh);

  /*...Update screen and send last graphics buffer block*/
  /*   gpend();   */
} 


/*-----------------N/E/RPICK--------------------------------------------
 * These routines wait for a cursor position and return the closest
 * node/edge or an including region. If the input is invalid for any
 * reason, it is "unread" and 0 is returned.  
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

node *
npick (char *text)
#else

node *npick (text)
    char *text;
#endif
{
    double x,y, mdis, d, dis(); int ip; 
    node *n, *bn, *closest;

    if (root.node==0) return(0);	/* Don't waste my time... */

    if (!wget_grin (wmesh, &x, &y, &ip, text, 0)) 
	return (0); 			/* Out of window. */

    else {
	mdis = MAXFLOAT;
	WALK (root.node, n, bn)
	    if ((d = dis (n->x, n->y, x, y)) < mdis) {
		mdis = d;
		closest = n;
		}
	return (closest);
	}
}

#ifdef ANSI_FUNC

edge *
epick (char *text)
#else

edge *epick (text)
    char *text;
#endif
{
    double x,y, mdis, d, edis(); int ip; 
    edge *e, *be, *closest;

    if (root.edge==0) return(0);	/* Don't waste my time... */

    if (!wget_grin (wmesh, &x, &y,&ip, text, 0)) 
	return (0);			/* Out of window. */

    else {
	mdis = MAXFLOAT; 
	WALK (root.edge, e, be)
	    if ((d = edis (e, x, y)) < mdis) {
		mdis = d;
		closest = e;
		}
	return (closest);
	}
}

#ifdef ANSI_FUNC

region *
rpick (char *text)
#else

region *rpick (text)
    char *text;
#endif
{
    double x, y; int ip, ptnreg(); region *r, *br;

    if (root.reg==0) return(0);	/* Don't waste my time... */

    if (!wget_grin (wmesh, &x, &y, &ip, text, 0)) 
	return (0);			/* Out of window */

    else {
	WALK (root.reg, r, br)
	    if (ptnreg (x, y, r)) return (r);
	
      /*...Oops, fell out of the loop. Warn @ */
	uerr ("That point is not in any region");
	wunget_grin (wmesh, x, y, ip);
	return (0);
	}
}
