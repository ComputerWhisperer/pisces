/*	skelp.c		Version 1.3		*/
/*	Last Modification:	3/27/90 13:15:18		*/


/*----------------------------------------------------------------------
 *
 * Plotting routines for skel.
 * Handles coordinate transformation window<->problem and clipping
 *
 * Copyright c 1984 The board of trustees of the Leland Stanford
 *                  Junior University. All rights reserved.
 * This subroutine may not be used outside of the SUPREM4 computer
 * program without the prior written consent of Stanford University.
 *
 * Original : MEL (IDS program)
 * Modified : CSR window xform coeffs carry the fill/no-fill info.
 *---------------------------------------------------------------------*/
#include "dbase.h"
#include "skelp.h"
#include "general.h"
#include "gplot.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*----------------------------------------------------------------------
 *this routine allows the user to set up a window
 *simple malloc and return pointer.
 *----------------------------------------------------------------------*/
window cr_window()
{
    window temp; char *set_window();

    temp = (window) malloc (sizeof (struct WINDOW));
    /*set up default window parameters*/
    temp->vxmin = 0.0;
    temp->vxmax = 1.0;
    temp->vymin = 0.0;
    temp->vymax = 1.0;
    set_window (temp, 0.0, 1.0, 0.0, 1.0);
    temp->cw = DefCw;
    temp->ch = DefCh;
    temp->pw = 0;
    return(temp);
}



/*---------------------------DEL_WIN------------------------------------
 *this routine allows a user to dispose of a window.
 *----------------------------------------------------------------------*/
void del_win(ptr)
    window ptr;
{
    /*free up the malloc'ed space*/
    free(ptr);
}


/*---------------------------SET_VIEWPORT-------------------------------
 *this function allows the viewport to be set in a window
 *----------------------------------------------------------------------*/
char *set_viewport(win, vxmin, vxmax, vymin, vymax)
    window win;
    double vxmin, vxmax, vymin, vymax;
{
    if (vxmin >= vxmax || vymin >= vymax) 
	return("Bad data to set_viewport");
    win->vxmin = vxmin;
    win->vxmax = vxmax;
    win->vymin = vymin;
    win->vymax = vymax;
    /*compute conversion numbers*/
    wadjust (win);
    return(0);
}


/*---------------------------SET_WINDOW---------------------------------
 *this function allows the window to be set in a window
 *----------------------------------------------------------------------*/
char *set_window(win, wxmin, wxmax, wymin, wymax)
    window win;
    double wxmin, wxmax, wymin, wymax;
{
    if (wxmin >= wxmax || wymin >= wymax) 
	return("Bad data to set_window");
    win->wxmin = wxmin;
    win->wxmax = wxmax;
    win->wymin = wymin;
    win->wymax = wymax;
    /*compute conversion numbers*/
    wadjust (win);		
    return(0);
}

/*---------------------------WADJUST------------------------------------
 * This sets the transformation coefficients to be the true reflection
 * of a window which is a little bigger than the problem window, and
 * which may be fixed for astigmatism.
 *----------------------------------------------------------------------*/
wadjust (w)
    window w;
{
    double  delx,dely ;
    double  txmax, txmin, tymax, tymin, wratio, vratio ;

    delx = (w->wxmax - w->wxmin)/30;
    dely = (w->wymax - w->wymin)/30;
    txmin = w->wxmin - delx;
    txmax = w->wxmax + delx;
    tymin = w->wymin - dely;
    tymax = w->wymax + dely;
    
    if (!DsFill) 	/* draw to scale, ie fix astig. */
	{ 
	wratio = (txmax - txmin)/(tymax - tymin);
	vratio = (w->vxmax - w->vxmin)/(w->vymax - w->vymin);
	if (wratio > vratio)
	    tymax = tymin + (txmax - txmin)/vratio;
	else
	    txmax = txmin + (tymax - tymin)*vratio;
	}
    w->cxa = txmin;
    w->cxb = w->vxmin;
    w->cxm = (w->vxmax - w->vxmin) / (txmax - txmin);
    w->cya = tymin;
    w->cyb = w->vymin;
    w->cym = (w->vymax - w->vymin) / (tymax - tymin);
}

/*-----------------WV_XFORM---------------------------------------------
 *Transform from window(problem) to viewport(inches) coordinates.
 *----------------------------------------------------------------------*/
void wv_xform (w, x, y, rx, ry)
    window w; 
    double x,y;		/* Window   coords */
    double *rx, *ry;	/* Viewport coords */
{
    *rx = (x - w->cxa) * (w->cxm) + w->cxb;
    *ry = (y - w->cya) * (w->cym) + w->cyb;
}

/*-----------------VW_XFORM---------------------------------------------
 *Transform from viewport(inches) to window(problem) coordinates.
 *----------------------------------------------------------------------*/
void vw_xform (w, rx, ry, x, y)
    window w;
    double rx, ry;	/* Inches */
    double *x, *y;	/* Microns, lightyears, or furlongs. */
{
    *x = w->cxa + (rx - w->cxb) / w->cxm;
    *y = w->cya + (ry - w->cyb) / w->cym;
}

/*---------------------------DRAW_VIEWPORT------------------------------
 *Draw the viewport on the screen.
 *----------------------------------------------------------------------*/
#define CLEAR -1
#define DRAW 1
void draw_viewport (w, mode)
    window w;
    int mode;
{
    float dummy1;

    if (mode == CLEAR)
	gplot2(G_DMODE, G_MCLR, dummy1, dummy1);
    else
	gplot2(G_DMODE, G_MSET, dummy1, dummy1);

    gmove (w->vxmin, w->vymin);
    gdraw (w->vxmax, w->vymin);
    gdraw (w->vxmax, w->vymax);
    gdraw (w->vxmin, w->vymax);
    gdraw (w->vxmin, w->vymin);
}


/*---------------------------VLABEL-------------------------------------
 *Label a viewport.
 *----------------------------------------------------------------------*/
void vlabel (v, x, y, hadj, vadj, text, color)
    window v; 
    double x, y;	/* Fractional coordinates from bottom left. */
    double hadj, vadj;	/* Horizontal and vertical adjust. */
    char *text;
    int color;
{
    {gnline (color);} /* gpost(); */	/* TPLOT/GPLOT dependence. */
    symb2 (v->vxmin + x*(v->vxmax - v->vxmin),
	   v->vymin + y*(v->vymax - v->vymin),
	   text, (verbose) ? strlen(text) : 1,	
	   v->ch, 				/* character size ~ height */
	   0.0, 				/* no rotation. */
	   vadj, hadj);
}


/*---------------------------LWIN---------------------------------------
 *Draw a line in a window. Takes problem coordinates and converts to
 *screen coordinates, clips to window, draws.
 *----------------------------------------------------------------------*/
void lwin(win, x, y)
    window win;
    double x, y;
{ 
    int inside, in_view();
    double rx, ry, clipax, clipay, clipbx, clipby;

/*...convert the input points to standard coordinates*/
    wv_xform (win, x, y, &rx, &ry);

/*...check to see if this point is in the window*/
    inside = in_view(win, rx, ry);

/*...if both this and the previous point are in - draw*/
    if (inside && win->previous)
	{gdraw(rx, ry);}
    else    {
    /*...must clip to screen coords. */
	clipax = win->prevx; clipay = win->prevy;
	clipbx = rx; clipby = ry;
	if (clip(win, &clipax, &clipay, &clipbx, &clipby)){
	    gmove (clipax, clipay);
	    gdraw (clipbx, clipby);
	    }
    }
    

/*...update the previous point data*/
    win->prevx = rx;
    win->prevy = ry;
    win->previous = inside;
}




/*---------------------------MWIN---------------------------------------
 *Move to a point in a window. Takes problem coordinates, converts to
 *screen coordinates, moves.
 *----------------------------------------------------------------------*/
void mwin(win, x, y)
    window win;
    double x, y;
{
    int inside, in_view();
    double rx, ry;

/*...convert the input points to standard coordinates*/
    wv_xform (win, x, y, &rx, &ry);

/*...find out if this point is in the window*/
    if (inside = in_view(win, rx, ry)) 
	gmove(rx, ry);

/*...save the last coordinates*/
    win->prevx = rx;
    win->prevy = ry;
    win->previous = inside;
}


/*---------------------------DOTWIN-------------------------------------
 *Draw a "dot" at a point in a window. Input in problem coordinates.
 *----------------------------------------------------------------------*/
void dotwin(win, x, y, size, color)
    window win;
    double x, y;
    int size, color;
{ 
    int inside; double rx, ry, ht;

/*...convert the input points to standard coordinates*/
    wv_xform (win, x, y, &rx, &ry);

/*...size small is a single pixel. sizes medium and large are diamonds */
    if (color < 0) color = 1; {gnline(color);}
    if (inside = in_view (win, rx, ry)){
	if (size==SMALL) {
		gmove (rx, ry); gdraw (rx, ry);
		}
	else { 
	  /*...Don't draw any bigger than the margin around the edge! */
	    ht = dmin ((wmesh->wxmin - wmesh->wxmax)/90 *wmesh->cxm, 
		       (wmesh->wymax - wmesh->wymin)/90 *wmesh->cym);
	    gmove (rx,      ry - ht);
	    gdraw (rx + ht, ry);
	    gdraw (rx,      ry + ht);
	    gdraw (rx - ht, ry);
	    gdraw (rx,      ry - ht);
	    gmove (rx, ry);	/*...Don't forget to move to center! */
	    }
	}
  /*...set up window pointers */
    wmesh->prevx = rx;
    wmesh->prevy = ry;
    wmesh->previous = inside;
    
}



/*---------------------------IN_WIN-------------------------------------
 *Is screen point in the viewport?
 *----------------------------------------------------------------------*/
int in_view (win, rx, ry)
    double rx, ry;
    window win;
{
    return ( (rx >= win->vxmin) && (rx <= win->vxmax) && 
	     (ry >= win->vymin) && (ry <= win->vymax) );
}

int in_win (win, x, y)
    double x,y;
    window win;
{
    return ( (x >= win->wxmin) && (x <= win->wxmax) && 
	     (y >= win->wymin) && (y <= win->wymax) );
}

/*---------------------------CLIP---------------------------------------
 *  Clip the line from (ax,ay) -> (bx,by)
 *  Returns true : line does not intersect screen
 *  Returns false:      does.
 * 
 *  Algorithm from anonymous graphics textboox.
 *  Original : CSR Sep 84
 *  C port   : CSR Feb 85
 *----------------------------------------------------------------------*/
int clip (w, pax, pay, pbx, pby)
    window w;
    double *pax, *pay, *pbx, *pby;
{
    double tx,ty,ax = *pax, ay = *pay, bx = *pbx, by = *pby;
    int aleft,aright,atop,abot,bleft,bright,btop,bbot, 
    cleft,cright,ctop,cbot,lafix;

    aleft  = ax < w->vxmin; aright = ax > w->vxmax;
    abot   = ay < w->vymin; atop   = ay > w->vymax;

    bleft  = bx < w->vxmin; bright = bx > w->vxmax;
    bbot   = by < w->vymin; btop   = by > w->vymax;

/*.....While not both in window do: */
    while (aleft || aright || atop || abot || bleft || bright || btop || bbot) {

	if((aleft&&bleft) || (aright&&bright) || (atop&&btop) || (abot&&bbot))
	    return (FALSE);

	if (aleft || aright || atop || abot) {
	    cleft = aleft;
	    cright = aright;
	    ctop = atop;
	    cbot = abot;
	    lafix = TRUE;
	}else{
	    cleft = bleft;
	    cright = bright;
	    ctop = btop;
	    cbot = bbot;
	    lafix = FALSE;
	}

	if (cleft) {
	    ty = ay + (by-ay)*(w->vxmin-ax)/(bx-ax);
	    tx = w->vxmin;
	}else if (cright) {
	    ty = ay + (by-ay)*(w->vxmax-ax)/(bx-ax);
	    tx = w->vxmax;
	}else if (cbot) {
	    tx = ax + (bx-ax)*(w->vymin-ay)/(by-ay);
	    ty = w->vymin;
	}else if (ctop) {
	    tx = ax + (bx-ax)*(w->vymax-ay)/(by-ay);
	    ty = w->vymax;
	}

	if (lafix) {
	    ax = tx;
	    ay = ty;
	    aleft  = ax < w->vxmin;
	    aright = ax > w->vxmax;
	    abot   = ay < w->vymin;
	    atop   = ay > w->vymax;
	}else{
	    bx = tx;
	    by = ty;
	    bleft  = bx < w->vxmin;
	    bright = bx > w->vxmax;
	    bbot   = by < w->vymin;
	    btop   = by > w->vymax;
	}
    }
*pax = ax; *pay = ay; *pbx = bx; *pby = by;
return (TRUE);
}


/*-----------------GET_GRIN/UNGET_GRIN----------------------------------
 *Gets graphic input from the terminal in screen coordinates.
 *Maintains a pushback stack.
 *Returns TRUE or FALSE depending on whether the coords are in the window.
 *----------------------------------------------------------------------*/
static struct Sgrin 
    {double x, y; int p ; 
     struct Sgrin *prev;} 
    *grinstack;

int get_grin (v, x, y, p, text)
    window v;
    double *x, *y; int *p;
    char *text;
{
    struct Sgrin *t; float fx, fy; 
    int iv[4];
    float fv[4];
    float dummy1;

    if (!grinstack) {
	vlabel (v, 0.5, 1.0, 0.5, -0.1, text, 1);

	gpgeti(G_CLOC, iv, fv);

	fx = fv[0];
	fy = fv[1];
	*p = iv[0];
	gplot2(G_DMODE, G_MCLR, dummy1, dummy1);
	vlabel (v, 0.5, 1.0, 0.5, -0.1, text, 0);
	gplot2(G_DMODE, G_MSET, dummy1, dummy1);
        *x = fx; *y = fy;
        }
    else {
	*x = grinstack->x; 
	*y = grinstack->y; 
	*p = grinstack->p;
	t = grinstack; 
	grinstack = grinstack->prev; 
	free(t);
	}

    something_happened();

    return (in_view (v, *x, *y));
}

void unget_grin (x, y, p)
    double x, y; int p;
{
    struct Sgrin *t;
    t = grinstack;
    grinstack = (struct Sgrin *) malloc (sizeof (struct Sgrin));
    grinstack->x = x;
    grinstack->y = y;
    grinstack->p = p;
    grinstack->prev = t;
}

/*-----------------WGET_GRIN/WUNGET_GRIN--------------------------------
 *Gets graphics input from the terminal and converts to problem coords.
 *Also allows the user to type coordinates if they so desire.
 *Kludge to detect numerical input.
 *----------------------------------------------------------------------*/
int wget_grin (win, x, y, p, text, fix)
    window win; double *x, *y; int *p;
    char *text;				/* What to print on window */
    int fix;				/* Whether to round coords */
{
    double dread_topl(), inside, rx, ry;

    inside = get_grin (win, &rx, &ry, p, text);

    if (*p == 'n' || rx > win->vxmax) { 
      /*...User wants to type coordinates. Test against viewport, not
       *...window, to avoid confusing them. */
        *x = dread_topl ("Type x-> "); if (*x == MAXFLOAT) return(0);
        *y = dread_topl ("Type y-> "); if (*y == MAXFLOAT) return(0);
	gatog();
	wv_xform (win, *x, *y, &rx, &ry);
	inside = in_view (win, rx, ry);
    }else{ 
      /*...Do the inverse transformation to problem coords */
	vw_xform (win, rx, ry, x, y);

      /*...Round off to background grid. */
	if (DsGrid && fix) {
	    *x = xback * round (*x/xback);
	    *y = yback * round (*y/yback);
	    }
	}

  /*...If its outside its not going to be used by the caller, so push it */
    if (!inside) wunget_grin (win, *x, *y, *p);

    return (inside);
}

void wunget_grin (win, x, y, p)
    window win; double x, y; int p;
{
  /*...Convert */
    x = (x - win->cxa)*win->cxm + win->cxb;
    y = (y - win->cya)*win->cym + win->cyb;
    unget_grin (x, y, p);
}
    
/*-----------------POLKADOT---------------------------------------------
 * Cover the viewport with regularly spaced little dots to help with input.
 *----------------------------------------------------------------------*/
polkadot (w)
    window w;
{
    double rx0, ry0, rdx, rdy, rx, ry, x0, y0, xratio, yratio; 
    static char err[80]; 

    if (!DsGrid) return;

  /*...Convert grid space to standard coordinates. */
    rdx = xback * w->cxm;	
    rdy = yback * w->cym;	

  /*...Increase step if necessary to avoid thousands of dots.
   *...DON'T change xback or yback. */
    for (xratio=1; (w->vxmax - w->vxmin) / rdx > 20 ; xratio *= 2, rdx *=2 ) ;
    if (xratio != 1) 
	{sprintf(err,"x-grid rounded up to %g\0",xback*xratio); uerr(err);}

    for (yratio=1; (w->vymax - w->vymin) / rdy > 20 ; yratio *= 2, rdy *= 2) ;
    if (yratio != 1) 
	{sprintf(err,"y-grid rounded up to %g\0",yback*yratio); uerr(err);}

  /*...Take the viewport corner to problem coords, bring up to nearest multiple,
   *...convert back. */

    vw_xform (w, w->vxmin, w->vymin, &x0, &y0);
    x0 = (xratio*xback) * iceil (x0 / (xratio*xback));
    y0 = (yratio*yback) * iceil (y0 / (yratio*yback));
    wv_xform (w, x0, y0, &rx0, &ry0);

  /*...Go draw some little dots. */
    {gnline(1);}
    for (rx = rx0;  rx < w->vxmax; rx += rdx)
	for (ry = ry0; ry < w->vymax; ry += rdy) {
	    gmove (rx, ry);
	    gdraw (rx, ry);
	    }
}

