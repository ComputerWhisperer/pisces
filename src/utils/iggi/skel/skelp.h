/* $Header: skelp.h,v 1.1 85/05/16 19:58:54 conor Exp $*/

/*----------------------------------------------------------------------
 *
 * skelp.h - definitions for graphics windows and functions.
 *
 * Copyright c 1985 The board of trustees of the Leland Stanford
 *                  Junior University. All rights reserved.
 * This subroutine may not be used outside of the SUPREM4 computer
 * program without the prior written consent of Stanford University.
 *
 * Original:
 *---------------------------------------------------------------------*/
#ifdef STATIC_ALLOCATION_TIME
#define EXT 
#else
#define EXT extern
#endif


struct WINDOW   {
    double cw, ch;		/*character width and height in this window*/
    double vxmin, vxmax;	/*viewport x min and max - inches*/
    double vymin, vymax;	/*viewport y min and max - inches*/
    double wxmin, wxmax;	/*window x min and max - microns or lightyears*/
    double wymin, wymax;	/*window y min and max - microns or lightyears*/
    double cxa, cxm, cxb; 	/*conversion numbers for x*/
    double cya, cym, cyb; 	/*conversion numbers for y*/
    double prevx, prevy;	 /*the last position in the window moved to*/
    int previous;	 	/*the last spot in the window??*/
    struct WINDOW *pw;		/*previous window in a stack */
    };

typedef struct WINDOW *window;

window cr_window();

EXT window wmesh, wmenu;
EXT double ScrHt, ScrWd;		/* Graphics screen height, width */
EXT double DefCh, DefCw;		/* Default character width, height*/

EXT int DsNode, DsEdge, DsReg, DsTri; 	/* Display N,E,R,T */
EXT int DsAxis;				/* Display axis */
EXT int ClrEdge, ClrElec, ClrReg, ClrTri;	/* Colors */
EXT int DsObt;				/* Shade obtuse */
EXT int DsFill;				/* Distort geometry to fit screen? */
EXT double xback, yback;		/* Background grid spacing */
EXT int verbose;			/* Print long or short messsages? */
#define DsGrid (xback != 0 && yback != 0)

#define MEDIUM 1
#define SMALL  0

/***********************************************************************
 *                                                                     *
 *                 Functions.                                          *
 *                                                                     *
 ***********************************************************************/
window cr_window();

char *set_viewport(), *set_window();

int in_view(), in_win(), get_grin(), wget_grin();

void del_win(), draw_viewport(), vlabel(), lwin(), mwin(), dotwin(),
     unget_grin(), wunget_grin();
     
/* skelp.c */
window cr_window(void);
void del_win(window ptr);
char *set_viewport(window win, double vxmin, double vxmax, double vymin, double vymax);
char *set_window(window win, double wxmin, double wxmax, double wymin, double wymax);
int wadjust(window w);
void wv_xform(window w, double x, double y, double *rx, double *ry);
void vw_xform(window w, double rx, double ry, double *x, double *y);
void draw_viewport(window w, int mode);
void vlabel(window v, double x, double y, double hadj, double vadj, char *text, int color);
void lwin(window win, double x, double y);
void mwin(window win, double x, double y);
void dotwin(window win, double x, double y, int size, int color);
int in_view(window win, double rx, double ry);
int in_win(window win, double x, double y);
int clip(window w, double *pax, double *pay, double *pbx, double *pby);
int get_grin(window v, double *x, double *y, int *p, char *text);
void unget_grin(double x, double y, int p);
int wget_grin(window win, double *x, double *y, int *p, char *text, int fix);
void wunget_grin(window win, double x, double y, int p);
void polkadot(window w);
