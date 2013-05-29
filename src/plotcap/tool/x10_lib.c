/* Mon Sep 18 12:30:22 PDT 1989 (dredge--stanford)
 *
 * "x_lib.c": X library specific routines to implement dplot-tool.
 *
 * written:  Michael Eldredge (dec 86)
 * modified: Michael Eldredge (feb 87) added new_icon_pixmap() to allow
 *	the 64x64 bit icons to be scaled down for hardware that can't 
 *	handle that size (lihe the uVax).
 * modified: Michael Eldredge (apr 87) stop select()ing on stdin after
 *	EOF reached.
 * modified: Michael Eldredge (apr 87) Better behaved when stdin just
 *	goes away (EOF hit before EN command found).  Doesn't chew
 *	cpu cycles.
 * modified: MJE (may 87) Fixed initial aspect ratio calculation.
 * modified: MJE (jul 87) Added line() and dmode() funcs.
 *	Added Color line types.
 * modified: MJE (jul 88) 
 *	+++ Added better color getting method allowing #ffffff
 *	+++ Added STAND_ALONE code so that these routines can be used
 *	all by themselves (simple interface to windowing).
 *	+++ Added .Xdefaults calls to get the forground, background colors.
 *	+++ Added LocationsFile stuff.
 * modified: MJE -- stanford (aug 88) Added W_size().
 * modified: MJE -- stanford (sep 89) Added W_sync().
 */

#include <stdio.h>
#include <ctype.h>
#include <X/Xlib.h>

/* Needed ONLY for FILE LOCKING of LocationsFile */
#define LOCK_FILE
#ifdef LOCK_FILE
#  include <sys/file.h>
#endif



/* ----------------------------------------------------------------------- */

/* Default Line colors, can be changed in .Xdefaults */
#define LINE_COLOR_DEFAULT "red,green,yellow,blue,olive,green,pink"

/* .Xdefault names */
#define	XD_LINE_COLORS	"LineColors"
#define XD_BG_COLOR	"Background"
#define XD_FG_COLOR	"Foreground"
#define XD_LOCS_FILE	"LocationsFile"
#define XD_LOCS_DX	"LocationsDX"
#define XD_LOCS_DY	"LocationsDY"
#define XD_GEOM		"Geom"

#define MAX_AREA_POINTS	  256


/* ----------------------------------------------------------------------- */
#define T 1
#define F 0

char*	malloc() ;

static		exposed() ;
static	int	get_locs() ;
static		handle_events() ;
static		mk_vlist() ;
static		new_icon_pixmap() ;
static		tool_loop() ;

static		save_cmd() ;
static		save_val() ;
static		Replay() ;

#ifdef STAND_ALONE
static	int	catch_events() ;
#endif




/* commands that have been saved.  Ones that leave something on the screen */
#define	C_RESET 0	/* reset the saver */
#define	C_SEGM	1
#define C_MOVE	2
#define C_DRAW  3
#define C_LINE	4	/* change line type */
#define C_DMODE 5
#define C_PEN   6
#define C_AREA	7	/* area: begin, points, end, fill patern */

char*	prog = (char*)0 ;		/* program name, for Xdefaults */

/* We've got the modified sunview icon... */
#include "x.icon"
int	icon_width, icon_height ;	/* incase different than def'd above */

/* General cursor, locator cursor, and all-done cursor */
#include "x.curs"


#define DPLOT_LABEL "DPLOT tool"

static	int	imax_def, jmax_def ;
      	int	imax, jmax  ;
static	int	inited = F ;		/* inited yet? */

/* drawing modes: use non-zero values here */
#define DM_SET	1
#define DM_CLR  2
#define DM_CMP  3

static	int	pen_wid = 1, pen_hit = 1 ;
static	int	dmode             = DM_SET ;

/* Color Line type Pixel stuff */
static int*	LinePixels ;		/* to be malloc()ed if need be */
static int	NLinePixels = 0 ;

/* Area fill type and Vertex lists */
static	Vertex*	vlist  = 0 ;		/* to be malloc()ed if need be */
static	int	vcount = 0 ;		/* how many */

/* ======================================================================== */

#define	DEF_EVENTS	(ExposeWindow)
#define LOC_EVENTS	(ButtonReleased|KeyPressed|ExposeWindow)
#define END_EVENTS	(ButtonReleased|ExposeWindow)

/* X specific variables */
static	char*	display = (char*)0 ;

/* Drawing canvas */
/*static*/	Window	main_window ;
static	Cursor	main_cursor, loc_cursor, end_cursor  ;
static	OpaqueFrame main_frame ;

/* Icon window */
static	Window	icon_window ;
static	Pixmap	icon_pixmap ;

static	int	main_bw = 3 ;		/* main window's boarder width */
static	int	icon_bw = 3 ;

/* Draw modes, SetPixel to set, ClrPixel to clear, DrawPixel is current one */
static	int	ForePixel  = BlackPixel ;
static	int	BackPixel  = WhitePixel ;
static	int	SetPixel = BlackPixel ;		/* this will be changed below*/
static  int	ClrPixel = WhitePixel ;
static	int	DrawPixel ;
static	int	AreaPixel ;

/* Bit setting Xfunctions */
static	int	SetFunc  = GXcopy ;
static	int	ClrFunc = GXcopyInverted ;
static	int	DrawFunc ;

#define Error(S) { fprintf(stderr,"%s: %s\n", prog, S); exit(102) ; }

/* Error in w_begin() found */
#define Berror(S) { fprintf(stderr,"%s: %s\n",prog,S) ; return 0 ; }



/*FUNC:*/
int
W_begin(isize, jsize)
	int	isize, jsize ;
	{
#ifndef STAND_ALONE
	extern	int	m_argc ;
	extern	char**	m_argv ;
#endif
	char	req_geom[60], def_geom[40] ;
	char*	p ;
	Color	def ;

	if (inited) XDestroyWindow(main_window) ;	/* could be smarter..*/
	inited    = T ;

	if (!prog) {
#ifndef STAND_ALONE
		if (m_argc > 0 && m_argv[0]) prog = m_argv[0] ;
		else
#endif
			prog = "xgplot" ;
		}

	/* open the display */
	if (!XOpenDisplay(display)) Berror("can't open display") ;

	/* Get the geom specs for creating the window */
	sprintf(def_geom, "=%dx%d+100+80", isize, jsize) ;
	if (!get_locs(isize, jsize, req_geom))
		sprintf(req_geom, "=%dx%d"       , isize, jsize) ;

	if (DisplayCells() > 2) {
		/* Get the coloring pixel values */
		if ((p = XGetDefault(prog, XD_FG_COLOR)) &&
					XParseColor(p, &def) &&
					XGetHardwareColor(&def))
			ForePixel = def.pixel ;
		if ((p = XGetDefault(prog, XD_BG_COLOR)) &&
					XParseColor(p, &def) &&
					XGetHardwareColor(&def))
			BackPixel = def.pixel ;
		}
	else {		/* reverse video? */
		char*	pf, *pb ;
		pf = XGetDefault(prog, XD_FG_COLOR) ;
		pb = XGetDefault(prog, XD_BG_COLOR) ;
		if (pf && pb && strcmp(pf,"White") == 0 &&
				strcmp(pb,"Black") == 0) {
			ForePixel = WhitePixel ;
			BackPixel = BlackPixel ;
			}
		}

	SetPixel = ForePixel ;	/* now that we know */
	ClrPixel = BackPixel ;	/* now that we know */

	/* Reset to DEFAULTS */
	W_line(0) ;		/* default */
	W_pen(0) ;		/* default */
	W_dmode(0) ;		/* default */
	W_area(0, 0, 'F') ;	/* default */
	vcount = 0 ;		/* no vertexes */

	
	/* Store some pixmaps */
	new_icon_pixmap(x_width, x_height, x_bits) ;
	if (icon_pixmap == 0) Berror("can't make icon") ;

	/* A cursor in the plotting window */
	main_cursor = XCreateCursor(
		curs_width, curs_height, curs_bits , curs_bits  ,
		curs_x_hot, curs_y_hot , ForePixel,  BackPixel  , GXcopy ) ;
	if (!main_cursor) Berror("can't make cursor") ;

	loc_cursor = XCreateCursor(	/* for the locator function */
		loc_width, loc_height, loc_bits , loc_bits  ,
		loc_x_hot, loc_y_hot , ForePixel, BackPixel , GXcopy ) ;
	if (!loc_cursor) loc_cursor = main_cursor ;	/* oh, well */

	end_cursor = XCreateCursor(	/* for the all-done (end) function */
		end_width, end_height, end_bits , end_bits  ,
		end_x_hot, end_y_hot , ForePixel, BackPixel , GXcopy ) ;
	if (!end_cursor) end_cursor = main_cursor ;	/* oh, well */

	/* MAIN WINDOW setup */
	main_frame.bdrwidth  = main_bw ;
	main_frame.border    = XMakeTile(ForePixel) ;
	main_frame.background= XMakeTile(BackPixel) ;

	/* create frame and canvas */
	main_window = XCreate(
		DPLOT_LABEL ,		/* for prompt label */
		prog , 			/* For Xdefaults */
		req_geom, def_geom,	/* Requested and defaults */
		&main_frame ,		/* setup frame */
		100, 100 ) ;		/* min sizes */
	if (main_window == 0) Berror("can't make window") ;
	XStoreName(main_window, DPLOT_LABEL) ;

	icon_window = XCreateWindow(
		RootWindow,
		50	,  500 ,	/* where */
		icon_width , icon_height ,	/* size */
		icon_bw ,		/* boarder size */
		BlackPixmap ,		/* boarder color */
		icon_pixmap ) ;		/* background color */
	if (icon_window == 0) Berror("can't make icon window") ;
	
	XSetIconWindow(main_window, icon_window) ;

	XDefineCursor(main_window, main_cursor) ;
	XSelectInput(main_window,  DEF_EVENTS) ;

	/* Let's find out what the size is */
	imax_def = isize ;		/* requested size */
	jmax_def = jsize ;
	imax = main_frame.width ;	/* acutal size */
	jmax = main_frame.height ;

	/* If this is a color/grey scale system, make some line types */
	if (DisplayCells() > 2) {
		char*	color_list, *cl ;
		char	color[128] ;
		int	n ;
		char*	pars_color() ;

		if (! (color_list = XGetDefault(prog, XD_LINE_COLORS)))
			color_list = LINE_COLOR_DEFAULT ;

		/* pre-parse the loop, to count how many */
		for (n = 0, cl = color_list; cl = pars_color(cl, color); ++n)
			;

		if (n > 0 && (LinePixels = (int*)malloc(n*sizeof(int))) ) {
			NLinePixels = n ;

			/* Now, parse and allocate the colors */
			n = 0 ;
			while (color_list = pars_color(color_list, color)) {
				if (XParseColor(color, &def) && 
				    XGetHardwareColor(&def) && 
				    n < NLinePixels)
					LinePixels[n++] = def.pixel ;
				}/*while colors to allocate*/
			}/*if colors to allocate */
		}

	XMapWindow(main_window) ;

	save_cmd(C_RESET) ;

	tool_loop( dpyno() ) ;	/* send in lu of X server */
	
#ifndef STAND_ALONE
	/* If we should return from tool_loop() that means that there
	 *  was an eary EOF from stdin (haven't found an EN command).
	 *  In this case, just clean up as if EN had been given.
	 */
	W_end() ;
#endif /*STAND_ALONE*/
	return 1 ;	/* never really reached... */
	}


/* "get_locs": See if the user is keeping track of locations.
 */
#define YDEL 20
#define XDEL (2*YDEL)
static int
get_locs(imax, jmax, geom)
	int	imax, jmax ;	/* window size */
	char*	geom ;		/* X type geom spec */
	{
	char*	p ;
	FILE*	fp ;
	int	iloc, jloc ;
	int	idir, jdir ;
	int	isize = DisplayWidth() ;
	int	jsize = DisplayHeight() ;
	static	int	dx = XDEL, dy = YDEL ;
	int	it, jt ;		/* temp values */
	int	iret ;
	
	/* LOCATIONS FILE given */
	if ((p = XGetDefault(prog, XD_LOCS_FILE))) {
		fp = fopen(p, "a+") ;
		if (!fp) return 0 ;
#ifdef LOCK_FILE
		(void) flock(fileno(fp), LOCK_EX) ;
#endif /*LOCK_FILE*/
		rewind(fp) ;

		if ((p = XGetDefault(prog, XD_LOCS_DX)))  dx = atoi(p) ;
		if ((p = XGetDefault(prog, XD_LOCS_DY)))  dy = atoi(p) ;

		if (fscanf(fp, "%d %d %d %d", &iloc,&jloc, &idir,&jdir) == 4) {
			iloc += idir * dx ;
			jloc += jdir * dy ;
			if (iloc <= 0) { iloc = 0 ; idir = 1 ; }
			if (jloc <= 0) { jloc = 0 ; jdir = 1 ; }
			if (iloc+imax >= isize) {
				iloc = isize - imax ; idir = -1 ;
				}
			if (jloc+jmax >= jsize) {
				jloc = jsize - jmax ; jdir = -1 ;
				}
			}
		else {		/* first time */
			iloc = isize / 2 ; /* start from top middle */
			jloc = dy ;
			idir = -1 ;  jdir = 1 ;
#ifdef FROM_CORNER
			iloc = jloc = 0 ;	/* start from corner */
			idir = jdir = 1 ;
#endif
			/* if a geom was given, use it */
			if ((p = XGetDefault(prog,XD_GEOM))) {
				iret = XParseGeometry(p, &iloc,&jloc, &it,&jt);
				if (iret&XNegative) iloc = isize-1 - iloc-imax;
				if (iret&YNegative) jloc = jsize-1 - jloc-jmax;
				}
			}

		rewind(fp) ;
		fprintf(fp, "%d %d %d %d\n", iloc, jloc, idir, jdir) ;
#ifdef LOCK_FILE
		flock(fileno(fp), LOCK_UN) ;
#endif /*LOCK_FILE*/
		fclose(fp) ;
		}/*of if locs-file given */
	
	/* no LOCATIONS FILE given */
	else {
		iloc = jloc = 0 ;	/* should be setting these, but... */
		/* if a geom was given, use it */
		if ((p = XGetDefault(prog,XD_GEOM))) {
			iret = XParseGeometry(p, &iloc,&jloc, &it,&jt);
			if (iret&XNegative) iloc = isize-1 - iloc-imax;
			if (iret&YNegative) jloc = jsize-1 - jloc-jmax;
			}

		else return 0 ;	/* nothing good was given, allow defaults */
		}

	/* DEFINE THE GEOMETRY */
	sprintf(geom, "=%dx%d+%d+%d" , imax, jmax, iloc, jloc) ;
	return 1 ;
	}


/* "new_icon_pixmap": Call XMakePixmap() for an icon, but make sure it is
 *	the correct size for the hardware.  We are not trying to be fast
 *	nor pretty here.  Just make sure that the Make icon window call will
 *	work if the icon is (as defined) is too big for the hardware.
 */
#define OBIT(R,C) (bits[ R*(( wid+15)/16) + C/16 ] &  (1 << ((C%16))) )
#define NBIT(R,C) nbits[ R*((nwid+15)/16) + C/16 ] |= (1 << ((C%16)))
static
new_icon_pixmap(wid, hit, bits)
	int	wid, hit ;	/* desired width and height */
	short	bits[] ;
	{

	int	nwid, nhit ;
	float	xwid, xhit ;
	int	r,c, nr,nc, or,oc, n ;
	short*	nbits ;		/* if need be... */

	XQueryTileShape(wid, hit, &nwid, &nhit) ;

	if (nwid < wid || nhit < hit) {		/* make a smaller one */
		n = (nwid * nhit + 15) / 16 ;	/*  round up  */
#ifndef lint
		nbits = (short*)malloc( n * sizeof(short) ) ;
#else
		nbits = (short*)0 ;	/* be quiet! */
#endif
		if (!nbits) {icon_pixmap = 0 ; return ; }	/* hope not! */
		while (n > 0) nbits[--n] = 0 ;

		xwid = (float)wid / (float)nwid ;
		xhit = (float)hit / (float)nhit ;

		for (r = nr = 0 ; r <= hit-hit/nhit;  r += hit/nhit, nr++) {
			for (c=nc=0 ; c <= wid-wid/nwid; c += wid/nwid, nc++){
				or = xhit * (float)nr ;
				oc = xwid * (float)nc ;
				if (OBIT(or, oc)) NBIT(nr, nc) ;
				}
			}

		}

	else {	/* good enough */
		nbits = bits ;
		nwid  = wid ;
		nhit  = hit ;
		}

	/* error check this outside */
	icon_pixmap = XMakePixmap(
		(Bitmap)XStoreBitmap( nwid, nhit, nbits),
			BlackPixel, WhitePixel ) ;
	icon_width = nwid ;
	icon_height= nhit ;
	}

/*FUNC:*/
W_end()
	{
	XEvent	ev ;
	int	done = F ;

	XSelectInput(main_window, END_EVENTS) ;
	XDefineCursor(main_window, end_cursor) ;
	while (!done) {
		XNextEvent(&ev) ;
		switch (ev.type) {
		case ExposeWindow:
			exposed() ;
			break ;

		case ButtonReleased:
			done = T ;		/* finally all done */
			break ;
			}
		}

	if (NLinePixels > 0) XFreeColors(LinePixels, NLinePixels) ;

	XDestroyWindow(main_window) ;
	exit(0) ;			/* that's all folks... */
	}

/*FUNC:*/
W_clear()
	{
	XClear(main_window) ;
	save_cmd(C_RESET) ;
	}

/*FUNC:*/
W_sync() {
	/* we could actually sync with the server, but there really isn't
	 *  a need to do so.  In the gplot sense, just make sure all the
	 *  internal buffers have gone and received by the tool.
	 */
	}

/*FUNC:*/
W_size(i, j, st)
	int*	i ;
	int*	j ;
	int*	st ;
	{
/*	*i = *j = *st = 0 ;	/* default */
	*st = 1 ;
	*i = imax ;
	*j = jmax ;
	imax_def = imax ;	/* ..._def is what they think it is..*/
	jmax_def = jmax ;	/*  so assume, they know better now. */
	}

#ifndef STAND_ALONE

#define bit(n) (1 << n)
#include <sys/time.h>

static
tool_loop(xlu)
	int	xlu ;
	{

	int	ifds, ofds ;
	int	Ifds, Ofds ;
	int	n ;

	Ifds = bit(0) | bit(xlu) ;	/* stdin & X */
	Ofds = 0 ;

	for (;;) {
		/* check for X-incoming events */
		if ((n = XPending()) > 0) handle_events(n) ;

		ifds = Ifds; ofds = Ofds ;
		(void)select(32, &ifds, &ofds, (int*)0, (struct timeval*)0) ;

		/* See what happened */
		if (ifds & bit(xlu)) 	/* an X event */
			handle_events(XPending()) ;

		if (ifds & bit(0)) {	/* more input form gplot */
			n = handle_input(0) ;
			/* see if EOF found */
			if (n == 0) break ;	/* and return */
			/*if (n == 0) Ifds &= ~bit(0) ;	/* don't check */
			}
		}
	}

#endif /*STAND_ALONE*/

#include <signal.h>
#include <fcntl.h>

static
tool_loop(xlu)
	int	xlu ;
	{

	signal(SIGIO, SIG_IGN) ;
	fcntl(xlu, F_SETFL, FASYNC) ;

	catch_events() ;
	}

static int
catch_events()
	{
	int	n ;

	signal(SIGIO, SIG_IGN) ;
	if ((n = XPending()) > 0) handle_events(n) ;
	signal(SIGIO, catch_events) ;
	}
#endif /* STAND_ALONE*/

#ifndef STAND_ALONE
static
handle_input(lu)
	int	lu ;
	{
	int	n ;

	while ((n = slurp(lu)) == 1) do_comd() ;

	if (n < 0) return 0 ;	/* done... */

	return 1 ;
	}
#endif /* STAND_ALONE*/

static
handle_events(n_events)
	int	n_events ;	/* how many are we expected to handle? */
	{
	XEvent	ev ;

	while (n_events-- > 0) {
		XNextEvent( &ev ) ;

		switch (ev.type) {
		case ExposeWindow:
			exposed() ;
			break ;
			}
		}
	}

/* check for new shape and replay the commands... */
static
exposed()
	{
	WindowInfo  winf ;

	XQueryWindow(main_window, &winf) ;
	imax = winf.width ;
	jmax = winf.height ;
	Replay() ;
	}



/* for draw/move stuff */
static	int	w_prev = 0, h_prev = 0 ;

/* Draw a segment in an X manner... */
static
do_seg(w0, h0, w1, h1)
	int	w0, h0, w1, h1 ;
	{
	int	i0, i1, j0, j1 ;

	/* Incase size has changed... */
	i0 = (w0 * imax) / imax_def ;
	i1 = (w1 * imax) / imax_def ;
	j0 = (h0 * jmax) / jmax_def ;	j0 = jmax - j0 ;
	j1 = (h1 * jmax) / jmax_def ;	j1 = jmax - j1 ;

	/* draw the requested vector */
	XLine(main_window,
		i0, j0,
		i1, j1,
		pen_wid, pen_hit,
		DrawPixel,
		DrawFunc ,
		AllPlanes ) ;
#ifdef STAND_ALONE
	{int n ;
	if ((n = XPending()) > 0) handle_events(n) ;
	}
#endif
	}

/*FUNC: Draw a segment */
W_seg(w0, h0, w1, h1)
	{

	do_seg(w0, h0, w1, h1) ;
	w_prev = w1 ;
	h_prev = h1 ;

	save_cmd(C_SEGM) ;
	save_val(w0) ; save_val(h0) ;
	save_val(w1) ; save_val(h1) ;
	}

/*FUNC: move to the given point */
W_move(w, h)
	int	w, h ;
	{
	w_prev = w ;
	h_prev = h ;

	save_cmd(C_MOVE) ;
	save_val(w) ; save_val(h) ;
	}

/*FUNC: draw from previous point to current point */
W_draw(w, h)
	int	w, h ;
	{
	do_seg(w_prev, h_prev, w, h) ;
	w_prev = w ;
	h_prev = h ;

	save_cmd(C_DRAW) ;
	save_val(w) ; save_val(h) ;
	}

/*FUNC: Change pensize */
W_pen(n)
	int	n ;
	{
	/* set to defaults */
	if (n <= 0) pen_wid = pen_hit = 1 ;
	else XQueryBrushShape(n, n, &pen_wid, &pen_hit) ;

	save_cmd(C_PEN) ;
	save_val(n) ;
	}

/*FUNC: Change linetype -- change colors (software linetyping done higher) */
W_line(n)
	int	n ;
	{
	/* Line { ..0} invalid; 1 solid FG; {2.. } from LinePixels */
	if (n <= 0) n = 1 ;			/* map to valid */
	if (n == 1) SetPixel = ForePixel ;
	else {
		SetPixel = LinePixels[ (n-2) % NLinePixels ] ;
		}

	if (dmode != DM_CLR) DrawPixel = SetPixel ;

	save_cmd(C_LINE) ;
	save_val(n) ;
	}

/*FUNC: Change drawing mode (Set bits, clear bits, complement bits) */
W_dmode(n)
	int	n ;	/* new mode */
	{

	switch (n) {
	case 0:			/* default */
	case DM_SET:		/* Turn bits on */
		DrawPixel = SetPixel ;
		DrawFunc  = SetFunc ;
		break ;

	case DM_CLR:		/* Turn bits off */
		DrawPixel = ClrPixel ;
		DrawFunc  = SetFunc ;
		break;

	case DM_CMP:		/* Flip bit state */
		break ;

	default:
		return ;	/* unknown... */
		}

	dmode = n ;		/* remember for changing lines */

	save_cmd(C_DMODE) ;
	save_val(n) ;
	}

/*FUNC: Define and fill an area */
W_area(h, v, cmd)
	int	h, v ;	/* to where */
	int	cmd ;	/* begin, define, end/fill */
	{

	int	flag = 0 ;
	int	i, j ;
	int	n ;

	/* !'F' so we can call W_area() to set default pattern */
	if (!vlist && cmd != 'F') mk_vlist() ;

	switch (cmd) {
	case 'B':		/* begin an area define */
		vcount = 0 ;
		flag |= VertexStartClosed ;
		/*FALLTHROUGH*/
	case 'P':		/* a point to add to the list */
		/* Incase size has changed... */
		i = (h * imax) / imax_def ;
		j = (v * jmax) / jmax_def ;	j = jmax - j ;

		/* quietly ignore too many points */
		if (vcount < MAX_AREA_POINTS-1) {
			vlist[vcount  ].x     = i ;
			vlist[vcount  ].y     = j ;
			vlist[vcount++].flags = flag ;
			}
		break ;
	case 'E':		/* end define, draw the area */
		if (vcount < 3) return ;
		n = vcount - 1 ;
		if (vlist[n].x != vlist[0].x || vlist[n].y != vlist[0].y) {
			vlist[vcount  ].x     = vlist[0].x ;
			vlist[vcount  ].y     = vlist[0].y ;
			vlist[vcount++].flags = flag ;
			}
		vlist[vcount-1].flags |= VertexEndClosed ;

		XDrawFilled(main_window,
			vlist, vcount, 
			AreaPixel,
			GXcopy,
			AllPlanes) ;
		break ;
	case 'F':		/* change fill pattern */
		n = h ;		/* parm1 ('h') is the pattern number */
		/* Pattern{ ..0} invalid; 1 solid FG; {2.. } from LinePixels */
		if (n <= 0) n = 1 ;			/* map to valid */
		if (n == 1) AreaPixel = SetPixel ;
		else {
			AreaPixel = LinePixels[ (n-2) % NLinePixels ] ;
			}
		break ;
	default:
		return ;		/* without saving cmd */
		}

	save_cmd(C_AREA) ;
	save_val(h) ;
	save_val(v) ;
	save_val(cmd) ;
	}

/* "mk_vlist": Malloc up the vertex list */
static
mk_vlist()
	{
	vlist = (Vertex*)malloc(MAX_AREA_POINTS * sizeof(Vertex)) ;
	if (!vlist) Error("no memory, can't make vertex list") ;
	vcount = 0 ;
	}


/*FUNC: */
/* "W_cloc": return where the cursor is in the window */
W_cloc(iloc, jloc, key)
	int	*iloc, *jloc ;
	int	*key ;		/* key hit to set location */
	{

	XKeyOrButtonEvent ev ;
	int	tmp ;
	char*	p ;

	/* Change what we want; and Look just for them */
	XSelectInput(main_window, LOC_EVENTS) ;
	XDefineCursor(main_window, loc_cursor) ;

   another:
	XWindowEvent(main_window, LOC_EVENTS, (XEvent*)&ev) ;
	switch (ev.type) {
	case ButtonPressed:		/* which is it???? */
	case ButtonReleased:
	case KeyPressed:
		p = XLookupMapping(&ev, &tmp) ;
		if (tmp > 0) *key = (int)p[0] ;	/* hmm? suspect, huh? */
		else         *key = ev.detail ;
		break ;

	case ExposeWindow:
		exposed() ;
		goto another ;	/* didn't get what we wanted yet. */
		break ;
		}

	/* restore Input events and cursor */
	XSelectInput(main_window, DEF_EVENTS) ;
	XDefineCursor(main_window, main_cursor) ;

	/* lets see what that character is... */

	/* Where? */
	*iloc = ev.x ;
	tmp = jmax - ev.y ;
	*jloc = tmp * jmax_def / jmax ;
	}

/* "pars_color": Given a string of Color names, return the next color
 *	in COLOR and update P to the next spot to begin searching.
 *	Return NIL pointer when finished with the string
 * Format:
 *	ColorName := Name0{[\s\t]+nameN}*
 *	ColorList := [\s\t]*ColorName[\s\t]*
 *		   | ColorList,[\s\t]*ColorName[\s\t]*
 * Example:
 *		"  Red, Green, Blue,, dark   blue  ,  light\t blue  "
 *	Gives:
 *		'Red'  'Green'  'Blue'  'dark blue'  'light blue'
 */
char*
pars_color(p, color)
	register char*	p ;
	char*	color ;
	{
	register char*	cp = color ;
	char	prev, cur ;

	*cp = '\0' ;		/* Just in case ... */
	if (!p || !*p) return (char*)0 ;

	prev = ' ' ;
	for (cur = *p ; cur && cur != ','; cur = *++p) {
		if (isspace(cur)) {
			cur = ' ' ;
			if (prev == ' ') continue ;
			prev = ' ' ;
			}
		else prev = cur ;
		
		*cp++ = cur ;
		}

	if (cp != color && cp[-1] == ' ') --cp ;	/* final ' '? */
	*cp = '\0' ;
	if (*p) ++p ;			/* skip that ',' */

	/* in case " ... ,, .. " (ie: null name), try again */
	if (!color[0]) return pars_color(p, color) ;

	return p ;
	}
		



			
/* ======================================================================= */

/* there are many ways to store stuff for redisplay, so let's experiment
 *  with one.
 */

/* just buffer it up. Here we use malloc() and realloc() to get space... */
#define	SAVE_BEG	2048	/* beginning size */
#define	SAVE_EXT	1024	/*  extent size (if need more) */
static	short*	save_buf ;
static	short*	save_p   ;
static	short*	save_end ;
static	int	save_siz = 0 ;
static	int	save_on  = 1 ;

/* save stuff for replay */
static
save_cmd(cmd)
	int	cmd ;
	{

	/* make some buffer space if need be... */
	if (save_siz == 0) {
		save_buf = (short*)malloc( SAVE_BEG * sizeof(short) ) ;
		if (!save_buf) {
			save_on = 0 ;
			return ;	/* no room, damn! */
			}
		save_siz = SAVE_BEG ;
		save_end = &save_buf[ save_siz ] ;	/* just past */
		save_p   = save_buf ;
		}
	
	if (!save_on) return ;

	switch (cmd) {
	case C_RESET:
		save_p   = save_buf ;
		break ;/*RESET*/

	default:
		save_val(cmd) ;
		break ;
		}
	}

static
save_val(val)
	int	val ;
	{

	int	cur ;
	char*	realloc() ;

	if (save_siz == 0) save_cmd(C_RESET) ;

	if (!save_on) return ;

	/* see if we've run out of room */
	if (save_p >= save_end) {
		cur  = (int)(save_p - save_buf) ;

		save_siz += SAVE_EXT ;
		save_buf = (short*)realloc(save_buf, save_siz*sizeof(short) );
		if (!save_buf) {
			save_on = 0 ;
			save_siz= 0 ;
			return ;
			}
		save_end = &save_buf[ save_siz ] ;
		save_p   = &save_buf[ cur      ] ;
		}

	*save_p++ = (short)val ;
	}


static
Replay()
	{
	int	save_save = save_on ;
	register short*	p = save_buf ;
	static  int	skip_first = 1 ;	/* skip first replay! */

	if (skip_first) { skip_first = 0 ; return ; }

	if (save_siz == 0) return ;

	/* turn off saving here */
	save_on = 0 ;

	if (save_p > save_buf) {
	/*	W_clear() ;	/* not if we don't need to! */

		while (p < save_p) {
			switch (*p++) {
			case C_SEGM:
				W_seg( p[0], p[1], p[2], p[3]) ;
				p += 4 ;
				break ;

			case C_DRAW:
				W_draw(p[0], p[1]) ;
				p += 2 ;
				break ;

			case C_MOVE:
				W_move(p[0], p[1]) ;
				p += 2 ;
				break ;

			case C_AREA:
				W_area(p[0], p[1], p[2]) ;
				p += 3 ;
				break ;

			case C_LINE:
				W_line(p[0]) ;
				p++ ;
				break ;
			case C_PEN:
				W_pen(p[0]) ;
				p++ ;
				break ;
			case C_DMODE:
				W_dmode(p[0]) ;
				p++ ;
				break ;
				}
			}/*while stuff to replay*/
		}

	save_on = save_save ;
	}
