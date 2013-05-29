/* Mon Sep 18 12:28:17 PDT 1989 (dredge--stanford)
 *
 * "lib_sun.c": sun specific routines to implement dplot-tool.
 *	This must answer all the functions from tool.c (the main)
 *	ignoring those that we don't care about.
 *
 * written:  Michael Eldredge (jun 86)
 * modified: Michael Eldredge (dec 86) Clean-up. Try to solve lossage problem.
 * modified: Michael Eldredge (may 87) Fixed W_clear() problem. (Wasn't
 *		clearing....
 * modified: Michael Eldredge (sep 89) Added W_sync()
 */

#define T 1
#define F 0

#include <stdio.h>

/* ------------------------------------------------------------------------ */
static	int	imax_def, jmax_def ;
static	int	imax, jmax  ;
static	int	inited = 0 ;

#define DPLOT_LABEL "DPLOT tool"

/* ------------------------------------------------------------------------ */
#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <sunwindow/notify.h>

static short  icon_image[] = {
#	include "sunview.icon"
	} ;
DEFINE_ICON_FROM_IMAGE(sunview_icon, icon_image) ;


static int	sw_resize() ;
static Notify_value handle_input() ;

static  Frame	frame ;
static  Canvas	canvas ;
static  Pixwin*	pw ;

/* ------------------------------------------------------------------------ */

/*FUNC: */
int
W_begin(isize, jsize)
	int	isize, jsize ;
	{
	extern	int	m_argc ;
	extern	char**	m_argv ;
	static	int	need_address ;
	static	int*	me = &need_address ;


	if (inited) window_destroy(frame) ;
	inited = 1 ;

	/* create frame and canvas */
	frame = window_create(NULL, FRAME,
		FRAME_ARGS,	m_argc, m_argv,
		FRAME_ICON,	&sunview_icon,
		FRAME_LABEL,	DPLOT_LABEL,
		FRAME_NO_CONFIRM,	T,	/* no conf DONE */
		0) ;

	/* If we want something other that the default */
	if (isize > 0 && jsize > 0) window_set(frame,
		WIN_HEIGHT, isize,
		WIN_WIDTH,  jsize,
		0) ;

	canvas= window_create(frame, CANVAS,
		CANVAS_RESIZE_PROC, sw_resize ,
		0) ;

	/* get the canvas pixwin into which to draw */
	pw = canvas_pixwin(canvas) ;

	/* Let's find out what the size is */
	imax_def = isize ;
	jmax_def = jsize ;
	imax = (int)window_get(frame, WIN_WIDTH) ;
	jmax = (int)window_get(frame, WIN_HEIGHT) ;

	/* make sure that we can still get the input */
	(void)notify_set_input_func(me, handle_input, 0) ;

	window_main_loop(frame) ;

	return 1 ;	/* ok */
	}

static Notify_value
handle_input(me, fd)
	int*	me ;
	int	fd ;
	{
	int	n ;

	/* while something in a buffer (==1) do the command */
	while ((n = slurp(fd)) == 1) do_comd() ;

	if (n < 0) {	/* done with file.. */
		(void)notify_set_input_func(me, NOTIFY_FUNC_NULL, 0) ;
		return NOTIFY_DONE ;	/* child hangs out */
		}

	return NOTIFY_DONE ;
	}

static int
sw_resize(can, wid, hit)
	Canvas	can ;
	int	wid ;		/* width */
	int	hit ;		/* height */
	{

	jmax = hit ;
	imax = wid ;
	}


/*FUNC: */
W_end()
	{
	/*window_destroy(frame) ;*/
	}

/*FUNC: */
W_clear()
	{
	pw_write(pw, 0, 0, imax-1, jmax-1, 
		PIX_SRC ,		/* source is all null */
		NULL, 0, 0) ;
	}

/*FUNC:*/
W_sync() {
	/* we could actually sync with the server, but there really isn't
	 *  a need to do so.  In the gplot sense, just make sure all the
	 *  internal buffers have gone and received by the tool.
	 */
	}


static	int	w_prev = 0, h_prev = 0 ;

/* Resizes will cause all subsequent plotting to scale to full window size */
static
do_seg(w0, h0, w1, h1)
	int	w0, h0, w1, h1 ;
	{
	int	i0, i1, j0, j1 ;
	int	it, jt ;

	i0 = (w0 * imax) / imax_def ;
	i1 = (w1 * imax) / imax_def ;
	j0 = (h0 * jmax) / jmax_def ;	j0 = jmax - j0 ;
	j1 = (h1 * jmax) / jmax_def ;	j1 = jmax - j1 ;

	/* draw the requested vector */
	pw_vector(pw, i0, j0, i1, j1, PIX_SRC, 1) ;
	}


/*FUNC: */
W_seg(w0, h0, w1, h1)
	int	w0, h0, w1, h1 ;
	{
	/* draw the requested vector */
	do_seg(w0, h0, w1, h1) ;
	w_prev = w1 ;
	h_prev = h1 ;
	}

/*FUNC: draw to point */
W_draw(w, h)
	int	w, h ;
	{
	do_seg(w_prev, h_prev, w, h) ;
	w_prev = w ;
	h_prev = h ;
	}

/*FUNC: move to point */
W_move(w, h)
	int	w, h ;
	{
	w_prev = w ;
	h_prev = h ;
	}

/*FUNC: */
W_pen(n)
	int n ;
	{ ; }


/*FUNC: */
W_line(n)
	int n ;
	{ ; }


/*FUNC: */
W_dmode(n)
	int n ;
	{ ; }


/*FUNC: */
W_area(i, j, cmd)
	int	i, j ;
	int	cmd ;
	{ ; }

/*FUNC:*/
W_size(i, j, st)
	int*	i ;
	int*	j ;
	int*	st ;
	{
	*i = *j = *st = 0 ;	/* default */
	}

/*FUNC: */
/* return the current location of the cursor */
W_cloc(iloc, jloc, key)
	int*	iloc, *jloc ;
	int*	key ;			/* key that was struck */
	{
	*key = 0 ;	/* can't do much yet, huh? */
	*iloc = *jloc = 0 ;
	}
