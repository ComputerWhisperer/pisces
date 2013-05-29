/* Mon Sep 18 12:07:58 PDT 1989 (dredge--stanford)
 *
 * "dplottool": Main.  Take commands from gplot output (plotcap commands)
 *	and call the local functions (handled by the appropriate library
 *	for the differnt window systems.
 */
static char*	version = "1.7" ;
/*
 * Functions to be defined in the library:
 *      int
 *	W_begin(isize, jsize)	- create a window (i by j pixels)
 *			Must be prepared to handle more than one call.
 *	W_end()		- all done. Lib can handle this as it will.
 *			Could keep the window alive until it wants to be
 *			killed.
 *	W_sync()		- don't return until output is posted
 *	W_draw(i,j)		- Draw to a point.
 *	W_move(i,j)		- Move to a point.
 *	W_seg(i0,j0, i1,j1)	- Draw a line segment.
 *	W_clear()		- clear the window and prepare for more.
 *	W_cloc(&i, &j, &key)	- Return the locator value and key struck
 *	W_line(n)		- Switch to a new line type.
 *	W_pen(n)		- Change pen size.
 *	W_dmode(n)		- Switch to a new drawing mode.
 *	W_size(&i, &j, &stat)	- Return actual size of window.
 *
 * Functions/Values Available:
 *	slurp(lu)		- Get more data from the input.
 *	do_comd()		- if slurp() returned a 1 then there is a
 *			command to be done; call do_comd().
 * notes:
 *	+ Should make this get binary data instead of ascii.
 *
 * written:  Michael Eldredge (jun 86)
 * modified: Michael Eldredge (dec 86) Clean up slurp(). Try (still unsuccess-
 *		fully) to fix sunview data loss problem.
 * modified: MJE (feb 87) lint'ed
 * modified: MJE (jul 87) new line types; draw modes.
 * modified: MJE -- stanford (jul 88) Ignore Interupt signal
 * modified: MJE -- stanford (aug 88) Added W_size().
 * modified: MJE -- stanford (sep 89) Added SY/W_sync() and VE (version)
 * modified: MJE -- stanford (dec 89) Write to lu 1 instead of lu 2.
 */

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#define T 1
#define F 0

#define STDIN  0		/* lu 0 */
#define STDOUT 1		/* where to write answers back to ... */


#define MAX_IVALS 10 

/* gather/command states */
#define	C_NEW	 0
#define C_NUMB	 1		/* collecting a number */
#define C_SKIP	 2		/* looking for the next 'thing' */
#define C_GCMD	 3		/* gathering a command */

#ifdef DEBUG
static char*	states[] = { "NEW", "NUMB", "SKIP", "GCMD" } ;
#endif


static int	c_initd = F ;	/* initialized yet? */
static int	c_done  = F ;	/* can never do anything after done */
static int	c_haderr= F ;

static int	c_state = C_NEW ;
static int	c_ivals[MAX_IVALS] ;
static int	c_nval = 0 ;
static char	c_cmd[2] ;		/* all 2 character commands */

static	int	s_bufsiz = 1 ;		/* set slurp buffer size */

/* These need to be externable by other files */
int	m_argc ;	/* GLOBAL copies of these */
char**	m_argv ;

main(argc, argv)
	char**	argv ;
	{

	int	n ;
	int	igsig = T ;

	m_argc = argc ;
	m_argv = argv ;
	
	for (n = 1 ; n < argc; n++) {
		if (*argv[n] != '-') break ;

		/* don't ignore signals */
		if (strcmp(argv[n], "-s") == 0) igsig = F ;
		}

	if (igsig) (void)signal(SIGINT, SIG_IGN);
			

	while ((n = slurp(STDIN)) >= 0)
		if (n == 1) do_comd() ;

	exit(0) ;
	}

/* slurp up input from the pipe and do what we can... */
/* returns: -1 if eof
 *	     0 if buffer empty: an incomplete command
 *	     1 if command to be done.
 */
#define BUF_MAX		1024
slurp(lu) {
	static	char	buf[BUF_MAX], *bp ;
	static	int	cnt = 0 ;
	static	int	eof = F ;
	static	int	punt= F ;

	if (eof) return -1 ;
	if (punt) { punt = F ; return 0 ; }

	if (cnt <= 0) {		/* reload */
		if ((cnt = read(lu, buf, s_bufsiz)) <= 0) {
			eof = T ;
			return -1 ;
			}
		bp = buf ;
		}

	/* use up the buffer or get a command (whichever is first) */
	/*while (cnt-- > 0 && (need_more = take_input(*bp++))) ;*/
	while (cnt-- > 0) {
		/* no more-input-needed? Then return that we have cmd to do */
		if (!take_input(*bp++)) {
			if (cnt <= 0) punt = T ;
			return 1 ;
			}
		}

	return 0 ;	/* ok, more is needed, just emptied the buffer */
	}

/* TEST suntools buffering? */
/* reset slurp buffer size */
static
slurp_buf(siz)
	int	siz ;
	{

	if (siz > BUF_MAX) siz = BUF_MAX ;
	else if (siz <= 0) siz = 1 ;

	s_bufsiz = siz ;
	}

/* return 1 if need more stuff and 0 if a command is ready... */
static int
take_input(c)
	char	c ;
	{
	int	i, want_more = T ;	/* assume we need more... */

	switch (c_state) {
	case C_NEW:
		for (i=0; i < MAX_IVALS; ) c_ivals[i++] = 0 ;
		c_nval = 0 ;
		if (isdigit(c)) {
			c_state = C_NUMB ;
			c_ivals[c_nval] = c - '0' ;
			}

		else if (isalpha(c)) {
			c_state = C_GCMD ;
			c_cmd[0] = c ;
			}
		break ;

	case C_NUMB:
		if (isdigit(c)) {
			c_ivals[c_nval] = c_ivals[c_nval] * 10 + c-'0' ;
			}

		else if (isalpha(c)) {
			c_state = C_GCMD ;
			if (++c_nval >= MAX_IVALS) --c_nval ;
			c_cmd[0] = c ;
			}

		else if (isspace(c) || c == ',') {
			c_state = C_SKIP ;
			if (++c_nval >= MAX_IVALS) --c_nval ;
			}
		break ;

	case C_SKIP:
		if (isalpha(c)) {
			c_state = C_GCMD ;
			c_cmd[0] = c ;
			}

		else if (isdigit(c)) {
			c_state = C_NUMB ;
			c_ivals[c_nval] = c_ivals[c_nval] * 10 + c-'0' ;
			}

		else if (isspace(c) || c == ',') ;
		break ;

	case C_GCMD:
		c_state = C_NEW ;
		if (isalpha(c)) {	/* go and execute */
			c_cmd[1] = c ;
			c_cmd[2] = 0 ;

			want_more = F ;	/* need to do it */
			}

		/* else BAD command, just skip it all */
		break ;
		}

	return want_more ;
	}/*of function gat_input() */


/* macro to decide if two commands are equal */
#define EQ(A,B) (A[0] == B[0] && A[1] == B[1])

/* "do_comd": Exec the current command (if it is one) */
do_comd()
	{

	/* if done never do anything more */
	if (c_done || (!c_initd && !EQ("IN", c_cmd)) ) return ;

	if (c_haderr) {
		/* commands that want to write() */
		if (EQ("SY",c_cmd) || EQ("WH",c_cmd) ||
		    EQ("SZ",c_cmd) || EQ("VE",c_cmd)) {
			char*	junk = "1 1 1 1 1 1 1\n" ;
			write(STDOUT, junk, strlen(junk)) ;
			}
		return ;
		}

	/* Leave W_seg and W_draw first */
	if (EQ("SG", c_cmd)) {	/* line segment */
		W_seg(c_ivals[0],c_ivals[1],c_ivals[2],c_ivals[3]) ;
		}

	else if (EQ("DR", c_cmd)) {	/* Draw to a point */
		W_draw( c_ivals[0], c_ivals[1] ) ;
		}

	else if (EQ("MV", c_cmd)) {	/* Move to a point */
		W_move( c_ivals[0], c_ivals[1] ) ;
		}

	/* Area definition and fill */
	else if (EQ("AP", c_cmd) || EQ("AB", c_cmd) ||
	  	 EQ("AE", c_cmd) || EQ("AF", c_cmd) ) {
		W_area(c_ivals[0], c_ivals[1], c_cmd[1]) ;
		}

	else if (EQ("CL", c_cmd)) {	/* clear the screen */
		W_clear() ;
		}

	else if (EQ("IN", c_cmd)) {	/* Init things for plotting */
		int W_begin() ;
		c_initd = T ;
		slurp_buf(1024) ;	/* TEST suntools buffering? */
		if (!W_begin(c_ivals[0], c_ivals[1]))
			c_haderr = T ;
		}

	else if (EQ("EN", c_cmd)) {	/* END of plotting */
		c_done = T ;	/* never do work again! */
		W_end() ;
		}
	
	else if (EQ("SY", c_cmd)) {	/* Sync up plotting */
		W_sync() ;
		write(STDOUT," 1\n",3) ;	/* let them know it is sync'd */
		}
	
	else if (EQ("LN", c_cmd)) {	/* New Line type */
		W_line(c_ivals[0]) ;
		}

	else if (EQ("PE", c_cmd)) {	/* New Line type */
		W_pen(c_ivals[0]) ;
		}

	else if (EQ("DM", c_cmd)) {	/* New Line type */
		W_dmode(c_ivals[0]) ;
		}

	else if (EQ("WH", c_cmd)) {	/* Where is the curor? */
		int	i, j, ch ;
		char	buf[80] ;

		W_cloc(&i, &j, &ch) ;
		sprintf(buf, "%d %d %d\n", i, j, ch) ;
		write(STDOUT, buf, strlen(buf)) ;
		}

	else if (EQ("SZ", c_cmd)) {	/* Acutal window size */
		int	i, j, st;
		char	buf[80] ;

		W_size(&i, &j, &st) ;
		sprintf(buf, "%d %d %d\n", i, j, st) ;
		write(STDOUT, buf, strlen(buf)) ;
		}

	else if (EQ("VE", c_cmd)) {	/* return what version this is */
		char	vbuf[80] ;
		sprintf(vbuf," %s\n",version) ;
		write(STDOUT,vbuf,strlen(vbuf)) ;
		}

	/* Not Valid for any device.... */
#ifdef DEBUG
	else fprintf(stderr,"[%c%c] Unknown command\n",c_cmd[0],c_cmd[1]);
#endif
	}
