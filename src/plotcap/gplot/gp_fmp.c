/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/* Need the definitions for open flags */
#include <fcntl.h>
#include <stdlib.h>
#include "gplot.h"


/* "gp_fmp.c" : routines to read/write from/to a file with binary saves from
 *	gplot.
 * written:  Michael Eldredge
 * modified: MJE (dec 86) Too many writes, need to buffer calls for speed.
 *	We could just use the stdio code (fdopen(), fwrite(), and fread()) 
 *	instead of writing our own buffering code.  However, the plot library
 *	has (almost) no depenency on any of the stdio functions- thus they
 *	don't need to be loaded.  So we wrote some simple buffering routines.
 *	[[ The only place the stdio funtions are used is a) for debugging, b)
 *	in the pgetXXX() functions ( these can be replaced with atoi(), atox()
 *	etc....) c) in disXX() (a debegging function), d) mkuniq() to make
 *	a temp name and e) pg_erset() to print error messages - which could be
 *	replaced with whatever routine you like. ]]
 */

/* some integers to tag the file.  This is by no means fool proof, but if
 *  it is going to get beat, then what the hey.
 */
#define GPLOT_FILE_CODE 0x0307
#define GPLOT_DATE_CODE 0x0905

#define BUF_SIZE	1024		/* buffer i/o calls */
#define N_FILES		 32		/* max possible open files */

/* define the record structure for raw gplot files. */
struct _gp_record {
	int    cmd ;
	int    sub ;
	float  xval;
	float  yval;
	} ;
#define DEFSAVSZ (sizeof(struct _gp_record) )

/* Flags for io_flg */
#define FL_RD	0x0		/* not-writing */
#define FL_WR	0x2		/* writing? */
#define	FL_BF	0x4		/* buffered? */

/* buffers for open files... */
struct _iobufs {
	char*	io_buf ;		/* beginning */
	char*	io_p ;			/* current location */
	short	io_cnt ;		/* number of bytes around */
	char	io_flg ;		/* flags FL_xxx */
	} ;

static 	struct _iobufs	iobufs[N_FILES] ;
static	int	inited = 0 ;		/* setup yet... */

static int GPread(int lu, register char *p, register int cnt);
static int GPwrite(int lu, register char *p, int cnt);
static void GP_init(void);
static int GPmakbuf(int lu, int size);
/* "gpread" : read a single gplot record from the file opened with gpopen()
 */
#ifdef ANSI_FUNC

int gpread(int lu, int *cmd, int *sub, float *xval, float *yval)
#else

int 
gpread (lu, cmd, sub, xval, yval)
int lu;
int *cmd;
int *sub;
float *xval;
float *yval;
#endif
	{
	struct _gp_record gp ;
	int   n;

	/*NOSTRICT*/
	if ((n=GPread(lu, (char *)&gp, DEFSAVSZ)) != DEFSAVSZ) 
		return( (n<=0 ? n : -1) );

	*cmd  = gp.cmd ;
	*sub  = gp.sub ;
	*xval = gp.xval;
	*yval = gp.yval;

	return(1);
	}


/* "gpwrite" : write one gplot record to the file opened with gpopen()
 */
#ifdef ANSI_FUNC

int gpwrite(int lu, int cmd, int sub, double x, double y)
#else

int 
gpwrite (lu, cmd, sub, x, y)
int lu;
int cmd;
int sub;
double x;
double y;
#endif
	{
	struct _gp_record gp ;
	
	gp.cmd  = cmd;
	gp.sub  = sub;
	gp.xval = x  ;
	gp.yval = y  ;    

	/*NOSTRICT*/
	if (GPwrite(lu, (char *)&gp, DEFSAVSZ) != DEFSAVSZ) return(-1);
	return(1);
	}

/* "gpopen" : Open a file to write/read gplot records.  This function makes
 *	sure that the first record of the file is a header record to identify
 *	the file as a gplot save format file.
 */
#ifdef ANSI_FUNC

int gpopen(char *namr, int mode)
#else

int 
gpopen (namr, mode)
char *namr;
int mode;
#endif
	{

	int	lu ;
	int	fc, dc ;
	float	r1, r2 ;
	int	how, err ;
	short	is_write = (mode & GPO_WR) ;

	/* make sure all flags are zero-ed!! */
	if (!inited) GP_init() ;

	if (is_write) {
		how  = (mode & GPO_RD? O_RDWR  : O_WRONLY) | O_CREAT ;
		how |= (mode & GPO_AP? O_APPEND: O_TRUNC) ;

		lu = open(namr, how, 0666) ; /*newer open (don't need creat)*/
		if (lu < 0) return -1 ;

		iobufs[lu].io_flg = FL_WR ;	/* init this */

		/* write in the first record something special */
		if (mode&GPO_HD) {
			if (mode&GPO_AP) {
				lseek(lu, 0, 0) ;	/* rewind */
				if (gpread(lu, &fc,&dc,&r1,&r2) > 0) {
					err = (fc != GPLOT_FILE_CODE) ||
					      (dc != GPLOT_DATE_CODE) ;
					if (!err) lseek(lu, 0,2); /* eof */
					}
				else	/* the file was empty */
					err = (gpwrhed(lu) <= 0) ;
				}

			else {	/* no append... */
				err = (gpwrhed(lu) <= 0) ;
				}

			if (err) {	/* an earlier error? */
				close(lu) ;
				return -1 ;
				}
			}
		}

	else  {
		how  = O_RDONLY ;
		if ((lu = open(namr, how)) < 0) return(-1);

		iobufs[lu].io_flg = FL_RD ;	/* init this */

		/* check that this is a gplot save file */
		if (mode&GPO_HD && 
				(gpread(lu, &fc, &dc, &r1, &r2) <= 0) || 
				    fc != GPLOT_FILE_CODE  ||
				    dc != GPLOT_DATE_CODE )    {
			close(lu);
			return(-2);
			}
		}

	return(lu);
	}

/* "gpclose" : 	Post buffers and close up.
 */
#ifdef ANSI_FUNC

int gpclose(int lu)
#else

int 
gpclose (lu)
int lu;
#endif
	{
	(void)GPmakbuf(lu, -1) ;	/* flush and free the space */
	close(lu);
	}

/* "gpwrhed" : write the header record... */
	/* not static, gpwork() calls directly... */
#ifdef ANSI_FUNC

int gpwrhed(int lu)
#else

int 
gpwrhed (lu)
int lu;
#endif
	{

	return (gpwrite(lu, GPLOT_FILE_CODE, GPLOT_DATE_CODE, 0., 0.) ) ;
	}

/* ========================================================================= */
/* init the io-buf structs... */
#ifdef ANSI_FUNC

static void GP_init(void)
#else

static void 
GP_init ()
#endif
{
	int	i ;

	inited = 1 ;

	/* make sure all flags are zero-ed!! */
	for (i = 0 ; i < N_FILES; ) iobufs[i++].io_flg = 0 ;
	}


/* GPmakbuf: establish a buffered stream.. */
#ifdef ANSI_FUNC

static int GPmakbuf(int lu, int size)
#else

static int 
GPmakbuf (lu, size)
int lu;
int size;
#endif
	{

	struct _iobufs*	iob  ;

	/* make sure all flags are zero-ed!! */
	if (!inited) GP_init() ;

	if (lu < 0 || lu >= N_FILES) return 0 ;	/* failed */

	iob = &iobufs[lu] ;

	/* Close */
	if (size <= 0) {
		if (iob->io_flg & FL_BF) {
			if (iob->io_cnt > 0 && iob->io_flg&FL_WR)/* FLUSH */
				write(lu, iob->io_buf, iob->io_cnt) ;

			(void)free( iob->io_buf ) ;
			}
		iob->io_flg = 0 ;
		return 1 ;
		}

	/* Open */
	iob->io_buf = malloc((unsigned)size) ;
	if (!iob->io_buf) return 0 ;

	iob->io_p   = iob->io_buf ;
	iob->io_cnt = 0 ;
	iob->io_flg |= FL_BF ;	/* mode set by gpopen() call */

	return 1 ;
	}

/* "GPread": return the number of bytes requested (if possible) */
#ifdef ANSI_FUNC

static int GPread(int lu, register char *p, register int cnt)
#else

static int 
GPread (lu, p, cnt)
int lu;
register char *p;
register int cnt;
#endif
	{

	int	n, save_cnt = cnt ;
	register struct _iobufs*	iob ;
	if (lu < 0 || lu >= N_FILES) return -1 ;
	iob = &iobufs[lu] ;

	/* make sure buffering is ON */
	if (!(iob->io_flg & FL_BF) && !GPmakbuf(lu, BUF_SIZE))
		return -1 ;	/* couldn't buffer... */

	/* Fill the buffer */
    fill_up:
	if (iob->io_cnt <= 0) {
		n = read(lu, iob->io_buf, BUF_SIZE) ;
		if (n <= 0) return ((n = save_cnt-cnt) > 0? n: -1) ;

		iob->io_p   = iob->io_buf ;
		iob->io_cnt = n ;
		}

	/* fill the user buffer from internal buffer */
	for ( ; cnt > 0 && iob->io_cnt > 0; --cnt, --iob->io_cnt)
		*p++ = *iob->io_p++ ;

	/* Done Or Ran out buf bytes...? */
	if (cnt > 0) goto fill_up ;

	return save_cnt - cnt ;
	}

#ifdef ANSI_FUNC

static int GPwrite(int lu, register char *p, int cnt)
#else

static int 
GPwrite (lu, p, cnt)
int lu;
register char *p;
int cnt;
#endif
	{

	int	n, save_cnt = cnt ;
	register struct _iobufs* iob ;
	if (lu < 0 || lu >= N_FILES) return -1 ;
	iob = &iobufs[lu] ;

	/* make sure buffering is ON */
	if (!(iob->io_flg & FL_BF) && !GPmakbuf(lu, BUF_SIZE))
		return -1 ;	/* couldn't buffer... */

	for ( ; cnt-- > 0 ; iob->io_cnt++) {
		if (iob->io_cnt == BUF_SIZE) {	/* FULL ? */
			n = write(lu, iob->io_buf, BUF_SIZE) ;
			if (n != BUF_SIZE) return -2 ;

			iob->io_cnt = 0 ;
			iob->io_p   = iob->io_buf ;
			}

		*iob->io_p++ = *p++ ;
		}

	return save_cnt ;
	}

#ifdef TEST_WITH_MAIN
/* <<<<<<<<<<<<<TESTING WITH A MAIN>>>>>>>>>>> */
#include <stdio.h>

#ifdef ANSI_FUNC

int 
main (int argc, char **argv)
#else

main(argc, argv)
	char**	argv ;
#endif
	{
	int	lus, lud ;
	char*	p = argv[0] ;
	char*	s = argv[1] ;
	char*	d = argv[2] ;
	int	cmd, sub ;
	float	x, y ;


	if (argc != 3) {
		fprintf(stderr,"Usage: %s src dest\n", p) ;
		exit(2) ;
		}

	lus = gpopen(s, 0) ;	/* read from src */
	if (lus < 0) {
		fprintf(stderr,"%s: can't open source '%s'\n",  p, s ) ;
		exit(1) ;
		}

	lud = gpopen(d, 1) ;	/* write to dest */
	if (lud < 0) {
		fprintf(stderr,"%s: can't open destination '%s'\n",  p, d ) ;
		exit(1) ;
		}

	while (gpread(lus, &cmd, &sub, &x, &y) > 0)
		if (gpwrite(lud, cmd, sub, x, y) <= 0) break ;

	gpclose(lus) ;
	gpclose(lud) ;
	}

#endif /*TEST_WITH_MAIN*/
