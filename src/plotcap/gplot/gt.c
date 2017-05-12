/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* "gt" : test program for GPLOT software
 * date: 20 jan 84
 * written: Michael Eldredge (jan 84)
 */


#include <stdio.h>
#include "auxfns.h"
#include "gp_alu12.h"
#include "gplot.h"
#include "gp_def.h"
#include "gp_com.h"

/* Commands other than actual GPLOT commands */
#define C_UNKN  -99
#define C_HELP	-98
#define C_QUIT	-97
#define C_TPLOT -96
#define C_LSEE  -95
#define C_HOWTO -94
#define C_LISTP -93
#define C_PDEV  -92
#define C_PFIL  -91
#define C_SFIL  -90
#define C_MAXSZ -89
#define C_WHERE -88
#define C_QQUIT -87
#define C_TRACE -86
#define C_PLOTG -85

struct _comd_list {
	char	 cl_cmd ;
	int	 cl_com ;
	char	*cl_des ;
	} *clp, comd_list[] = {
	{ 'q'	, C_QUIT   , "Quit"        } ,
	{ 'Q'	, C_QQUIT  , "Quit no PEND"},
	{ '?'	, C_HELP   , "Help (this)" } ,
	{ 'a'	, G_ANGLE  , "angle"       } ,
	{ 'c'	, G_CLEAR  , "clear"       } ,
	{ 'd'	, G_DRAW   , "DRAW"        } ,
	{ 'D'	, C_PDEV   , "Plot device" } ,
	{ 'e'	, G_PEND   , "pend"        } ,
	{ 'f'	, C_TPLOT  , "tplot file"  } ,
	{ 'F'	, C_PFIL   , "Plot file"   } ,
	{ 'g'	, C_PLOTG  , "Gplot file"  } ,
	{ 'h'	, C_HOWTO  , "HowTo"       } ,
	{ 'l'	, C_LSEE   , "let's see"   } ,
	{ 'L'	, C_LISTP  , "List prog"   } ,
	{ 'm'	, G_MOVE   , "MOVE"        } ,
	{ 'M'	, C_MAXSZ  , "Max sizes"   } ,
	{ 'r'	, G_ROTATE , "rotate"      } ,
	{ 's'	, G_SCALE  , "scale"       } ,
	{ 'S'	, C_SFIL   , "autosave"    } ,
	{ 't'	, G_TRANS  , "translate"   } ,
	{ 'T'	, C_TRACE  , "Trace mode"  } ,
	{ 'u'	, G_USR1   , "call USRx"   } ,
	{ 'w'	, G_CLOC   , "where cursor"} ,
	{ 'W'	, C_WHERE  , "cursor unit" } ,
	{ '\0'  }
	} ;

#ifdef ANSI_FUNC

int 
main (int argc, char **argv)
#else

main(argc, argv)
	int   argc;
	char  **argv;
#endif
	{

	int   com, sub, lu;
	float x, y, a;
	int   pen, howto;
	int	 n ;
	char  cmd, *cp;
	char  tfile[40];
	char  pdev[20];
	char  buf[40];
	char  *index();
	FILE  *fpjunk;
	int	ivect[20] ;
	float	fvect[20] ;
	int	ncloc = 0 ;


#ifdef DEBUG
	if (argc > 1)  bug_level = atoi(argv[1]);
	else           bug_level = 0;
	if (bug_level > 0)
		fpbug = fopen("gt.bug", "w");
	else
		fpbug = fopen("/dev/null", "w") ;
#endif

#ifdef NEVER
	if ((fpjunk = fopen("gt.pid", "w"))) {
		fprintf(fpjunk,"%d",  getpid() ) ;
		fclose(fpjunk) ;
		}
#endif

#ifdef DEBUG
	if (bug_level >= 9)
		fpjunk = fopen("gt.log" , "w");
#endif


	printf(" Size of C. %d\n", sizeof(C));
	printf("dev [def: '.']: ");
	scanf("%s", pdev);
	if (pdev[0] == '.') pdev[0] = '\0' ;

	if (strlen(pdev) >0)   {
		if (setpdev(pdev) < 0) {
			fprintf(stderr,"gt: device '%s' not found\n", pdev);
			exit(0);
			}
		
#ifdef DEBUG
		dis18(fpbug, g_prog, g_poff, g_idat ) ;
#endif
		}


	for(;;) {
		gplot2(G_GTOA, 0, 0.,0.) ;

		printf("com(`?') ");
		if (scanf("%1s", &cmd) < 0) break ;
		
		if (cmd == 'Q') break;  /* QUIT */
		if (cmd == 'q') {
			gplot2(G_PEND, 0, .0, .0) ;
			break ;
			}

		/* Look up the command */
		for (clp=comd_list; clp->cl_cmd; clp++)
			if (clp->cl_cmd == cmd) break ;
		
		com = (clp->cl_cmd ? clp->cl_com : C_UNKN) ;

		/* do the command */
		switch (com) {
		case C_HELP:
			for (clp=comd_list, n = 0; clp->cl_cmd; clp++) {
				if (! (n++ % 4)) printf("\t") ;
				printf(" %c: %-12s",clp->cl_cmd,clp->cl_des);
				if (! (n   % 4)) printf("\n") ;
				}
			printf("\n") ;
			break;

		case C_PLOTG:		/* Gplot save file */
	    	case C_TPLOT:		/* TPlot save file */
			printf("tplot file: ");
			scanf("%s", tfile);
			cp = tfile ;
			if (*cp == '!') cp++ ;

			if (com == C_TPLOT) lu=open(cp,0) ;
			else                lu = gpopen(cp,GPO_RD|GPO_HD) ;

			if (lu < 0) printf("can't open '%s'\n",cp);
			else {
				for (;;) {
					if (com == C_TPLOT)
						n = tp_read(lu, &x,&y,&pen) ;
					else	n = gpread(lu,&pen,&sub,&x,&y);
					if (n <= 0) break ;

					if (com == C_TPLOT) {
						sub = 0 ;
						if (pen < 0) {
							sub = -pen ;
							pen = G_LINE ;
							}
						}
#ifdef DEBUG
					if (bug_level >= 9) {
						fprintf(fpjunk,
							"%d %d %g %g\n",
								pen,sub,x,y);
						fflush(fpjunk);
						}
#endif

					gplot2(pen,sub,x,y);
					}
				if (tfile[0] == '!') {
					gplot2(G_PEND,0,.0,.0) ;
					exit() ;
					}
				close(lu);
				}
			break;

		    case G_DRAW:
		    case G_MOVE:
			printf(" (x,y): ");
			scanf("%f %f", &x, &y);
			gplot2(com,howto, x,y);
			break;

		    case G_SCALE:
		    case G_TRANS:
			printf(" (x,y): ");
			scanf("%f %f", &x, &y);
			gplot2(com,howto, x, y);
			break;

		    case G_ROTATE:
		    case G_ANGLE:
			printf(" (x,y,a): ");
			scanf("%f %f %f", &x, &y, &a);
			gplot2(G_ROTATE, 0, x,   y);
			gplot2(G_ANGLE , 0, a, 0.0);
			break;

		    case G_CLEAR:
			gplot2(G_CLEAR, 0, 0.,0.);
			break;

		    case G_CLOC:
			ivect[2] = ncloc ;
			gpgeti(G_CLOC, ivect, fvect) ;
			gplot2(G_GTOA,0, 0.,0.) ;
			cmd = ivect[2] ;
			printf(" Got: Key %d (0x%x),`%c',",
				ivect[2] , ivect[2], (char)ivect[2] ) ;
			printf(" i %d, j %d,  x %g, y %g\n",
				ivect[0], ivect[1], fvect[0], fvect[1]) ;
			break ;

		    case C_WHERE:
			printf(" Cloc unit: ") ;
			scanf("%d", &ncloc) ;
			break ;

		    case C_LSEE:
			/* letsee(stdout) ; */
			break;

		    case G_USR1:
			printf("1,2 s: ") ;
			scanf("%d %d", &com, &sub) ;
			if (com == 2) com = G_USR2 ;
			gplot2(com, sub, 0., 0.) ;
			break ;

		    case C_HOWTO:
			printf(" 0-incrm, 1-reset, 2-setto : ");
			scanf("%d", &howto);
			howto += 100;
			break;

		    case C_LISTP:
			dis18(stdout, g_prog, g_poff, g_idat) ;
			break ;

		    case C_TRACE:
			printf(" 0-unset, 1-set: ") ;
			scanf("%d", &howto) ;
			if (howto)	g_prog[0] |=  HEAD_TRACE ;
			else		g_prog[0] &= ~HEAD_TRACE ;
			break ;

		    case C_MAXSZ:
			printf(" Sizes: prog %d (%d), idat %d (%d)\n",
				g_poff, MAXPROG, g_ioff, MAXIDAT) ;
			break ;

		    case C_PFIL:
			printf(" output file: ");
			scanf("%s", buf);
			if (setpfil(buf) < 0) {
				printf("Error: can't set file `%s'\n", buf);
				}
			break;

		    case C_PDEV:
			printf(" plot device: ");
			scanf("%s", buf);
			if (setpdev(buf) < 0) {
				printf("Error: can't set device `%s'\n", buf);
				}
			break;

		    case C_SFIL:
			printf(" autosave file: ");
			scanf("%s", buf);
			if (setsfil(buf) < 0) {
				printf("Error: trying to set autosave file\n");
				}
			break;


		    case C_UNKN:
			printf(" c,s,x,y: ");
			scanf("%d %d %f %f", &com, &sub, &x, &y);
			gplot2(com,sub , x, y);
			break;

		    default:
			gplot2(com, 0, x, y);
			break;
		    }/* of switch */
		}/* of for loop */

#ifdef DEBUG
	fclose(fpbug);
#endif

	}


#ifdef ANSI_FUNC

int 
prP (ipoint2d *P)
#else

prP(P)
	ipoint2d *P;
#endif
	{

	printf("(prP) P is (%d,%d)\n", P->x, P->y);
	}



/* "tp_read.c" : routine to do the absolute binary reads for tplot.  */
#ifdef ANSI_FUNC

int 
tp_read (int lu, float *x, float *y, int *pen)
#else

int tp_read(lu, x,y,pen) 
	int   lu;
	float *x, *y;
	int   *pen;
#endif
{

	char buf[20];
	char *bp;
	int  nch;

	/* ---start of tp_read--- */
	if ( (nch=read(lu, buf, 20)) <= 0) return (nch);

	bp = buf + 4;   /* skip the byte count stored in a long int */

	*x   = *(float *)bp;   bp += sizeof (float);
	*y   = *(float *)bp;   bp += sizeof (float);
	*pen = *(int   *)bp;

	return (nch);
}
