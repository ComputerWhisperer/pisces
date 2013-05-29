/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/*
 * Tue Oct  3 15:04:28 PDT 1989 (dredge--stanford)
 *ff
 * "mksym" : reads a file containing symbol definitions and creates
 * 	an include file that defines the symbol arrays.
 *
 * usage:
 *	mksym [-o outfile] [-l veclocnam] [-v vectsnam] [-n structnam]
 *		[-s] [-p] [-P] [deffile]
 *
 * where:
 *	-o	- Output file name (default 'symb.h')
 *	-l	- Name of vect locations array (default: 'SymLocs')
 *	-v	- Name of vector array (default: 'SymVecs')
 *	-n	- Name of symbol definitions structure (default: 'SymStrc').
 *	-s	- Process the definitions file, but no output (show).
 *	-p	- Plot each defined character.
 *	-P	- Plot the full table of characters (definded of not).
 *	deffile - definitions file. [default: standard input]
 *
 * notes:
 *	> To compile this program before the plot library is available,
 *	  define 'NOPLOT'.  This gives a version that can generate the
 *	  symbol definitions include files, but hasn't the capability
 *	  to plot the results for review.
 *	> This version generates an include file for C programs.
 *	> The maximum cell size for this version is 10 x 10, default
 *	   cell is 7 x 7 (w/ 3 distender).
 *of
 * written: Michael Eldredge (oct 83)
 * mod    : MJE (nov 84)
 * mod    : MJE (dec 85) Make created arrays static so symbol is not
 *	open to every one.
 * mod    : MJE (dec 86) update library calls.
 * modified:MJE (oct 89) the struct def was "static struct ... { ...} ;"
 *	the "static" is not needed and made gcc complain.
 *
 * more notes:
 *	1. There are some more commands that can be used, but we don't 
 *	   need to tell anybody about them.  They are:
 *		-d plotdevice
 *		-W <char width>
 *		-H <char height>
 *		-D <distender size>
 *
 * 	> Complilation:
 *		Usual case (when the 'gplot' library is around):
 *			cc mksym.c symbl2.o -lgplot -lm  -o mksym
 *				-I/usr/include/local  -- maybe needed too.
 *
 *		Before the 'gplot' library is available:
 *			cc -DNOPLOT mksym.c -o mksym
 */


#include <stdio.h>

#ifndef NOPLOT
#   include "gplot.h"
#endif

/* maximum x,y (i,j h,w) values ever must be less than the following.
 *  Because of encoding scheme used...
 */
#define ABS_MAX_I  10
#define ABS_MAX_J  10

/* End Of Vector list */
#define EOV        255

/* maximum number of vectors that will be encountered for all symbols */
#define MAX_VECTS 2000

/* default values for h,w,d */
#define DEF_H 7
#define DEF_W 7
#define DEF_D 3

#define T 1
#define F 0

#define strequ(A,B)  (strcmp(A,B)==0)

/* some global values */
int    Hval = DEF_H ;      /* height of a symbol */
int    Wval = DEF_W ;      /* symbol width */
int    Dval = DEF_D ;      /* symbol distender size */
int    maxI , maxJ  ;

short  vecpnt[128], curpnt, vpnt;  /* vector location array */
short  counts[128];
char   vects[MAX_VECTS];           /* symbol vector list */

char   *namlocs = "SymLocs" ;       /* Location array name */
char   *namvect = "SymVecs" ;       /* Symbol vector array name */
char   *namstrc = "SymStrc" ;       /* Default structure name */


/* "mksym" : The main program. */
main(argc, argv)
	int   argc;
	char  *argv[];
	{

	char   *ofile   = "symb.h" ;        /* final output file */
	char   *defsfil = "" ;              /* Definitions file */
	char   *plotdev = "" ;              /* Plot device */

	FILE   *fpdefs;                     /* for the defs file */

	int    show   = F;
	int    plotab = 0 ;                 /* Plot table to be output ? */
	int    iret    ;                    /* Return from functions */
	char   *errmes ; 

	/* ------- start ------- */

	/* step through the argument list... */
	errmes = "" ;     /* no error yet */

	while (--argc) {
		argv++ ;    /* past the program name */

		if      (strequ(*argv, "-o")) {  /* new output file name */
			if (--argc) ofile = *++argv;
			else errmes = "no output file given\n";
			}
		else if (strequ(*argv,"-l")) {  /* vector location array name */
			if (--argc) namlocs = *++argv ;
			else errmes = "no vector location array name given\n";
			}
		else if (strequ(*argv,"-v")) { /* vector list array name */
			if (--argc) namvect = *++argv;
			else errmes = "no vector list array name given\n" ;
			}
		else if (strequ(*argv,"-n")) { /* structure name */
			if (--argc) namstrc = *++argv;
			else errmes = "no structure name given\n";
			}
		/**/else if (strequ(*argv,"-d")) { /* output plot device */	
			if (--argc) plotdev = *++argv;
			else errmes = "no plot device given\n";
			}
		/**/else if (strequ(*argv,"-W")) { /* width of characters */
			if (--argc) {
				if (!sscanf(*++argv,"%d", &Wval)) 
					errmes = "non-numeric value for -w\n";
				}
			else errmes = "no character width given \n";
			}
		/**/else if (strequ(*argv,"-H")) { /* character height */
			if (--argc) {
				if (!sscanf(*++argv,"%d", &Hval))
					errmes = "non-numeric value for -h\n";
				}
			else errmes = "no character height given\n";
			}
		/**/else if (strequ(*argv,"-D")) { /* distender count */
			if (--argc) {	
				if (!sscanf(*++argv,"%d", &Dval))
					errmes = "non-numeric value for -d\n";
				}
			else errmes = "no character distender size given\n";
			}
		else if (strequ(*argv,"-s")) show = T;  /* show only */

		else if (strequ(*argv,"-p") && !plotab) plotab = 1;

		else if (strequ(*argv,"-P")) plotab = 2;

		else { /* definitions file name */
			if (! *defsfil) defsfil = *argv;
			else errmes = "only one defintions file allowed\n";
			}

		/* see if there was an error, print it and quit */
		if (*errmes) {
			fprintf(stderr, errmes);
			exit(1);
			}
		}/* of while arguments loop */

	maxI = Wval;
	maxJ = Hval + Dval;

	/* Here we start the real work .... */

	/* open the definitions file */
	if (*defsfil) {	
		if ((fpdefs = fopen(defsfil,"r")) != NULL) { /* got a file */
			iret = mk_sym(fpdefs);
			fclose(fpdefs);
			}

		else {  /* error on open */
			fprintf(stderr,"Can't open definitions file `%s'\n",defsfil);
			exit(1);
			}
		}
		
	else  iret = mk_sym(stdin);      /* no file, use standard input */

	if (iret != 0) exit(1);  /* error in making symbols */

	/* if they want an output file, make one */
	if (!show) out_defs(ofile); 

#ifndef NOPLOT
	/* see if they want results plotted, if so plot'em */
	plot_defs(plotdev, plotab);
#else
	/* we can't plot so let them know.... */
	if (*plotdev || plotab != 0) {
		fprintf(stderr,
  "Warning: mksym was not compiled to plot the finished symbols. Sorry.\n");
		}
#endif

	}/*of main */

/* "mk_sym" : Hash up the definitions file and make the vectors etc... */
int
mk_sym(fp)
	FILE *fp;
	{
	char   cmd[80], nstr[10];

	int    move;
	int    didlast, inbadone, usechar, doneany;
	char   curcmd[4];
	int    i1,j1, ival, iloc, oloc, val;
	char   cloc;
	int    ch, ierr;

	int    i, v;

	/* start of mk_sym */
	ierr = 0 ;    /* no errors so far */

	/* make everything point, for now, to the first set of vectors
	 *  this is a null draw, in case the character is not defined.
	 */
	curpnt = 0;
	for (i=0; i<128; i++) {
		vecpnt[i] = curpnt;         /* set each to null draw */
		counts[i] = 0;              /* to know if redefinition */
		}

	move = 1;
	inbadone = 0;
	doneany  = F;     /* haven't done any symbols yet */
	
	vects[curpnt++] = EOV;              /* all undefined symbols will */
	vpnt = curpnt;                      /*  point here, a null draw   */
	didlast = 1;
	
	for (;;)  /* loop through the file of definitions */
		{	
		if (fscanf(fp, "%s", cmd) == EOF) break;

		if (cmd[0] == '#') { /* a comment, pooch to EOLN */
			while ( ((ch=getc(fp)) != EOF) && ch != '\n') 
				ch = ch;
			continue;
			}

		usechar = 0;

		switch (*cmd)
			{

		/* Iff haven't done any work yet, can reset w,h or d */
		case 'W':
		case 'H':
		case 'D':
			if (fscanf(fp,"%d", &val)==EOF) break;
			if (doneany) {
				fprintf(stderr,
	"can't reset character sizes after definitions have begun\n");
				continue;
				}

			/* just make the assignment, check validity later */
			if      (*cmd == 'W') Wval = val;
			else if (*cmd == 'H') Hval = val;
			else if (*cmd == 'D') Dval = val;
			break;
			
		/* define a character */
		case 'u':
			usechar = 1;     /* get location to take vects from */
		case 'c':
			inbadone = 0;    /* starting a new character def */

			/* if first time through, check some values */
			if (!doneany) {
				doneany = T;

				maxI = Wval;
				maxJ = Hval + Dval;

				if (maxI < 0 || maxI > ABS_MAX_I) {
					fprintf(stderr,
"Character width must be between 0 and %d\n", ABS_MAX_I);
					ierr = -1;
					break;
					}

				if (maxJ < 0 || maxJ > ABS_MAX_J) {
					fprintf(stderr,
"Character height plus distender size must be between 0 and %d\n", ABS_MAX_J);
					ierr = -1;
					break;
					}

				}/*of values check */

			if (fscanf(fp,"%s", nstr) == EOF) break;
			if (!usechar) printf(" char[%-3s]",nstr);

			iloc = -1;
			if (nstr[0]=='\'' && nstr[2]=='\'') iloc = nstr[1];

			else if (sscanf(nstr, "%d", &iloc)) ;

			else       /* never got a char location */
			   	{
				fprintf(stderr," bad value(%s) for 'c'\n", nstr);
			   	inbadone = 1;
			   	}

			if (iloc > 127)
				{
				printf(" error: location > 127\n");
				inbadone = 1;
				}
			else if (usechar)     /* use vects of other char */
				{
				i = vecpnt[iloc];
				while ( (v=(vects[i++] & 0377)) !=EOV)
					vects[vpnt++] = v;
				}

			else
				{
				if (!didlast)
					{
					vects[vpnt++] = EOV;

					vecpnt[oloc] = curpnt;
					curpnt = vpnt;

					didlast = 1;
					}
				oloc = iloc;
				move = 1;
				}

			break;

		case 'm': 
			if (inbadone) break;
			move = 1;
		case 'd':
			if (!doneany) {
				fprintf(stderr,
	"Move or Draw given before a character location ('c' command)\n");
				inbadone = T;
				}
					
			if (inbadone) break;
			if (fscanf(fp,"%d %d",&i1, &j1) == EOF) break;

			if (i1 <0 || i1 >= maxI)
				{
				printf(" error I out of range\n");
				inbadone = 1;
				}
			if (j1 <0 || j1 >= maxJ)
				{
				printf(" error J out of range\n");
				inbadone = 1;
				}

			ival = i1 + (10 * j1);  /* shift up J */
			if (move) ival += 100;  /* flag a move */

			vects[vpnt++] = ival;
			move = 0;
			didlast = 0;
			break;

			
		default:
			printf(" unknown command '%s'\n", cmd);
			break;
			}/* of switch */

		if (ierr != 0) break;   /* oops, some errors were found */

		}/* of for loop */

	/* terminate the last vector and mark the last characters pointer */
	if (doneany) {
		vects[vpnt] = EOV;   
        	vecpnt[oloc] = curpnt;
		}

	return(ierr);
	}/*of mk_sym */

/* "out_defs" : output the symbol locations and vectors arrays */
int
out_defs(namr)
	char *namr;
	{

	FILE *fp;
	int  i;

	if ((fp = fopen(namr, "w")) == NULL) {
		fprintf(stderr,"can't open output file `%s'\n", namr);
		return(-1);
		}

	/* print out the vector array into the soon to be include file */
        /* 'vpnt' is size used of VECTS */

printf("\n Number of vectors defined (size, in bytes, of VECTS array) is %d\n",
									vpnt);


	/* OUTPUT array definitions to the output file */
fprintf(fp,"/* Vector array and vector locations for the 'symbol' routine.\n");
fprintf(fp," *  This file was generated by 'mksym'.\n");
fprintf(fp," *  Number of vectors (size of VECTS) = %d bytes\n",vpnt);
fprintf(fp," */\n");

fprintf(fp,"\n");

fprintf(fp,
  "/* First: Define a structure to hold all the symbol drawing information\n");
fprintf(fp," *  This contains a pointer to to the array of vector locations\n");
fprintf(fp," *  for each symbol, a pointer to the array of vectors, and the\n");
fprintf(fp,
     " *  three sizes of symbols herein defined (width, height, distender).\n");
fprintf(fp," */\n");
fprintf(fp,
     "struct _symb2 {   /* to hold information about symbol defs */\n");
fprintf(fp,"	short  *sym_locs;    /* each symbols vector list location*/\n");
fprintf(fp,"	char   *sym_vect;    /* vector list for all symbols */\n");
fprintf(fp,"	int     sym_hit ;    /* symbol height         */\n");
fprintf(fp,"	int     sym_wid ;    /* symbol width          */\n");
fprintf(fp,"	int     sym_dis ;    /* symbol distender size */\n");
fprintf(fp,"	};\n\n");


	/* THE VECTOR LIST ARRAY */
fprintf(fp, "/* Vector list array for all symbols for 'symb2' */ \n");
fprintf(fp, "static char %s[] = {", namvect);  /* !! use given name !! */

for (i=0; i<vpnt; i++) 
	fprintf(fp, (i%15 == 0) ? "\n     %3d," : " %3d,", vects[i]&0377);
fprintf(fp, "    %3d };\n\n", vects[vpnt]&0377 );


   	/* OUTPUT THE VECTOR LOCATION POINTERS */
fprintf(fp, "static short %s[128] = { ", namlocs);
for (i=0; i<127; i++)
    fprintf(fp, (i % 10) ? " %5d," : "\n    %5d,",vecpnt[i]);
fprintf(fp, " %5d", vecpnt[127]);
fprintf(fp, " };\n\n");

	/* CREATE A _SYMB2 STRUCTURE TO HOLD THE NEW INFORMATION */
fprintf(fp,"/* This is the structure for the above defined symbols */\n");
fprintf(fp,"static struct _symb2 %s = {\n", namstrc);
fprintf(fp,"	%s ,	/* the locations array   */\n", namlocs);
fprintf(fp,"	%s , 	/* the vector list array */\n", namvect);
fprintf(fp,"	   %d ,	/* symbol height         */\n", Hval   );
fprintf(fp,"	   %d ,	/* symbol width          */\n", Wval   );
fprintf(fp,"	   %d ,	/* symbol distender size */\n", Dval   );
fprintf(fp,"	};\n\n");

fprintf(fp,"/* end of symbol vectors */\n");

	fclose(fp);    /* close up the definitions file */
	}/*of OUT_DEFS */


#ifndef NOPLOT

/* "plot_defs" : plot the definitions if so desired */
plot_defs(pdev, ptab)
	char  *pdev;
	int    ptab;
	{

#define boarder   .01
#define labhite   .08
#define labspc    .02

#define XMIN      boarder
#define YMIN      boarder 

#define box       .52
#define boxspc    .1

	float   cx, cy;
	float   x,  y ;
	float   xmax, ymax ;	/* get these from gplot */
	int     pen;
	unsigned short v;
	int     vpnt;
	char    *vp;

	char    label[40];
	int     len, i;
	char    c;

	float   Xinc, Yinc;
	float   size, Hs, Ds, Ws, Js;
	char    junk[10];

	int	iv[8] ;
	float	fv[8] ;

	if (!ptab) return;     /* no need to plot */

	size = Hval + Dval;
	if (Wval > size) Wval = size;
	size-- ;

	Ds = (float)Dval / size * box;   /* set up physical size of box */
	Hs = (float)Hval / size * box;
	Js = (float)maxJ / size * box;
	Ws = (float)Wval / size * box;

	Xinc = Ws + boxspc;
	Yinc = Js + labhite + labspc + boxspc;


	/* init plottting */
	if (*pdev) setpdev(pdev);
	gclear();

	/* find out how big the plot device is */
	gpmisc(G_PSIZE, 0, iv, fv, "");
	xmax = fv[0] ;
	ymax = fv[1] ;

	xmax  =   xmax -  boarder ;
	ymax  =   ymax -  boarder - labhite - labspc ;

	cx = XMIN;
	cy = ymax - box - labhite - labspc;

	for(i=0; i<128; i++) {
		/* if ptab == 1 only do non-null symbols */
		if (ptab == 1 && (((unsigned)vects[vecpnt[i]] & 0377) == EOV))
			continue;

		c = (char) i;
		if (c < ' ' || c > '~') c = ' ';

		sprintf(label, "%c:%3d", c, i);
		len = strlen(label);


		/* print the name of the character as above */
		/*symb2(cx, cy+box+labspc, label, len, labhite, 0., 0., 0.);*/
		symbl2(cx, cy+box+labspc, label, len, labhite, labhite,
				0., 0., 0.);

		/* draw the outline of the box that the char sits in */
		gnline(11);

		gmove(cx        , cy       ); /* full box */
		gdraw(cx        , cy + Js  );
		gdraw(cx + Ws   , cy + Js  );
		gdraw(cx + Ws   , cy       );
		gdraw(cx        , cy       );

		gmove(cx        , cy + Ds  ); /* distender mark */
		gdraw(cx + Ws   , cy + Ds  );

		/*
		gmove(cx        , cy + b6  );
		gdraw(cx + b6   , cy + b6  );

		gmove(cx + b3   , cy + b9  );
		gdraw(cx + b3   , cy + b3  );
		*/

		gnline(1);


		/* plot the character inside the box */
		vpnt = vecpnt[i];

		while ( (v = (unsigned)vects[vpnt++] & 0377) != EOV)
			{
			if (v > 100) { pen = G_MOVE; v -= 100; }
			else         { pen = G_DRAW;           }

			/* 10 is max point size, 9 is one less (to get ratio)*/
			x =  (float)(v % 10) / 9.0 * box  + cx;
			y =  (float)(v / 10) / 9.0 * box  + cy;

			if (1) gplot2(pen, 0, x, y);
			if (0) printf(" (%.3g,%.3g) w/ %d\n", x, y, pen);
			}

		/* advance the current (x,y) */
		cx += Xinc;
		if ( (cx+box) > xmax)
			{
			cx = XMIN;
			cy -= Yinc;
			}
		}

	gpend();

	}/* of plot_defs */
#endif
