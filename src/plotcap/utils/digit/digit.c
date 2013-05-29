/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/***
*	"digit: digitizer program - uses gplot() interface.
*	Todd Atkins
*	Created: 7/15/85
*	Last Written: 3/16/87
***/

#include <ctype.h>
#include "gplot.h"
#include <math.h>
#include <stdio.h>

/* Warning definitions */
#define BADSWITCH   0
#define BADFILE 1
#define BADVALX 3
#define BADVALY 4
#define NOSETUP 5
#define BADDEV 6
#define NOVAL 7
#define EXSET 8

/* Command Definitions */
#define HELP "help"
#define SETUP "setup"
#define SHOW "show"
#define DIGITIZE "digitize"
#define DEVICE "device"
#define QUIT "quit"
#define APPENDFILE "append"
#define WRITEFILE "write"
#define FILLFILE "file"
#define XMIN "min.x"
#define XMAX "max.x"
#define YMIN "min.y"
#define YMAX "max.y"
#define XLOG "log.x"
#define YLOG "log.y"
#define XLIN "lin.x"
#define YLIN "lin.y"
#define GRAPH "graph"
#define VERBO "verbose"

/* macros */
#define abs(x) (((x) >= 0) ? (x) : -(x))    /* gives absolute value of x */
#define rtod(x) ((x)*180/PI)	/* changes x from radian to decimal */
#define dtor(x) ((x)*PI/180)    /* changes x from decimal to radian */
#define sqr(x)  ((x) * (x))     /* finds the square of x */
#define yesno(F) fprintf(stderr,"%s", ((F)? "YES" : "NO"))

/* Constants */
#define PI  3.1415
#define SWITCH '-'

/* Global variables */
FILE *OutFile, *fopen();

float Xst = 0.0,Yst = 0.0;
float MinX = 0.0,MaxX = 0.0,MinY = 0.0,MaxY = 0.0;
float I_lenx = 0.0,I_leny = 0.0;	/* input lengths */
float O_lenx, O_leny;			/* output lengths */
float Rotang;	/* angle used for grotat() */

char ComPrompt[40];   	/* command prompt */
char Fname[256], Mode;	/* file name and mode */
char *Devnam;		/* device name */

/* Flags */
int Xlog = 0,Ylog = 0;
int XM = 0, XMX = 0, YM = 0, YMX = 0;
int Done = 0;
int Setup = 0;
int InitAll = 1;
int Interact = 0;
int Verbose = 0;
int Graph = 0;

main(argc,argv)
int argc;
char *argv[];
{
	int ac;
	float offang();
	char *getenv();
	char *ap;
	char *scmd;
	char cmd[40];
	char cmd1[40], cmd2[40];
	char cmd_ct;

	sprintf(ComPrompt,"\n%s>",*argv);

	if (argc > 1)	/* get switches */
		while (--argc > 0) {
			ap = *++argv;
			if (*ap == SWITCH) {
				scmd = *argv;
				++scmd;
				ac = do_switch(scmd,argv);
			} else
				warn(BADSWITCH,*argv);
			if (ac) {
				argv += ac;
				argc -= ac;
			}
		}
	
	if (Devnam == '\0')   /* get device name */
		if ((Devnam = getenv("DEFPDEV")) == '\0')
			Devnam = getenv("TERM");


	if (Interact)   /* interact with user */
		while (!Done) {
			prompt(ComPrompt,cmd,30);
			cmd_ct = proc_command(cmd,cmd1,cmd2);
			(void) do_command(cmd1,cmd2,cmd_ct);
		}

	else 
		do_all();

	if  (OutFile != NULL)
		fclose(OutFile);

	gpend();
}

do_switch(cmd,argv)
/* interprets the switch in argv and processes the switch command */
char cmd[], *argv[];
{
	int argcount = 0;
	int cmd_ct = 2;
	char *cmd2;

	cmd2 = *++argv;
	if (cmd[0] == 'i')
		Interact = 1;
	else {
		if (cmd2 == '\0')
			cmd_ct = 1;

		else if (cmd2[0] == '-' && isalpha(cmd2[1]))
			cmd_ct = 1;

		argcount = do_command(cmd,cmd2,cmd_ct);
	}
	return(argcount);
}

prompt(ps,as,ss)
/* gives the user a prompt, ps[], and waits until it gets some
*  input, as[].  Input which is beyond the string size, ss, is
*  discarded.
***/
char ps[],as[];
int ss;
{
	char c;
	int ok = 0, pos;

	ggtoa();
	while (!ok) {
		fprintf(stderr,"%s ",ps);
		pos = 0;
		while ((c = getchar()) == ' ' || c == '\t')
			;
		if (c != '\n') {
			ok = 1;
		        as[0] = c;
			while ((c = getchar()) != '\n' && c != '\t' && ++pos < ss) {
				as[pos] = c;
			}
		}
	}
	if (++pos < ss)
		as[pos] = '\0';
}

proc_command(cmd_ln,cmd1,cmd2)
/* takes the input command line, cmd_ln, and gives the first word to cmd1
*  and the second word, if there is one, to cmd2.  It returns the number 
*  of words found on the line, either one or two.
***/
char cmd_ln[], cmd1[], cmd2[];
{
	return(sscanf(cmd_ln,"%s %s", cmd1, cmd2));
}

do_command(cmd1,cmd2,cmd_ct)
/* executues cmd1 after checking for errors in the input, and
*  returns cmd_ct which is the number of legal values the procedure
*  recieved.
***/
char cmd1[], cmd2[];
int cmd_ct;
{
	if (!strcmp(cmd1,XMIN)) {
		if (cmd_ct == 2 && isnum(cmd2)) {
		 	MinX = atof(cmd2);
			XM = 1;
		} else
			warn(NOVAL,cmd1);
			
	} else if (!strcmp(cmd1,XMAX)) {
		if (cmd_ct == 2 && isnum(cmd2)) {
			MaxX = atof(cmd2);
			XMX = 1;
		} else
			warn(NOVAL,cmd1);

	} else if (!strcmp(cmd1,YMIN)) {
		if (cmd_ct == 2 && isnum(cmd2)) {
			MinY = atof(cmd2);
			YM = 1;
		} else
			warn(NOVAL,cmd1);

	} else if (!strcmp(cmd1,YMAX) && isnum(cmd2)) {
		if (cmd_ct == 2) {
			MaxY = atof(cmd2);
			YMX = 1;
		} else
			warn(NOVAL,cmd1);

	} else if (!strcmp(cmd1,XLOG))
		Xlog = 1;

	else if (!strcmp(cmd1,YLOG))
		Ylog = 1;

	else if (!strcmp(cmd1,XLIN))
		Xlog = 0;

	else if (!strcmp(cmd1,YLIN))
		Ylog = 0;

	else if (!strcmp(cmd1,APPENDFILE))
		if (cmd_ct == 2)
			cmd_ct = open_file(cmd2,'a');
		else
			warn(NOVAL,cmd1);

	else if (!strcmp(cmd1,WRITEFILE) || !strcmp(cmd1,"o"))
		if (cmd_ct == 2)
			cmd_ct = open_file(cmd2,'w');
		else
			warn(NOVAL,cmd1);

	else if ((cmd1[0] == '?') || !(strcmp(cmd1,HELP)))
		show_commands();

	else if (!strcmp(cmd1,QUIT) || !strcmp(cmd1,"q"))
		Done = 1;

	else if (!strcmp(cmd1,SETUP))
		setup();

	else if (!strcmp(cmd1,FILLFILE)) {
		if (cmd_ct == 2)
			cmd_ct = open_file(cmd2,'w');
	
		if (!Setup)
			setup();

		digitize();

	} else if (!strcmp(cmd1,DIGITIZE))
		do_all();
	
	else if (!strcmp(cmd1,SHOW)) {
		show_inp();

	} else if (!strcmp(cmd1,VERBO) || !strcmp(cmd1,"v"))
		Verbose = Verbose ? 0 : 1;

	else if (!strcmp(cmd1,GRAPH) || !strcmp(cmd1,"g")) {
		Graph = Graph ? 0 : 1;

	} else if (!strcmp(cmd1,"d") || !strcmp(cmd1,DEVICE)) {
		if (cmd_ct == 2)
			if (setpdev(cmd2))
				warn(BADDEV,cmd2);
			else
				Devnam = cmd2;
		else {
			cmd_ct = 1;
			warn(NOVAL,cmd1);
		}
	} else
		fprintf(stderr,"digit: bad command: %s\n",cmd1);

	return(cmd_ct - 1);	
}

isnum(s)  /* returns 1 if string s[] is a legal number */
char s[];
{
	int i = 0;
	while (s[i] != '\0') {
		if (!(s[i] >= '0' && s[i] <= '9') && s[i] != '-' 
                    && s[i] != '.' && s[i] != '+' && s[i] != 'e'
						  && s[i] != 'E')
			return(0);
		++i;
	}
	return(1);
}

open_file(fname,mode)
/* open_file gets the output file name and opens that file in
* the mode it recieved.
***/
char fname[];
char mode;
{
	if (OutFile != NULL)
		fclose(OutFile);

	if (mode == 'w') {
		if ((OutFile = fopen(fname,"w")) == NULL) {
			warn(BADFILE,fname);
			return(1);
		} else
			fprintf(stderr,"Writing output into %s\n\n",fname);

	} else {
		if ((OutFile = fopen(fname,"a")) == NULL) {
			warn(BADFILE,fname);
			return(1);
		} else
			fprintf(stderr,"Appending output onto %s\n\n",fname);

	}
	strcpy(Fname,fname); Mode = mode;
	return(2);
}

do_all()
/* does setup procedure then it digitizes */
{
	setup();
	if (Setup)
		digitize();

}
setup()
/* finds the angle that the graph is offset, Rotang.  Then it
*  makes sure that legal values are entered for the scaling of the 
*  graph.  The it turns on the Setup flag and the InitAll flag.
***/
{
	char fs[40];
	char conf[40];

	ggtoa();
	fprintf(stderr,"\n\nPlace your graph on the tablet and ");
	fprintf(stderr,"secure it by taping if possible.\n");
	fprintf(stderr,"Then enter the location of the axes by");
	fprintf(stderr," depressing any of the buttons on\n");
	fprintf(stderr,"the mouse after you put the mouse over");
	fprintf(stderr," the following points:\n");
	fprintf(stderr,"   the origin\n");
	fprintf(stderr,"   the end of the X axis\n");
	fprintf(stderr,"   the end of the Y axis\n\n");

	gclear();
	Rotang = offang();

	if (Rotang == 999.0) {
		warn(EXSET,"");
		return;
	}
	ggtoa();

	if (!XM) {
		prompt("\nEnter the minimum value for X -->",fs,15);
		MinX = atof(fs);
		XM = 1;
	}
	if (!XMX) {
		prompt("Enter the maximum value for X -->",fs,15);
		MaxX = atof(fs);
		XMX = 1;
	}
	if (!YM) {
		prompt("\nEnter the minimum value for Y -->",fs,15);
		MinY = atof(fs);
		YM = 1;
	}
	if (!YMX) {
		prompt("Enter the maximum value for Y -->",fs,15);
		MaxY = atof(fs);
		YMX = 1;
	}

	while (MaxX <= MinX) {
		warn(BADVALX,"");
		prompt("\nEnter the minimum value for X -->",fs,15);
		MinX = atof(fs);
		prompt("Enter the maximum value for X -->",fs,15);
		MaxX = atof(fs);
	}	

	while (MaxY <= MinY) {
		warn(BADVALY,"");
		prompt("\nEnter the minimum value for Y -->",fs,15);
		MinY = atof(fs);
		prompt("Enter the maximum value for Y -->",fs,15);
		MaxY = atof(fs);
	}	

	Setup = 1;
	InitAll = 1;

	if ((!Xlog && !Ylog) && (islog10(MaxX,MinX) || islog10(MaxY,MinY))) {
		prompt("\nAre values of X scaled logarithmically in base 10? ",conf,5);
		if (conf[0] == 'y' || conf[0] == 'Y')
			Xlog = 1;
		prompt("Are values of Y scaled logarithmically in base 10? ",conf,5);
		if (conf[0] == 'y' || conf[0] == 'Y')
			Ylog = 1;
	}
}

islog10(a,b)
/* determines whether or not 'a' and 'b' seem to be the minimum and maximum
*  values for a log scale.
***/
float a,b;
{
	if (a <= 0.0 || b <= 0.0)
		return(0);
	else
		return(abs(log10(a) - log10(b)) >= 1);
}

digitize()
/* makes sure that an output fike has been opened and that the setup
*  procedure has been completed.  Then it calls init_input() which
*  resets the scales on the tablet using gplot procedures.
*  Then it calls getinp()
***/
{
	char fname[256], mode[6];

	if (!Setup) {
		warn(NOSETUP,"");
		return;
	}

	if (MaxX == MinX || MaxY == MinY) {
		fprintf(stderr,"warning: bad values for scale given");
		return;
	}	

	while (OutFile == NULL) {
		prompt("\nEnter the output file name -->",fname,256);
		
		while (mode[0] != 'w' && mode[0] != 'a') {
			prompt("Enter mode (\"w\" for write \"a\" for append) >",mode,6);
		}
		(void) open_file(fname,mode[0]);
	}

	fprintf(stderr,"\nNow enter your data points by depressing\n");
	fprintf(stderr,"any button on the mouse.  When you are\n");
	fprintf(stderr,"finished entering data depress any key while on\n");
	fprintf(stderr,"a point which is clearly outside ");
	fprintf(stderr,"of the graph.\n\n");

	if (Graph) draw_axes();

	if (Xlog) {
		MinX = (float)(log10((double)MinX));
		MaxX = (float)(log10((double)MaxX));
	}
	if (Ylog) {
		MinY = (float)(log10((double)MinY));
		MaxY = (float)(log10((double)MaxY));
	}

	init_input();
	getinp();

	gscale(((MaxX - MinX)/I_lenx),((MaxY - MinY)/I_leny));

	if (Xlog) {
		MinX = (float)(pow(10.0,(double)MinX));
		MaxX = (float)(pow(10.0,(double)MaxX));
	}
	if (Ylog) {
		MinY = (float)(pow(10.0,(double)MinY));
		MaxY = (float)(pow(10.0,(double)MaxY));
	}
}


float offang()
/***
*	returns the angle at which the axes must be rotated for the
*		X axis to be horizontal (in degrees)
*
*	sets (I_lenx) equal to the length of the X axis
*	sets (I_leny) equal to the length of the Y axis
*	sets (Xst,Yst) equal to the origin
*
*	NOTE: The three coordinates which are entered by the user
*	      must NOT include the UPPER-RIGHTHAND corner.
***/
{
	float axlen();
	int iv[10];
	float fv[10];
	int ok = 0;
	float xang,yang,err;
	double dif;
	float dx,dy;
	float x[3],y[3];
	int pos;
	int loop = 0;
	gnline(1);

	while (!ok) {
		if (loop == 3)
			return(999.0);

		/* get coordinates */
		for (pos = 0; pos <= 2; pos++) {
			gpgeti(G_CLOC,iv,fv);			
			x[pos] = fv[0];  y[pos] = fv[1];
		}
		++loop;
		sort(x,y);

		/* find angle of Y axis */
		dx = x[1] - x[0];

		if (dx) {
			dy = y[1] - y[0];
 			dif = dy/dx;
			yang = atan(abs(dif));
		} else
			yang = PI/2.0;

		if (dif < 0.0)
			yang = PI - yang;


		/* find angle of X axis */
		dx = x[2] - x[0];
		if (dx) {
			dy = y[2] - y[0];
			dif = dy/dx;
			xang = atan(abs(dif));
		} else 
			xang = PI/2.0;

		if (dif < 0.0)
			xang = -xang;

		err = abs(rtod(yang - xang - (PI/2.0)));

		if (err < 3.0) 
			ok = 1;
		else {
		   if (loop != 3) {
			ggtoa();
			printf("axes are not orthogonal. Please try again\n");
			}
		}
	}
	I_lenx = abs(axlen(x[0],y[0],x[2],y[2]));
	I_leny = abs(axlen(x[0],y[0],x[1],y[1]));

	Xst = x[0];  Yst = y[0];

	return(rtod(xang));
}


sort(x,y)
float x[], y[];
/***
*	Coordinates are sorted so that they are in the following order:
*		first - the location of the origin
*		second - the upper end of the Y axis
*		third - the upper end of the X axis
***/
{

	if (x[0] > x[1]) {
		swap(&x[0],&x[1]);
		swap(&y[0],&y[1]);
	}

	if (x[1] > x[2]) {
		swap(&x[1],&x[2]);
		swap(&y[1],&y[2]);

		if (x[0] > x[1]) {
			swap(&x[0],&x[1]);
			swap(&y[0],&y[1]);
		}
	}

	if (y[0] > y[1]) {
		swap(&x[0],&x[1]);
		swap(&y[0],&y[1]);
	}
}

swap(a,b)       /** interchange a and b **/
float *a,*b;
{
	float temp;

	temp = *a;
	*a = *b;
	*b = temp
;}

float axlen(x1,y1,x2,y2)
float x1,y1,x2,y2;
/**	returns the distance between the points (x1,y1) and (x2,y2) **/
{
	return(sqrt(sqr(x2-x1)+sqr(y1-y2)));
}

init_input()
/***
*	initializes gplot variables so that input can be processed
*	accurately.
***/
{
	if (InitAll) {
		gtrans(Xst,Yst);
		grotat(0.0,0.0,Rotang);
	}
	gscale(1/(((MaxX - MinX)/I_lenx)),1/(((MaxY - MinY)/I_leny)));
	gtrans(-MinX,-MinY);
	InitAll = 0;
}

draw_axes() 
/***
* 	draws an axes box
***/
{
	float   x_axang = 0.0,	/* angle of X axis */
		y_axang = 90.0, /* angle of Y axis */

		xltic, /* length between tic marks in X direction */
		yltic, /* length between tic marks in Y direction */

		labht = 0.1,  /* height of major tic lables */
		tlang = 270.0,/* angle of tic lables with respect to axang */

		tang  = 90.0,	/* angle at which tics are drawn */
		tht   = 0.2,	/* height of major tics in "top" direction */
		tdp   = 0.1,    /* height of major tics in "bot" direction */

		axtht = 0.2,	/* height of title string */

		aspect, /* aspect ratio */
		tmp;	/* temporary value */

	int	xutic,	/* numder of unlabled tics on the X-axes */
		yutic,	/* number of unlabled tics on the Y-axes */

		tloc  = 1,	/* location of tic lables: 1=top, 2=bottom */
		lform = 1;	/* location of title: +=top -=bot 1=axang */

	char	title[10],	/* title string */
		vform[5];	/* format of numeric lables */

 	/* find and set screen size dependent values */
	int	 iv[5] ;
	float	 fv[5] ;

 	if (gpgeti(G_PSIZE, iv, fv) < 0) {
		ggtoa();
		fprintf(stderr,"plot: bad devise for plotting");
		exit(0);
	}

	O_lenx = fv[0] * 0.8;
	O_leny = fv[1] * 0.8;
	aspect = I_lenx/I_leny;

	if (aspect > 1.0) {
		if ((tmp = O_lenx/aspect) <= O_leny) O_leny = tmp;
		else 				     O_lenx = O_leny * aspect;
	} else if (aspect > 0.0 && aspect < 1.0) {
		if ((tmp = O_leny * aspect) <= O_lenx) O_lenx = tmp;
		else				     O_leny = O_lenx / aspect;
	}
		
	Xst = fv[0] * 1.5/10;
	Yst = fv[1] * 1/10;
	axtht = 0.2 * (fv[0] * fv[1])/100;
	labht = 0.2 * (fv[0] * fv[1])/100;

	if (Xlog) {
	 	xltic = 10.0;
		xutic = -1;
	} else {
		xltic = abs(MaxX-MinX)/10;
		xutic = 3;
	}

	if (Ylog) {
		yltic = 10.0;
		yutic = -1;
	} else {
		yltic = abs(MaxY-MinY)/10;
		yutic = 3;
	}

	/*** axplt routines ***/

	/* plot left Y-axis */
	strcpy(title,"Y-Axis"); strcpy(vform,"g");

	axplt2(Xst,Yst,O_leny,y_axang,MinY,MaxY,MinY,MaxY,yltic,yutic,
		labht,tlang,tloc,tang,tht,tdp,title,axtht,lform,vform);

	/* plot bottom X-axis */
	tloc = 0; tht = 0.1; tdp = 0.2;
	tlang = 0.0;
	strcpy(title,"X-Axis"); 
	lform = -1;
	
	axplt2(Xst,Yst,O_lenx,x_axang,MinX,MaxX,MinX,MaxX,xltic,xutic,
		labht,tlang,tloc,tang,tht,tdp,title,axtht,lform,vform);

	/* plot top X-axis */
	tdp = 0.2; strcpy(title," "); tloc = 1; labht = 0.0;

	axplt2(Xst,Yst+O_leny,O_lenx,x_axang,MinX,MaxX,MinX,MaxX,xltic,
		xutic,labht,tlang,tloc,tang,tht,tdp,title,axtht,lform,vform);

	/* plot right  Y-axis */
	tloc = 0;

	axplt2(Xst+O_lenx,Yst,O_leny,y_axang,MinY,MaxY,MinY,MaxY,yltic,
		yutic,labht,tlang,tloc,tang,tht,tdp,title,axtht,lform,vform);

}

getinp()
/* gets numbers from the tablet and if the number is on the graph
*  it will put the numbers into the output otherwise it will exit
***/
{
	int iv[10];
	float fv[10];
	int done = 0, pen = G_MOVE;
	int inp_ct = 0;
	float x,y;
	float out_trans();
	while (!done) {
		gpgeti(G_CLOC,iv,fv);
		x = fv[0];
		y = fv[1];

		if (inrange(x,y)) { /* put into output */
			if (Graph) {
				gplot(out_trans('x',x),out_trans('y',y),pen);
				pen = G_DRAW;
				}

			if (Xlog) {
				x = (float)(pow(10.0,(double)x));
			}
			if (Ylog) {
				y = (float)(pow(10.0,(double)y));
			}
			fprintf(OutFile,"%g %g\n",x,y);

			if (Verbose) { /* show input on the screen */
				fprintf(stderr,"%f %f\n",x,y);
 				++inp_ct;
			}
		} else
			done = 1;
	}

	if (Verbose) 
		fprintf(stderr,"%d data points were entered.\n",inp_ct);
}

inrange(x,y)
/* returns 1 if the coordinate (x,y) is inside an imaginary box
*  which has the axes for two of the sides.
***/
float x,y;
{
	float xdev,ydev;
	xdev = abs((MaxX - MinX) * 0.05);
	ydev = abs((MaxY - MinY) * 0.05);

	if (x < (MinX - xdev))
		return(0);

	if (x > (MaxX + xdev))
		return(0);

	if (y < (MinY - ydev))
		return(0);

	if ((y) > ((MaxY) + ydev))
		return(0);

	return(1);
}

float
out_trans(ax,f)
char ax;
float f;
{
	if (ax == 'x') return(f * O_lenx/I_lenx);
	if (ax == 'y') return(f * O_leny/I_leny);
}

show_inp()
/* prints the values of all of the variables onto the screen */
{
	fprintf(stderr,"\nPlotting device is %s",Devnam);
	fprintf(stderr,"\nOutput file is %s",
	  ((OutFile == NULL) ? "undefined" : Fname) );
	if (OutFile == NULL)
		fprintf(stderr,"\n");
	else
		fprintf(stderr,"    (%s mode)\n",
 	  	  ((Mode == 'a') ? "append" : "write") );

	fprintf(stderr,"\nmin.x = %g\tmax.x = %g",MinX,MaxX);
	fprintf(stderr,"\nmin.y = %g\tmax.y = %g\n",MinY,MaxY);

	fprintf(stderr,"\nThe X scale is %s.\n",
	  (Xlog ? "logarithmic" : "linear") );

	fprintf(stderr,"\nThe Y scale is %s.\n",
	  (Ylog ? "logarithmic" : "linear") );

	fprintf(stderr,"\n%sverbose\n",(Verbose ? "" : "not "));
	fprintf(stderr,"automatic graph mode is %s\n",(Graph ? "on" : "off") );
}

show_commands()
/* prints a summary of legal digit commands onto the screen */
{
fprintf(stderr,"The possible commands are:\n");
fprintf(stderr,"\tdigitize - runs all necessary digitizing procedures.\n");
fprintf(stderr,
  "\t    NOTE: No other commands should be necessary if you use digitize.\n\n");

fprintf(stderr,"\tmin.x n - sets minimum value of x to be equal to n.\n");
fprintf(stderr,"\tmax.x n - sets maximum value of x to be equal to n.\n\n");

fprintf(stderr,"\tmin.y n - sets minimum value of y to be equal to n.\n");
fprintf(stderr,"\tmax.y n - sets maximum value of y to be equal to n.\n\n");

fprintf(stderr,"\tlog.x - x values are in logarithmic form.\n");
fprintf(stderr,"\tlog.y - y values are in logarithmic form.\n\n");

fprintf(stderr,"\tlin.x - x values are in linear form. (default)\n");
fprintf(stderr,"\tlin.y - y values are in linear form. (default)\n\n");

fprintf(stderr,
	"\twrite filename - opens filename and puts it into write mode.\n");
fprintf(stderr,
       "\tappend filename - opens filename and puts it into append mode.\n\n");

fprintf(stderr,"\td plot-device - sets the plot device.\n");
fprintf(stderr,
	"\tsetup - initializes program to accept input from the tablet.\n");
fprintf(stderr,"\tfile - will start digitizing numbers and putting them\n");
fprintf(stderr,"\t     into the output file.\n\n");

fprintf(stderr,"\tshow - shows the current value for each variable.\n");
fprintf(stderr,"\tverbose - turns the verbose mode on and off.\n");
fprintf(stderr,"\tgraph - turns the graph mode on and off.\n\n");
fprintf(stderr,"\tquit - exits program.\n");
}	

warn(warning,st)
/* prints warnings onto the screen */
int warning;
char st[];
{
	switch(warning) {
	case BADSWITCH:
	  fprintf(stderr,"warning: bad switch given: %s\n",st);
	  break;
	case BADFILE:
	  fprintf(stderr,"warning: bad file spec given: %s\n",st); break;
	case BADVALX: 
	  fprintf(stderr,
	    "Bad X values given for scale. Try again.\n");
	  break;
	case BADVALY: 
	  fprintf(stderr,
	    "Bad Y values given for scale. Try again.\n");
	  break;
	case NOSETUP:
	  fprintf(stderr,"warning: you must setup first\n");
	  break;
	case EXSET:
	  fprintf(stderr,"warning: aborting setup procedure\n");
	  break;
	case BADDEV:
	  fprintf(stderr,"warning: bad plot device %s\n",st);
	  break;
	case NOVAL:
	  fprintf(stderr,"warning: no value or bad value given for %s\n",st);
	  break;
	}
}
