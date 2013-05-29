/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* "macros-f77" : The macros defined in "gplot.h" are here implemented as
 *	true subroutines for f77.  Thus f77 programs can "call" the macros
 *	just as in C (or C can call macros that look like subroutines).
 *
 * written:  Michael Eldredge (aug 84)
 * modified: Michael Eldredge (dec 86) For new release (w/gpmisc() ).
 */

#include "gplot.h"
#include "nwstrc.c"


/* stpdev: (int function) Set up the plot device defined in the Plotcap 
 *	data base file for plotting to.  (For gplot to be able to plot).
 *
 * calling sequence:
 *	<i> = stpdev(devnam)
 *
 * where:
 *	devnam	- (character array) Name of device to 'setup'.  Plotcap
 *		file is searched for an entry matching 'devname'.
 *	<i>	- (integer function return) Return error code: 0::ok.
 *
 * NOTE: for full documentation, see 'setpdev.c'.
 * NOTE: The f77 version name is "stpdev"; shortened to 6 characters for
 *	fortran.
 * NOTE: The f77 version (here) creates its own null terminated copy of the
 *	device name (using nwstrc()).
 */
long int
stpdev_(devnam, blen)   /* f77 callable 'setpdev' */
	char *devnam;
	long int blen;       /* f77 declared array len of devnam */
	{
	long iret ;
	char *dn , *nwstrc();

	dn = nwstrc(devnam, blen);	/* get a local copy of the string */

	iret = setpdev(dn);          /* HERE's what the fuss is all about */

	free(dn);
	return(iret);
	}

/* stpfil: (int function) Set up the plot output file for gplot.
 *
 * calling sequence:
 *	<i> = stpfil(filnam)
 *
 * where:
 *	filnam	- (character array) Name of file to 'setup'. This will
 *		override the default output file given in the plotcap
 *		data base.
 *	<i>	- (integer function return) Return error code: 0::ok.
 *
 * NOTE: for full documentation, see 'setpfil.c'.
 * NOTE: The f77 version name is "stpfil"; this is shortened to 6 characters
 *	for fortran.
 * NOTE: The f77 version (here) creates its own null terminated copy of the
 *	file name (using nwstrc()).
 */
long int
stpfil_(filnam, blen)   /* f77 callable 'setpfil' */
	char *filnam;
	long int blen;       /* f77 declared array len of filnam */
	{
	long iret ;
	char *fn , *nwstrc();

	fn = nwstrc(filnam, blen);	/* get a local copy of the string */

	iret = setpfil(fn);           /* HERE's what the fuss is all about */

	free(fn);
	return(iret);
	}


long int
stsfil_(s, blen)	/* set save file name */
	char *s;
	long blen;
	{
	long iret ;
	char *fn , *nwstrc();

	fn = nwstrc(s, blen);		/* get a local copy of the string */

	iret = setsfil(fn); 	/* HERE's what the fuss is all about */

	free(fn);
	return(iret);
	}
	

long int
fpgeti_(cmd, iv, fv)
	long*	cmd ;
	long	iv[] ;
	float	fv[] ;
	{
	return gpmisc((int)*cmd, G_GET, iv, fv, "") ;
	}

long int
fpseti_(cmd, iv, fv)
	long*	cmd ;
	long	iv[] ;
	float	fv[] ;
	{
	return gpmisc((int)*cmd, G_SET, iv, fv, "") ;
	}

fclear_() { gclear(); }

fpend_()  { gpend();  }

fplot_(x,y,p)
	float *x, *y;
	long  *p;
	{
	gplot(*x, *y, (int)*p);
	}

fmove_(x,y)
	float *x, *y;
	{
	gmove(*x, *y);
	}

fdraw_(x,y)
	float *x, *y;
	{
	gdraw(*x, *y);
	}

fscale_(x,y)
	float *x, *y;
	{
	gscale(*x, *y);
	}

ftrans_(x,y)
	float *x, *y;
	{
	gtrans(*x, *y);
	}

frotat_(x,y,a)
	float *x, *y, *a;
	{
	grotat(*x, *y, *a);
	}

fnline_(n)
	long int *n;
	{
	gnline((int)*n);
	}

fnpen_(n)
	long int *n;
	{
	gnpen( (int)*n );
	}

fnfill_(n)
	long int *n;
	{
	gnfill((int)*n);
	}

farea_(x,y,b)
	float *x, *y;
	long  *b;
	{
	garea(*x, *y, (int)*b) ;
	}

fclipl_(x, y, h)
	float*	x, *y ;
	long*	h ;
	{
	gclipl(*x, *y, (int)*h ) ;
	}

fcliph_(x, y, h)
	float*	x, *y ;
	long*	h ;
	{
	gcliph(*x, *y, (int)*h ) ;
	}

fatog_() { gatog() ; }
fgtoa_() { ggtoa() ; } 

