/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* "mkuniq" : make the given template file name into a unique file name
 *
 * usage:
 *	mkuniq(name, tmploc)
 *
 * where:
 *	name	- (char *) file name to become unique.
 *	tmploc  - (char *) pointer to location of template.  Six characters
 *		beginning at 'tmploc' in 'name' will be overwritten with
 *		the current process id and a letter.  The contents of those
 *		six locations can be anything since they will be over written.
 *		IF: tmploc == 0 then nothing happens (take as no unique name
 *		needed).
 *
 * Notes:
 *	1. This version can make up to 62 files using the current PID and 
 *		one character (a-z,A-Z,0-9) tagged on. 
 *		will be bumped up.
 */

/* Needed for the 'rindex()' used below */
#include "auxfns.h"

int
mkuniq(name, tmploc)
	char *name, *tmploc;
	{

	int  l ;                /* the letter */

	if (!tmploc) 
		return( access(name,0) == -1) ;    /* NOP */

	sprintf(tmploc, "%05d" , getpid() );   /* fill in pid */
	tmploc += 5;
	l = 'a';

	for(;;) {   /* till we get a uniq name */
		*tmploc = l;

		if (access(name, 0) == -1) break;  /* got one */
		else {   
			if      (l == 'z')  l = 'A';
			else if (l == 'Z')  l = '0';
			else if (l == '9')  {
				*name = 0;
				break;
				}
			else  l++ ;
			}
		}

	return ((int)*name);
	}


/* "needunq" : see if file name contains six '?'s for use as a template for a
 *	unique file name. If yes then return a character pointer to that
 *	point in namr. Else return 0.
 */

char *
needunq(namr)
	char *namr;
	{

	char *tp, *rindex();

	if ((tp = rindex(namr, '?')) && strncmp("??????", (tp-5), 6)==0) 
		return(tp - 5);

	return(0);
	}
