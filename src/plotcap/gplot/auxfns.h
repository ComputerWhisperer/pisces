/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* Some aux functions, both defines and type defines */

/* Just in case (this is certain to lead to problems, but...) */
#ifndef stderr
#   include <stdio.h>
#endif

#ifndef BSD
#   define index   strchr
#   define rindex  strrchr
#endif

/* ifdef VFORK ??? */
#ifdef BSD
#   define fork vfork
#endif

#include <ctype.h>

/* function types */
#include <string.h>

/* Psuedo-functions */
#define strequ(A,B)	(strcmp(A,B) == 0)
#define strnequ(A,B,N)	(strncmp(A,B,N) == 0)
#define min(A,B)           (A < B ? (A) : (B))
#define max(A,B)           (A > B ? (A) : (B))

#define member(S,C)	(C && index(S,C))
#define offset(S,C)	( (int)(index(S,C) - S) )
#define memloc(S,C)	(member(S,C) ? offset(S,C) : -1)


/* The old defrot debugging/tracing tool */
#define defrot(NAM)  char *rotnam = NAM 

#define gperr(STRN)	gperr2("%s",STRN)
#define gperr2(FORM,VALU) \
	fprintf(stdout,"** Error ** (%s) ", rotnam), \
	fprintf(stdout, FORM, VALU ), \
	fprintf(stdout, "\n")

#ifndef Bool
#  define Bool	int
#  define T	1
#  define F	0
#endif
