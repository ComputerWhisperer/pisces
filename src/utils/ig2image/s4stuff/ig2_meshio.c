

static char SccsID[] = "@(#)ig2_meshio.c	1.2\t5/3/89";


/*----------------------------------------------------------------------
 *
 * ig2_io -   Reads/writes iggi-II files.
 *
 * Copyright c 1985 The board of trustees of the Leland Stanford
 *                  Junior University. All rights reserved.
 * This subroutine may not be used outside of the SUPREM4 computer
 * program without the prior written consent of Stanford University.
 *
 * Original: CSR Nov85 (tri program)
 * Modified: CSR Mar85 (do triangle codes on the way out)
 * Hacked  : CSR May85 (version for use in Suprem 4 - prior to making
 *			all this stuff rational)
 * Modified: MEL Apr86 (added the node save for the structure card)
 *                     (decided to postpone rationality a little longer)
 *---------------------------------------------------------------------*/
#include "global.h"
#include "constant.h"
#include "geom.h"
#include <stdio.h>
#include <ctype.h>
#include "material.h"
#include "impurity.h"
#include "more_imp.h"
#include "sysdep.h"
#include "expr.h"	/*for last_temp*/

#define nop(J,I) nd[ tri[I]->nd[J] ]->pt
#define ngh(J,I) tri[I]->nb[J]

/*-----------------IG2_READ---------------------------------------------
 * Do the read.
 * Bugs: completely loses it on synchronization errors. Should do this
 *       in a line-oriented instead of token-oriented fashion. Some day.
 * Warnings: user input point / triangle / node numbers must be consecutive.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

int 
ig2_read (
    char *name,		/* where the data is hiding */
    int flip,		/* flip y coords? */
    double scale	/* scale factor */
)
#else

ig2_read (name, flip, scale)
    char *name;		/* where the data is hiding */
    int flip;		/* flip y coords? */
    float scale;	/* scale factor */
#endif
{

    char iline[BUFSIZ];
    char flag[2], *err, *alloc_tri(), *alloc_pt(), *alloc_nd(), *ip;
    int nitem, i, ierr, line, n, wreg, a, b, j, k, r, e1, e2, e3;

    FILE *lu;
    double x, y, h,
	   scalx = 1e-4*scale,
	   scaly = 1e-4*((flip)? -scale : scale);


    /* See if the file really exists */
    if ((lu = fopen(name, "r")) == NULL) {
	fprintf(stderr, "ig2_read: cannot open %s\n", name);
	return (-1);
    }

    /* Junk the previous mesh */
    dis_all();

    /* Read */
    line = ierr = 0;
    wreg = -1;
    while (fgets( iline, BUFSIZ, lu) != NULL) {
	line++;
	sscanf( iline, "%1s", flag);

	switch(flag[0]) {

	case 'c' :
	    if ((nitem = sscanf(iline,"c %d %lf %lf %lf",&i,&x,&y,&h)) != 4)
		ugh (line,"need 4 items",ierr++);
	    else {
		if (err = alloc_pt())
		    ugh(line, err, ierr++);
		pt[ np-1 ]->cordo[ 0] = pt[ np-1 ]->cord[ 0 ] = x*scalx;
		pt[ np-1 ]->cordo[ 1] = pt[ np-1 ]->cord[ 1 ] = y*scaly;
	    }
	    break;

	case 'e':
	    /* We don't use edge data but we have to get past it. */
	    if ((nitem = sscanf(iline,"e %d %d %d %d", &i, &a, &b, &n)) != 4)
		ugh (line, "need 4 items", ierr++);
	    break;

	case 'r' :
	    /* Note the infamous down by -1 kludge */
	    if ((nitem = sscanf(iline,"r %d %d", &i, &n)) != 2)
		ugh(line, "need 2 items", ierr++);
	    else {
		wreg = i-1;
		mattyp[wreg] = n;
		if (wreg+1 > nmat) nmat = wreg+1;
	    }
	    break;

	case 'b' :
	    if (wreg == -1)
		ugh(line,"no region yet",ierr++);
	    else
		if ((nitem = sscanf(iline,"b %d", &i)) != 1)
		    ugh (line, "need 1 item", ierr++);

		/* This code is borrowed from iggi2, which uses edges.
		   Suprem-IV doesn't (yet...)
		else
		    if (err = ad_edge (wreg, i, reg[wreg]->bnd,MAYBE,BEFORE))
			    ugh (line, err, ierr++);
		*/
	    break;

	case 't' :
	   /*...Finally find a use for the edge codes. */
	    if ((nitem = sscanf(iline,"t %d %d %d %d %d %d %d %d",&n,&r,&i,&j,&k,
		&e1,&e2,&e3)) != 8) ugh (line, "need 8 items", ierr++);
	    else {
		if (err = alloc_tri())
		    ugh(line, err, ierr++);
		tri[ne-1]->nd[0] = i-1;
		tri[ne-1]->nd[1] = j-1;
		tri[ne-1]->nd[2] = k-1;
		tri[ne-1]->regnum = r-1;
		tri[ne-1]->nb[0] = (e1>0)? e1-1 : e1;
		tri[ne-1]->nb[1] = (e2>0)? e2-1 : e2;
		tri[ne-1]->nb[2] = (e3>0)? e3-1 : e3;

		/* Also store some stuff in the pt structure */
		for (j=0; j < 3; j++)
		    if (tri[ne-1]->nb[j]<0 && tri[ne-1]->nb[j] != BC_OFFSET){
			pt[tri[ne-1]->nd[(j+1)%3]]->flags |= SURFACE;
			pt[tri[ne-1]->nd[(j+2)%3]]->flags |= SURFACE;
		    }
	    }
	    break;
	
	case 's' :
	    /*read in the number of solutions and their impurity numbers*/
	    if ( sscanf(iline,"s %d", &n_imp) != 1 ) 
		ugh( line, "no impurities on the s line", ierr++ );

	    /*loop to read in all the solution numbers*/
	    /*position ip after s %d*/
	    ip = iline+1;
	    while( isspace(*ip)) ip ++;
	    while(!isspace(*ip)) ip ++;

	    /* enter loop sitting on first character after digit */
	    for(i = 0; i < n_imp; i++) {

		/* better be a number */
		if ( sscanf(ip," %d", &(soltoimp[i])) != 1) {
		    ugh(line,"less data than specified impurity number",ierr++);
		    break;
		}
		imptosol[ soltoimp[i] ] = i;

		/* move up ip - depends on \n at end of line */
		while( isspace( *ip)) ip++;
		while(!isspace( *ip)) ip++;
	    }
	    break;

	case 'n' :
	    /*create a node*/
	    if (err = alloc_nd())
		{ ugh(line, err, ierr++); continue; }

	    /*read the line data*/
	    if ( sscanf(iline,"n %d %d", &i, &n ) != 2 )
		ugh( line, "incomplete node line", ierr++ );
	
	    nd[nn-1]->pt = i;
	    nd[nn-1]->mater = n;

	    /*read the solution values*/
	    ip = iline + 1;
	    while( isspace( *ip)) ip ++;
	    while(!isspace( *ip)) ip ++;
	    while( isspace( *ip)) ip ++;
	    while(!isspace( *ip)) ip ++;
	    for(i = 0; i < n_imp; i++) {
		if ( sscanf( ip, "%le", &x ) != 1 ) {
		    ugh( line, "incomplete node line", ierr++ );
		    break;
		}
		nd[nn-1]->sol[i] = x;
		while( isspace( *ip)) ip ++;
		while(!isspace( *ip)) ip ++;
	    }
	    break;

	case 'M':
	    if ( sscanf( iline, "M %d %le", &sub_ornt, &last_temp) < 1 )
		 ugh( line, "no orientation given", ierr++);
	    break;

	default :
	    /* ignore this line */
	    break;
	}
    } /* Next line */

    fclose(lu);

    if (np == 0 || ne==0 || nmat==0)
	ugh(line, "mesh is not complete!", ierr++);

    return(-ierr);
}

#ifdef ANSI_FUNC

int 
ugh (int line, char *s, int ierr)
#else

ugh(line,s,ierr)
    char *s;
    int line,ierr;
#endif
{
    fprintf (stderr,"Input mesh error in line %d: %s \0",line,s);
}

