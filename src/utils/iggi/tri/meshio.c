static char rcsid[]="$Header: meshio.c,v 1.8 86/05/23 17:36:41 conor Exp $";
/***********************************************************************
 *                                                                     *
 * meshio.c - Reads/writes mesh files.                                 *
 *            Uses ascii format for easy inspection and porting.       *
 *            Computes triangle-boundary numbers for simulators        *
 *            which don't want to know about edges.                    *
 *                                                                     *
 * Copyright c 1985 The board of trustees of the Leland Stanford       *
 *                  Junior University. All rights reserved.            *
 * This subroutine may not be used outside of the SUPREM4 computer     *
 * program without the prior written consent of Stanford University.   *
 *                                                                     *
 * Original: CSR Nov85 (tri program)                                   *
 * Modified: CSR Mar85 (do triangle codes on the way out)              *
 ***********************************************************************/
#include "general.h"
#include "dbase.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
		

/*-----------------RUMESH-----------------------------------------------
 * Read mesh. Converts input integers to pointers, sets up data base.
 *----------------------------------------------------------------------*/
char * rumesh(name)
    char name[];
{

#   define LL 80
    FILE *fp; static char estring[30], flag[2], *err;
    char iline[LL];
    int nitem, i, ierr, line, n, s, wreg, a, b, j, k, r, e1, e2, e3;
    double x, y, h;


    if ((fp = fopen(name,"r"))==0)
	{
	sprintf(estring,"rumesh: cannot open file %s\0",name);
	return(estring);
	}

    line = wreg = ierr=0;
    while (fgets( iline, LL, fp) != NULL) {
	sscanf(iline,"%1s",flag);
	line++;

	switch(flag[0]) {

	case 'c' :
	    if ((nitem = sscanf(iline,"c %d %lf %lf %lf",&i,&x,&y,&h)) != 4)
		ugh (line,"need 4 items",ierr++);
	    else
		if (err = cr_node (&i,x,y,h)) ugh (line, err, ierr++);
	    break;

	case 'e':
	    if ((nitem = sscanf(iline, "e %d %d %d %d", &i, &a, &b, &n)) != 4)
		ugh (line, "need 4 items", ierr++);
	    else
		if (err = cr_edge (&i,n,a,b)) ugh (line,err,ierr++);
	    break;

	case 'r' :
	    if ((nitem = sscanf(iline, "r %d %d", &i, &n)) != 2)
		ugh(line, "need 2 items", ierr++);
	    else
		if (!(err = cr_reg (&i, n))) wreg = i;
		else ugh (line, err, ierr++);
	    break;

	case 'b' :
	    if (wreg == 0)
		ugh(line,"no region yet",ierr++);
	    else
		if ((nitem = sscanf(iline, "b %d", &i)) != 1)
		    ugh (line, "need 1 item", ierr++);
		else
		    if (err = ad_edge (wreg, i, reg[wreg]->bnd,MAYBE,BEFORE))
			    ugh (line, err, ierr++);
	    break;

	case 't' :
	    if ((nitem=sscanf(iline,"t %d %d %d %d %d %d %d %d",&n,&r,&i,&j,&k,
		&e1,&e2,&e3)) != 8) ugh (line, "need 8 items", ierr++);
	    else
		if (err=cr_tri(&n,r,i,j,k,e1,e2,e3)) ugh(line,err,ierr++);
	    break;

	default :
	    /* Ignore the input line */
	    break;
        }
    } /* Next line */


    fclose(fp);
	if (!ierr) return(0);
	else {
	    sprintf(estring,"%d",ierr);
	    strcat(estring," error(s) in mesh input");
	    return(estring);
	    }
}

ugh(line,s,ierr)
    char *s;
    int line,ierr;
{
    char ebuf[80];
    sprintf (ebuf,"Input mesh error in line %d: %s \0",line,s);
    uerr (ebuf);
}


/*-----------------WUMESH-----------------------------------------------
 * Write mesh. A straight dump except for triangles, which have to work
 * out their electrode codes.
 *----------------------------------------------------------------------*/
char * wumesh(name)
    char name[];
{
/* DECLARE */
    int ii, in, jn, ie, it, telec[3];
    static char err[40];
    FILE *fp;
    struct LLedge *e_at_p[MAXNODE], *fpb, *bpb, *tt, *tt2;


    if ((fp = fopen(name,"w"))==0) 
	{
	sprintf(err,"wmesh: cannot open file %s\0",name);
	return(err);
	}

    FOR (ii, 1, nnode)
	fprintf(fp,"c %d %g %g %g\n", ii, node[ii]->x,node[ii]->y, node[ii]->h);

    FOR (ii, 1, nedge)
	fprintf(fp,"e %d %d %d %d\n", ii, edge[ii]->n[0], edge[ii]->n[1],
					     edge[ii]->elec);

    FOR (ii, 1, nreg) 
	{
	fprintf(fp,"r %d %d\n", ii, reg[ii]->mat);
	WALK (reg[ii]->bnd, fpb, bpb)
	    fprintf(fp,"b %d\n",fpb->edge);
	}

   /*...The only pain is computing edge numbers for triangles.
    *...We avoid a quadratic search through all the edges by prestoring
    *...lists of edges at each node*/
    FOR (ii, 1, nnode) e_at_p[ii] = 0;
    FOR (ii, 1,nedge)
	FOR (ie, 0, 1) {			/* Add edge to each node list */
	    in = edge[ii]->n[ie];
	    tt = e_at_p[in];
	    e_at_p[in] = (struct LLedge *) malloc (sizeof (struct LLedge));
	    e_at_p[in]->next = tt;
	    e_at_p[in]->edge = ii;
	    }
	
    FOR (ii, 1, ntri) {
       /*...Find elec number of each triangle edge */
	FOR (it, 0, 2) {
	    telec[it] = tri[ii]->e[it];		/* gets neighbor # or -1 */
	    in = tri[ii]->n[(it+1)%3];		/* in,jn = nodes on edge */
	    jn = tri[ii]->n[(it+2)%3];
	    /* 
	     * Examine edges at in until we get (the) one with jn.
	     * The triangle elec# is then the edge elec# + offset,
	     * except in the special case of a 0 elec# separating
	     * two triangles.
	     */
	    for (tt = e_at_p[in]; tt != 0; tt=tt->next) {
		if (ined (jn, tt->edge)
		    && !(telec[it] >= 0 && edge[tt->edge]->elec == 0)) 
		    {
		    telec[it] = OFFSET_ELEC + edge[tt->edge]->elec;
		    break;
		    }
		}
	    }
	fprintf(fp,"t %d %d %d %d %d %d %d %d\n", ii, tri[ii]->reg,
	    tri[ii]->n[0], tri[ii]->n[1], tri[ii]->n[2],
	    telec[0], telec[1], telec[2]);
	}

   /*...and release the claimed storage */
    FOR (ii, 1, nnode)
	for (tt=e_at_p[ii]; tt != 0; tt = tt2) {
	    tt2 = tt->next; free (tt);
	    }

    fclose(fp);
    return(0);
}


/*-----------------WPMESH-----------------------------------------------
 * Write a mesh file for PISCES-II
 *----------------------------------------------------------------------*/
char * wpmesh(name)
    char name[];
{
    int in,it,ie,ir,f,nb,ne;
    struct Snode *np; struct Stri *tp;
    static char err[40];
    FILE *fp;


    if ((fp = fopen(name,"w"))==0) {
	sprintf(err,"wpmesh: cannot open file %s\n",name);
	return(err);
	}

  /*
   * Count how many electrodes, electrode nodes.
   * Slight crock: electrode code 0 is called no electrode (for simulators
   * that think a Neumann BC is no BC)
   */
    set_elec();
    for (ne = 0, ie=1; ie <= nedge; ie++)
	if (edge[ie]->elec > ne) ne = edge[ie]->elec;
    for (nb = 0, in=1; in <= nnode; in++)
	if (ELEC(in) && node[in]->elec != 0) nb++;

  /*...Fortran file - likes to know how many first. */
    fprintf(fp,"%d %d %d\n",nnode,ntri,nb);
    fprintf(fp,"%d %d\n",ne,nreg);

    for (in = 1; in <= nnode; in++) {
	np = node[in];
	fprintf(fp,"%g %g \n", 1e-4*np->x, -1e-4*np->y);
	}

    for (it = 1; it <= ntri; it++) {
	tp = tri[it];
	fprintf(fp,"%d %d %d %d \n", tp->reg, tp->n[0], tp->n[1], tp->n[2]);
	}

    for (in = 1; in <= nnode; in++)
	if (ELEC(in) && node[in]->elec != 0)
	    fprintf(fp,"%d %d\n",in,node[in]->elec);

    for (ir = 1; ir <= nreg; ir++)
	fprintf(fp,"%d\n",reg[ir]->mat);

    return(0);
}
