
/*----------------------------------------------------------------------
 *
 * meshio.c - Reads/writes mesh files.
 *            Uses ascii format for easy inspection and porting.
 *            Computes triangle-boundary numbers for simulators
 *            which don't want to know about edges.
 *
 * Copyright c 1985 The board of trustees of the Leland Stanford
 *                  Junior University. All rights reserved.
 * This subroutine may not be used outside of the SUPREM4 computer
 * program without the prior written consent of Stanford University.
 *
 * Original: CSR Nov85 (tri program)
 * Modified: CSR Feb85 (use database of skel program)
 * Modified: CSR Mar85 (do triangle codes on the way out)
 *---------------------------------------------------------------------*/
#include "general.h"
#include "dbase.h"
#include <stdio.h>
#include <string.h>



#define MAXNODE         3000
#define MAXTRI          6000
#define MAXEDGE		2000
#define MAXREG   	  20
		

/*-----------------RUMESH-----------------------------------------------
 * Read mesh. Converts input integers to pointers, sets up data base.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
rumesh (char name[])
#else
char * rumesh(name)
    char name[];
#endif
{
    
    node *nadd[MAXNODE]; triangle *tadd[MAXTRI]; region *radd[MAXREG]; 
    edge *eadd[MAXEDGE];

#   define LL 80
    FILE *fp; static char estring[30], flag[2];
    char iline[LL];
    node *np; edge *ep; triangle *tp; region *rp;
    char *es;
    int nitem, i, ierr, line, n, wreg, a, b, j, k, r, e1, e2, e3, save_c;
    double x, y, h;

      
    if ((fp = fopen(name,"r"))==0) 
	{
	sprintf(estring,"rumesh: cannot open file %s\0",name);
	return(estring);
	}

    /* Turn off checking to speed this up. */
    save_c = check; check = 0;

    line = wreg = ierr=0;
    while (fgets( iline, LL, fp) != NULL) {
	line++;
	sscanf( iline, "%1s", flag);

	switch(flag[0]) {

	case 'c' : 
	    if ((nitem = sscanf(iline,"c %d %lf %lf %lf",&i,&x,&y,&h)) != 4)
		umesh_err (line,"need 4 items",ierr++); 
	    else
		if (es = cr_node (&np, x, y)) 
		    umesh_err (line, es, ierr++); 
		else
		    {
		    np->h = h; 
		    if (i<1 || i>= MAXNODE) 
			umesh_err (line, "bad index", ierr);
		    else nadd[i] = np;
		    }
	    break;

	case 'e':
	    if ((nitem = sscanf(iline, "e %d %d %d %d", &i, &a, &b, &n)) != 4)
		umesh_err (line, "need 4 items", ierr++);
	    else {
		if (i<1 || i>=MAXEDGE || a<1 || a>=MAXNODE || b<1 || b>=MAXNODE)
		    {umesh_err (line, "bad index", ierr++); continue;}
		if (es = cr_edge (nadd[a], nadd[b], &ep)) 
		    umesh_err (line, es, ierr++);
		else {
		    ep->elec = n;
		    eadd[i] = ep;
		    }
		}
	    break;

	case 'r' : 
	    if ((nitem = sscanf(iline, "r %d %d", &i, &n)) != 2)
		umesh_err(line, "need 2 items", ierr++);
	    else {
		if (i < 1 || i > MAXREG)
		    {umesh_err (line, "bad index", ierr++);continue;}
		if (es = cr_reg(&rp)) 
		    umesh_err (line, es, ierr++);
		else {
		    rp->mat = n; 
		    radd[i]=rp; wreg=i; 
		    }
		}
	    break;

	case 'b' : 
	    if (wreg == 0) 
		umesh_err(line,"no region yet",ierr++); 
	    else
		if ((nitem = sscanf(iline, "b %d", &i)) != 1)
		    umesh_err (line, "need 1 item", ierr++);
		else
		    if (i < 1 || i > MAXEDGE)
			umesh_err (line, "bad index" ,ierr++);
		    else
			if (es = link_edge (radd[wreg], eadd[i], 
                                            radd[wreg]->bnd, BEFORE))
			    umesh_err (line, es, ierr++);
			else /* OK */;
	    break;

	case 't' :
	    if ((nitem=sscanf(iline,"t %d %d %d %d %d %d %d %d",&n,&r,&i,&j,&k,
		&e1,&e2,&e3)) != 8) umesh_err (line, "need 8 items", ierr++);
	    else {
		if (i<1|| i>=MAXNODE|| j<1|| j>=MAXNODE|| k<1|| k>=MAXNODE|| 
		    r<1|| r>=MAXREG || n<1|| n>=MAXTRI)
		    {umesh_err(line,"bad index",ierr++);continue;}
		if (es = cr_tri(&tp, nadd[i], nadd[j], nadd[k], radd[r],
				e1, e2, e3))
		    umesh_err (line, es, ierr++);
		else 
		    tadd[n] = tp;
		}
	    break;

	default : 
	    /* ignore this line */
	    break;
        }
    } /* Next line */

    fclose(fp);

    /* Restore checking. */
    check = save_c;

    if (!ierr) return(0);
    else {
	sprintf(estring,"%d",ierr);
	strcat(estring," error(s) in mesh input");
	return(estring);
	}
}

#ifdef ANSI_FUNC

int 
umesh_err (int line, char *s, int ierr)
#else

umesh_err(line,s,ierr)
    char *s;
    int line,ierr;
#endif
{
    char ebuf[80];
    sprintf (ebuf,"Input mesh error in line %d: %s \0",line,s);
    uerr (ebuf);
}


/*-----------------WUMESH-----------------------------------------------
 * Write mesh. Convert the data base pointers to integers, write.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
wumesh (char name[])
#else

char * wumesh(name)
    char name[];
#endif
{
/* DECLARE */
    int ii; 
    node *pn, *bn; region *pr, *br; lledge *fpb, *bpb; 
    triangle *pt, *bt; edge *pe, *be;
    static char err[40];
    FILE *fp;


    if ((fp = fopen(name,"w"))==0) 
	{
	sprintf(err,"wmesh: cannot open file %s\0",name);
	return(err);
	}

    ii=1; WALK (root.node, pn, bn)
	{
	pn->iocode = ii;
	fprintf(fp,"c %d %g %g %g\n", ii++, pn->x, pn->y, pn->h);
	}

    ii=1; WALK (root.edge, pe, be)
	{
	pe->iocode = ii;
	fprintf(fp,"e %d %d %d %d\n", ii++, pe->n[0]->iocode, 
					 pe->n[1]->iocode, pe->elec);
	}

    ii=1; WALK (root.reg, pr, br)
	{
	pr->iocode = ii;
	fprintf(fp,"r %d %d\n", ii++, pr->mat);
	WALK (pr->bnd, fpb, bpb)
	    {
	    fprintf(fp,"b %d\n",fpb->edge->iocode);
	    }
	}

    ii=1; WALK (root.tri, pt, bt)
	{
	pt->iocode = ii;
	fprintf(fp,"t %d %d %d %d %d %d %d %d\n", ii++, pt->r->iocode, 
	    pt->n[0]->iocode, pt->n[1]->iocode, pt->n[2]->iocode, 
	    pt->e[0], pt->e[1], pt->e[2]);
	}

    fclose(fp);

return(0);
}

/*-----------------WGMESH-----------------------------------------------
 * Function to dump stick pictures so dplot can read them. Fairly stupid.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
wgmesh (char name[])
#else

char * wgmesh(name)
    char name[];
#endif
{
    FILE *fp; 
    static char err[80];
    edge *f, *b; node *nf, *nb; triangle *tf, *tb;
    double xmin, xmax, ymin, ymax, dx, dy;

    if ((fp = fopen(name,"w"))==0) 
	{
	sprintf(err,"wmesh: cannot open file %s\0",name);
	return(err);
	}

  /*...Work out how big the thing is.*/
    xmin = ymin = MAXFLOAT;
    xmax = ymax = -MAXFLOAT;
    WALK (root.node, nf, nb) {
	if (nf->x < xmin) xmin = nf->x;
	if (nf->x > xmax) xmax = nf->x;
	if (nf->y < ymin) ymin = nf->y;
	if (nf->y > ymax) ymax = nf->y;
	}
    dx = (xmax - xmin);		dy = (ymax - ymin);
    if (dx > dy) ymax = ymin + dx; 	else xmax = xmin + dy;

    dx /= 30;	dy /= 30;
    xmin -= dx;	xmax += dx;
    ymin -= dy; ymax += dy;

    fprintf (fp, "$ncols 2 $col.x 1 $col.y 2\n");
    fprintf (fp, "$min.x %g  $max.x %g\n", xmin, xmax);
    fprintf (fp, "$min.y %g  $max.y %g\n", ymin, ymax);

  /*...Nodes. */
    fprintf (fp, "# Nodes first\n");
    WALK (root.node, nf, nb) {
	fprintf (fp, "$line 1\n");
	fprintf (fp, "%g %g\n", nf->x,      nf->y - dy);
	fprintf (fp, "%g %g\n", nf->x + dx, nf->y);
	fprintf (fp, "%g %g\n", nf->x,      nf->y + dy);
	fprintf (fp, "%g %g\n", nf->x - dx, nf->y);
	fprintf (fp, "%g %g\n", nf->x,      nf->y - dy);
	}

  /*...Draw edges. Use electrode codes as line type. */
    fprintf (fp, "# Edges now\n");
    WALK (root.edge, f, b) {
	fprintf (fp, "$line %d\n", f->elec);
	fprintf (fp, "%g %g\n", f->n[0]->x, f->n[0]->y);
	fprintf (fp, "%g %g\n", f->n[1]->x, f->n[1]->y);
	}

  /*...Draw triangles. */
    fprintf (fp, "# Triangles \n");
    WALK (root.tri, tf, tb) {
	fprintf (fp, "$line 2\n");
	fprintf (fp, "%g %g\n", tf->n[0]->x, tf->n[0]->y);
	fprintf (fp, "%g %g\n", tf->n[1]->x, tf->n[1]->y);
	fprintf (fp, "%g %g\n", tf->n[2]->x, tf->n[2]->y);
	fprintf (fp, "%g %g\n", tf->n[0]->x, tf->n[0]->y);
	}

    fclose(fp);
    return(0);
}


