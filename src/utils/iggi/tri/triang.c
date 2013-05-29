/*	%M%		Version %I%		*/
/*	Last Modification:	%G% %U%		*/

/*static char rcsid[]="$Header: triang.c,v 1.6 85/10/18 13:43:53 conor Exp $";*/
/*************************************************************************
 *                                                                       *
 * triang.c - generate triangular grid.                                  *
 *            Roughly based on PLTMG's trigen routine.                   *
 *            [R.E. Bank and A.H. Sherman, Computing, 26, 91-105 (1981)] *
 *            This is the logic section - the geometry is in triheur.c   *
 *                                                                       *
 *     Copyright c 1985 The board of trustees of the Leland Stanford     *
 *                      Junior University. All rights reserved.          *
 *     This subroutine may not be used outside of the IGGI-2 computer    *
 *     program without the prior written consent of Stanford University. *
 *                                                                       *
 * Original : CSR Nov84                                                  *
 *                                                                       *
 *************************************************************************/
#include "general.h"
#include "dbase.h"
#include "thyme.h"
#include <stdio.h>
#include <stdlib.h>

int reg_pl(struct Sreg *i, int color);

/*-----------------TMESH------------------------------------------------
 * Just a control dummy.
 *----------------------------------------------------------------------*/
char * tmesh(divide)
int divide;
{
    char *err,*hinit(),*bfill(),*triang(),*ckmesh();
    int ir; double elap;


   /*...If this grid has been trianulated before, bag it */
    if (ntri > 0) 
	{fprintf (stderr, "Warning - grid already has triangles\n"); return(0);}

   /*...Check what we've got and set up counter-clockwise regions. */
    elap = log_cpu (0.0, "");
    err = ckmesh();					if (err) return(err);
      
   /*...First step is to calculate local spacing for each node. */
    err = hinit(); 					if (err) return(err);

   /*...Now add new points to edges. */
    if (divide)  {
	err = bfill(); 					
	if (err) return(err);
    }

   /*...Triangulate each region. */
    for (ir=1; ir <= nreg; ir++)  {
	err = triang (ir); 				if (err) return(err);
	}

    elap = log_cpu (elap, "Triangulation");
    return(0);
}




/*-----------------TRIANG-----------------------------------------------
 * Triangulate a region. The core routine.
 *----------------------------------------------------------------------*/
char * triang(ir)
    int ir;
{
    int ie,nt,f,save_nedge,save_nreg,divide(),nn,split;
    char *err, *sp_reg(), *dvpram(), *dvedge(), *dupl();
    struct LLedge *bp,*nbp,*bnd,*chop(),*quad();
    double ha,hb,lej,ff,rr,l_edge();

  /* 
   * Save values of nreg, nedge. Storage beyond original 
   * limits is used for temporary stack.
   */
    save_nedge = nedge;
    save_nreg = nreg;

  /*
   * Copy region to working area (don't want to hack user regions)
   */
    err = dupl(ir);					if (err) return(err);

  /*...Triangulation loop */
    split=0;
    while (nreg > save_nreg) {

	if (debug2) reg_pl(reg[nreg],1);
	
      /*
       * If the next region is a triangle, save it.
       * The triangle gets region number ir.
       */
	if (reg[nreg]->len == 3) {
	    nt = 0; 
	    bnd = reg[nreg]->bnd;
	    err = cr_tri (&nt, ir, nB(bnd->prev), nB(bnd), nB(bnd->next),-1,-1,-1);
	    if(err) return(err);

	  /*...And dispose of the region. */
	    for (f=1, bp=bnd; (bp != bnd) || f; bp = nbp, f=0) {
	         nbp = bp->next; free(bp);
		 }
	    free(reg[nreg]);
	    reg[nreg--] = 0;
	    split = FALSE;
	    printf("After Triangle\n");
	    }
      
      /*...Special-case quadrilaterals */
	else if (reg[nreg]->len == 4) {
	    bp = quad(reg[nreg]);	    if (!bp) return("Error in quad");
	    err = sp_reg (bp->prev, bp->next);		if (err) return(err);
	    split = TRUE;
	    printf("After Quad\n");
	    }

      /*...Cut off an acute triangle? */
	else if ((bp = chop (reg[nreg], FALSE))) {
	    err = sp_reg (bp->prev, bp->next); 		if (err) return(err);
	    split = TRUE;
	    printf("After Chop\n");
	    }

      /*...Cut it in half? */
	else if (divide(reg[nreg],&bp,&nbp)) {
	    err = sp_reg (bp, nbp);			if(err) return(err);
	    split = TRUE;
	    lej = l_edge(nedge);
	    ha  = node[edge[nedge]->n[0]]->h;
	    hb  = node[edge[nedge]->n[1]]->h;
	    err = dvpram(ha, hb, lej, &nn, &rr, &ff);	if (err) return(err);
	    err = dvedge(nedge, lej, nn, rr, ff);	if (err) return(err);
	    printf("After Divide\n");
	    }

      /*
       * Well nothing else worked, so we cut off the least awful 
       * triangle we can.
       */
	 else {
	    bp = chop(reg[nreg],TRUE); 	       if (!bp) return("error in chop");
	    err = sp_reg (bp->prev, bp->next);		if(err) return(err);
	    split = TRUE;
	    printf("After Forced Chop\n");
	    }

      if (debug2 && split) {
	  reg_pl(reg[nreg],1);
	  reg_pl(reg[nreg],1);
	  }
	}
    

  /*...Dispose of edge storage and reset true values of nreg,nedge. */
    for (ie=save_nedge+1; ie <= nedge; ie++) {
        free (edge[ie]);
	edge[ie] = 0; 
	}
    nreg = save_nreg;
    nedge = save_nedge;

    return(0);
}
