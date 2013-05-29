/*	%M%		Version %I%		*/
/*	Last Modification:	%G% %U%		*/


/*static char rcsid[]="$Header: /users/suprem/ig2/tri/RCS/main.c,v 1.7 85/10/30 23:25:46 conor Exp $";*/
/***********************************************************************
 *                                                                     *
 * tri - triangle generation and optimization program.                 *
 *                                                                     *
 * Copyright c 1985 The board of trustees of the Leland Stanford       *
 *                  Junior University. All rights reserved.            *
 * This subroutine may not be used outside of the IGGI computer        *
 * program without the prior written consent of Stanford University.   *
 *                                                                     *
 * Original: CSR Nov.84                                                *
 *                                                                     *
 ***********************************************************************/
#define STATIC_ALLOCATION_TIME
#include "general.h"
#include "dbase.h"
#include "skelp.h"
#include "sparse.h"
#include "thyme.h"
#include "griph.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void cleardb(void);

main(argc,argv)
    int argc;
    char *argv[];
{
    int iop,it;
    int divide = TRUE;
    char infil [DIRSIZ], outfil [DIRSIZ], pisfil [DIRSIZ], cmd[20];
    char c,*err, *rumesh(), *tmesh(), *wumesh(), *wpmesh(), *opt(), 
	   *init_screen(), *list_p2p(), *list_t2t(), *list_p2t();
    int itmax = 15;			/* Max number of GN loops */
    double tol = 1e-2;			/* Finishing tolerance. */
    double intol = 1e-2;		/* Inner tolerance. */

  /*...Initialize the dbase. */
    cleardb();

    infil[0]  = '\0';
    outfil[0] = '\0';

  /*...Get options. */
    infil[0] = pisfil[0] = '\0'; strcpy (cmd, "fa");

    for (iop=1; iop < argc; iop++) {
	if   (!strncmp(argv[iop],"-C",2)) strcpy (cmd, &argv[iop][2]);
	else if (strequ(argv[iop], "-D")) debug2 = TRUE;
	else if (strequ(argv[iop], "-c")) cpu_set("opt.cpu");
	else if (strequ(argv[iop], "-d")) debug1 = TRUE;
	else if (strequ(argv[iop], "-i")) sscanf (argv[++iop], "%lf", &intol);
	else if (strequ(argv[iop], "-l")) sscanf (argv[++iop], "%d", &itmax);
	else if (strequ(argv[iop], "-m")) sscanf(argv[++iop], "%lf",&mgeom);
	else if (strequ(argv[iop], "-p")) sscanf (argv[++iop], "%s",pisfil);
	else if (strequ(argv[iop], "-r")) sscanf(argv[++iop], "%lf",&mr);
	else if (strequ(argv[iop], "-s")) summary = TRUE;
	else if (strequ(argv[iop], "-t")) sscanf (argv[++iop], "%lf", &tol);
        else if (strequ(argv[iop], "-o")) strcpy(outfil,argv[++iop]);
        else if (strequ(argv[iop], "-n")) divide = FALSE;
	else if (argv[iop][0]=='-') 
	    printf("tri: unknown option %s\n",argv[iop]);

      /*...Anything else is taken as an input file. */
	else
	    sscanf(argv[iop],"%s",infil);
	}

  /*...Check input strings. */
    if (!infil[0]) { printf("Usage : tri input -o output [-d]\n"); exit(1); }
    if (!outfil[0] && !pisfil[0]) strcpy(outfil,"t.out");

   /*...Read user input.  */
    err = rumesh(infil); 			
    if(err) {printf("Error in input...\n%s\n",err); exit(1);}

   /*...If debug1/display then set up initial display. */
    if (debug2) {debug1=TRUE; DsReg=1;}
    if (debug1) {
	if (err = init_screen())
	    {printf("tri: %s\n",err); exit(1);}
	DsNode = DsEdge = TRUE;
	DsReg  = DsTri  = FALSE;
	draw_mesh();
    }

   /*...Generate triangles. */
    if (err = tmesh(divide))
        { printf("tri: error in triangulation...%s\n",err); exit(1); }

    if (debug1) term_screen();

    if (summary) {score(NL); score("Initial triangulation");}

  /*...Compute connectivities */
    if ((err = list_p2t()) || (err = list_t2t()) || (err = list_p2p()))
	{ printf("tri: error in connectivites...%s\n", err); exit(1); }

  /*...Optimise */
    DsNode = DsEdge = DsReg = FALSE;
    DsTri = TRUE;
    err = opt (cmd, tol, intol, itmax); 
    if (err) {printf("tri: error in optimization... %s\n",err);exit(1);}
/*
    if (debug1) term_screen();
*/
  /*...Output options */
    if (*pisfil) {
	err = wpmesh(pisfil); if (err) {printf("tri: %s\n",err); exit(1);}
	}
    if (*outfil) {
	err = wumesh(outfil); if (err) {printf("tri: %s\n",err); exit(1);}
	}

    exit(0);
}


/* 
 * Initialize the db.
 */
void cleardb(void)
{
    int i;

    for (i=0; i < MAXNODE; i++) node[i] = 0;
    for (i=0; i < MAXTRI;  i++) tri[i]  = 0;
    for (i=0; i < MAXREG;  i++) reg[i]  = 0;
    for (i=0; i < MAXEDGE; i++) edge[i] = 0;

    nedge = 0; 
    ntri  = 0;
    nreg  = 0;
    nnode = 0;

    debug1 = FALSE;
    debug2 = FALSE;

    mr = 1.5;
    mgeom = 0.6;

    DefCw = DefCh = 0.1333;
    DsFill = 0;
    ClrElec = 2; ClrReg=3; ClrTri = 1;
    DsNode = DsEdge = DsReg = DsTri = TRUE;
}
