static char rcsid[]="$Header: /users/suprem/ig2/tri/RCS/opt.c,v 1.7 85/10/30 23:25:27 conor Exp $";
/***********************************************************************
 *                                                                     *
 * opt.c - main loop of optimizer.                                     *
 *                                                                     *
 * Copyright c 1985 The board of trustees of the Leland Stanford       *
 *                  Junior University. All rights reserved.            *
 * This subroutine may not be used outside of the SUPREM4 computer     *
 * program without the prior written consent of Stanford University.   *
 *                                                                     *
 * Original: CSR Nov.84                                                *
 *                                                                     *
 ***********************************************************************/
#include "general.h"
#include <stdio.h>
#include "dbase.h"

extern int ntfunc;		/* Imported from gn_func.c */


/*-----------------OPT--------------------------------------------------
 * Main loop of optimizer.
 *----------------------------------------------------------------------*/
char *opt ( cmd, tol, intol, itmax )
    char *cmd;
    double tol, intol;
    int itmax;
{
    int icmd;
    double *scale;
    char *err, *lscale(), *gn_opt(), *laplace(), *flip(), *mx_init();

  /*...Compute length scales, used in stopping criteria. */
    err = lscale(&scale);	if (err) return(err);

  /*...Mark edge nodes as not to be moved. */
    set_elec();

  /*...Main loop */
    for (icmd=0; cmd[icmd] != '\0'; icmd++) {
	switch  (cmd[icmd]) {

	case 'f':
	    err = flip (); 	if (err) return(err);
	  /*...Must fix connectivities - triangle neighbors are ok */
	    list_p2t ();	
	    list_p2p ();
	    if (summary) score("After Lawson flip");
	    break;

	case 'a':
	    err = mx_init (1);	if (err) return(err);
	    err = laplace();    	if (err) return(err);
	    mx_free();
	    if (summary) score("After averaging");
	    break;
	    
	case 'o':
	    err = mx_init (2);	if (err) return(err);
	    err = gn_opt (2*nnode,ntri*ntfunc,6, tol,intol,itmax, debug2,scale);
	    if (err) return(err);
	    mx_free();
	    if (summary) score("After optimization");
	    break;
	
	default:
	    fprintf(stderr,"opt: unknown command %c\n",cmd[icmd]);
	    break;
	}
    }

    return(0);
}

