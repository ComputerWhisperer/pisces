static char rcsid[]="$Header: /users/suprem/ig2/sparse/RCS/min_fill.c,v 1.2 85/06/02 02:14:10 conor Exp $";
/*************************************************************************
 *									 *
 *     Copyright c 1984 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the SUPREM4 computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/

/************************************************************************
 *									*
 *	This file contains the routines which implement the minimum 	*
 *  fill reordering of nodes.  						*
 *									*
 ************************************************************************/

#include "general.h"
#include <stdlib.h>
#include <stdio.h>
#define MAXINT neq+100		/* Bigger than the biggest variable */


/************************************************************************
 *									*
 *	min_fill( neq, ia, ja, reorder)                                 *
 *  generate a reordering list for the minimum fill of the	        *
 *  matrix.								*
 *									*
 *  Original:	MEL	11/84						*
 *  Modified:   CSR     12/84  To use linked list representation.       *
 *  Modified:   CSR      6/85  To use no inkludges.                     *
 *									*
 ************************************************************************/

	    
char * min_fill (neq, ia, ja, reorder)
    int neq;			/* Number of equations */
    int *ia;			/* Column pointers. */
    short *ja;			/* Row entries */
    int *reorder;		/* Returned : order to pivot.    */
{
    int **next; 		/*current list of neighbors*/
    int *cnt;			/*number of neighbors*/
    int *size;			/*space allocated so far*/
    int *new;			/*the new neighbors in the fill portion*/
    int *list;			/*the list of all those who are left to do*/
    int listnum;		/*the number of the above*/
    int update;			/*location at which best was found in list*/
    int i, j, k;		/*guess what these are used for*/
    int best;			/*the node currently beign "eliminated"*/
    int elim;			/*the number of eliminated nodes so far*/
    int fom;			/*the number of neighbors of best*/
    int oldfom;			/*a number to compare against from last pass*/
    int curr, spot;		/*useful helpers*/


    /*the idea here is to do a mock gauss elimintion, updating the pt to
      pt list at each step.  At each step, a decision is made about the
      optimal node to work on next.					*/

    /*...Grab some space */
    if (!(
	 (next = (int **) calloc(neq, sizeof (int *)) ) &&
	 (cnt  = (int *)  calloc(neq, sizeof (int))   ) &&
	 (size = (int *)  calloc(neq, sizeof (int))   ) &&
	 (new  = (int *)  calloc(neq, sizeof (int))   ) &&
	 (list = (int *)  calloc(neq, sizeof (int))   ) 
	)) return("Insufficient storage for min_fill arrays");
    
    /*initialize one and all - 0 to 1 convention fixed here.*/
    for (i = 0; i < neq; i++) {
	cnt[i] = ia[i+1] - ia[i];
	size[i] = (20 > cnt[i]) ? 20 : cnt[i];
	next[i] = (int *) calloc(size[i], sizeof(int));
	for (j = 0; j < cnt[i]; j++)
	    next[i][j] = ja[ia[i]+j] -1;
	new[i] = FALSE;
	list[i] = i;
	}
    listnum = neq;

    /*loop until all the nodes are done*/
    oldfom = -1;
    for(elim = 0; elim < neq; elim++) {

	/*implement a simple search to find the best node*/
	for(fom = MAXINT, i = 0; (i < listnum) && (fom != oldfom); i++) {
	    if (cnt[ list[i] ] < fom ) {
		fom = cnt[ list[i] ];
		best = list[i];
		update = i;
	    }
	}
	oldfom = fom;

	/*we are now going to "eliminate" node best, update the pointers*/
	/*if A(p) = adjacency list of p, then for every p in A(best),
	  p = A(best) union A(p)					*/
	/* (I wish it were that simple to code. - MEL) 			*/

	for(i = 0; i < cnt[best]; i++) {
	    curr = next[best][i];
	    
	    /*fortunately, we do not have to do this if i has been eliminated*/
	    if ((curr != best) && (curr != -1)) {
		
		/*figure the adjancency of curr*/
		for(j = 0; j < cnt[curr]; j++) {
		    new[k = next[curr][j]] = TRUE;
		    if (k == best)
			spot = j;
		}

		/*remove the best node from the list of neighbors*/
		if (spot != (cnt[curr] - 1))
		    next[curr][ spot ] = next[curr][cnt[curr] - 1];
		cnt[curr]--;

		/*allocate enough space for the new nodes*/
		if (size[curr] < (cnt[curr] + cnt[best])) {
		    size[curr] = cnt[curr] + cnt[best];
		    next[curr] = (int *)realloc(next[curr],
					    sizeof(int) * size[curr]);
		}

		/*make the new list by adding the adjacency of best to the end*/
		for(j = 0; j < cnt[best]; j++) {
		    if ( ! new[ next[best][j] ] ) {
			next[curr][ cnt[curr]++ ] = next[best][j];
		    }
		}
		/*reset the neighbor list*/
		for(j = 0; j < cnt[curr]; j++)
		    new[ next[curr][j] ] = FALSE;
		new[best] = FALSE;

		if (cnt[curr] < oldfom)
		    oldfom = cnt[curr];
	    }
	}

	/*mark best as gone and dead*/
	cnt[best]     = -1;
	reorder[best] = elim;
	free( next[best] );
	list[update] = list[ --listnum ];
    }
    free(cnt);	
    free(size);
    free(new);
    free(list);
    return(0);
}

