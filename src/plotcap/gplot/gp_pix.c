/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* date: 03 may 84 (mje)
 *
 * "gp_pix.c": Pixel (raster device) handling routines.
 *
 * written: Michael Eldredge (sep 83)
 * mod # 1: Mje - (may 84) Have drawing modes now (SET, COMP, CLR)
 */



#include "auxfns.h"
#include "al_alu18.h"
#include "gp_def.h"
#include "gp_com.h"
#include "gplot.h"


/* "setpix" : this routine will set a pixel in a linear byte array.
 *              it does the proper indexing (using externals) to set
 *              the pixel in column 'col' and row 'row'.
 */
#ifdef ANSI_FUNC

int 
setpix (int col, int row)
#else

setpix(col,row)
	int row, col;
#endif

{
	int p_byt;                  /* indexed byte in 'pixels' */
	char p_bit;                  /*  and the bit in that byte */
	static char bmask[8] =       /* bit mapping matrix */
		{
		001 , 002 , 004 , 010 , 020 , 040 , 0100 , 0200
		};	/* Expand this up to some max possible bits */

	/* check to make sure that a valid point first */ /* !!! ditch this*/
#ifdef NO_CLIPPING
	if (row < 0 || row > C.PIXY || col < 0 || col > C.PIXX)
		return;
#endif

	/* index the the correct byte in the 'pixels' array */
	p_byt = bytes_line * row  +  col / C.RPXB;    /* which byte ? */

	if (C.RREV)    p_bit = bmask[ C.RPXB - 1 - (col % C.RPXB) ];
	else           p_bit = bmask[ col % C.RPXB ];

	/* set pixels according to current draw mode (on, off, complement) */
	switch (g_drawmd) { 
	case G_MSET:  /* most often used */	
		pixels[p_byt] |= p_bit;
		break;

	case G_MCMP:  /* complement */
		pixels[p_byt] ^= p_bit;
		break;

	case G_MCLR:  /* clear the bit */
		pixels[p_byt] &= ~p_bit;
		break;
		}
}




#ifdef ANSI_FUNC

int 
out_pix (void)
#else

out_pix()
#endif
	{
	int nextot, curtot;

	/* the opening sequence for a plot is first */
	docmd(C.PLOP);

	curtot = tot_bytes ;
	while (curtot > 0) {
		nextot = curtot - bytes_line;

		docmd(C.BFOP) ;
		al_outb( &pixels[nextot] , bytes_line ) ;
		docmd(C.BFCL) ;

		curtot = nextot;
		}

	docmd(C.PLCL);
	docmd(ALU_POST) ;	/* make sure everything is out */
	/* ^ maybe remove this since setdn should do the final post */
	return (T);
	}



/* "clear_pix" : reset the big pixel array to zero or the initial value
 *	given by PIVL.
 *
 * date: 03 feb 83 (mje)
 */

#ifdef ANSI_FUNC

int 
clear_pix (void)
#else

clear_pix()
#endif
	{
	register int  i;

	for (i=0; i < tot_bytes; ) pixels[i++] = C.PIVL;
	}
