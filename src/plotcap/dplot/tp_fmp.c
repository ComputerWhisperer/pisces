/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* "tpfmp.c" : routines to open and read "tplot" type binary save plot files.
 *
 * tpopen : open the given file and try to determine if it is a tplot file.
 * tpread : read the x, y, pen values from the file.
 */

/* to define record size:
 *   <byte count:long> <x:float> <y:float> <pen:long> <byte count:long>
 */
#define  CNTSIZE  sizeof(long)
#define  REALSIZE sizeof(float)
#define  INTSIZE  sizeof(long)

/* define the size of the tplot record: */
#define TPSIZE  ( 2*CNTSIZE + 2*REALSIZE + INTSIZE )
#define BCOUNT  ( 2*REALSIZE + INTSIZE )

/* offset into record for beginning of second byte count:
 *   TPSIZE - sizeof( <final-byte-count:long> )
 */
#define N2LOC  (TPSIZE - CNTSIZE)


static char tpbuf[TPSIZE] ;

/* for "tpopen" :  return >= 0 then lu of file.
 *	else BADFILE means doesn't look like a tplot file
 *	else BADOPEN means couldn't get the file open before the check.
 */
#define BADOPEN  -1
#define BADFILE  -2

int 
tpopen(namr, mode)
	char  *namr;
	int   mode ;	/* open mode:  0=read */
	{
	int  lu ;
	long n1, n2;

	switch (mode) {

	case 0:		/* READ mode */

		if ((lu = open(namr, mode)) < 0) {  /* couldn't open. */
			return(BADOPEN);
			}

		/* check first record to see if it looks like a tplot type */
		if (read(lu, tpbuf, TPSIZE) != TPSIZE) return(BADFILE);

		/*NOSTRICT*/
		n1 = *(long *)tpbuf ;		/* record byte count ? */
		/*NOSTRICT*/
		n2 = *(long *)(tpbuf + N2LOC);  /* record byte count ? */


		if (n1 == n2  &&  n1 == BCOUNT) {    /* !! GOOD !! */
			lseek(lu, 0L, 0) ;		/* back to beginning */
			}

		else {    /* really missed on this one */
			close(lu);
			lu = BADFILE ;		/* flag bad file */
			}

		return(lu);
		break;

		}/*of switch */

	return -1 ;
	}/*of 'tpopen' */

int tpread(lu, x,y,pen) 
	int   lu;
	float *x, *y;
	int   *pen;
{

	char *bp;
	int  nch;

	/* ---start of tpread--- */
	if ( (nch=read(lu, tpbuf, TPSIZE)) <= 0) return (nch);

	bp = tpbuf + CNTSIZE ;   /* skip the record byte count */
	*x   = *(float *)bp;   bp += REALSIZE ;
	*y   = *(float *)bp;   bp += REALSIZE ;
	*pen = *(long  *)bp;
	return (nch);
}
