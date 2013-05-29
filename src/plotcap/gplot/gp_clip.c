/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* "fclip" : clips line segments that extend outside a box
 *
 * date: 26 feb 87 (mje)
 *
 * returns:
 *	-1 	Both points are out of the box.
 *	 0	Both points are in the box.
 *	 1,2,3,4 Point P1new was clipped to the given side.
 *
 *      Thus, a reasonable method of use is:
 *
 *		if (clip(&P1new,&P2new, &P1old,&P2old ) >= 0)
 *			"draw_from P1new to P2new" ;
 *		else    "no_draw" ;
 * notes:
 *	++ A box is numbered like, where (n) is a point/side number and
 *		[n] is a side number:
 *
 *		(1) ---- [2] ---- (2)
 *		 |	           |
 *		 |		   |
 *		[1]	     	  [3]
 *		 |		   |
 *		 |		   |
 *		(0) ---- [4] ---- (3)
 *
 * written:  Michael Eldredge (sep 83)
 * modified: Michael Eldredge (mar 85) Take {x,y}{min,max} from common. Thus
 *	calling sequence changed from:
 *		iclip(&P1new,&P2new, &P1old,&P2old, xmin,xmax, ymin,ymax) ;
 *	to:
 *		iclip(&P1new,&P2new, &P1old,&P2old ) ;
 *	This should speed things up a bit (by passing less parameters).
 * modified: Michael Eldredge (jun 86) floating point clip
 * modified: Michael Eldredge (feb 87) return which side was intersected.
 *	Whole return interface changed, beware!
 */

#include "auxfns.h"
#include "gp_def.h"
#include "gp_com.h"

/* The low and high bound of the box */
#define X0 B[0].x
#define Y0 B[0].y
#define X1 B[2].x
#define Y1 B[2].y

/* each side, 1st(lowest) bit is side 1, 2nd bit is side 2, etc.. */
#define SIDE1	0x01
#define SIDE2	0x02
#define SIDE3	0x04
#define SIDE4	0x08


/* "fclip": clip floats to within box B */
int
fclip(P1new, P2new, P1old, P2old, B)
	fpoint2d *P1new, *P2new, *P1old, *P2old;
	fpoint2d  B[] ;		/* bounding box */
	{

	Bool   P1out, P2out;         /* flag if point out of box */
	int    is, is2;		/* intersected side. */

	/* in case none out of bounds, just copy over to new point */
	P1new->x = P1old->x ;
	P1new->y = P1old->y ;
	P2new->x = P2old->x ;
	P2new->y = P2old->y ;

	/* flag if Point1 and/or Point2 are out of bounds and which sides
	 * they may have gone through
	 */
	P1out = P2out = 0 ;
	if      (P1old->x < X0) P1out |= SIDE1 ;
	else if (P1old->x > X1) P1out |= SIDE3 ;
	if      (P1old->y < Y0) P1out |= SIDE4 ;
	else if (P1old->y > Y1) P1out |= SIDE2 ;

	if      (P2old->x < X0) P2out |= SIDE1 ;
	else if (P2old->x > X1) P2out |= SIDE3 ;
	if      (P2old->y < Y0) P2out |= SIDE4 ;
	else if (P2old->y > Y1) P2out |= SIDE2 ;

	if (!P1out && !P2out) return (0);  /* return (NO CLIP done) */

/* Box sides */
#define BS1 &B[0],&B[1]
#define BS2 &B[1],&B[2]
#define BS3 &B[2],&B[3]
#define BS4 &B[3],&B[0]

/* Psect(P1, SIDE1) --> P1\**\out&SIDE1 --> P1out & SIDE 1 */
#define Psect(PNT,SID, BS) \
	((PNT ## out & SID) && fnsect(PNT ## new, P1old, P2old,  BS))

	/* clip if Point1 is out of bounds */
	is = 0;
	if (P1out) {
		if      (Psect(P1, SIDE1, BS1)) is = 1 ;
		else if (Psect(P1, SIDE2, BS2)) is = 2 ;
		else if (Psect(P1, SIDE3, BS3)) is = 3 ;
		else if (Psect(P1, SIDE4, BS4)) is = 4 ;
#ifdef FOO
		else  /* both out and no intersection: a way out segment */
			return (-1);
#endif
		}

	is2 = 0 ;
	if (P2out) {
		if      (Psect(P2, SIDE1, BS1)) is2 = 1 ;
		else if (Psect(P2, SIDE2, BS2)) is2 = 2 ;
		else if (Psect(P2, SIDE3, BS3)) is2 = 3 ;
		else if (Psect(P2, SIDE4, BS4)) is2 = 4 ;
		}

	if (P1out && P2out) {  /* may have to switch points if both out */
		if (is == 0 && is2 == 0) return -1 ; /* both out */
		}
#ifdef DEBUG
	if (P1out && P2out) {  /* may have to switch points if both out */
		/*DEBUG - old way, may have to switch */
		float  j;
		float  dx, dy;	/* int */
		float  L1, L2;	/* int */

		if (is == 0)
		  fprintf(stderr,"fclip: P1out= 0x%x, is =0!\n",P1out);
		if (is2== 0)
		  fprintf(stderr,"fclip: P2out= 0x%x, is2=0!\n",P2out);
		dx = P1old->x  -  P1new->x ;
		dy = P1old->y  -  P1new->y ;
		L1 = dx * dx  +  dy * dy   ;

		dx = P1old->x  -  P2new->x ;
		dy = P1old->y  -  P2new->y ;
		L2 = dx * dx  +  dy * dy   ;

		if (L1 > L2) {
			fprintf(stderr,"fclip: swap\n");
			j        = P1new->x ;
			P1new->x = P2new->x ;
			P2new->x = j        ;

			j        = P1new->y ;
			P1new->y = P2new->y ;
			P2new->y = j        ;

			is = is2 ;		/* it was the other */
			}
		}
#endif /*DEBUG*/

	return (is);        /* return that we did a clip */
	}


/* "fnsect" : determine if an intersection exists between the line segments
 *	'A' and 'B'. If an intersection, then 'P' will be that point, else
 *	'P' is unchanged.
 *
 * date: 24 feb 87
 *
 * written:  Michael Eldredge (nov 83)
 * modified: Michael Eldredge (feb 87) Internals need to be doubles.
 *	If one segement was huge (ie: A = (2,-10e6),(2,20) ) then the
 *	lose can lead to intersections being off by 1" or so.
 */

#define rnd .001
int
fnsect(P,  A1,A2, B1,B2)
	fpoint2d  *P, *A1, *A2, *B1, *B2;
	{

	double  x43, x21, x31;
	double  y43, y21, y31;
	double  denom, T1, T2;   /* denominator, parametric params */
	int    stat;

	/* assign the differences between points for later */
	x43  = B2->x  -  B1->x ;
	x21  = A2->x  -  A1->x ;
	x31  = B1->x  -  A1->x ;

	y43  = B2->y  -  B1->y ;
	y21  = A2->y  -  A1->y ;
	y31  = B1->y  -  A1->y ;

	stat = 0;

	denom = y21*x43 - x21*y43 ;     /* calc denom */
	if (denom != 0.0)               /* if not parellel */
		{
		T1 = (y31*x43 - x31*y43) / denom;
		T2 = (y31*x21 - x31*y21) / denom;

		/* New way- I did this for a reason, what was it? back to
		 *	the old way until I remember
		 */
	/*	if (0.0 <  T1 && T1 <= 1.0 && 0.0 <  T2 && T2 <= 1.0 ) */
		if (0.0 <= T1 && T1 <= 1.0 && 0.0 <= T2 && T2 <= 1.0 )
	/*	if (0.0 <  T1 && T1 <  1.0 && 0.0 <  T2 && T2 <  1.0 ) */
			{
			/* segments intersect, return that point */
			P->x = A1->x + (T1*x21 ) ;
			P->y = A1->y + (T1*y21 ) ;
			stat = 1;
			}
		}

	return (stat);
	}
