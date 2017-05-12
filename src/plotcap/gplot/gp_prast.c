/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* "prast.c" : Point rasterizer. Fills in all points between P1 and P2
 *
 * date: 14 sep 83 (mje)
 * written:  Michael Eldredge (sep 83)
 */

#define abs(X)    ((X < 0) ? -X : X)

/* the rasterizer */
#ifdef ANSI_FUNC

int 
prast (int x1, int y1, int x2, int y2)
#else

prast(x1,y1, x2,y2)
	int x1, y1, x2,y2;
#endif
	{

	int   dx, dy;            /* delta x,y */
	float slope;
	int   withX;
	int   point1, point2;
        int   point ;

	float s,  t;             /* parametric variables */
	float S1, T1;            /*  and starting point  */
	int   is, it;            /* some integer values for s,t */


	/* ----start of prast---- */
	dx = x1-x2;
	dy = y1-y2;

	withX = ( abs(dx) > abs(dy) );        /* count by X or by Y */

	slope = 0.0;
	if (dx != 0) slope = (float)dy / (float)dx;

        if (!withX && slope!=0.0) slope = 1. / slope;

	if (withX) {point1=x1; point2=x2; S1=x1; T1=y1; }
	else       {point1=y1; point2=y2; S1=y1; T1=x1; }

	/* always count to bigger point */
	if (point1 > point2) {point = point1; point1=point2; point2=point;}

	/* step through each point and find the pixel closest to the 
	 *  real point as given by the parametric equation 
	 */
	for (s=point1; s<=point2; s++)
		{
		t = slope * (s-S1)  +  T1;    /* determine other co-ord */

		it = t + 0.5;                 /* truncate to nearest int */
		is = s + .0001;               /* just in case s a little low */

		if (withX) setpix(is,it);
		else       setpix(it,is);
		}

	/* that's  all folks */
	}
