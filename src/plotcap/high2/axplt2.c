/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* Thu Jul 14 16:02:53 PDT 1988 (dredge--stanford)
 *ff
 * "axplt" : user interface for axis plotting.
 *
 * calling sequence:
 *  axplt2(xst, yst, len, axang, Fval, Eval, Flab, Llab, ltic, utic, labht,
 *            tlang, tloc, tang, tht, tdp, title, axtht, lform, vform)
 *
 * where:
 *  xst -- (float) X coordinate of start of axis, in plot units
 *  yst -- (float) Y coordinate of start of axis, in plot units
 *  len -- (float) length of axis in plot units, drawn from (xst, yst)
 *  axang   -- (float) angle at which axis is to be drawn, measured from
 *         horizontal. "axang" > 0 implies angle is measured
 *         counterclockwise from horizontal.
 *  Fval    -- (float) axis value at start of axis
 *  Eval    -- (float) axis value at end of axis
 *  Flab    -- (float) first labelled value on the axis.  This value
 *         will always appear on the axis.
 *  Llab    -- (float) maximum label that will be printed on the axis.
 *         This value may or may not appear, depending on whether
 *         it is an integral number of major ticks away from Flab.
 *  ltic    -- (float) value between labelled ticks, i.e. "distance" between
 *         each labelled tick mark.  For a logarithmic axis, this
 *         must be a 10.
 *  utic    -- (int) number of unlabelled tick marks between each labelled
 *         tick. "utic" < 0 means logarithmic spacing of ticks.  If
 *         "utic" is in fact less than 0, its value represents the
 *         percent increment of the minor tick marks.  For example,
 *         if utic = -2, then minor tick marks will be placed at
 *         20%, 40%, 60%, and 80% of the decade.  utic = -3 means
 *         minor ticks at 30%, 60%, and 90%, and so on.  Of course,
 *         if utic has a magnitude greater than 10 in the
 *         logarithmic case, bogus spacings will occur.  The only
 *         exception is if utic = -1.  In this case, no tick is
 *         placed at 10%, with ticks being placed at 20%, 30%, 40%, ....
 *  labht   -- (float) height of major tick labels.  0 ==> no labels.
 *  tlang   -- (float) angle at which labels on ticks are to be drawn.  This
 *         angle is measured as positive being counterclockwise from
 *         axang.
 *  tloc    -- (int) flag naming the location of the tick labels.  This
 *         parameter has 2 values:
 *          1 ==> labels on "top" of axis.  Top is regarded as
 *                above the axis if axang were 0.
 *          0 ==> labels on "bottom" of axis.  This is interpreted
 *                the same way as "top."
 *  tang    -- (float) angle at which tick itself is to be drawn.
 *         This angle is measured with respect to "axang."
 *  tht -- (float) height of major ticks, in plot units.  This value
 *         is in the direction of the "top" of the axis.
 *  tdp     -- (float) depth of major tick, in plot units.  This value is
 *         in the direction of the "bottom" of the axis.
 *  title   -- (char []) address of character array containing axis title.
 *  axtht   -- (float) axis title height, in plot units.
 *  lform   -- (int) location of axis title, according to the
 *         following key:
 *          2 ==> top, axang+180
 *          1 ==> top, axang
 *             -1 ==> bottom, axang
 *             -2 ==> bottom, axang+180
 *  vform   -- (char []) format of numerical labels.  This should not
 *         have a '%' in it, as this routine adds one.
 *
 * return values:  none
 *
 * notes:
 *  * This routine is insane with the number of parameters.  Its intention
 *    is to be everything to everyone in axis drawing.   This is of course
 *    an absurd hope.  It does, however, do many things for many people.
 *    In adding the flexibility, there is also a great deal of complexity.
 *    Likewise, there are still some bugs when this is used  in some
 *    situations.
 *
 *of
 * written: Daniel Lopez
 * Revision 1.1  86/04/30  11:19:47  dredge
 * Initial revision
 *
 * Revision 1.7  85/12/19  10:47:07  dredge
 * Well round off is a problem for log plots on machines with higher (than
 *  the VAX) precision exponents. So it has been refixed for this case.
 *
 * Revision 1.6  85/12/02  16:30:45  dredge
 * Fixed bug for rounding to zero.  shouldn't have been done for log plots.
 *
 * Revision 1.5  85/11/15  15:03:33  dredge
 * fixed debug bug (getenv"AXPLTO")
 *
 * Revision 1.4  85/11/13  12:22:37  dredge
 * DEBUG version. Reset "lab_off" for Axis titles to be .55% labht, But
 *   for debugging the env var AXPLTO can be set to a new value and axplt
 *   will use that (e.g: setenv AXPLTO 0.75).
 *
 * Revision 1.3  85/11/04  13:36:11  dredge
 * Fixed: very small numbers for labels will be written as Zero.
 *
 * Revision 1.2  85/07/17  16:11:34  dredge
 *   Conor's fixes to end point problems.
 *
 * Revision 1.2  85/07/08  12:20:28  conor
 *   Bug fixes: allow for roundoff problems, variable strt used before
 *   defined.
 * modified: Michael Eldredge (mar 87) Put gplot2(G_MARK, ...) marks.
 *
 */

#include <string.h>
#include <stdio.h>
#include <math.h>
#include "gplot.h"
#include "gpmarks.h"

#ifdef DEBUG
#include <stdio.h>
#endif

#define PI  3.141592654
#define abs(X) ((X) < 0.0? -(X): (X))

static double log_10(double val);
static float Fmod(float num, float denom);

/***
 * This is a lookup table for heighth and widht orientations of labels.
 * These values tell numb() and symb() how to use the point passed to it. These
 * orientation fields are used as follows:
 *
 *       1 ---------------------------
 *         |                         |
 *  hornt  |   string to be printed  |
 *       0 |                         |
 *         ---------------------------
 *         0                         1
 *                   wornt
 *
 * If the angle that the string to be printed at is less than or equal to
 * the given boundary angle, then the corresponding values of hornt and
 * wornt will be sent to numb() or symb().  The bornt array is used if the
 * string is to be printed on the "bottom" of the axis.  The tornt array is
 * used if the string is to be printed on the "top" of the axis.
 ***/

static struct {
    double boundang, hornt, wornt;
    } bornt[] = {
		{ 45., 1.0, 0.5},
		{135., 0.5, 1.0},
		{225., 0.0, 0.5},
		{315., 0.5, 0.0}
    };


static struct {
    double boundang, hornt, wornt;
	} tornt[] = {
	    { 45., 0.0, 0.5},
	    {135., 0.5, 0.0},
	    {225., 1.0, 0.5},
	    {315., 0.5, 1.0}
	};


/* "axplt2": Draw a 2d axis */
axplt2(xst, yst, len, axang, Fval, Eval, Flab, Llab,
			ltic, utic, labht, tlang,  tloc, tang, tht, tdp,
			title, axtht, lform, vform)

	float xst, yst, len, axang,     /* axis location paramters  */
	Fval, Eval, Flab, Llab, ltic,labht;/* axis value parameters */
	int   utic, tloc;
	float tlang, tang, tht, tdp;        /* tick mark information    */
	char  *title;               /* axis title           */
	float axtht;                /* axis title height        */
	int lform;              /* axis title location      */
	char  *vform;               /* format for numerical labels  */
	{

#define RNDOFF 1e-4
    int j, logtic, next, prev, strt= -999, numtic, curlen, maxlen;
    float curx, scale, tradAng, sFlab, dx;
    float nxtLab, udx, ux, strtdx;
    float majtkhdx, majtkhdy, mintkhdx, mintkhdy;
    float majtkddx, majtkddy, mintkddx, mintkddy;
    float maxy, labyloc, hornt, wornt;
    float val ;
    float rnd_prob, rnd_plot;
    float lab_off ;             /*offset for label locations*/

    /* MARK this as an axis */
    gplot2(G_MARK, GM_AXIS, 0., 0.) ;

/***
 * Log plot or not?
 ***/
    logtic = utic < 0 ;

/***
 * Protection from bad input
 ***/
    if (Fval == Eval) {
        Fval = Fval * 1.0001 ;
        Eval = Eval * 0.9999 ;
    }
/***
 * Define round-off size in problem and plot units.
 ***/
    rnd_plot = RNDOFF * len;
    if (logtic)	rnd_prob = RNDOFF * (log_10(Eval) - log_10(Fval));
    else	rnd_prob = RNDOFF * (Eval - Fval);

/***
 * To make dealing with angles real easy, we translate the axis to the
 * starting x,y coordinates, then translate it at the given axis.  We
 * therefore have created a virtual axis that can be considered as
 * horizontal and vertical, so no angle offsets need to be computed.
 ***/
    gtrans(xst,  yst);
    gplot2(G_ANGLE, 0, axang, 0.0);

/***
 * we now move the pen to the starting point of the virtual axis and draw
 * it.
 ***/
    plota(0., 0., G_MOVE);
    plota(len, 0., G_DRAW);

/***
 * most of the work is in keeping the differences between logarithmic and
 * linear scales separate.  Below, we compute a few of the parameters
 * needed later.
 ***/
    if(utic < 0) {      /* we're drawing a logarithmic scale    */
        utic = -utic;

        if(Fval == 0.)  /* 0 is illegal in log scale.       */
            Fval = 1.;

        scale = len / log_10(Eval/Fval);/* plot units per decade    */
        sFlab = scale*log_10(Flab/Fval);/* location of first label  */
        dx = scale;         /* major tick dx        */
/***
 * for utic = -1, we don't put a tick at 10%, so our starting value of
 * is at 2(strt = 2).  This is because we want next value/current value
 * to equal 2.  Also, the number of ticks to print(numtic) is only 8.
 * Also, on a logarithmic axis the distance between each minor tick is
 * variable.  We compute the first value(strtdx) here also.
 ***/
        if(utic == 1) {
            strtdx = scale * log_10(2.);
            strt = 2;
            numtic = 8;

        } else {
            strtdx = scale * log_10((double)utic);
            strt = utic;
            numtic = 10/utic;
        }

    } else if(utic != 0) {      /* linear scale         */
        scale = len / (Eval - Fval);    /* plot units per "value"   */
        sFlab = (Flab-Fval)*scale;  /* location of first label      */

        dx = ltic * scale;      /* major tick dx        */
        strtdx = dx / (utic + 1);   /* minor tick dx    */
        numtic = utic;
    }

/***
 * Here, we figure out if any tick marks need to be written before the
 * first label. "curx" is set to the location of the first major tick
 * that would occur before the beginning of the axis.
 ***/
    curx = sFlab;
    if (abs(curx) > rnd_plot) {
        curx = Fmod(sFlab, dx) ;
        curx = curx - dx ;
    }
    /*
    while(curx > rnd_plot) curx -= dx;
    */

/***
 * If the ticks are at an angle, we calculate the offsets required to
 * produce the desired tick angle.
 * First, we do it in the direction of the tick height. . .
 ***/
    tradAng = PI*tang/180.;
    majtkhdx = tht*cos(tradAng);
    majtkhdy = tht*sin(tradAng);
    mintkhdx = majtkhdx/2.;
    mintkhdy = majtkhdy/2.;

/***
 * . . . then in the direction of tick depth.
 ***/
    tradAng = PI*(tang + 180.)/180.;
    majtkddx = tdp*cos(tradAng);
    majtkddy = tdp*sin(tradAng);
    mintkddx = majtkddx/2.;
    mintkddy = majtkddy/2.;

/***
 * If the tick labels are on "top" of the axis, we need to look in the
 * tornt orientation array to find the appropriate values of hornt and
 * wornt to send symb() and numb().
 ***/
    if((tlang = Fmod(tlang, 360.)) < 0.)
        tlang += 360.;

    /* In calculating the locations, we need a little extra room to
     *  avoid overlapping.  This used to be the crass .1 thinking that
     *  everyone would plot in inches. Since people like to scale to
     *  logical values, we need to find this extra room as a function
     *  of something.  Therefore:  lab_off is the room as a percentage
     *  of the label size.  (dredge@fuji 16jul85)
     */
    lab_off = .6 * labht ;          /* 60% label height */

    if(tloc) {
        labyloc = tht + lab_off ;
        for(j = 0; j < 4; j++) {
            if(tlang <= tornt[j].boundang)
                break;
        }
        hornt = tornt[j].hornt;
        wornt = tornt[j].wornt;
/***
 * otherwise, we look in the bornt orientation array.
 ***/
    } else {
        labyloc = -(tdp + lab_off);
        for(j = 0; j < 4; j++) {
            if(tlang <= bornt[j].boundang)
                break;
        }
        hornt = bornt[j].hornt;
        wornt = bornt[j].wornt;
    }

/****
 * Now plot all tick marks and tick labels
 ****/
    nxtLab = Flab;          /* start off at first label */

/***
 * we only draw ticks as long as the value of the major tick is
 * less than the end value of the axis.
 ***/
    maxlen = 0;
    while(nxtLab <= Eval+rnd_prob) {

/***
 * we don't plot major tick marks if they are off the axis.  This may occur
 * if the user specifies the first label to be different from the beginning
 * value of the axis.
 ***/
        if(curx >= -rnd_plot) {
            plota(curx + majtkhdx,  majtkhdy, G_MOVE);
            plota(curx - majtkddx, majtkddy, G_DRAW);
        }

/***
 * we only plot labels after we've reached the first label.  Since "nxtLab"
 * starts out as sFlab, we don't want it to change until the current x
 * position is at least sFlab.  However, since "nxtLab" is used to break
 * out of the loop, we do have to keep incrementing it, even after we
 * have plotted Llab.
 ***/
        if(curx >= sFlab-rnd_plot) {
            if(logtic) {

                if(nxtLab <= Llab+rnd_prob)  {
                    val = (float)log_10((double)nxtLab) ;
                    if (abs(val) < rnd_prob) val = 0.0 ;
                    curlen = numb2(curx,labyloc,val,
						labht,tlang,hornt,wornt,vform);
                }
                nxtLab *= ltic;

            } else {

                if(nxtLab <= Llab+rnd_prob) {
                    val = (float)nxtLab ;
                    if (abs(val) < rnd_prob) val = 0.0 ;
                    curlen = numb2(curx, labyloc,val,labht,
						tlang,hornt,wornt,vform);
                }
                nxtLab += ltic;

            }
            if(curlen > maxlen-rnd_plot)  /*keep track of maximum string len */
                maxlen = curlen;
        }

/***
 * now we calculate the minor tick dx.  If the axis is logarithmic, this
 * dx value will change from one minor tick to the next.  If it's linear,
 * this dx value will be constant.
 ***/
        if(utic != 0) {
            ux = curx + strtdx;
            prev = strt;
            next = prev;
/***
 * enter the loop to start writing minor ticks.
 ***/
            for(j=0; j < numtic; j++) {

                if(ux >= len+rnd_plot)      /* don't write beyond axis end */
                    break;

/***
 * As with major tick marks, we don't plot minor ticks if they are
 * before the beginning of the axis.
 ***/
                if(ux > rnd_plot) {
                    plota(ux + mintkhdx, mintkhdy, G_MOVE);
                    plota(ux - mintkddx, mintkddy, G_DRAW);
                }

/***
 * here, we calculate the value of the minor tick dx.  For logarithmic axis,
 * the formula to find dx for the (n+1)'th tick and the n'th tick is:
 *          dx(n)   =  scale   *   log_10(tick(n+1)/tick(n))
 ***/
                if(logtic) {
                    next += utic;
                    udx = scale * log_10((double)next/prev);
                    prev = next;

                } else
                    udx = strtdx;

                ux += udx;      /* add the dx to the current x pos.  */
            }               /* end of minor tick loop   */
        }
        curx += dx;         /* bump to the next major tick  */
    }               /* end of major tick loop   */

/***
 * we need to figure out where to put the axis title.  If tloc = 1 and
 * lform positive, then the axis title is on the same side as the tick labels,
 * so extra space has to be added.  A similar situation occurs with
 * tloc = 0 and lform negative.  If the axis title is on the same side
 * as the major tick mark labels, we need to see what the angle is between
 * the tick labels and the axis title.  If this angle is other than 0, the
 * axis title may run into the major tick labels.  This is why we kept
 * track of the maximum string length above.  We assume the width of the
 * tick labels is the same as their height, so we simply multiply the
 * maximum length by the label height to get the amount of room to make for
 * the axis title.
 ***/

#ifdef DEBUG
    /* See discussion of lab_off above */
    {   /* DEBUG: to tweek this... */
        char    *z_ep , *getenv() ;
        float    z_of ;
        z_of = 0.55 ;           /* Looks good */
        if ((z_ep = getenv("AXPLTO"))) z_of = atof(z_ep) ;
        lab_off = labht * z_of ;
    }   /* END DEBUG (mje 12nov85) */
#else
    lab_off = labht * .55 ;
#endif /*DEBUG*/

    switch(lform) {
    case 2:
        if(tloc == 1) {     /* title and labels are on the same side */
            if(tlang != axang)  /* title and labels could intersect */
                maxy = labyloc + (labht*maxlen) + lab_off;
            else
                maxy = labyloc + labht + lab_off;

        } else
            maxy = tdp + lab_off;

        symb2(len/2., maxy,title,strlen(title),axtht,180.,1.,.5);
        break;

    case 1:
        if(tloc == 1) { /* title and labels on the same side */
            if(tlang != axang)  /* title and label could intersect */
                maxy = labyloc + (labht*maxlen) + lab_off;
            else
                maxy = labyloc + labht + lab_off;

        } else
            maxy = tdp + lab_off;

        symb2(len/2., maxy,title,strlen(title),axtht,0.,0.,0.5);
        break;

    case -1:
        if(tloc == 0) {    /* title and labels on the same side */
            if(tlang != axang)  /* title and label could intersect */
                maxy = -labyloc + (labht*maxlen) + lab_off;
            else
                maxy = -labyloc + labht + lab_off;

        } else
            maxy = tht + lab_off;

        symb2(len/2., -maxy,title,strlen(title),axtht,0.,1.,0.5);
        break;

    case -2:
        if(tloc == 0) {    /* title and labels on the same side */
            if(tlang != axang)  /* title and label could intersect */
                maxy = -labyloc + (labht*maxlen) + lab_off;
            else
                maxy = -labyloc + labht + lab_off;

        } else
            maxy = tht + lab_off;

        symb2(len/2., -maxy,title,strlen(title),axtht,180.,0.,.5);
        break;
    }

/***
 * Finally, we move the virtual axis back to where it was before axplt was
 * called so subsequent plots don't get screwed up.
 ***/
    gplot2(G_ANGLE, 0, -axang, 0.0);
    gtrans(-xst, -yst);

    /* UN-MARK this as an axis */
    gplot2(G_MARK, -GM_AXIS, 0., 0.) ;

}               /* end of axplt         */

static double log_10(double val)
    {
    double log10() ;
    if (val <= 0.0) {
        printf("log_10: val < 0.0\n") ;
        return -38.0 ;  /* safety! */
   	}
    return log10(val) ;
    }


static float Fmod(float num, float denom)
    {
    float sign = (num > 0.0? 1.0: -1.0) ;
    if (denom == 0.0) return num ;

    if (sign < 0) num = -num ;

    return sign * (num - (int)(num/denom) * denom) ;
    }
