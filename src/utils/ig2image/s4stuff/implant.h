/*************************************************************************
 *									 *
 *   Original : MEL         Stanford University        Jan, 1987	 *
 *									 *
 *     Copyright c 1984 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the SUPREM4 computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/
/*   implant.h                Version 3.2     */
/*   Last Modification : 11/17/87  09:04:10 */


#ifdef STATIC_ALLOCATION_TIME
#define EXTERN
#else
#define EXTERN extern
#endif

/*the structures for the rectangular mesh*/
struct rect_nd {
    int mat;
    double conc;
    double dam;
};

struct rect_pt {
    int nn;
    struct rect_nd *nd;
};

/*define ions which are not also impurities*/
#define BF2 	-1

/*data structures for this stuff*/
EXTERN struct rect_nd *rn;	/*the rectangular nodes*/
EXTERN struct rect_pt *rp;	/*the rectangular points*/
EXTERN double *x;		/*the x locations in the rectangular mesh*/
EXTERN double *y;		/*the y locations in the rectangular mesh*/
EXTERN int nx, ny;		/*the number of x and y*/

/*is there a manual overide value set for the implant*/
int override;
double Rp, delRp, Rgam, Rkurt;

/*the ignore distance separator*/
#define ERR	1.0e-10

/*is the value within ERR of the coordinate*/
#define WITHIN(A, B) ( ((A)-(B) < ERR) && ((B)-(A) < ERR) )


/* A good dx to use in integrating charge, etc. */
#define  PRS_DX 5.0e-4

/* Data array offsets that are independent of distribution function */
#define	 HASHSZ  6		/* Number of layers is good */
#define  MAXZ    0		/* Max allowable input to Pearson func */
#define  AREA    1		/* Area for this Pearson curve */
#define  SIGLAT  2		/* The lateral standard deviation*/
#define  SIZE    8		/* Number of above things */

/* Pearson distribution offsets */
#define	 PRS_A0    3
#define  PRS_B0    4
#define  PRS_B2    5
#define	 PRS_RP	   6		/* Projected range */
#define	 PRS_PEAK  7		/* Value at the centre */

/* Gaussian distribution offsets */
#define	 GUS_RP    3
#define  GUS_SIG   4

/*the type of vertical distribution*/
#define PEARS 1
#define GAUSS 2
EXTERN int imp_model;

/*damage parameters for silicon*/
#define DAM_A1  0
#define DAM_A2  1
#define DAM_A3  2
#define DAM_NV	3
#define DAM_C1  4
#define DAM_C2  5
#define DAM_Z0  6
#define DAM_WT  7

/* undefine this if your machine doesn't like register vars (esp. with -O) */
#define  reg register	
#define  Y_SLICE  TRUE 		/* get_edge() direction, want a y-slice */
#define  X_VAL  0		/* cord[X_VAL] is an X value */
#define  Y_VAL  1		/*   .... similar idea .... */

/*various double functions*/
extern double imp_vert();
extern double prson();
extern double zeqv();
extern double qeqv();
extern double dam_vert();

#define IMP_LAT( A, B, C ) (0.5 * ( erf((B)/(A)) - erf((C)/(A)) ) )
