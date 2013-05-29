/*************************************************************************
 *									 *
 *     Copyright c 1984 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the SUPREM4 computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/
/*   matrix.h                Version 3.8     */
/*   Last Modification : 9/28/88  16:59:47 */


#ifdef STATIC_ALLOCATION_TIME
#define EXTERN
#else
#define EXTERN extern
#endif

/************************************************************************
 *									*
 *	This file contains definitions of the matrices that are helpful	*
 *  in the sparse matrix calculations that have to go on.		*
 *									*
 *  Matrices are stored in the following way:				*
 *	The matrix is stored in a symmetric fashion.  The diagonal	*
 *      elements are in the first locations, followed by the lower	*
 *      triangular elements in a column ordered way.  Offset by aoff,	*
 *	is the upper triangular portion stored row ordered.  This 	*
 *	allows us to save half the pointer space, and to do symmetric	*
 *	operations much faster.						*
 *	Reference:	Banks  ( ? )					*
 *									*
 *									*
 ************************************************************************/

#define MATRIX

#define MAXBACK 6

EXTERN int *ia;			/*the row index pointers*/

EXTERN int aoff;		/*the upper triangular offset*/
EXTERN double *a[MAXIMP][MAXIMP];	/*the values used in the matrix*/
EXTERN int blktype[MAXIMP][MAXIMP];	/*are they diagonals or blocks*/

EXTERN int *irow;		/*row indices must be 2x ia length*/
EXTERN int *jrow;		/*row index locations must be 2x ia length*/

EXTERN double *ocoeff;		/*the summed up old coupling coefficients*/
EXTERN double *mcoeff;		/*the summed up mid coupling coefficients*/
EXTERN double *ncoeff;		/*the summed up new coupling coefficients*/

EXTERN int *ss_il;		/*column index pointers for steady state solve*/
EXTERN int ss_loff;		/*upper triangular offset for steady state solve*/
EXTERN int *tm_il;		/*column index pointers for time dependent solve*/
EXTERN int tm_loff;		/*upper triangular offset for time dependent solve*/
EXTERN double *ss_l;		/*the lower triangular values for steady state*/
EXTERN double *tm_l;		/*the lower triangular values for time dependent*/
EXTERN int ss_sol[MAXIMP];	/*solution numbers of those being worked on*/
EXTERN int ss_nsol;		/*number of those being worked on*/
EXTERN int tm_sol[MAXIMP];	/*solution numbers of those being worked on*/
EXTERN int tm_nsol;		/*number of those being worked on*/

EXTERN int tm_elim[MAXIMP];
EXTERN int tm_nelim;

EXTERN int *il;
EXTERN int loff;

EXTERN double *wrkspc[MAXIMP];	/*scratch space for calculations*/

/*arrays for use with the Conjugate Gradient method*/
EXTERN double *p[MAXBACK][MAXIMP];	/*current direction vector*/
EXTERN double *ap[MAXBACK][MAXIMP];	/*matrix a * p*/
EXTERN double *r[MAXIMP];		/*residual*/
EXTERN double *qinvr[MAXIMP];		/*Qinv residual (Q is preconditioner)*/
EXTERN double *aqinvr[MAXIMP];		/*matrix a * qinvr */

/*what types are each of the blocks*/
#define B_NONE 0	/*no block*/
#define B_DIAG 1	/*block is a diagonal*/
#define B_BLCK 2	/*block is a sparse full block*/

/*declare some stuff which specifies type of linear solve being done*/
#define SOR   1
#define SIP   2
#define LU    3
#define ICCG  4

/*time step method*/
#define TRBDF  1
#define FORM   2

/*preconditioner flags level*/
#define FULL 1
#define DIAG 2
#define NONE 3

/*refactorization frequency*/
#define RF_ALL 1
#define RF_ERR 2
#define RF_TIM 3

/*decalre a single structure to hold the method information*/
struct meth_str {
    int block;			/*the block iterative method*/
    int fill;			/*the level of fill to calculate*/
    int back;			/*the number of backing vectors to use with iccg*/
    int minfill;		/*do a minimum fill reorder*/
    double minfreq;		/*the minfill frequency parameter*/
    int factor;			/*the matrices need to be factored*/
    int timemeth;		/*the time integration method*/
    int defmodel;		/*the defect model being used*/
    char dtform[1024];  	/*character sttring for the timestep*/
    double init_time;   	/*the initial time step to be used*/
    int blkitlim;		/*maximum number of block iterations*/
};

EXTERN struct meth_str methdata;
EXTERN int UpdateSymbolic;
EXTERN int UpdateSymbolic;
