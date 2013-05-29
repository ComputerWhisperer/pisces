/*************************************************************************
 *									 *
 *     Copyright c 1984 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the SUPREM4 computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/
/*   impurity.h                Version 3.11     */
/*   Last Modification : 11/16/88  09:47:35 */

/************************************************************************
 *									*
 *	This file contains the definitions and constants for the 	*
 *  impurity data.  This also includes the defect data.  It depends on	*
 *  constant.h being included first.					*
 *									*
 ************************************************************************/

#ifdef STATIC_ALLOCATION_TIME
#define EXTERN
#else
#define EXTERN extern
#endif

/*typedef a pointer to a function, (it works, trust me)*/
typedef int (* PTR_FNC)();
typedef double (* PTR_DBLFNC)();

/*define constants for each impurity*/
#define V  0		/*Vacancies*/
#define I  1		/*Interstitials*/
#define As 2		/*Arsenic*/
#define P  3		/*Phosphorus*/
#define Sb 4		/*antimony*/
#define B  5		/*Boron*/
#define ELE 6		/*electron concentration*/
#define HOL 7		/*hole concentration*/
#define XVEL 8		/*X velocity*/
#define YVEL 9		/*Y velocity*/
#define O2 10		/*Dry O2*/
#define H2O 11		/*Wet O2*/
#define T  12		/*Interstitial Traps*/
#define Au 13		/*Gold*/
#define Psi 14		/*Potential*/
#define Sxx 15		/* Components of stress - not solution variables*/
#define Syy 16		/* but too expensive to recompute for plotting */
#define Sxy 17
#define Cs  18		/* Cesium for oxide charges */
#define DELA 19		/*change in interface area - not solution variable*/

extern double charge();

/*set up an array of pointer from solution numbers to impurity number*/
EXTERN int soltoimp[MAXIMP];

/*set up an array of impurity numbers to solution indices*/
EXTERN int imptosol[MAXIMP];

/*declare an structure for each impurity of all its needed data*/
struct imp_str {
    double constant[MAXMAT][25];	/*diffusion and generation constants*/
    double seg[8][MAXMAT][MAXMAT];	/*the segregation coefficients*/
    int flags;				/*steady state, diffusing, or mobile*/
    PTR_DBLFNC diff_coeff;		/*routine which calculates diff coeffs*/
    PTR_FNC coupling;			/*set up impurity-impurity coupling*/
    PTR_FNC boundary;			/*set up boundary conditions*/
    PTR_FNC algebc;			/*set up algebraic boundary conditions*/
    PTR_FNC active;			/*active concentration calculator*/
    PTR_FNC time_val;			/*time term calculation function*/
};

/*flag values and flag tests*/
#define STEADY    0x01		/*is this a time dependent variable*/
#define DIFFUSING 0x02		/*is this a spatial dependent variable*/
#define MOBILE    0x04		/*is this a variable with del * D del C terms*/
#define LOCKSTEP  0x08		/*does this have to be solved in lock step*/
#define PSIACT    0x10          /*does the active concentration depend on psi*/
#define PSEUDO    0x20          /*pseudo variable for diffusion?*/
#define IS_DIFFUSE(A) ( impur[A].flags & DIFFUSING )
#define IS_MOBILE(A)  ( impur[A].flags & MOBILE )
#define IS_STEADY(A)  ( impur[A].flags & STEADY )
#define IS_LOCKED(A)  ( impur[A].flags & LOCKSTEP )
#define IS_PSIACT(A)  ( impur[A].flags & PSIACT )
#define IS_PSEUDO(A)  ( impur[A].flags & PSEUDO )

/*defines for the different values of the seg coefficients*/
#define SEG0	0
#define SEGE	1
#define TRN0	2
#define TRNE	3

/*declare a space for each one of the impurities and its data*/
EXTERN struct imp_str impur[MAXIMP];

/*now a variable to count how many impurities we are actually solving*/
EXTERN int n_imp;

/*structure for the parameters to be passed to the boundary code routine*/
struct bound_str {
    int nx[2];			/*the node numbers involved*/
    double delta;		/*timestep*/
    float temp; 		/*temperature*/
    double vel[2]; 		/*boundary velocity*/
    double conc[2];		/*solution values*/
    double eq[2];		/*equilibrium values*/
    int mat[2];			/*material values*/
    int loc[2][2];		/*coupling locations*/
    double cpl;			/*coupling length*/
    double **rhs;		/*right hand side*/
    float cord[2];		/*location of the nodes*/
    double vmax;		/*maximum velocity*/
    double dela[2];		/*change in the area*/
};


/*what oxide model to use*/
#define O_ANALYTIC 0
#define O_VERTICAL 1
#define O_ELASTIC  2
#define O_VISCOUS  3
#define O_VISCOEL  4
#define O_ERF1     5
#define O_ERF2     6
#define O_ERFG     7
EXTERN int oxide_model;

/*the following are definitions used in the impurity routine so that future
  implementers of models can do it without memorizing the data structure.*/
extern double get_conc();

/*
 *	A - the row index
 *	B - the impurity index
 *      C - the vector to store it in
 *	D - the value to be put in
 */
#define left_side(A, B, D)  a[B][B][A] += D
#define right_side(A, B, C, D) C[B][A] += D


extern clear_row();
extern remove_time();
extern double get_area();

/*
 *	A - is the row
 *	B - imp1
 *	C - imp2
 *	D - diagonal value
 *	E - the coupling value
 */

#define add_couple(A, B, C, D, E)  a[B][B][A] += D; a[B][C][A] = E

/*define aids for use in the one/two dimensional decisions for defects*/
#define TWODIM   0x00	
#define FERMI    0x02	
#define SSTATE   0x04
#define FULLCPL  0x08

