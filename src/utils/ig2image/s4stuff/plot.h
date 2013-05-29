/*************************************************************************
 *									 *
 *     Copyright c 1984 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the SUPREM4 computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/
/*   plot.h                Version 3.1     */
/*   Last Modification : 9/14/87  10:41:36 */

#ifdef STATIC_ALLOCATION_TIME
#define EXTERN
#else
#define EXTERN extern
#endif

/*downward compatibility for a while*/
EXTERN char *label;
EXTERN char *title;
EXTERN int sel_imp;
EXTERN int sel_log;
EXTERN float z[MAXPNT];

/*function for quick sort of floats*/
extern compar();

/*following stuff is all used in the three dimensional code*/
/*definitions of the corners we are looking at*/
#define RIGHT 0x01
#define LEFT  0x02
#define UPPER 0x04
#define LOWER 0x08

/*file wide locals for the three dimensional processing*/
EXTERN int locate;		/*flag for previous point in/out*/
EXTERN int ncntr;		/*total number of contours being drawn*/
EXTERN int corner;		/*corner we are looking at this time*/

/*This structure is used to hold the data for 1d extracter*/
struct d_str {
    float x;
    float y;
    int mat;
};

/*temporary calculation based guys for 3d code*/
EXTERN float sinbeta, cosbeta, singama, cosgama, xp, yp, xpold, ypold;
EXTERN float yptmax[500], yptmin[500], ypsav;
EXTERN float xold, yold;
EXTERN struct d_str ln[500]; 

/*used for the one d extractor*/
EXTERN int xory, numpt;

/*type of one d plot to make*/
#define XSEC 1
#define YSEC 2
#define BND  3

/*plot mins and maxs*/
EXTERN float xmin, xmax;
EXTERN float ymin, ymax;
EXTERN float zmin, zmax;

/*viewport mins and maxs*/
EXTERN float vxmin, vxmax, vymin, vymax;

/*is the y axis upside down??*/
EXTERN int yflip;

