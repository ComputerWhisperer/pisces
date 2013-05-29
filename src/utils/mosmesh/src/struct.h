/*----------------------------------------------------------------------
**  Copyright 1988 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/



typedef struct {
    double poly_width;
    double spacer;
    double poly_metal;
} lat_str;

typedef struct {
    double phos_junc;
    double ars_junc;
    double chan_junc;
    double oxide_thick;
    double drain_sub_bias;
    double source_sub_bias;
    double substrate_dop;
    int substrate_dop_type;
    double substrate_depth;
} vert_str;

#define N_TYPE 		0
#define P_TYPE 		1
#define SUPREM3ASCII	2
#define SUPREM3EXPORT	3
#define NONE		4
#define BORON		5
#define PHOSPHORUS	6
#define ARSENIC		7
#define ANTIMONY	8

#define MAXNAMESIZE 	80 

typedef struct {
    int type;
    union dop_union {
	struct  anal_str {
	    double peak_dop;
	    double std_dev;
	    double peak_loc;
	} anal_dop;

	struct  sup3_str {
	    int type;  /* dopant species for export format or n/p for ascii */
	    double std_dev;
	    double peak_dop;
	    char filename[MAXNAMESIZE];
	} sup3_dop;
    } dop_tag;
} dop_str;

#define N_POLY 1
#define P_POLY 2
#define METAL 3

#define OXIDE 1
#define SILICON 2
#define POLY 3
typedef struct {
    int num;
    double loc;
    double rat;
    int mat;
} node_str;

typedef struct {
    int xlo, xhi;
    int ylo, yhi;
} elim_str;

#define ERROR 		-1
#define LAT_RATIO 	80  /* this number gets divided by 100 */
#define APPROXTOSTD	20  /* percentage error allowed between a generated
				grid spacing and std, the standard deviation
				for a doping species.  this will be divided
				by 100 internally
			     */
#define MINSHORTGRID	10 /* this number gets divided by 100 to determine
				the minimum grid spacing for short channel
				devices. in this case, the default is
				1000 angstroms (0.1um)
			   */



#ifdef STATIC_ALLOCATION_TIME
#define EXTERN
#else
#define EXTERN extern
#endif



/***************************************************
 *                                                 *
 *            Standard variables.                  *
 *                                                 *
 ***************************************************/
#define V_QUIET  10
#define V_NORMAL 100
#define V_CHAT   1000
#define V_BARF   10000
EXTERN int verbose;

/***************************************************
 *                                                 *
 *            Standard macros.                     *
 *                                                 *
 ***************************************************/
#define FOR(i,l,u) for(i = (l); i <= (u); i++)
#define max(a,b) (((a) > (b))?(a):(b))
#define min(a,b) (((a) < (b))?(a):(b))
#define strequ(a,b) (!strcmp(a,b))

#include <stdlib.h>
/*extern char *malloc (), *calloc(), *realloc();*/
/*EXTERN void *MallocResult;*/

#define salloc(O,N) (O *) malloc((N)*sizeof(O))
#define scalloc(O,N) (O *) calloc(N,sizeof(O))
#define sralloc(O,N,P) (O *) realloc(P, (N)*sizeof(O))
/*#define salloc(O,N) (O *) ((MallocResult =  malloc((N)*sizeof(O))) ? MallocResult : panic("Out of memory"))*/
/*#define scalloc(O,N) (O *) ((MallocResult =  calloc(N,sizeof(O))) ? MallocResult : panic("Out of memory"))*/
/*#define sralloc(O,N,P) (O *) ((MallocResult = realloc(P, (N)*sizeof(O))) ? MallocResult : panic("Out of memory"))*/
/***************************************************
 *                                                 *
 *            Standard procedures.                 *
 *                                                 *
 ***************************************************/

extern char *panic();
