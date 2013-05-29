/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/*	struct.h		Version 1.3		*/
/*	Last Modification:	1/29/90 01:40:04		*/




typedef struct {
    double emitter_width;
    double emitter_length;
    double base_width;
    double base_length;
    double collector_width;
    double collector_length;
    double contact_width;
    double contact_length;
    double contact_edge_spacing;
    double e_c_spacing;
} lat_str;

typedef struct {
    double emitter_depth;
    double ext_base_depth;
    double int_base_depth;
    double collector_depth;
    double c_e_bias;
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
#define N_GAUSSIAN     10
#define P_GAUSSIAN     11
#define N_ERFC	       12
#define P_ERFC	       13
#define OHMIC_CONTACT  15
#define OTHER_CONTACT  16

#define MAXPLANES	3

#define MAXNAMESIZE 	80 

typedef struct {
    int num;
    int xlo, xhi;
    int ylo, yhi;
    int zlo, zhi;
} plane_str;

typedef struct {
    int num;
    double xlo, xhi;
    double ylo, yhi;
    double zlo, zhi;
} area_str;


typedef	struct {
    double peak_dop;
    double xdev;
    double ydev;
    double zdev;
    double xlo, xhi;
    double ylo, yhi;
    double zlo, zhi;
    int ierfcx;
    int ierfcy;
    int ierfcz;
    int num_planes;
    area_str *planes[MAXPLANES];
} stride_dopstr;

typedef	struct  {
    double peak_dop;
    double std_dev;
    double peak_loc;
} anal_dopstr;

typedef	struct  {
    int type;  /* dopant species for export format or n/p for ascii */
    double std_dev;
    double peak_dop;
    char filename[MAXNAMESIZE];
} sup3_dopstr;

typedef struct {
    int type;
    union dop_union {
	anal_dopstr anal_dop;
	sup3_dopstr sup3_dop;	
	stride_dopstr stride_dop;
    } dop_tag;
} dop_str;


#define SILICON 2

typedef struct {
    int num;
    int localnum;
    double loc;
    double rat;
    int mat;
} node_str;

typedef struct {
    int xlo, xhi;
    int ylo, yhi;
} elim_str;

typedef struct {
    int xlo, xhi;
    int ylo, yhi;
    int zlo, zhi;
    int type;
    int nplane;
    float eps;
    double ni0;
} reg_str;

typedef struct {
    int dir;
    int xlo, xhi;
    int ylo, yhi;
    int zlo, zhi;
    int cnt;
    int typ;
    int shp;
    float work;
    plane_str *pln;
    float pot;
} electrode_str;


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


#define NPN	1

#ifdef STATIC_ALLOCATION_TIME
#define EXTERN
#else
#define EXTERN extern
#endif


EXTERN int elnum;
EXTERN int xnum;
EXTERN int ynum;
EXTERN int znum;
EXTERN lat_str lat_info;
EXTERN vert_str vert_info;
EXTERN dop_str dop_data[4];
EXTERN node_str xnd[50];
EXTERN node_str ynd[50];
EXTERN node_str znd[50];
EXTERN elim_str elim[10];
EXTERN reg_str reg[10];
EXTERN electrode_str electrode[10];
EXTERN int dopingmode;
EXTERN int computemode;
EXTERN int inputmode;
EXTERN int stridedeck;
EXTERN int num_regions;
EXTERN int nelect;
EXTERN int ndop;
EXTERN float mincollector;
EXTERN float maxcollector;
EXTERN float maxbase;
EXTERN float minbase;
EXTERN float collectorincrement;
EXTERN float baseincrement;
EXTERN double xlat_factor;
EXTERN double ylat_factor;


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

extern panic();
