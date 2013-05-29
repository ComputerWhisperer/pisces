/*************************************************************************
 *									 *
 *   Original : MEL         Stanford University        Sept, 1984	 *
 *									 *
 *     Copyright c 1984 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the SUPREM4 computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/
/*   global.h                Version 3.2     */
/*   Last Modification : 6/3/88  12:47:24 */

#ifdef STATIC_ALLOCATION_TIME
#define EXTERN
#else
#define EXTERN extern
#endif

#define TRUE 1
#define FALSE 0


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
EXTERN void *MallocResult;

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
extern int    mod();

extern char *panic();
extern get_bool();
extern get_int();
extern float get_float();
extern char *get_string();

