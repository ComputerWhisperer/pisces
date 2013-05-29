/* $Header: /users/suprem/ig2/sparse/RCS/sparse.h,v 1.1 85/05/17 14:42:50 conor Exp $*/
#ifdef STATIC_ALLOCATION_TIME
#define EXT
#else
#define EXT extern
#endif

EXT int *ia, *il, *iu;	            /* Map of A and its LU decomp */
EXT short *ja, *jl, *ju;
EXT double *a, *l, *di, *u;         /* A, L, U (diag(U) stored separately)*/
EXT double *x;		            /* Scratch vector */
EXT int *ipri, *ipc;		    /* Pivoting order. */

