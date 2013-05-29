/*************************************************************************
 *									 *
 *     Copyright c 1984 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the SUPREM4 computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/
/*   sysdep.h                Version 3.6     */
/*   Last Modification : 12/1/88  07:27:38 */

#ifdef STATIC_ALLOCATION_TIME
#define EXTERN
#else
#define EXTERN extern
#endif

/*set up the pointers for the data files*/
EXTERN char *MANDIR;
EXTERN char *KEYFILE;
EXTERN char *MODELFILE;
EXTERN char *IMP_FILE;

#ifdef BSD
#include <sys/file.h>
#define PAGER "more"
#define strchr index
#define strrchr rindex
#define strncmp strcmpn
#define strncpy strcpyn
#endif

#ifdef HPUX
#include <fcntl.h>
#define PAGER "more"
#define index strchr 
#define rindex strrchr 
#define strcmpn strncmp 
#define strcpyn strncpy 
#endif

#ifdef CONVEX
#include <sys/file.h>
#define PAGER "more"
#define strchr index
#define strrchr rindex
#define strncmp strcmpn
#define strncpy strcpyn
#endif

#ifdef ALLIANT
#include <sys/file.h>
#define PAGER "more"
#define strchr index
#define strrchr rindex
#define strncmp strcmpn
#define strncpy strcpyn
#define Snumfac(A, B, C, D, E, F, G) fnumfac_(&A, B, &C, D, E, &F, G)
#define Snumbac(A, B, C, D, E) fnumbac_(&A, B, &C, D, E)
#define mxv(A, B, C, N, D, E, F) fmxv_( &A, B, &C, &N, D, E, F )
#endif

#ifdef CRAY
#include <fcntl.h>
#define PAGER "cat"
#define Snumfac(A, B, C, D, E, F, G) SNUMFAC(&A, B, &C, D, E, &F, G)
#define Snumbac(A, B, C, D, E) SNUMBAC(&A, B, &C, D, E)
#define index strchr 
#define rindex strrchr 
#define strcmpn strncmp 
#define strcpyn strncpy 
#define vfork fork
#endif

#ifdef AMDAHL
#include <fcntl.h>
#define PAGER "pg"
#define index strchr 
#define rindex strrchr 
#define strcmpn strncmp 
#define strcpyn strncpy 
#define vfork fork
#endif

/* Modify this if you start making changes to the code. */
/* Try "SUPREM-IV A.1 organization.12345" */
EXTERN char VersionString[80];
#define VERSION VersionString
