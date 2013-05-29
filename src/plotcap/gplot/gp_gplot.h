/* date: 26 nov 85 (dredge@su-fuji)
 *
 * "gp_gplot.h": Some extended gplot2() commands that are not to be user
 *	callable (like those in "gplot.h").
 */

/* The following are `plotcap' intrinsic functions and should be numbered
 *	as small negative numbers (ie: |n| < 32)
 */
#define GX_NULL		 0
#define GX_POST		-1
#define GX_P0		-2
#define	GX_P1		-3
#define GX_READ		-4
#define GX_IDEV		-5
#define GX_ODEV		-6
#define GX__ABORT	-7
#define GX__DUMP	-8
#define GX__TRACE	-9
#define GX__NOTRACE	-10

/* If we get some that are not intrinsics, they can go here with big neg values
 */
