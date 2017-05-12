/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/* Random functions of some help here and there... */


#include "auxfns.h"
#include <stdio.h>
#include "gplot.h"
#include "gp_def.h"
#include "gp_com.h"

/* most loaders don't like it of there is not a single function / entry
 *	point defined.  That could happend if there are no defines
 *	made that will include one or more of the routines.
 *	So: define a dummy routine, just incase.
 */
#ifdef ANSI_FUNC

static void 
RandDummy (void)
#else

static void
RandDummy()
#endif
 { ; }


#ifdef really_do_need_this
/* "gp_poke" : DEBUG. Poke in values to commons */
#ifdef ANSI_FUNC

int 
gp_poke (int cmd, int ival)
#else

gp_poke(cmd, ival)
	int	cmd, ival ;
#endif
	{

	switch (cmd) {
	case 1:
		lu_oplt = ival ;
		break ;
#ifdef DEBUG
	case 2:
		bug_level = ival ;
		break ;
	case 3:
		db_writeok = ival ;
		break ;
#endif

	default:
		break ;
		}
	}
#endif

#ifdef DO_STATS

/* Init statistics */
#ifdef ANSI_FUNC

int 
init_stats (void)
#else

init_stats()
#endif
	{
	long	 time() ;

	gp_stats.st_type  = -1 ;
	gp_stats.st_uid   = getuid() ;
	gp_stats.st_time  = time(0)  ;

	gp_stats.st_psiz  = -1 ;
	gp_stats.st_pmax  = MAXPROG ;

	gp_stats.st_dsiz  = -1 ;
	gp_stats.st_dmax  = MAXIDAT ;

	gp_stats.st_stsz  = -1 ;
	gp_stats.st_stmx  = STABSIZE ;

	gp_stats.st_sesz  = -1 ;
	gp_stats.st_semx  = STABENTS ;
	}


/* Put out statistics */
#ifdef ANSI_FUNC

int 
save_stats (void)
#else

save_stats()
#endif
	{
	/* BIN STATS
	int	 lu ;

	lu = open(STATS_FILE, 2) ;
	if (lu < 0) return ;

	write(lu, (char *)&gp_stats, sizeof(gp_stats) ) ;
	close(lu) ;
	*/

	/* ASC STATS */
#  define S gp_stats
	FILE	*fp ;
	char	*getenv(), *ctime() ;

	fp = fopen(STATS_FILE, "a") ;
	if (!fp) return ;

	fprintf(fp,"\n%-14s typ %d\t %s\t%s" , 
		S.st_dnam, S.st_type, getenv("USER"), ctime(&S.st_time)) ;
	fprintf(fp,"\tprog %d (%d),  idat %d (%d)\n",
		S.st_psiz, S.st_pmax, S.st_dsiz, S.st_dmax);
	fprintf(fp,"\tstab %d (%d),  sent %d (%d)\n",
		S.st_stsz, S.st_stmx, S.st_sesz, S.st_semx);

	fclose(fp) ;


	}

#endif


