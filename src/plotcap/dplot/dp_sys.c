/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* "dp_sys" : access to the system.
 * Original: Michael Eldredge -- Stanford.
 * Modified: Michael Eldredge -- Stanford.  Was doing a close() of
 *	the input end of the pipe after the dp_file() call.  But
 *	I had fdopen()ed it, so it needs to be an fclose() (which it
 *	now is...).
 */

#include <stdio.h>
#include <signal.h>
#include "dp_def.h"
#include "dp_com.h"
#include "gplot.h"


/* "sysin" : execute the command line in a shell, but catch all the output
 *	and give that back to dplot as input (wizzy, huh? Idea: thanks to
 *	Conor.
 */
sysin(cmdlin, sin)
	char *cmdlin;
	int  sin;		/* steal input back to dplot ? */
	{

	int (*istat)() , (*qstat)() ;
	int  lup[2];      /* LUs for the pipe */
	int  status, w, pid;
	char *sh, *esh, *getenv(), *rindex() ;


	if ( ! *cmdlin ) return ;	/* Don't bother... */

	ggtoa() ;	/* off to alpha mode. */

	if (sin) pipe(lup);                  /* get a pipe */

	/* get the login shell */
	if (! (sh = getenv("SHELL")) ) sh = "/bin/sh";
	if ( esh = rindex(sh, '/') ) esh++ ;
	else                         esh = sh ;
	
	if ((pid = fork()) == 0) {          /* the kid, soon to be the shell */
		if (sin) {
			close(lup[0]);      /* don't need this end here */

			close(1);
			dup(lup[1]);
			}

		execl(sh, esh, "-c", cmdlin, 0);
		dperr2("can't start shell for `%s'", cmdlin); /* missed it */
		_exit(1);
		}

	/* we don't want dplot to die, so catch some signals */
	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT,SIG_IGN);

	/* the daddy... */
	if (sin) {
		FILE*	fp = fdopen(lup[0], "r") ;
		close(lup[1]);       /* we don't need this end */

		dp_file(cmdlin, fp);    /* get the output */
		fclose(fp);
		}

	/* If just $sys then wait before going back to dplot
	 * If $sys.in   then the wait will clean up zombies
	 *   either way -- wait.
	 */
	while ( (w=wait(&status)) != pid && w != -1) ;

	/* restore the signals that we caught */
	signal(SIGINT , istat);
	signal(SIGQUIT, qstat);
	}
