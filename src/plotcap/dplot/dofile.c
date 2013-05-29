/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* date: 02 aug 84 (mje)
 * 
 * "dofile" : interpret a text file for dplot until an eof.
 *	This takes the file pointer to an open file and uses `getword' to
 *	get the dplot commands and data from the given file.
 */

#include <stdio.h>
#include "dp_def.h"
#include "dp_com.h"
#include "gplot.h"


dofile()
	{

	char  word[WORDSIZE], *wp;
	char  cmd[WORDSIZE] , sub[WORDSIZE] ;
	int   n ;

	log_lev++;   /* one file deeper */
	pen = G_MOVE ;    /* pick up the pen for each new file. */

	while ( (n = getword(word, W_MUST)) > 0) {

		*cmd = 0;
		*sub = 0;

		/* see if it is a system call out */
		if (n == SYS_W || n == SIN_W) {
			/* so shell gets replaced */
			char *ex = "exec " ;
			/* char *ex = "" ;	*/

			strcpy(word, ex);
			wp = (word + strlen(ex)) ;

			(void)getword(wp, W_LITR) ; /*collect litteral string*/

			docmd("sys" , (n==SIN_W ? "in" : "") , word);
			}

		/* see if it is a command */
		else if (*word == cmd_char) {
			wp = word + 1;
			if (!mkcmd(wp, cmd, sub)) continue;

			/* get the (possible) argument */
			(void)getword(word, W_ANY ) ;

			/* got a command, so no arg */
			if (*word == cmd_char || *word == sys_char) { 
				putback(word);
				*word = 0;       /* null argument */
				}

			/* interpret the command, if word not used, putback */
			if (docmd(cmd, sub, word) == DOBACK) putback(word);
			}


		/* a number or some word that we don't care about */
		else {
			docmd("" , "" , word);
			}

		}/* of loop through file */

	log_lev--;        /* back from a file depth */
	}/* of dofile */
