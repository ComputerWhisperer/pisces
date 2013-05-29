/* ----------------------------------------------------------------------
 *   Copyright 1990 by
 *   The Board of Trustees of the Leland Stanford Junior University
 *   All rights reserved.
 *
 *   This routine may not be used without the prior written consent
 *   of the Board of Trustees of the Leland Stanford University.
 *-----------------------------------------------------------------------
 */

#include <ctype.h>
#include <readline/readline.h>


/*
 * Call the gnu readline()
 */

char*
read_line(prompt)
	char*	prompt ;
	{

	char*	p ;
	char*	q ;
	static	int first_time = 1 ;

	if (first_time) {
		first_time = 0 ;
		using_history() ;
		}
	
	p = readline(prompt) ;

	for (q = p ; q && *q ; ++q) {
		if (! isspace(*q)) {
			add_history(p) ;
			break ;
			}
		}

	return p ;
	}
