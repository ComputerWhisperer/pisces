/* ----------------------------------------------------------------------
 *   Copyright 1990 by
 *   The Board of Trustees of the Leland Stanford Junior University
 *   All rights reserved.
 *
 *   This routine may not be used without the prior written consent
 *   of the Board of Trustees of the Leland Stanford University.
 *-----------------------------------------------------------------------
 */

#define LINE_MAX	2048

#include <stdio.h>

/*
 * Simple interface.  No edit capabilities here.
 */

char*
read_line(prompt)
	char*	prompt ;
	{

	static	buffer[LINE_MAX]


	printf("%s", prompt) ;
	return gets(buffer) ;
	}
