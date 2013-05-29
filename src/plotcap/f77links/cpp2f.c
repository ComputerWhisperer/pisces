/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* "ctof" : convert the gplot include file from a C form to one good for
 *      fortran (with PARAMETER statements)
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

struct _stoplist {
	char*	orig ;		/* original string */
	char*	new1 ;		/* new one to use, else null for STOP */
	} stops[] = {
	{ "G_CLEAR"	, "G_CLR"	} ,
	{ "G_CLIPL"	, "G_CLPL"	} ,
	{ "G_CLIPH"	, "G_CLPH"	} ,
	{ "G_BEGIN"	, "G_BEG"	} ,
	{ "G_NOT_*"	, 0		} ,
	{ "GPO_*"	, 0		} ,
	{ 0		, 0		}
	} ;


main(argc, argv)
	int   argc;
	char  **argv;
	{
	FILE *fp;
	char line[132] , *lp;
	char comd[132]      ;
	char word[132] , *wp;
	char value[132], *vp;
	char *p1, *p2;
	int  n, ismac ;


	if (argc <= 1) {
		printf("usage: %s cfile.h\n", argv[0]);
		exit(1);
		}

	fp = fopen(*++argv, "r");
	if (fp == NULL) {
		printf("error: can't open `%s'\n", argv[-1]);
		exit(2);
		}

	/* f77 preamble */
printf("C NOTE:\n");
printf("C  1. This is the f77 version of the gplot include file\n");
printf("C     and is supposed to be the same but for Fortran includes.\n");
printf("C  2. All underscores from the C form have been changed to 'x',\n");
printf("C     thus: G_DRAW has become GxDRAW\n");
printf("C  3. The macro functions have not been included.  These functions\n");
printf("C     have been implemented as fortran subroutines and are\n");
printf("C     available in the library.\n");
printf("C  4. This file was generated from `gplot.h' (the C form) by a\n");
printf("C     program, and as such is not so `pretty'. Sorry.\n");
printf("\n");
	
	while (fgets(line, 132, fp)) {	/* read a line from the file */
		lp = line;

		while (*lp && (*lp==' ' || *lp=='\t')) lp++;
		if (! *lp) {
			printf(" \n");
			continue;
			}

		if (*lp == '*' || (*lp=='/' && *(lp+1)=='*')) { /* comment */
			if (1) continue ;
			printf("c ");   /* r4 comment */

			while (*++lp) {
				if (*lp=='*' && *(lp+1)=='/') {
					putchar('\n');
					break;
					}
				putchar(*lp);
				}
			continue;
			}

		/* define ? */
		if (*lp == '#') {
			*comd = 0 ;  *word = 0 ; *value = 0 ;
			sscanf(lp, "%s %s %s", comd, word, value);

			ismac = ( word[ strlen(word)-1 ] == ')' ) ;

			if (strcmp(comd, "#define")==0 && ismac) continue ;

			if (strcmp(comd, "#define")==0 && !ismac ) {
				
				ckstop(word) ;	/* check against stop list */

				if (! word[0]) continue ;
				/* remove underbars */
#ifdef Old_Way
				p1 = p2 = word;
				for (p1 = p2 = word; *p1 ; p1++)
					if (*p1 != '_') *p2++ = *p1 ;
				*p2 = 0;
#endif
				for (p1 = word; *p1; p1++)
					if (*p1 == '_') *p1 = 'x' ;

				if (value[0] == '0') {  /* convert oct to dec */
					if (value[1] == 'x')
						sscanf(&value[2], "%x", &n);
					else	sscanf(&value[1], "%o", &n);
					sprintf(value, "%d", n);
					}

				printf("        integer  %s\n", word ) ;
				printf("      parameter (%s=%s)\n",word,value) ;
				}

			else continue ; /* printf("c%s", line); */
			}

		else continue ; /* printf("c%s", line); */

		}/* of while */

	}/*of program */

ckstop(word)
	char	word[] ;
	{
	struct _stoplist*	p ;

	/* See if there is a substitution or a STOP */
	for (p = stops; p->orig ; ++p) {
		if (Wordcmp(word, p->orig)) {
			if (p->new1) strcpy(word,p->new1) ;
			else         word[0] = '\0' ;	/* stop */
			return ;
			}
		}
	
	/* Hmm, if it is too long shorten it */
	if (strlen(word) > 6) {
		fprintf(stderr, "Warning: %s shortened to %*.*s\n",
			word, 6,6,word) ;
		word[6] = '\0' ;
		}
	}

int
Wordcmp(word,tmpl)
	char*	word ;
	char*	tmpl ;
	{

	for ( ; *word && *tmpl ; ++word, ++tmpl) {
		if (*tmpl == '*') return 1 ;	/*match*/
		if (*word != *tmpl) return 0 ;	/*no*/
		}

	return (*word == *tmpl) ;	/* both null, then match */
	}
