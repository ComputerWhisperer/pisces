/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/*
 * Wed Jan 31 14:32:44 PST 1990 (dredge--stanford)
 *
 * "dp_fmp" : file managment routines for dplot.  These routines hide
 *	 what is really happening as far as files go.  It keeps a stacked
 *	list of files and returns words from the deepest (current) file.
 *
 * modified: Michael Eldredge -- stanford (jan 90) Added the EDLINE code
 */

#include <stdio.h>
#include "dp_def.h"
#include "dp_com.h"
#include "gplot.h"


/* information that we keep for each file */
struct _dpfile {
	FILE	*dp_fp;		/* file pointer */
	int	 dp_lin;	/* current line in the file */
	int	 dp_col;	/* current column in file */
	int	 dp_unc;	/* ungetted character */
	int	 dp_ch0;	/* current character */
	bool	 dp_tty;	/* is a tty or not */
	char	*dp_nam;	/* current file name (as opened) */
	struct  _dpfile *dp_nxt;/* next file to work on (previous opened) */
	} ;
typedef struct _dpfile DFILE ;
/*#define DFILE struct _dpfile*/

/* no ungetted character */
#define NOUNC -2

/* some local data space */
static  DFILE  *dp_cur = 0;		/* current file pointer */

static  DFILE  *dptmp ;			/* temp values for opens */


/* "dp_file" : Process a file, add it to the file list, and read commands 
 *	etc... 
 *
 * calling sequenc:
 *	dp_file(namr, fp);
 *
 * where:
 *	namr	- (char *) A string "name" to associate with the "file".
 *	fp	- (FILE *) An already open file descripter pointer.
 */

dp_file(namr, fp)
	char *namr;
	FILE *fp;
	{
	DFILE *dp ;
	char  *malloc();

	/*
	 * ----- create a descriptor block for the new file ---
	 */

	/* make a dplot file descriptor */
	/*NOSTRICT*/
	dp = (DFILE *)malloc( sizeof(DFILE) );
	if (dp == NULL) {
		dperr2("can't get enough memory to open file '%s'\n",namr) ;
		return  ;
		}

	/* init the new one */
	dp->dp_fp  = fp ;
	dp->dp_lin =  0 ;
	dp->dp_col = -1 ;
	dp->dp_unc = NOUNC ;
	dp->dp_ch0 = NULLC ;
	dp->dp_tty = isatty( (int) dp->dp_fp->_file );

	dp->dp_nam = malloc( strlen(namr)+1 );
	if (dp->dp_nam == NULL) {
		dperr2("can't get enough memory to open file '%s'\n",namr) ;
		/*NOSTRICT*/
		free( (char*)dp ) ;
		return ;
		}
	strcpy(dp->dp_nam , namr);

	/* and add to the list of others */
	dp->dp_nxt = dp_cur ;
	dp_cur     = dp ;


	/* 
	 * ---- process the current file until an EOF is found ----- 
	 */
	dofile();


	/* 
	 *  ---- close up our descriptor block --------- 
	 */

	free(dp_cur->dp_nam);		/* release buffer space for name */

	/* free the descriptor block */
	dptmp  = dp_cur ;
	dp_cur = dp_cur->dp_nxt ;
	
	/*NOSTRICT*/
	free( (char *)dptmp );
	}



/* a place to store old words..... */
static  char  old_word[WORDSIZE] = "";

/* "getword" : get the next word from the current input stream or the putback
 *		word if there is one.
 *
 * Note: for ignoring white/non-white ...
 *	bool  igwhite;
 *	while ( ((ch=getc(fp)) != EOF) && (igwhite ^ (ch==' ' || ch=='\t')) ) ;
 */
int
getword(word, how)
	char *word;
	int   how;  /* MUST , ANY , LITR */
	{

	register int   ch;
	bool  must , litr ;
	char  *wp;
	int   n = REG_W;		/* assume good return */

	wp = word;
	must = (how == W_MUST) ;
	litr = (how == W_LITR) ;

	/* do we have a word that was put back? */
	if (*old_word) {
		strcpy(word, old_word);    /* get the last word */
		*old_word = NULLC;         /* not in use now    */
		}

	/* no backed up word, get a new one */
	else {
		*wp = 0;

		/* if litteral get, then just get it */
		if (litr) {
			while ((ch = dpgetc()) != EOF && ch != '\n')
				*wp++ = ch ;
			*wp = NULLC;
			goto done_word;
			}

	  once_more:		/* if Must && after a comment, try again */
		while ( ((ch = dpgetc())!=EOF) && 
					(ch==' ' || ch=='\t' || ch=='\n')) {

			if (ch == '\n' && !must) goto done_word;
			}
		if (ch == EOF) return(EOF_W);

		/* System call ? */
		if ( (dp_cur->dp_col == 0) && ch == sys_char ) {

			if (dpgetc() == sin_char) n = SIN_W ;
			else {
				n = SYS_W ;
				dpunc();	/* return the character */
				}
			goto done_word;
			}

		/* Comment line? */
		if (ch == cmt_char) {
			while ((ch = dpgetc()) != EOF && ch != '\n') 
				;
			if (ch == EOF) return(EOF_W) ;
			if (must) goto once_more ;
			goto done_word ;
			}

		/* Quoted string ? */
		if (ch == '"' || ch == '\'') {
			dpunc();
			getquot(word);
			goto done_word;
			}

		dpunc();
		while ( (ch=dpgetc())!=EOF && 
					!(ch==' ' || ch=='\t' || ch=='\n'))
			*wp++ = ch;
		*wp = 0;
		dpunc();
		}
  done_word:
	return (n);
	}


/* "putback" : put back a word to be gotten later. 
 */

putback(word)
	char *word;

	{
	strcpy(old_word, word);
	}


/* "getquot" : return the quoted string */
getquot(str)
	char *str;
	{

	int  ch;
	char qu, c0 = 0;

	ch = dpgetc();    /* decide type of quote, single/double */
	qu = ch;

	while ((ch = dpgetc()) != EOF && ch != '\n') {
		if (ch == qu && c0 != '\\') break;    /* if not escaped... */

		if (ch != '\\') *str++ = ch;
		c0 = ch;
		}

	*str = 0;
	}

#ifdef EDLINE
/* the edline code will return a pointer to a full line w/ no NL */
static	char*	nxtchar = (char*)0 ;

extern	char*	read_line() ;
#endif

/* "dpgetc" : like 'getc' but we do a little more for dplot */
int
dpgetc()
	{
	register int ch;

	if ((ch = dp_cur->dp_unc) != NOUNC) {    /* incase one back */
		dp_cur->dp_unc = NOUNC ;
		return(ch);
		}

	/* adjust column and line counts */
	dp_cur->dp_col++ ;
	if (dp_cur->dp_ch0 == '\n') {   /* a new line */
		dp_cur->dp_lin++ ;
		dp_cur->dp_col = 0;
		lin_count++ ;
		}

	/* we may want to prompt for input */
	if (dp_cur->dp_col == 0 && dp_cur->dp_tty) {
		ggtoa() ;
#ifndef EDLINE
		printf("%s", dp_prmt );
#else
		nxtchar = read_line(dp_prmt) ;
		if (!nxtchar) {
			ch = EOF ;
			goto done ;
			}
#endif
		}

#ifdef EDLINE
	if (nxtchar) {			/* have one in wait? */
		ch = *nxtchar++ ;		/* grab it */
		if (!ch) {			/* no more in this line? */
			ch = '\n' ;			/* end-of-line */
			nxtchar = (char*)0 ;
			}
		}
	else
#endif
	ch = getc(dp_cur->dp_fp);		/* get next char */

#ifdef EDLINE
    done:
#endif
	dp_cur->dp_ch0 = ch;
	return (ch);
	}

/* "dpunc" : like ungetc .. */
dpunc()
	{
	dp_cur->dp_unc = dp_cur->dp_ch0 ;    /* save it  */
	}




/* "dperr" : Basic Error statement for dplot */
dperr2(Form, Val)
	char *Form, *Val;
	{

	ggtoa() ;

	fprintf(stderr,"dplot: Error");
	if (ErrCode) fprintf(stderr,"(%d)", ErrCode);

	if (dp_cur) fprintf(stderr," from `%s'[l%d,c%d]: " , 
			dp_cur->dp_nam, dp_cur->dp_lin, dp_cur->dp_col );
	else        fprintf(stderr," on command line: "); 

	fprintf(stderr,Form,Val);
	fprintf(stderr,"\n");

	ErrCode = 0 ;   /* clear the error */
	haveERR = T ;   /*  but let the caller know.... */
	}
