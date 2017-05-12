/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/* date: 09 may 86 (mje)
 *
 * "pt.c": Plotcap/pgetXXX test program.
 *
 * A very simple program to test the pgetXXX() routines before going
 * on to debug the gplot stuff.  This can also be use to look at the
 * dis-assembled "program" compiled by pgetprg.m
 *
 * useage:
 *	pt [-p plotcap_file] device
 *
 * written:  Michael Eldredge (apr 84)
 * mod's     many....
 */


#include <stdio.h>
#include <ctype.h>
#include "auxfns.h"
#include "pg_lex.h"
#include "pg_def.h"

/* catch some signals and get back to some sane point */
#include <signal.h>
#include <setjmp.h>
static	 jmp_buf  env ;
int	 onintr() ;

#define usage() fprintf(stderr,"Usage: pt [-p capfile] device\n")

#define PSIZE	 256
#define ISIZE	 512
#define STAB_SIZE	256
#define STAB_ENTS	20

int	 a_prog[PSIZE] ;
char	 a_idat[ISIZE] ;
int	 a_poff = 0, a_ioff = 0 ;

/* Some (but not nec. all) of the program entries to try and get */
struct {
	char	*e_nam ;
	int	 e_ent ;
	} *ep, elist[] = {
		{ "DRAW"  , 0 } , { "MOVE"  , 0 } , { "PCLR"  , 0 } ,
		{ "INIT"  , 0 } , { "PEND"  , 0 } ,
		{ "PLOP"  , 0 } , { "PLCL"  , 0 } , { "CLOC"  , 0 } ,
		{ "LINE"  , 0 } , { "AREA"  , 0 } , { "FILS"  , 0 } ,
		{ "ATOG"  , 0 } , { "GTOA"  , 0 } , { "SFORM" , 0 } ,
		{ "" }
		} ;

main(argc, argv)
	char	**argv ;
	{
	char	 cmd[40], line[100] ;
	int	 n ;
	float	 nval ;
	char	*capfil = "plotcap" ;
	FILE	*fp , *opfile() ;
	char	 id[80], sval[256], *ip, *pg_fndid() ;


   more_args:
	argv++, --argc ;
	if (argc < 1) { usage() ; exit(1) ; }

	if (strequ(*argv, "-p")) {
		if (--argc <= 0) {
			fprintf(stderr,"Need a plotcap file name\n") ;
			exit(1) ;
			}

		capfil = *++argv ;
		goto more_args ;
		}

	if (argc < 1) {
		usage() ;
		exit(1) ;
		}

	/* ----------- start with current args --------------- */

	if (stab_mk(STAB_SIZE, STAB_ENTS) < 0) {
		printf("Can't make symbol table\n") ;
		exit(1) ;
		}

	if (pgetent(capfil, *argv) < 0) {
		printf("Error from pgetent...\n") ;
		}

	printf("Definition buffer:\n\n%s\n", p.defbuf ) ;

	setjmp(env) ;
	signal(SIGINT, onintr) ;	/* ^c back to here */

	for (;;) {

		printf("which('?' for help): ") ;
		if (! gets(line) ) break ;
		if ((n = sscanf(line, "%s%s", cmd, id)) <= 0) continue ;
		else if (n == 1) id[0] = 0 ;

		if (strcmp(cmd, "q")==0) break ;

		switch (*cmd) {
		case '?':
			printf("\tq - quit\n") ;
			printf("\tb - redisplay buffer\n") ;
			printf("\td - dis-asm prog [d file - output to file\n");
			printf("\te - show entry points for std programs\n") ;
			printf("\tg - get the std programs\n") ;
			printf("\ti - show input data\n") ;
			printf("\tl - show current array limits\n") ;
			printf("\tr - run the alu on given entry\n") ;
			printf("\t    ['R' run but do nothing]\n") ;
			printf("\n") ;
			printf("\tf - find flag (boolean)\n") ;
			printf("\tn - find numeric\n") ;
			printf("\tp - find program ['P' shows Dis-asm'd]\n") ;
			printf("\ts - find string\n") ;
			break ;
		case 'i':
			printf("\tInput data:\n") ;
			prstr(stdout, a_idat, a_ioff) ;
			printf("\n\n") ;
			break ;
		case 'b':
			printf("Definition buffer:\n\n%s\n", p.defbuf ) ;
			break ;
		case 'n':
			if (n != 2) ask("numeric capability", id) ;
			n = pgetnum(id, &nval) ;
			printf("\t%s(%d) = %g\n", id, n, nval) ;
			break ;
		case 's':
			if (n != 2) ask("string capability", id) ;
			n = pgetstr(id, sval) ;
			printf("\t%s(%d) = [", id, n) ;
			prstr(stdout, sval, n, 50) ;
			printf("]\n") ;
			break ;
		case 'f':
			if (n != 2) ask("flag (boolean) capability", id) ;
			n = pgetflag(id) ;
			printf("\t%s = %c\n", id, (n? 'T': 'F') ) ;
			break ;
		case 'p':
		case 'P':
			if (n != 2) ask("program entry", id) ;
			n = pgetprg(id, a_prog, &a_poff, a_idat, &a_ioff) ;
			printf("\t%s(%d) =\n", id, n) ;
			if (n >= 0) {
				printf("\t%s(%d) =\n", id, n) ;
				if (*cmd == 'P') 
					dis18(stdout, a_prog, a_poff, a_idat) ;
				}
			else printf("\t--- error %d ---\n", n) ;
			break ;
		case 'd':
			if (n == 2) fp = opfile(id) ;
			else        fp = stdout ;
			dis18(fp, a_prog, a_poff, a_idat) ;

			if (fp != stdout) fclose(fp) ;
			break ;
		case 'r':
		case 'R':
			if (n != 2) ask("run which entry", id) ;
			for (ep = elist; *ep->e_nam; ep++) {
				if (strequ(ep->e_nam,id) && ep->e_ent > 0) {
					printf("Running %s (%d) ...\n") ;
					if (*cmd == 'r') 
						/* alu18(ep->e_ent) ; */
						printf("can't run yet...\n") ;
					else tst18(a_prog, ep->e_ent, a_idat) ;
					}
				}
			break ;
		case 'l':
			printf("\tProg size %d,  lim %d\n", a_poff, PSIZE) ;
			printf("\tIdat size %d,  lim %d\n", a_ioff, ISIZE) ;
			break ;
		
		case 'g':
			for (ep = elist; *ep->e_nam; ep++) {
				printf("\t%s", ep->e_nam) ;
				n = pgetprg(ep->e_nam, a_prog, &a_poff,
						a_idat, &a_ioff) ;
				if (n > 0) {
					ep->e_ent = n ;
					printf("\t%d\n", n) ;
					}
				}
			break ;
		case 'e':
			for (ep = elist; *ep->e_nam; ep++) {
				if (ep->e_ent > 0)
				   printf("\t%s\t- %d\n",ep->e_nam,ep->e_ent) ;
				}
			printf("\n") ;
			break ;
		default:
			printf("huh?\tTry '?' for help.\n") ;
			break ;
			}

		}
	}

/* ask for some more info ... */
#ifdef ANSI_FUNC

int 
ask (char *prompt, char *buf)
#else

ask(prompt, buf)
	char	*prompt, *buf ;
#endif
	{
	printf("%s: ",prompt) ;
	scanf("%s", buf) ;
	}

/* open a file, write errors, (set fp to stdout if error) */
#ifdef ANSI_FUNC

FILE *
opfile (char *name)
#else

FILE *
opfile(name)
	char	*name ;
#endif
	{

	char	*how = "w" ;		/* for writting... */
	FILE	*fp ;

	if (*name == '+') {
		name++ ;
		how = "a" ;
		}

	fp = fopen(name, how) ;
	if (!fp) {
		printf("can't open `%s'\n", name) ;
		perror("open") ;
		fp = stdout ;
		}
	return fp ;
	}

/* to catch interrupts */
#ifdef ANSI_FUNC

int 
onintr (void)
#else

int
onintr()
#endif
	{
	printf("\n") ;
	longjmp(env) ;
	}
