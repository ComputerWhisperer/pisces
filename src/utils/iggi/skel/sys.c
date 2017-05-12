/*----------------------------------------------------------------------
 *
 * System-dependent routine to initialize the graphics and alpha
 * parameters of the user's terminal.
 *
 * Copyright c 1985 The board of trustees of the Leland Stanford
 *                  Junior University. All rights reserved.
 * This subroutine may not be used outside of the IGGI2 computer
 * program without the prior written consent of Stanford University.
 *
 * Original CSR Feb85
 *---------------------------------------------------------------------*/
#include "general.h"
#include "gplot.h"	/* For the gpgets call */
#include "skelp.h"		/* Tty size gets stored here. */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <termcap.h>


static char u_tty[80];			/* User's terminal */
static char AlGoto[32];			/* ...cursor motion string */
static char AlClear[16];		
static char AlCleol[16];
static int  AlRow, AlCol;

/*-----------------TTY_PARM---------------------------------------------
 *Get the screen sizes for the /etc/plotcap and /etc/termcap data bases.
 *Leaves the terminal in alpha mode.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
tty_parm (void)
#else

char * tty_parm()
#endif
{
    int ierr, tgetent(), tgetnum();
    int iv[4];
    float fv[4];
    float f1, f2;
    char tty_desc[1024]; 		/* termcap entry. */
    char *getenv(), *tgetstr(), *temp;


  /*...Call TERMCAP for terminal capabilities. */
    strcpy (u_tty, getenv("TERM"));	

    if (!u_tty[0]) 
	return ("You have no $TERM defined!");

    ierr = tgetent (tty_desc, u_tty);
    if (ierr != 1) 
	return ("Termcap problem for this $TERM");

    AlRow = tgetnum("li");
    AlCol = tgetnum("co");

    temp = AlGoto;    tgetstr("cm", &temp);
    temp = AlClear;   tgetstr("cd", &temp);
    temp = AlCleol;   tgetstr("ce", &temp);

  /*...Call GPLOT to determine graphics screen size from environment. */
    gpgeti(G_PSIZE, iv, fv);
    f1 = fv[0];
    f2 = fv[1];

    if (f1 <= 0 || f2 <=0) 
	{
	ScrHt = 0;
	ScrWd = 0;
	ggtoa(); 	
	return("screen_init: gplot didn't recognize your terminal");
	}
    ScrWd = 0.99*f1;
    ScrHt = 0.99*f2;
    ggtoa();

    return(0);
}


/*-----------------GOTO_IJ----------------------------------------------
 * Go to position i,j on the alphanumeric screen 
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

int 
goto_ij (
    int i,	/* Row */
    int j	/* Column */
)
#else

goto_ij (i,j)
    int i;	/* Row */
    int j;	/* Column */
#endif
{
    printf ("%s", tgoto (AlGoto, j, i));
}


/*-----------------AL_CLEAR---------------------------------------------
 * Clear to the end of the alphanumeric screen.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

int 
al_clear (void)
#else

al_clear ()
#endif
{
    printf ("%s", AlClear);
}

/*-----------------AL_CLEAR---------------------------------------------
 * Clear to the end of the alphanumeric line.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

int 
al_cleol (void)
#else

al_cleol ()
#endif
{
    printf ("%s", AlCleol);
}
/*-----------------DO_SIGNAL--------------------------------------------
 *Trap signals.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

int 
do_signal (void)
#else

int do_signal()
#endif
{
    int really_exit(), hung(), tstop();

#ifdef PRODUCTION
    /*...For the moment...*/
    signal (SIGINT,  SIG_IGN);
    signal (SIGQUIT, SIG_IGN);
    signal (SIGTERM, SIG_IGN);
#endif
}

/*-----------------UESCAPE----------------------------------------------
 * @ takes a break.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
uescape (void)
#else

char *uescape()
#endif
{
#   define CLEN 80
    char commandline[CLEN], *allover();

    ggtoa();
    goto_ij (AlRow-2, 0);
    printf ("!"); 
    if (fgets (commandline, CLEN, stdin) == NULL)
	{uerr("End of input...quit"); allover();}
    system (commandline);
    printf ("\n-- Type return to continue --");
    wait_more();
    goto_ij (0, 0);
    al_clear();
    gatog();
    skel_screen();
    return(0);
}

    
/*-----------------ALLOVER----------------------------------------------
 * Exit.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
allover (void)
#else

char *allover()
#endif
		/* because it was defined as char* in the menu. */
{
    ggtoa();
    goto_ij (AlRow-2, 0);
    exit(0);
}


