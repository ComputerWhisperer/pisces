/*----------------------------------------------------------------------
 *                                                                     
 * iltty.c  - initialize terminal.
 * Calculate margins, menu sizes, and set up windows. A messy routine.
 *                                                                     
 * Copyright c 1985 The board of trustees of the Leland Stanford       
 *                  Junior University. All rights reserved.            
 * This subroutine may not be used outside of the SUPREM4 computer     
 * program without the prior written consent of Stanford University.   
 *                                                                     
 * Original CSR Feb85                                                  
 *---------------------------------------------------------------------*/
#include <general.h>
#include <stdio.h>
#include <gplot.h>
#include "skelp.h"		/* Screen, window variables */
#include "menu.h"		/* Menu width, depth. */
#include "dbase.h"		/* Verbose flag */


/*-----------------SCREEN_INIT------------------------------------------
 *----------------------------------------------------------------------*/
char * screen_init()
{
    double lmargin, tmargin, axmargin;
    char *err, *tty_parm();

  /*...First get the size of the alpha/graph screens. */
    if (err = tty_parm()) return(err);

  /*...Clear alpha then graphic screens. */
    goto_ij (0, 0); 
    al_clear();		/* Clear alpha */
    fflush(stdout);	/* Flush stdio before returning to gplot */
    gatog(); 		/* Leave terminal in graphics from now on. */
    gclear();	

  /*...Create viewports - reserve left margin and top line.
   *...Use default character size for axis labels & verbose texts;
   *...use 1.5 times bigger for terse texts.
   */
    if (!verbose) menu_wide = 1;
    lmargin = menu_deep * (menu_wide+1) * DefCw * (verbose? 1.0 : 1.5);
    tmargin = DefCh * (verbose? 1.0 : 1.5);
    axmargin = 2*DefCh; /* 2 because there's a space as well as the number */

  /*...Main display window */
    wmesh = cr_window ();    
    if (!wmesh) uerr ("init_screen: out of memory ?");
    set_viewport (wmesh, lmargin, ScrWd-axmargin, axmargin, ScrHt - tmargin ); 
    wmesh->cw = DefCw * (verbose? 1.0 : 1.5);
    wmesh->ch = DefCh * (verbose? 1.0 : 1.5);

  /*...Menu window */
    wmenu = cr_window ();    
    if (!wmenu) uerr ("init_screen: out of memory ?");
    set_viewport (wmenu, 0.0, lmargin, 0.0, ScrHt - tmargin );
    wmenu->cw = DefCw * (verbose? 1.0 : 1.5);
    wmenu->ch = DefCh * (verbose? 1.0 : 1.5);

    return(0);
}
