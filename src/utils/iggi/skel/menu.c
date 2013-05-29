/*----------------------------------------------------------------------
 *                                                                     
 * Menu routines for skel program.                                     
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
#include <ctype.h>		/* For lower->upper conversion */
#include "skelp.h"		/* Screen sizes. */
#include <gplot.h>	/* Plot library */
#include "menu.h"


#define MENU_LINEDRAW	1	/* linetype used to draw menu boxes
				 *   and text
				 */

#define DRAW    1	/* to draw things */
#define CLEAR  -1	/* to erase things */

/*-----------------COMMAND_LOOP-----------------------------------------
 * Dummy routine to intialize menu base and start.
 *----------------------------------------------------------------------*/
command_loop()
{
    void do_menu();
    wmenu->prevx = wmenu->vxmin;
    wmenu->prevy = wmenu->vymax;

  /*...Parse input and execute - never returns. */
    do_menu (main_menu);
}

/*-----------------DO_MENU----------------------------------------------
 * This is the core menu routine - recursively calls itself
 * for each new level of menu and pops whenever it gets a
 * null function.
 *----------------------------------------------------------------------*/
void do_menu (m)
	struct mitem *m;
{
	struct mitem *which, *wait_menu();
	char *err, *uerr();

	disp_menu (m, MENU_LINEDRAW, DRAW);

	do {
	    which = wait_menu (m);			    
	    if (which->sub_menu == 0)
		{
		if (which->func != 0)
		    {
		    err = (*(which->func))();	   	   /* Jump subroutine */
		    if (err) uerr(err);
		    }
		}
	    else do_menu (which->sub_menu);
	    }
	while (which->func != 0 || which->sub_menu != 0);
	
	disp_menu (m, MENU_LINEDRAW, CLEAR);	        /* Erase menu */
}

/*-----------------DISP_MENU--------------------------------------------
 * Display a menu. Location is at main menu window variables
 * wmenu->prevx, wmenu->vymax, text size is wmenu->ch;
 *----------------------------------------------------------------------*/
disp_menu (m, linetype, mode)
	struct mitem *m;
	int linetype;
	int mode;  /* 1 for draw, -1 for clear */
{
	double fieldw; 
	double fieldh;
	float dummy1;

	fieldw = (1.0 + (double)menu_wide) * wmenu->cw;
	fieldh = 2.0 * wmenu->ch;
      /*...Set linetype, if MENU_LINECLEAR  then pop indentation level */
	if (mode == CLEAR) { 
	    /* deactivate level */
	    m->active = 0;
	    wmenu->prevx -= fieldw;
	}
	else  {
	    m->active = mode;
	}
	gnline (linetype);

      /*...Walk down list of items. List ends with a null command name */

	wmenu->prevy = wmenu->vymax - fieldh;	/* Start 1 line down.*/
	for (; m->text[0] != '\0'; m++)
	    {
	    set_viewport (&(m->w), wmenu->prevx, wmenu->prevx + 0.9*fieldw,
				   wmenu->prevy - 0.9*fieldh, wmenu->prevy );
	    draw_viewport (&(m->w), mode);
	    m->w.ch = wmenu->ch; 
	    m->w.cw = wmenu->cw;

	    vlabel (&(m->w), 0.5, 0.5, 0.5, 0.5, m->text, linetype);
	    wmenu->prevy -= fieldh;
	    }

      /*...If new menu, push new indent level. */
	if (mode>0) wmenu->prevx += fieldw;

	ggtoa();

	/* reset mode after clearing out menu */
	if (mode == CLEAR)
	    gplot2(G_DMODE, G_MSET, dummy1, dummy1);
}


/*-----------------WAIT_MENU--------------------------------------------
 * Wait for the user to select one of the menu items.
 *----------------------------------------------------------------------*/

/* BSD does toupper wrong (groan) */
#define Toupper(c) (islower(c)?toupper(c):(c))

struct mitem * wait_menu (m)
	struct mitem *m;
{
	struct mitem *mm; double x,y; int ipen, found;
	
      /*...Check letter first, then box */
	do  {
	    get_grin (&(m->w), &x, &y, &ipen, "*");
	    for (found=0, mm = m; !found && mm->text[0] ; mm++)
		if (Toupper(ipen) == Toupper((int)mm->text[0]))
		    {found = 1; goto BYE;}

	    for (found=0, mm = m; !found && mm->text[0] ; mm++)
		if (in_view (&(mm->w), x, y)) 
		    {found = 1; goto BYE;}
	    } 
	while (!found);

   BYE: return (mm);
}


/*-----------------REFRESH_MENU-----------------------------------------
 *After a graphics clear.
 *----------------------------------------------------------------------*/
void refresh_menu ()
{
    wmenu->prevx = wmenu->vxmin;
    do_rfmenu (main_menu);
}

do_rfmenu (m)
    struct mitem *m;
{
    if (m->active) {
	/* draw top level menu */
	disp_menu (m, MENU_LINEDRAW);

	/* draw submenu */
	for (; m->text[0] != '\0'; m++) 
	    if (m->sub_menu != 0) do_rfmenu (m->sub_menu);
	}
}
