/*----------------------------------------------------------------------
 *
 * skel: Skeleton grid generator.
 *       Attempts to simplify the generation of input files for tri.
 *       Secondly (and unwisely?) it can be used to edit already
 *       generated grids.
 *       Thirdly it can be used to draw stick pictures.
 *
 * Copyright c 1985 The board of trustees of the Leland Stanford
 *                  Junior University. All rights reserved.
 * This subroutine may not be used outside of the SUPREM4 computer
 * program without the prior written consent of Stanford University.
 *
 * Original: CSR Feb85
 *---------------------------------------------------------------------*/
#include "general.h"
#include "dbase.h"
#include "skelp.h"
#include <stdlib.h>
#include <stdio.h>

main (ac, av)
    int ac;
    char * av[];

{
    char *err, *screen_init(), *rumesh(), *wreset();
    int an, Nfile;

    global_init();

/*...Check options before going to screen_init. */
    for (an = 1; an < ac; an++) 
	{
	if (av[an][0] == '-') 
	    if (av[an][1] == 's') verbose = 0;
	    else if (av[an][1] == 'n') check = 0;
	    else if (av[an][1] == 'a') DsAxis = 0;
	    else printf("skel: unknown option %s\n",av[an]);
	/* Ignore files for now */
	}

    err = screen_init();
    if (err) {
	printf("Error setting up screen for your terminal.\n%s\n",err);
	exit(1);
	}
    
/*...Once in graphics mode, trap interrupts. */
    do_signal();

/*...Input files. */
    for (Nfile=0, an = 1; an < ac; an++) 
	if (av[an][0] != '\0' && av[an][0] != '-')
	    { err = rumesh (av[an]); if (err) uerr(err); Nfile++;}

/*...Setup the initial screen and go. */
    if (Nfile) 
        if (err = wreset())
            uerr(err);
    draw_mesh();
    command_loop();	/* Never returns */
}
