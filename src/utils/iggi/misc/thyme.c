static char rcsid[] = "$Header: thyme.c,v 1.2 85/05/17 14:35:26 conor Exp $";

/*----------------------------------------------------------------------
 *
 * thyme.c - CPU timing functions.
 *
 * Copyright c 1985 The board of trustees of the Leland Stanford
 *                  Junior University. All rights reserved.
 * This subroutine may not be used outside of the SUPREM4 computer
 * program without the prior written consent of Stanford University.
 *
 * Original: CSR Nov 84
 *---------------------------------------------------------------------*/
#include <sys/types.h>
#include <sys/times.h>	
#include <stdio.h>	/* a) string functions b) FILE definition for...*/
#include <string.h>

FILE *cpu_fil = 0;	

/*-----------------CPU_SET----------------------------------------------
 * Set the CPU file.
 *----------------------------------------------------------------------*/
char * cpu_set (fname)
    char *fname;
{
    static char ebuf[80];
    if (! (cpu_fil = fopen(fname,"w"))) {
	sprintf (ebuf, "Could not open file %s", fname); return(ebuf);
	}
    return (0);
}

/*-----------------LOG_CPU----------------------------------------------
 * Pretty print the cpu time of a function. This routine is what gets
 * called from outside.
 * If tinit=0, return starting time,
 * otherwise assume tinit is the time we started and save the difference.
 *----------------------------------------------------------------------*/
double log_cpu (tinit, name)
    double tinit;	/* Initial time for routine. */
    char   *name;	/* Name of routine */
{
    char cbuf[50];  double now, cpu_time(); void rjust();

    /*...Anything doing? */
    if (cpu_fil == 0) return(0.0);

    if (tinit == 0.0) 
	return(cpu_time());
    else {
	now = cpu_time();
	rjust (cbuf, name, 50);
	fprintf (cpu_fil, "CPU time for %s ... %8.3f\n", cbuf, now-tinit);
	fflush (cpu_fil);
	return(now);
	}
}

/*-----------------CPU_TIME---------------------------------------------
 * Return CPU time in seconds.
 *----------------------------------------------------------------------*/
double cpu_time()
{
    struct tms user_tms;

    times (&user_tms);
    return ( (user_tms.tms_utime + user_tms.tms_stime)/60.0 );
}

/*-----------------RJUST------------------------------------------------
 * Right justify the string nam in tbe buff of length n.
 *----------------------------------------------------------------------*/
void rjust (buf, nam, n)
	char *buf, *nam;
	int n;
{
    int j,m;
    
    buf[n-1] = '\0';
    for (m = strlen(nam)-1, j = n-2; j >=0 && m >= 0; j--, m--)
	buf[j] = nam[m];
    for (; j >= 0; j--)
	buf[j] = ' ';
}
