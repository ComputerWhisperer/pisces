/*************************************************************************
 *									 *
 *     Copyright c 1989 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the MOSMESH computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/



/*	main.c		Version 1.6		*/
/*	Last Modification:	10/19/90 13:39:49		*/


#include <stdio.h>
#include <math.h>
#include <ctype.h>
#define STATIC_ALLOCATION_TIME
#include "expr.h"
#include "struct.h"


FILE *defaultsfile;
FILE *tempfile;
FILE *changefile;


double RATIO_LAT = (double)LAT_RATIO/100.0;  /* defaults to 0.8 */

int computemode = -1;  /* 0 for collector, 1 for base  */
int dopingmode = -1;   /* 0 for analytic profiles only
		     	* 1 for Suprem3 profiles only
		     	* 2 for both analytic and Suprem3 profiles
			*/
		     
int inputmode = 0;  /* 0 for user input, 1 for batch mode  */
int change = 0; /* 0 do not modify defaultfile, 1 modify defaultfile */
char changebuf[80]; /* store the modified value */
int changetag = 0; /* tag number of next item to be modified */
int tagnumber = 0; /* current tag number */
int fileempty = 0; /* 0 if not empty, 1 if empty default file */

int exporttype = 1;  /*  0 for binary export format from Suprem3
		      *  1 for ascii export format from Suprem3
		      *  Pisces version 9030 supports acsii format.
		      */

/* 0 for no debugginf info, 1 for debugging info */
#ifdef DEBUG
	int debug = 1;
#else
	int debug = 0;
#endif

extern read_float();
extern read_deffloat();
extern read_real();
extern read_defreal();
extern get_info();
extern generate_deck();
extern write_pisces();

#ifdef ANSI_FUNC

int 
main (int argc, char **argv)
#else

main(argc, argv)
int argc;
char **argv;
#endif
{

    char buf[80];
    int c;
    extern int optind;
    extern char *optarg;
    extern int opterr;
    char *infile = ".defaultmesh";
    char *outfile;
    char *modfile = "";
    int errflg = 0;

    mincollector = 0.0; /* minimum collector voltage */	  
    maxcollector = 0.0; /* maximum voltage for collector sweep */
    maxbase = 0.0;  /* maximum voltage or current for base sweep */
    minbase = 0.0; /* minimum voltage or current for base sweep */
    collectorincrement = 0.20; /* collector increment for collector sweep */
    baseincrement = 0.25; /* increment voltage or current for base sweep */

    outfile = malloc(80*sizeof(char));
    outfile = "bipmesh.out";
    while ((c = getopt(argc, argv, "abcdf:im:o:st3")) != EOF)
	switch (c)  {
		case 'a'  :
		    if ( dopingmode == -1 )
		        dopingmode = 0;
		    else  {
			printf("\tOnly one of [a,c,s] allowed\n" );
			fflush( stdout );
			exit(ERROR);
		    }
		    break;

	   	case 'b'  :
		    inputmode = 1;
		    break;

		case 'c'  :
		    if ( dopingmode == -1 )
		        dopingmode = 2;
		    else  {
			printf("\tOnly one of [a,c,s] allowed\n" );
			fflush( stdout );
			exit(ERROR);
		    }
		    break;

	        case 'd'  :
		    debug = 1;
		    break;

		case 'f'  :
		    infile = optarg;
		    break;

	        case 'i'  :
		    if ( computemode == -1 )  
			computemode = 0;
		    else  {
			printf(" mode already specified. ignore this flag\n");
			fflush( stdout );
		    }
		    break;

		case 'm'  :
		    change = 1;
		    modfile = optarg;
		    break;

		case 'o'  :
		    outfile = optarg;
		    break;

		case 's'  :
		    if ( dopingmode == -1 )
		        dopingmode = 1;
		    else  {
			printf("\tOnly one of [a,c,s] allowed\n" );
			fflush( stdout );
			exit(ERROR);
		    }
		    break;

	        case 't'  :
		    if ( computemode == -1 )   
			computemode = 1;
		    else  {
			printf("mode already specified. ignore this flag\n");
			fflush ( stdout );
	 	    }	
		    break;

		case '3'  :
		    stridedeck = 1;
		    outfile = "stridemesh.out";
		    break;

		case '?'  :
		    errflg++;
		    break;

	        default   :
		    printf("switch not supported. ignore flag. \n");
		    fflush( stdout );
		    break;
	} /* end switch */

    if (errflg)  {
fprintf(stderr, "Usage: bipmesh [-abf<filename>im<filename>o<filename>st3]\n");
	fprintf(stderr, "\ta - uses analytic profiles for doping[default]\n" );
	fprintf(stderr, "\tb - batch mode\n" );
/*
	fprintf(stderr, "\td - debug mode\n");   
*/
	fprintf(stderr, "\tf - uses file filename (default is .defaultmesh) for input\n");
	fprintf(stderr, "\ti - computes Ic vs. Vce (step Ib)\n" );
	fprintf(stderr, "\tm - use file filename to modify the defaults\n" );
	fprintf(stderr, "\to - uses file filename (default is bipmesh.out) for output\n");
	fprintf(stderr, "\ts - uses Suprem3 input files for doping\n" ); 
	fprintf(stderr, "\tt - computes Ic vs. Vbe (Vce fixed)\n" );
	fprintf(stderr, "\t3 - creates input deck for STRIDE\n" );
	exit(ERROR);
    }

/*  now that we have parsed the input information, look for an output file */
   
    if ( optind < argc  )
        outfile = argv[optind];

/*  open files for default information */

    if ( (defaultsfile = fopen( infile, "r" )) == NULL )
	fileempty = 1;

/* create a temporary defaults file */
    tempfile = fopen( ".tempfile", "w" );
    if ( tempfile == NULL )  {
	fprintf(stderr,  "Could not open the defaults file .tempfile\n" );
	exit(ERROR);
    }

/* open file for modification */
    if ( change == 1 )  {
	if ( (changefile = fopen( modfile, "r" )) == NULL)  {
	    fprintf(stderr,  "\terror in opening file %s\n", modfile );
	    exit( ERROR );
	}
	fgets( buf, 80, changefile );
	sscanf( buf, "%d %s", &changetag, changebuf );
    }



    if ( inputmode != 1 )  {
        printf( "\t\tBIPMESH Version 9040\n" );
        printf( "\tCurrently only BJT devices are supported\n" );
        printf( "\tDefault values based on your last run are in brackets\n" );
        printf( "\tInput from %s and output to %s\n\n", infile, outfile );
        fflush( stdout );
    }

/*  change doping mode to default if not modified above */
    if ( dopingmode == -1 )
        dopingmode = 0;

   
/*  get information on lateral diffusion here */
    if (!stridedeck)  {
      if ( inputmode != 1 )
	read_real( "lateral diffusion constant", &RATIO_LAT );
      else
	read_defreal( "lateral diffusion constant", &RATIO_LAT );
    }

    if ( (RATIO_LAT < 0.0) || (RATIO_LAT > 1.0) )  {
	fprintf( stderr, "\tvalue of lateral diffusion not in [0,1]. set to 0.8\n" );
	RATIO_LAT = 0.8;
    }

    if (stridedeck)
	get_3dinfo( dopingmode, inputmode );
    else
	get_info( dopingmode, inputmode );

    if ( debug == 1 ) dump_info(); 

    generate_deck();

    if  (inputmode == 1) 
	if (!stridedeck)
	    switch ( computemode ) {
		case  0  :

    read_deffloat( "start sweeping collector voltage from", &mincollector );
    read_deffloat( "sweep collector voltage to", &maxcollector );
    read_deffloat( "collector increment" , &collectorincrement );
    if ( collectorincrement == 0.0 )  {
	fprintf(stderr, "\tcan't have 0 value. will set to provide 20 steps\n");
	collectorincrement = (maxcollector - mincollector)/20.0;
    }
/*  sanity check on sign of collectorincrement */
    if ( ( (maxcollector - mincollector) / collectorincrement) <= 0.0 )  {
	fprintf(stderr, "\tsign of collectorincrement is inconsistent with other data.\n");
        fprintf(stderr, "\twill switch value of sign. if not right run again.\n" );
        fflush( stderr );
        collectorincrement = -collectorincrement;
    }
    read_deffloat( "starting base current", &minbase );
    read_deffloat( "sweep base current to", &maxbase );
    read_deffloat( "base current increment", &baseincrement );
    if ( baseincrement == 0.0 )  {
	printf("\tcannot have a zero value here. will set to provide 5 steps\n");
	baseincrement = (maxbase - minbase)/5.0;
    }
/*  sanity check on sign of baseincrement */
    if ( ( (maxbase - minbase) / baseincrement) <= 0.0 )  {
        fprintf(stderr, "\tsign of baseincrement is inconsistent with other data.\n");
        fprintf(stderr, "\twill switch value of sign. if not right run again.\n" );
        fflush( stderr );
        baseincrement = -baseincrement;
    }
		break;

		case  1  :

    read_deffloat( "collector voltage", &maxcollector );
/* sanity check on sign of maxcollector */
    if ( ((dop_data[2].type == P_TYPE) && (maxcollector > 0.0))
	    || ((dop_data[2].type == N_TYPE) && (maxcollector < 0.0)) )  {
        fprintf(stderr, "\tsign of collector is inconsistent with other data.\n");
        fprintf(stderr, "\twill switch value of sign. if not right run again.\n" );
        fflush( stderr );
        maxcollector = -maxcollector;
    }
	
    read_deffloat( "starting base voltage", &minbase );
    read_deffloat( "sweep base voltage to", &maxbase );
    read_deffloat( "base bias increment", &baseincrement );
    if ( baseincrement == 0.0 )  {
	printf("\tcannot have a zero value here. will set to provide 20 steps\n");
	baseincrement = (maxbase - minbase)/20.0;
    }
/*  sanity check on sign of baseincrement */
    if ( ( (maxbase - minbase) / baseincrement) <= 0.0 )  {
        fprintf(stderr, "\tsign of baseincrement is inconsistent with other data.\n");
        fprintf(stderr, "\twill switch value of sign. if not right run again.\n" );
        fflush( stderr );
        baseincrement = -baseincrement;
    }
		break;
    
		default  :
		break;

	    } /* end switch  computemode */
	else  /* stridedeck */
	    switch ( computemode ) {
		case  0  :
	/* since this case isn't allowed for STRIDE (yet) assume that
	 *   user wanted to ramp base voltage with collector fixed
	 */
		case  1  :

    read_deffloat( "collector voltage", &maxcollector );
/* sanity check on sign of maxcollector */
    if ( ((dop_data[2].type == P_TYPE) && (maxcollector > 0.0))
	    || ((dop_data[2].type == N_TYPE) && (maxcollector < 0.0)) )  {
        fprintf(stderr, "\tsign of collector is inconsistent with other data.\n");
        fprintf(stderr, "\twill switch value of sign. if not right run again.\n" );
        fflush( stderr );
        maxcollector = -maxcollector;
    }
	
    read_deffloat( "starting base voltage", &minbase );
    read_deffloat( "sweep base voltage to", &maxbase );
    read_deffloat( "base bias increment", &baseincrement );
    if ( baseincrement == 0.0 )  {
	printf("\tcannot have a zero value here. will set to provide 20 steps\n");
	baseincrement = (maxbase - minbase)/20.0;
    }
/*  sanity check on sign of baseincrement */
    if ( ( (maxbase - minbase) / baseincrement) <= 0.0 )  {
        fprintf(stderr, "\tsign of baseincrement is inconsistent with other data.\n");
        fprintf(stderr, "\twill switch value of sign. if not right run again.\n" );
        fflush( stderr );
        baseincrement = -baseincrement;
    }

    /* go set up initial values in electrode array */
    electrode[0].pot = 0.0;
    electrode[1].pot = minbase;
    electrode[2].pot = maxcollector;
		break;
    
		default  :
		break;

	    } /* end switch  computemode */
	/* end if then else stridedeck */
    /* end if inputmode == 0 */

    else  /* inputmode==0 */ 
	if (!stridedeck)
	    switch ( computemode ) {
		case  0  :

    read_float( "start sweeping collector voltage from", &mincollector );
    read_float( "sweep collector voltage to", &maxcollector );
    read_float( "collector increment" , &collectorincrement );
    if ( collectorincrement == 0.0 )  {
	fprintf(stderr, "\tcannot have a zero value here. will set to provide 20 steps\n");
	collectorincrement = (maxcollector - mincollector)/20.0;
    }
/*  sanity check on sign of collectorincrement */
    if ( ( (maxcollector - mincollector) / collectorincrement) <= 0.0 )  {
        fprintf(stderr, "\tsign of collectorincrement is inconsistent with other data.\n");
        fprintf(stderr, "\twill switch value of sign. if not right run again.\n" );
        collectorincrement = -collectorincrement;
    }
    read_float( "starting base current", &minbase );
    read_float( "sweep base current to", &maxbase );
    read_float( "base current increment", &baseincrement );
    if ( baseincrement == 0.0 )  {
	fprintf(stderr, "\tcannot have a zero value here. will set to provide 5 steps\n");
	baseincrement = (maxbase - minbase)/5.0;
    }
/*  sanity check on sign of baseincrement */
    if ( ( (maxbase - minbase) / baseincrement) <= 0.0 )  {
        fprintf(stderr, "\tsign of baseincrement is inconsistent with other data.\n");
        fprintf(stderr, "\twill switch value of sign. if not right run again.\n" );
        baseincrement = -baseincrement;
    }
		break;

		case  1  :

    read_float( "collector voltage", &maxcollector );
/* sanity check on sign of maxcollector */
    if ( ((dop_data[2].type == P_TYPE) && (maxcollector > 0.0))
	    || ((dop_data[2].type == N_TYPE) && (maxcollector < 0.0)) )  {
        fprintf(stderr, "\tsign of collector is inconsistent with other data.\n");
        fprintf(stderr, "\twill switch value of sign. if not right run again.\n" );
        maxcollector = -maxcollector;
    }
	
    read_float( "starting base voltage", &minbase );
    read_float( "sweep base voltage to", &maxbase );
    read_float( "base bias increment", &baseincrement );
    if ( baseincrement == 0.0 )  {
	fprintf(stderr, "\tcannot have a zero value here. will set to provide 20 steps\n");
	baseincrement = (maxbase - minbase)/20.0;
    }
/*  sanity check on sign of baseincrement */
    if ( ( (maxbase - minbase) / baseincrement) <= 0.0 )  {
        fprintf(stderr, "\tsign of baseincrement is inconsistent with other data.\n");
        fprintf(stderr, "\twill switch value of sign. if not right run again.\n" );
        baseincrement = -baseincrement;
    }
		break;

    
		default  :
		break;

	    } /* end switch */

	else /* stridedeck */
	    switch ( computemode ) {
		case  0  :
	fprintf(stderr, 
	"can't ramp base current with STRIDE. default to ramping voltage\n");
		case  1  :

    read_float( "collector voltage", &maxcollector );
/* sanity check on sign of maxcollector */
    if ( ((dop_data[2].type == P_TYPE) && (maxcollector > 0.0))
	    || ((dop_data[2].type == N_TYPE) && (maxcollector < 0.0)) )  {
        fprintf(stderr, "\tsign of collector is inconsistent with other data.\n");
        fprintf(stderr, "\twill switch value of sign. if not right run again.\n" );
        maxcollector = -maxcollector;
    }
	
    read_float( "starting base voltage", &minbase );
    read_float( "sweep base voltage to", &maxbase );
    read_float( "base bias increment", &baseincrement );
    if ( baseincrement == 0.0 )  {
	fprintf(stderr, "\tcannot have a zero value here. will set to provide 20 steps\n");
	baseincrement = (maxbase - minbase)/20.0;
    }
/*  sanity check on sign of baseincrement */
    if ( ( (maxbase - minbase) / baseincrement) <= 0.0 )  {
        fprintf(stderr, "\tsign of baseincrement is inconsistent with other data.\n");
        fprintf(stderr, "\twill switch value of sign. if not right run again.\n" );
        baseincrement = -baseincrement;
    }
    /* go set up initial values in electrode array */
    electrode[0].pot = 0.0;
    electrode[1].pot = minbase;
    electrode[2].pot = maxcollector;
		break;

    
		default  :
		break;

	    } /* end switch */
	/* end if then else stridedeck */
    /* end if then else inputmode */

    if (stridedeck)
	write_stride( outfile );
    else
	write_pisces( outfile );


    /* to move the contents of tempfile to defaultsfile, close the
     *	files, reopen them, and copy character by character until
     *  done
     */
    if ( fileempty != 1 )
        fclose( defaultsfile );    
    fclose( tempfile );

    defaultsfile = fopen( ".defaultmesh", "w" );
    tempfile = fopen( ".tempfile", "r" );

    while ( !feof(tempfile) )
	putc( fgetc(tempfile), defaultsfile );

    fclose( defaultsfile );
    fclose( tempfile );

    exit(0);
}
