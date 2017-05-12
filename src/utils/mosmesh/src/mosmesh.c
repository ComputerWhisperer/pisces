/*************************************************************************
 *									 *
 *     Copyright c 1988 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the MOSMESH computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/
/*   %M%                Version %I%     */
/*   Last Modification : %G% %U% */

#include <stdio.h>
#include <math.h>
#include <ctype.h>
#define STATIC_ALLOCATION_TIME
#include "expr.h"
#include "struct.h"


int elnum = 0;
int xnum = 0;
int ynum = 0;
FILE *defaultsfile;
FILE *tempfile;
FILE *changefile;

lat_str lat_info;
vert_str vert_info;
dop_str dop_data[10];
node_str xnd[50];
node_str ynd[50];
elim_str elim[10];

double RATIO_LAT = (double)LAT_RATIO/100.0;  /* defaults to 0.8 */

int computemode = -1; /* 0 for drain, 1 for gate */
int dopingmode = -1; /* 0 for analytic profiles only
		     * 1 for Suprem3 profiles only
		     * 2 for both analytic and Suprem3 profiles
		     */
int inputmode = 0; /* 0 for user input, 1 for batch mode */
double qf = 0.0; /* fixed charge */
float mindrain = 0.0; /* minimum drain voltage */	  
float maxdrain = 0.0; /* maximum voltage for drain sweep */
float maxgate = 0.0;  /* maximum voltage for gate sweep */
float mingate = 0.0; /* minimum voltage for gate sweep */
float drainincrement = 0.20; /* drain increment for drain sweep */
float gateincrement = 0.25; /* gate increment voltage for gate sweep */
int change = 0; /* 0 do not modify defaultfile, 1 modify defaultfile */
char changebuf[80]; /* store the modified value */
int changetag = 0; /* tag number of next item to be modified */
int tagnumber = 0; /* current tag number */
int fileempty = 0; /* 0 if not empty, 1 if empty default file */

int exporttype = 1;  /*  0 for binary export format from Suprem3
		      *  1 for ascii export format from Suprem3
		      *  Pisces version 8822 supports acsii format.
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
extern write_info();

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
    char *outfile = "mosmesh.out";
    char *modfile = "";
    int errflg = 0;

    while ((c = getopt(argc, argv, "abcdf:im:o:st")) != EOF)
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

		case '?'  :
		    errflg++;
		    break;

	        default   :
		    printf("switch not supported. ignore flag. \n");
		    fflush( stdout );
		    break;
	} /* end switch */

    if (errflg)  {
	fprintf(stderr, "Usage: mosmesh [-abf<filename>im<filename>o<filename>st]\n");
	fprintf(stderr, "\ta - uses analytic profiles for doping[default]\n" );
	fprintf(stderr, "\tb - batch mode\n" );
/*
	fprintf(stderr, "\td - debug mode\n");   
*/
	fprintf(stderr, "\tf - uses file filename (default is .defaultmesh) for input\n");
	fprintf(stderr, "\ti - computes drain I-V curves\n" );
	fprintf(stderr, "\tm - use file filename to modify the defaults\n" );
	fprintf(stderr, "\to - uses file filename (default is mosmesh.out) for output\n");
	fprintf(stderr, "\ts - uses Suprem3 input files for doping\n" ); 
	fprintf(stderr, "\tt - sweeps gate for fixed drain voltage\n" );
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
        printf( "\t\tMOSMESH Version 9030\n" );
        printf( "\tCurrently only MOS devices are supported\n" );
        printf( "\tDefault values based on your last run are in brackets\n" );
        printf( "\tInput from %s and output to %s\n\n", infile, outfile );
        fflush( stdout );
    }

/*  change doping mode to default if not modified above */
    if ( dopingmode == -1 )
        dopingmode = 0;

   
/*  get information on lateral diffusion here */
    if ( inputmode != 1 )
	read_real( "lateral diffusion constant", &RATIO_LAT );
    else
	read_defreal( "lateral diffusion constant", &RATIO_LAT );

    if ( (RATIO_LAT < 0.0) || (RATIO_LAT > 1.0) )  {
	fprintf( stderr, "\tvalue of lateral diffusion not in [0,1]. set to 0.8\n" );
	RATIO_LAT = 0.8;
    }

    get_info( dopingmode, inputmode );

    if ( debug == 1 ) dump_info(); 

    generate_deck();

    if ( inputmode == 1 )  {
    	read_defreal( "fixed charge", &qf );
	switch ( computemode ) {
	    case  0  :
	    read_deffloat( "start sweeping drain voltage from", &mindrain );
	    read_deffloat( "sweep drain voltage to", &maxdrain );
	    read_deffloat( "drain increment" , &drainincrement );
	    if ( drainincrement == 0.0 )  {
		fprintf(stderr, "\tcannot have a zero value here. will set to provide 20 steps\n");
		drainincrement = (maxdrain - mindrain)/20.0;
	    }
/*  sanity check on sign of drainincrement */
	    if ( ( (maxdrain - mindrain) / drainincrement) <= 0.0 )  {
	        fprintf(stderr, "\tsign of drainincrement is inconsistent with other data.\n");
	        fprintf(stderr, "\twill switch value of sign. if not right run again.\n" );
	        fflush( stderr );
	        drainincrement = -drainincrement;
 	    }
	    read_deffloat( "starting gate voltage", &mingate );
	    read_deffloat( "sweep gate voltage to", &maxgate );
	    read_deffloat( "gate increment", &gateincrement );
	    if ( gateincrement == 0.0 )  {
		printf("\tcannot have a zero value here. will set to provide 20 steps\n");
		gateincrement = (maxgate - mingate)/20.0;
	    }
/*  sanity check on sign of gateincrement */
	    if ( ( (maxgate - mingate) / gateincrement) <= 0.0 )  {
	        fprintf(stderr, "\tsign of gateincrement is inconsistent with other data.\n");
	        fprintf(stderr, "\twill switch value of sign. if not right run again.\n" );
	        fflush( stderr );
	        gateincrement = -gateincrement;
 	    }
	    break;

	    case  1  :
	    read_deffloat( "drain voltage", &maxdrain );
/* sanity check on sign of maxdrain */
	    if ( ((dop_data[2].type == P_TYPE) && (maxdrain > 0.0))
		    || ((dop_data[2].type == N_TYPE) && (maxdrain < 0.0)) )  {
	        fprintf(stderr, "\tsign of drain is inconsistent with other data.\n");
	        fprintf(stderr, "\twill switch value of sign. if not right run again.\n" );
	        fflush( stderr );
	        maxdrain = -maxdrain;
	    }
	
	    read_deffloat( "starting gate voltage", &mingate );
	    read_deffloat( "sweep gate voltage to", &maxgate );
	    read_deffloat( "gate bias increment", &gateincrement );
	    if ( gateincrement == 0.0 )  {
		printf("\tcannot have a zero value here. will set to provide 20 steps\n");
		gateincrement = (maxgate - mingate)/20.0;
	    }
/*  sanity check on sign of gateincrement */
	    if ( ( (maxgate - mingate) / gateincrement) <= 0.0 )  {
	        fprintf(stderr, "\tsign of gateincrement is inconsistent with other data.\n");
	        fprintf(stderr, "\twill switch value of sign. if not right run again.\n" );
	        fflush( stderr );
	        gateincrement = -gateincrement;
 	    }
	    break;
    
	    default  :
			break;

        } /* end switch */

    } 
    else  {

	read_real( "fixed charge", &qf );

	switch ( computemode ) {
	    case  0  :
	    read_float( "start sweeping drain voltage from", &mindrain );
	    read_float( "sweep drain voltage to", &maxdrain );
	    read_float( "drain increment" , &drainincrement );
	    if ( drainincrement == 0.0 )  {
		fprintf(stderr, "\tcannot have a zero value here. will set to provide 20 steps\n");
		drainincrement = (maxdrain - mindrain)/20.0;
	    }
/*  sanity check on sign of drainincrement */
	    if ( ( (maxdrain - mindrain) / drainincrement) <= 0.0 )  {
	        fprintf(stderr, "\tsign of drainincrement is inconsistent with other data.\n");
	        fprintf(stderr, "\twill switch value of sign. if not right run again.\n" );
	        drainincrement = -drainincrement;
 	    }
	    read_float( "starting gate voltage", &mingate );
	    read_float( "sweep gate voltage to", &maxgate );
	    read_float( "gate increment", &gateincrement );
	    if ( gateincrement == 0.0 )  {
		fprintf(stderr, "\tcannot have a zero value here. will set to provide 20 steps\n");
		gateincrement = (maxgate - mingate)/20.0;
	    }
/*  sanity check on sign of gateincrement */
	    if ( ( (maxgate - mingate) / gateincrement) <= 0.0 )  {
	        fprintf(stderr, "\tsign of gateincrement is inconsistent with other data.\n");
	        fprintf(stderr, "\twill switch value of sign. if not right run again.\n" );
	        gateincrement = -gateincrement;
 	    }
	    break;

	    case  1  :
	    read_float( "drain voltage", &maxdrain );
/* sanity check on sign of maxdrain */
	    if ( ((dop_data[2].type == P_TYPE) && (maxdrain > 0.0))
		    || ((dop_data[2].type == N_TYPE) && (maxdrain < 0.0)) )  {
	        fprintf(stderr, "\tsign of drain is inconsistent with other data.\n");
	        fprintf(stderr, "\twill switch value of sign. if not right run again.\n" );
	        maxdrain = -maxdrain;
	    }
	
	    read_float( "starting gate voltage", &mingate );
	    read_float( "sweep gate voltage to", &maxgate );
	    read_float( "gate bias increment", &gateincrement );
	    if ( gateincrement == 0.0 )  {
		fprintf(stderr, "\tcannot have a zero value here. will set to provide 20 steps\n");
		gateincrement = (maxgate - mingate)/20.0;
	    }
/*  sanity check on sign of gateincrement */
	    if ( ( (maxgate - mingate) / gateincrement) <= 0.0 )  {
	        fprintf(stderr, "\tsign of gateincrement is inconsistent with other data.\n");
	        fprintf(stderr, "\twill switch value of sign. if not right run again.\n" );
	        gateincrement = -gateincrement;
 	    }
	    break;
    
	    default  :
			break;

        } /* end switch */
    }

    write_info( outfile );


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
