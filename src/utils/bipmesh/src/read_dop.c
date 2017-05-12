/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/



/*	read_dop.c		Version 1.4		*/
/*	Last Modification:	1/29/90 01:39:44		*/


#include <stdio.h>
#include <math.h>
#include "struct.h"

extern FILE *defaultsfile;
extern FILE *tempfile;
extern FILE *changefile;
extern char *parse_expr();
extern char *eval_real();
extern change;  /* 1 if we are in modify mode */
extern changetag; /* teg of next modification */
extern char changebuf[80]; /* stores replacement string */
extern read_real();
extern calc_junc();
extern asciicalc();
extern fileempty;
extern tagnumber;
extern exporttype;
extern reads3as(); /* reads in Suprem3 export format (ascii) */ 
extern vert_str vert_info;
extern reads3_();  /* reads in Suprem3 export format (binary) */

#ifdef ANSI_FUNC

int 
read_dop (
    char *str,
    dop_str *dop,
    double substratedoping,
    int typeofprofile, /* 0 for analytic, 1	 for Suprem3 file */
    double *junction /* junction depth */
)
#else

read_dop( str, dop, substratedoping, typeofprofile, junction )
char *str;
dop_str *dop;
double substratedoping;
int typeofprofile; /* 0 for analytic, 1	 for Suprem3 file */
double *junction; /* junction depth */
#endif
{
    char buf[512];
    char filename[MAXNAMESIZE];
    char doping_type = 'P';
    double temp;
    double std_dev;
    double fx;
    double dfx;	
    int done = 0;
    char filetype = '0'; /* 'a' for ascii, 'b' for binary export, 
			'e' for ascii export , '0' for none */
    char defaultfilename[MAXNAMESIZE];
    int i;
    int j;
    FILE *okfile = NULL; /* NULL if doping information file cannot be opened */
    int tries = 0; /* give user 2 tries to find file */

    vert_info.substrate_depth = 0.0;
    *junction = 0.0;
    printf("%s\n", str);

    if ( typeofprofile == 0 )  {
        while( !done ) {
	    if ( (changetag == tagnumber) && (change == 1) ) {
		/* flush entry from defaults file */
		fgets( buf, 512, defaultsfile );
		sscanf( changebuf, "%1s", &doping_type );
		fgets( buf, 512, changefile );
		sscanf( buf, "%d %s", &changetag, changebuf );
	    }
	    else  
	        if (!fileempty)  {
		    fgets( buf, 512, defaultsfile );
 	            sscanf(buf, "%1s" , &doping_type);	
	        }

	    if ( !( (doping_type == 'P') || (doping_type == 'p')
		    || (doping_type == 'N') || (doping_type == 'n') ) )
 	        doping_type = '0';
	    printf("\tN/P/0(none) type doping? [%c] ", doping_type);
	    fflush(stdout);
	    gets(buf);

	    i = 0;
	    while ( buf[i] == ' ' ) i++;

	    if ( buf[i] == '\0' )  
		buf[i] = doping_type;

	    switch( buf[i] ) {
	    case 'n' :
	    case 'N' :
	        dop->type = N_TYPE;
	        done = 1;
    	        fprintf( tempfile, "N\t-\tDoping Type\t%d\n", tagnumber++ );
	        break;

	    case 'p' :
	    case 'P' :
	        dop->type = P_TYPE;
	        done = 1;
    	        fprintf( tempfile, "P\t-\tDoping Type\t%d\n", tagnumber++ );
	        break;

	    case '0' :
		dop->type = NONE;
		done = 1;
		fprintf( tempfile, "0\t-\tDoping Type\t%d\n", tagnumber++ );
		break;

	    default:
		fprintf( stderr, "\tillegal doping type [0,n,p]\n" );
		exit( ERROR );
		break;
	    } /* end switch */
        }
	if ( dop->type != NONE )  {
            printf("\tYou can enter doping information in 5 ways: \n");
            printf("\t\tGaussian Options:\n");
            printf("\t\t 1) peak conc., peak conc. location, std. dev \n");
            printf("\t\t 2) peak conc., peak conc. location, junc. depth \n");
            printf("\t\t 3) peak conc., peak conc. location, dose \n");
            printf("\t\t\t this one is not good for high concentrations!!\n");
	    printf("\n\t\tError Function Options:\n");
            printf("\t\t 4) peak conc., peak conc. location, std. dev \n");
            printf("\t\t 5) peak conc., peak conc. location, junc. depth \n");
            printf("\t\t\t concentration at junction \n");
            fflush(stdout);
	    doping_type = '0';

	    if ( (changetag == tagnumber) && (change == 1) ) {
		/* flush entry from defaults file */
		fgets( buf, 512, defaultsfile );
		sscanf( changebuf, "%1s", &doping_type );
		fgets( buf, 512, changefile );
		sscanf( buf, "%d %s", &changetag, changebuf );
	    }
	    else  
	        if (!fileempty)  {
		    fgets( buf, 512, defaultsfile );
 	            sscanf(buf, "%1s" , &doping_type);	
	        }

	    if (! ( (doping_type=='1') || (doping_type=='2') || 
		(doping_type=='3') || (doping_type=='4') ||
		(doping_type=='5') ) )
	        doping_type = '2';
            printf("\t\t please pick 1, 2, 3, 4, or 5 [%c] : ", doping_type );
            fflush(stdout);
            gets(buf);

	    i = 0;
	    while ( buf[i] == ' ' ) i++;

	    if ( buf[i] == '\0' )  {
	        fprintf( tempfile, "%c\t-\tProfile Type\t%d\n", doping_type,
		    tagnumber++ );
		buf[i] = doping_type;
	    }
	    else
	        fprintf( tempfile, "%c\t-\tProfile Type\t%d\n", buf[i],
		    tagnumber++ ); 

            switch( buf[i] ) {
	        case '1' :
    	            read_real("\tPeak Doping Concentration(0 for none)", 
			&((dop->dop_tag).anal_dop.peak_dop) );
		    if ( (dop->dop_tag).anal_dop.peak_dop != 0.0 )  {
    	                read_real("\tPeak Doping Location", 
			    &((dop->dop_tag).anal_dop.peak_loc) );
    	                read_real("\tStandard Deviation", 
			    &((dop->dop_tag).anal_dop.std_dev) );
		    }
		    else  {
		      (dop->dop_tag).anal_dop.peak_loc = 0.0;
		      (dop->dop_tag).anal_dop.std_dev = 0.0;
		    }
		    /* take advantage of the fact that gaussian stuff
		     *  is offset by 10 
       		     */
		    dop->type += 10;
	            break;

	        case '2' :
    	            read_real("\tPeak Doping Concentration", 
			&((dop->dop_tag).anal_dop.peak_dop) );
		    if ( (dop->dop_tag).anal_dop.peak_dop != 0.0 )  {
    	                read_real("\tPeak Doping Location", 
			    &((dop->dop_tag).anal_dop.peak_loc) );
	                read_real("\tJunction Depth", junction );
		        (dop->dop_tag).anal_dop.std_dev =
		            ( *junction - (dop->dop_tag).anal_dop.peak_loc)/
	                    sqrt(log(((dop->dop_tag).anal_dop.peak_dop)
		  	    /substratedoping));
		    }
		    else  {
		      (dop->dop_tag).anal_dop.peak_loc = 0.0;
		      (dop->dop_tag).anal_dop.std_dev = 0.0;
		    }
		    /* take advantage of the fact that gaussian stuff
		     *  is offset by 10 
       		     */
		    dop->type += 10;
	            break;

	        case '3' :
	            read_real("\tPeak Doping Concentration",
			&((dop->dop_tag).anal_dop.peak_dop) );
		    if ( (dop->dop_tag).anal_dop.peak_dop != 0.0 )  {
    	                read_real("\tPeak Doping Location",
			    &((dop->dop_tag).anal_dop.peak_loc) );
	                read_real("\tDose", &temp );
	                /* sqrt(pi) = 1.77245 */
		        (dop->dop_tag).anal_dop.std_dev =
	        	    2.0 * 1.0e4 * temp / 
			    ((dop->dop_tag).anal_dop.peak_dop * 1.77245);
		    }
		    else  {
		      (dop->dop_tag).anal_dop.peak_loc = 0.0;
		      (dop->dop_tag).anal_dop.std_dev = 0.0;
		    }
		    /* take advantage of the fact that gaussian stuff
		     *  is offset by 10 
       		     */
		    dop->type += 10;
	            break;
		
		case '4' :
    	            read_real("\tPeak Doping Concentration(0 for none)", 
			&((dop->dop_tag).anal_dop.peak_dop) );
		    if ( (dop->dop_tag).anal_dop.peak_dop != 0.0 )  {
    	                read_real("\tPeak Doping Location", 
			    &((dop->dop_tag).anal_dop.peak_loc) );
    	                read_real("\tStandard Deviation", 
			    &((dop->dop_tag).anal_dop.std_dev) );
		    }
		    else  {
		      (dop->dop_tag).anal_dop.peak_loc = 0.0;
		      (dop->dop_tag).anal_dop.std_dev = 0.0;
		    }

		    /* assume here that junction is point where the
		     *   doping drops off by 2 orders of magnitude from
		     *   the peak
		     */
		    *junction = 1.82 * (dop->dop_tag).anal_dop.std_dev + 
			(dop->dop_tag).anal_dop.peak_loc;

		    /* take advantage of the fact that erfc stuff
		     *  is offset by 12 
       		     */
		    dop->type += 12;
		    break;

		case '5' :
    	            read_real("\tPeak Doping Concentration", 
			&((dop->dop_tag).anal_dop.peak_dop) );
		    if ( (dop->dop_tag).anal_dop.peak_dop != 0.0 )  {
    	                read_real("\tPeak Doping Location", 
			    &((dop->dop_tag).anal_dop.peak_loc) );
	                read_real("\tJunction Depth", junction );
	                read_real("\tConcentration at junction", &temp );

			if ( temp == 0.0 )
		            (dop->dop_tag).anal_dop.std_dev =
				*junction/1.82;
			else  {
			    std_dev = *junction;
			    do  {
				fx = temp/(dop->dop_tag).anal_dop.peak_dop
				    - erfc((*junction-
				    (dop->dop_tag).anal_dop.peak_loc)
				    /std_dev);
				dfx = -(*junction)*1.128379/std_dev/std_dev*
				    exp(-( ( (*junction) -
					(dop->dop_tag).anal_dop.peak_loc) *
					( (*junction) -
					(dop->dop_tag).anal_dop.peak_loc) /
					std_dev/std_dev) );
				if ( fabs( fx/dfx ) > std_dev )
				    fx = -std_dev * dfx;
				std_dev = fabs( std_dev - fx/dfx );
			    }  while ( fabs(fx/dfx) > 1.0e-3 );
			    (dop->dop_tag).anal_dop.std_dev = std_dev;
			}
		    }
		    else  {
		      (dop->dop_tag).anal_dop.peak_loc = 0.0;
		      (dop->dop_tag).anal_dop.std_dev = 0.0;
		    }

		    /* take advantage of the fact that erfc stuff
		     *  is offset by 12 
       		     */
		    dop->type += 12;
		    break;
            } /* end switch */
	    if ( *junction == 0 )
	        calc_junc( (dop->dop_tag).anal_dop.peak_loc, 
		    (dop->dop_tag).anal_dop.peak_dop, 
		    (dop->dop_tag).anal_dop.std_dev, substratedoping, 
		    junction );
	} /* end if doping->type != NONE */
    } /* end if ( dopingmode == 0 ) */
    else  {
	if ( (changetag == tagnumber) && (change == 1) ) {
	    /* flush entry from defaults file */
	    fgets( buf, 512, defaultsfile );
	    sscanf( changebuf, "%1s", &filetype );
	    fgets( buf, 512, changefile );
	    sscanf( buf, "%d %s", &changetag, changebuf );
	}
	else  
	    if (!fileempty)  {
		fgets( buf, 512, defaultsfile );
 	        sscanf(buf, "%1s" , &filetype);	
	    }

	if ( !( (filetype == 'a') || (filetype == 'b') || (filetype == 'e') ) )
		filetype = '0';

	printf( "\tascii[a], binary export[b], ascii export[e], or none[0] [%c]  :  ", filetype );
	fflush( stdout );
	gets( buf );
	i = 0;
	while ( buf[i] == ' ' ) i++;

	if ( buf[i] == '\0' ) buf[i] = filetype;

	switch ( buf[i] )  {
	    case  'a' 	:
				dop->type = SUPREM3ASCII;
				fprintf( tempfile, "a\t-\tsuprem3 file type\t%d\n",
				    tagnumber++ );
				break;

	    case  'b'	:	dop->type = SUPREM3EXPORT;
				exporttype = 0;
				fprintf( tempfile, "b\t-\tsuprem3 file type\t%d\n",
				    tagnumber++ );
				break;

	    case  'e'	:	dop->type = SUPREM3EXPORT;
				exporttype = 1;
				fprintf( tempfile, "e\t-\tsuprem3 file type\t%d\n",
				    tagnumber++ );
				break;

	    case '0'	:	dop->type = NONE;
				fprintf( tempfile, "0\t-\tsuprem3 file type\t%d\n",
				    tagnumber++ );
				break;

	    default	: 	fprintf( stderr, "\tillegal filetype [a,e,0]\n" );
				exit( ERROR );
				break;

	} /* end switch */

	switch ( dop->type )  {
	    case  SUPREM3ASCII  :

	    	if ( (changetag == tagnumber) && (change == 1) ) {
		/* flush entry from defaults file */
		    fgets( buf, 512, defaultsfile );
		    sscanf( changebuf, "%s", defaultfilename );
		    fgets( buf, 512, changefile );
		    sscanf( buf, "%d %s", &changetag, changebuf );
	    	}
	    	else  
	            if (!fileempty)  {
			fgets( buf, 512, defaultsfile );
 	            	sscanf(buf, "%s" , defaultfilename);	
	            }

	        while ( (okfile == NULL) && (tries < 2) )  {
	            printf("\tSuprem3 filename for profile [%s]  :  ", defaultfilename );
	            fflush( stdout );
	            gets( buf );
	            sscanf( buf, "%s", filename );

		    i = 0;
		    while ( buf[i] == ' ' ) i++;

		    j = 0;
		    if ( buf[i] == '\0' )  {
	        	while ( (j < MAXNAMESIZE) && (defaultfilename[j] != '\0') )  {
	            	    filename[j] = defaultfilename[j];
	            	    j++;
	        	}
			filename[j] = '\0';
		    }
/*
		    printf( "\tFile to open is %s\n", filename );
*/
		    okfile = fopen( filename, "r" );
		    if ( okfile == NULL )  {
		        printf("\tfile not found for open. list of files in current directory are :\n");
		        system("ls");
		        printf("try again\n");
		        fflush( stdout );
		    }
		    tries++;
	        }
	        if ( tries >= 2 )  {
		    fprintf( stderr, "\tfile %s not found\n", filename );
		    exit( ERROR );
	        }

	        fprintf( tempfile, "%s\t-\tSuprem3 doping profile filename\t%d\n",
		    filename, tagnumber++ );
	        while ( (i < MAXNAMESIZE) && (filename[i] != '\0') )  {
	            (dop->dop_tag).sup3_dop.filename[i] = filename[i];
	            i++;
	        }
		/* get information for junction and std_dev */
		asciicalc(okfile, junction, &((dop->dop_tag).sup3_dop.std_dev),
			&((dop->dop_tag).sup3_dop.peak_dop),
			&(vert_info.substrate_dop) );
		fclose( okfile );
	        break;  /* case SUP3ASCII */

	    case  SUPREM3EXPORT  :
		done = 0;
		tries = 0;
        	while( (!done) && (tries < 2) ) {
	    	    if ( (changetag == tagnumber) && (change == 1) ) {
		    /* flush entry from defaults file */
		        fgets( buf, 512, defaultsfile );
		        sscanf( changebuf, "%1s", &doping_type );
			fgets( buf, 512, changefile );
			sscanf( buf, "%d %s", &changetag, changebuf );
	    	    }
	    	    else  
	        	if (!fileempty)  {
			    fgets( buf, 512, defaultsfile );
 	            	    sscanf(buf, "%1s" , &doping_type);	
	        	}
	    	    if ( !( (doping_type == 'B') || (doping_type == 'b')
		    	    || (doping_type == 'P') || (doping_type == 'p') 
			    || (doping_type == 'A') || (doping_type == 'a')
			    || (doping_type == 'S') || (doping_type == 's') ) )
 	        	doping_type = 'B';
	    	    printf("\tDoping Species(B,P,A(for As),S(for Sb)? [%c] ", doping_type);
	    	    fflush(stdout);
	    	    gets(buf);

	    	    i = 0;
	    	    while ( buf[i] == ' ' ) i++;

		    if ( buf[i] == '\0' )
			buf[i] = doping_type;

	    	    switch( buf[i] ) {
	    		case 'b' :
	    		case 'B' :
	        		(dop->dop_tag).sup3_dop.type = BORON;
				fprintf( tempfile, "B\t-\tDoping Type\t%d\n",
				    tagnumber++ );
	        		done = 1;
	        		break;

	    		case 'p' :
	    		case 'P' :
	        		(dop->dop_tag).sup3_dop.type = PHOSPHORUS;
				fprintf( tempfile, "P\t-\tDoping Type\t%d\n",
				    tagnumber++ );
	        		done = 1;
	        		break;


	    		case 'a' :
	    		case 'A' :
	        		(dop->dop_tag).sup3_dop.type = ARSENIC;
				fprintf( tempfile, "A\t-\tDoping Type\t%d\n",
				    tagnumber++ );
	        		done = 1;
	        		break;


	    		case 's' :
	    		case 'S' :
	        		(dop->dop_tag).sup3_dop.type = ANTIMONY;
				fprintf( tempfile, "S\t-\tDoping Type\t%d\n",
				    tagnumber++ );
	        		done = 1;
	        		break;

			default  :
				fprintf( stderr, "\tillegal doping type. choose one of [a,b,p,s].  try again.\n" );
				tries++;
				break;
		    } /* end switch buf[i] */
		} /* end while (!done) */
		if ( tries >= 2 )  {
		    fprintf( stderr, "\tillegal doping type. assumed to be boron\n" );
		    (dop->dop_tag).sup3_dop.type = BORON;
		    fprintf( tempfile, "B\t-\tDoping Type\t%d\n",
			tagnumber++ );
		}

	    	if ( (changetag == tagnumber) && (change == 1) ) {
		/* flush entry from defaults file */
		    fgets( buf, 512, defaultsfile );
		    sscanf( changebuf, "%s", defaultfilename );
		    fgets( buf, 512, changefile );
		    sscanf( buf, "%d %s", &changetag, changebuf );
	    	}
	    	else  
	            if (!fileempty)  {
			fgets( buf, 512, defaultsfile );
 	            	sscanf(buf, "%s" , defaultfilename);	
	            }
	        while ( (okfile == NULL) && (tries < 2) )  {
	            printf("\tSuprem3 filename for profile [%s]  :  ", defaultfilename );
	            fflush( stdout );
	            gets( buf );
	            sscanf( buf, "%s", filename );

		    i = 0;
		    while ( buf[i] == ' ' ) i++;

		    j = 0;
		    if ( buf[i] == '\0' )  {
	        	while ( (j < MAXNAMESIZE) && (defaultfilename[j] != '\0') )  {
	            	    filename[j] = defaultfilename[j];
	            	    j++;
	        	}
			filename[j] = '\0';
		    }
/*
		    printf( "\tFile to open is %s\n", filename );
*/
		    okfile = fopen( filename, "r" );
		    if ( okfile == NULL )  {
		        printf("\tfile not found for open. list of files in current directory are :\n");
		        system("ls");
		        printf("try again\n");
		        fflush( stdout );
		    }
		    tries++;
	        }
	        if ( tries >= 2 )  {
		    fprintf( stderr, "\tfile %s not found\n", filename );
		    exit( ERROR );
	        }

	        fprintf( tempfile, "%s\t-\tSuprem3 doping profile filename\n", filename );
	        while ( (i < MAXNAMESIZE) && (filename[i] != '\0') )  {
	            (dop->dop_tag).sup3_dop.filename[i] = filename[i];
	            i++;
	        }
		fclose( okfile );
		/* get information for junction and std_dev */

		if ( exporttype == 1 )  {

		    reads3as( filename, (dop->dop_tag).sup3_dop.type,
		        &((dop->dop_tag).sup3_dop.peak_dop), junction,
		        &((dop->dop_tag).sup3_dop.std_dev), 
		        &(vert_info.substrate_dop),
		        &(vert_info.substrate_dop_type),
		        &(vert_info.substrate_depth));
		}
		else  {

		    reads3_( filename, 
		        &((dop->dop_tag).sup3_dop.type),
		        &((dop->dop_tag).sup3_dop.peak_dop), junction,
		        &((dop->dop_tag).sup3_dop.std_dev), 
		        &(vert_info.substrate_dop),
		        &(vert_info.substrate_dop_type),
		        &(vert_info.substrate_depth) );
		}
	        break;  /* case SUP3EXPORT */

	    case NONE :	break;  /* nothing done but need to exit properly */
		
	    default  :
		fprintf( stderr, "\tdoping type not supported\n" );
		exit( ERROR );
		break;

        } /* end switch dop->type */


    } /* end if-then-else dopingmode == 0 */
} /* subroutine */
