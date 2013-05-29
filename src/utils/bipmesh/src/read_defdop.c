/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/*	read_defdop.c		Version 1.3		*/
/*	Last Modification:	1/24/90 10:34:22		*/




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
extern read_defreal();
extern calc_junc();
extern asciicalc();
extern fileempty;
extern tagnumber;
extern exporttype;
extern reads3as(); /* reads in Suprem3 export format (ascii) */
extern vert_str vert_info;
extern reads3_();  /* reads in Suprem3 export format (binary) */ 


read_defdop( dop, substratedoping, typeofprofile, junction )
dop_str *dop;
double substratedoping;
int typeofprofile; /* 0 for analytic, 1	 for Suprem3 file */
double *junction; /* junction depth */
{
    char buf[512];
    char filename[MAXNAMESIZE];
    char doping_type = 'P';
    double temp;
    double std_dev;
    double fx;
    double dfx;
    char filetype = '0'; /* 'a' for ascii, 'e' for export , '0' for none */
    int i;
    FILE *okfile = NULL; /* NULL if doping information file cannot be opened */

    if ( fileempty )  {
	printf( "\tillegal file on open. abort.\n" );
	exit( ERROR );
    }

    if ( typeofprofile == 0 )  {
	if ( (changetag == tagnumber) && (change == 1) ) {
	    /* flush entry from defaults file */
	    fgets( buf, 512, defaultsfile );
	    sscanf( changebuf, "%1s", &doping_type );
	    fgets( buf, 512, changefile );
	    sscanf( buf, "%d %s", &changetag, changebuf );
	}
	else  {
	    fgets( buf, 512, defaultsfile );
 	    sscanf(buf, "%1s" , &doping_type);	
	}
	if ( !( (doping_type == 'P') || (doping_type == 'p')
		|| (doping_type == 'N') || (doping_type == 'n') ) )
 	    doping_type = '0';

	switch( doping_type ) {
		case 'n' :
	    	case 'N' :
	        	dop->type = N_TYPE;
    	        	fprintf( tempfile, "N\t-\tDoping Type\t%d\n",
			    tagnumber++ );
	        	break;

	    	case 'p' :
	    	case 'P' :
	        	dop->type = P_TYPE;
    	        	fprintf( tempfile, "P\t-\tDoping Type\t%d\n",
			    tagnumber++ );
	        	break;

	    	case '0' :
			dop->type = NONE;
			fprintf( tempfile, "0\t-\tDoping Type\t%d\n",
			    tagnumber++ );
			break;

	    	default:
			fprintf( stderr, "\tillegal doping type [0,n,p]\n" );
			break;
	} /* end switch */

	if ( dop->type != NONE )  {
	    doping_type = '0';
	    if ( (changetag == tagnumber) && (change == 1) ) {
		/* flush entry from defaults file */
		fgets( buf, 512, defaultsfile );
		sscanf( changebuf, "%1s", &doping_type );
		fgets( buf, 512, changefile );
		sscanf( buf, "%d %s", &changetag, changebuf );
	    }
	    else {
		fgets( buf, 512, defaultsfile );
 	        sscanf(buf, "%1s" , &doping_type);	
	    }
	    if (! ( (doping_type=='1') || (doping_type=='2') || (doping_type=='3') ) )
	        doping_type = '2';
	    fprintf( tempfile, "%c\t-\tProfile Type\t%d\n", doping_type,
		tagnumber++ );

            switch( doping_type ) {
	        case '1' :
    	            read_defreal("\tPeak Doping Concentration(0 for none)", 
			&((dop->dop_tag).anal_dop.peak_dop) );
		    if ( (dop->dop_tag).anal_dop.peak_dop != 0.0 )  {
    	                read_defreal("\tPeak Doping Location", 
			    &((dop->dop_tag).anal_dop.peak_loc) );
    	                read_defreal("\tStandard Deviation", 
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
    	            read_defreal("\tPeak Doping Concentration", 
			&((dop->dop_tag).anal_dop.peak_dop) );
		    if ( (dop->dop_tag).anal_dop.peak_dop != 0.0 )  {
    	                read_defreal("\tPeak Doping Location", 
			    &((dop->dop_tag).anal_dop.peak_loc) );
	                read_defreal("\tJunction Depth", junction );
		        (dop->dop_tag).anal_dop.std_dev =
		            ( *junction - (dop->dop_tag).anal_dop.peak_loc)/
	                    sqrt(log(((dop->dop_tag).anal_dop.peak_dop)/substratedoping));
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
	            read_defreal("\tPeak Doping Concentration",
			&((dop->dop_tag).anal_dop.peak_dop) );
		    if ( (dop->dop_tag).anal_dop.peak_dop != 0.0 )  {
    	                read_defreal("\tPeak Doping Location",
			    &((dop->dop_tag).anal_dop.peak_loc) );
	                read_defreal("\tDose", &temp );
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
    	            read_defreal("\tPeak Doping Concentration(0 for none)", 
			&((dop->dop_tag).anal_dop.peak_dop) );
		    if ( (dop->dop_tag).anal_dop.peak_dop != 0.0 )  {
    	                read_defreal("\tPeak Doping Location", 
			    &((dop->dop_tag).anal_dop.peak_loc) );
    	                read_defreal("\tStandard Deviation", 
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
    	            read_defreal("\tPeak Doping Concentration", 
			&((dop->dop_tag).anal_dop.peak_dop) );
		    if ( (dop->dop_tag).anal_dop.peak_dop != 0.0 )  {
    	                read_defreal("\tPeak Doping Location", 
			    &((dop->dop_tag).anal_dop.peak_loc) );
	                read_defreal("\tJunction Depth", junction );
	                read_defreal("\tConcentration at junction", &temp );

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
    } /* end if ( typeofprofile == 0 ) */
    else  {
	if ( (changetag == tagnumber) && (change == 1) ) {
	    /* flush entry from defaults file */
	    fgets( buf, 512, defaultsfile );
	    sscanf( changebuf, "%1s", &filetype );
	    fgets( buf, 512, changefile );
	    sscanf( buf, "%d %s", &changetag, changebuf );
	}
	else { 
	     fgets( buf, 512, defaultsfile );
             sscanf(buf, "%1s" , &filetype);	
	}
	if ( !( (filetype == 'a') || (filetype == 'b') || (filetype == 'e') ) )
	    filetype = '0';
	fprintf( tempfile, "%c\t-\tsuprem3 file type\t%d\n", filetype,
	    tagnumber++ );

	switch ( filetype )  {
	    case  'a' 	:
				dop->type = SUPREM3ASCII;
				break;

	    case  'b'	:	dop->type = SUPREM3EXPORT;
				exporttype = 0;
				break;

	    case  'e'	:	dop->type = SUPREM3EXPORT;
				exporttype = 1;
				break;

	    case '0'	:	dop->type = NONE;
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
		    sscanf( changebuf, "%s", filename );
		    fgets( buf, 512, changefile );
		    sscanf( buf, "%d %s", &changetag, changebuf );
	    	}
	    	else  {
		    fgets( buf, 512, defaultsfile );
 	            sscanf(buf, "%s" , filename);	
	        }
		okfile = fopen( filename, "r" );
		if ( okfile == NULL )  {
		    fprintf(stderr, "\tfile not found for open.\n" );
		    exit( ERROR );
		}

	        fprintf( tempfile, "%s\t-\tSuprem3 doping profile filename\t%d\n",
		    filename, tagnumber++ );
		i = 0;
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
	            if ( (changetag == tagnumber) && (change == 1) ) {
		        /* flush entry from defaults file */
		        fgets( buf, 512, defaultsfile );
		        sscanf( changebuf, "%1s", &doping_type );
		        fgets( buf, 512, changefile );
		        sscanf( buf, "%d %s", &changetag, changebuf );
	            }
	            else  {
		        fgets( buf, 512, defaultsfile );
 	                sscanf(buf, "%1s" , &doping_type);	
	            }
	    	    if ( !( (doping_type == 'B') || (doping_type == 'b')
		    	    || (doping_type == 'P') || (doping_type == 'p') 
			    || (doping_type == 'A') || (doping_type == 'a')
			    || (doping_type == 'S') || (doping_type == 's') ) )
 	        	doping_type = 'B';

	    	    switch( doping_type ) {
	    		case 'b' :
	    		case 'B' :
	        		(dop->dop_tag).sup3_dop.type = BORON;
				fprintf( tempfile, 
				    "B\t-\tDoping Type\t%d\n" , tagnumber++ );
	        		break;

	    		case 'p' :
	    		case 'P' :
	        		(dop->dop_tag).sup3_dop.type = PHOSPHORUS;
				fprintf( tempfile, 
				    "P\t-\tDoping Type\t%d\n", tagnumber++ );
	        		break;


	    		case 'a' :
	    		case 'A' :
	        		(dop->dop_tag).sup3_dop.type = ARSENIC;
				fprintf( tempfile, 
				    "A\t-\tDoping Type\t%d\n", tagnumber++ );
	        		break;


	    		case 's' :
	    		case 'S' :
	        		(dop->dop_tag).sup3_dop.type = ANTIMONY;
				fprintf( tempfile, 
				    "S\t-\tDoping Type\t%d\n", tagnumber++ );
	        		break;

			default  :
				fprintf( stderr, "\tillegal doping type.\n" );
				exit( ERROR );
				break;
		    } /* end switch doping_type */

	    	if ( (changetag == tagnumber) && (change == 1) ) {
		/* flush entry from defaults file */
		    fgets( buf, 512, defaultsfile );
		    sscanf( changebuf, "%s", filename );
		    fgets( buf, 512, changefile );
		    sscanf( buf, "%d %s", &changetag, changebuf );
	    	}
	    	else  {
		    fgets( buf, 512, defaultsfile );
 	            sscanf(buf, "%s" , filename);	
	        }
		okfile = fopen( filename, "r" );
		if ( okfile == NULL )  {
		    fprintf(stderr, "\tfile not found for open.\n" );
		    exit( ERROR );
		}

	        fprintf( tempfile, "%s\t-\tSuprem3 doping profile filename\t%d\n", 
		    filename, tagnumber++ );
		i = 0;
	        while ( (i < MAXNAMESIZE) && (filename[i] != '\0') )  {
	            (dop->dop_tag).sup3_dop.filename[i] = filename[i];
	            i++;
	        }
		fclose( okfile );
		/* get information for junction and std_dev */

		if ( exporttype == 1 )  {

		    reads3as( filename,
		    	(dop->dop_tag).sup3_dop.type,
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
			&(vert_info.substrate_dop_type) ,
			&(vert_info.substrate_depth) );
		}
	        break;  /* case SUP3EXPORT */

	    case NONE  :  break;  /* need to exit properly */

	    default  :
		fprintf( stderr, "\tdoping type not supported\n" );
		exit( ERROR );
		break;

        } /* end switch dop->type */


    } /* end if-then-else dopingmode == 0 */
} /* subroutine */
