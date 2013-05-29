/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/*	write_stride.c		Version 1.5		*/
/*	Last Modification:	1/29/90 01:40:20		*/




#include <stdio.h>
#include <math.h>
#include "struct.h"

write_stride( name ) 
char *name;
{ 
    FILE *fp;
    register int i;
    int j;
    int type;
    int num_dop_specs;
 
    char *dop_type[2];

    plane_str *pl_ptr;
    area_str *ar_ptr;
    stride_dopstr *str_ptr;

    dop_type[N_TYPE] = "n.type";
    dop_type[P_TYPE] = "p.type";

    if ( (fp = fopen(name,"w")) == NULL ) {
	printf("can't open the output file %s\n", name);
	exit(-1);
    }


    /* line 1 - ludbg, method, iscale 
     *	use ILU-CGS
     */
    fprintf(fp, "0  2  0\t\t!no debug, ILU-CGS, no scale Jacobi of Poisson\n");

    /* line 2 - inp, dtype, ncalc, pcalc 
     *	assume for bipolar that we have 2 carriers.
     *	we want to solve for the majority current carriers
     *    last which means that for npn we solve for holes
     *    first and for pnp we solve for electrons first
     */
    if (NPN)
	fprintf(fp, "1  2  2  1\t!scaled Slotboom, 2 carriers, psi/p/n\n");
    else
	fprintf(fp, "1  2  1  2\t!scaled Slotboom, 2 carriers, psi/n/p\n");


    /* line 3 - isp, iabf, cdvvw0, dalfi
     *	for bipolar use MSP
     *	no block iteration necessary since we are doing 2 carrier
     *	0.0 acceleration factor for bipolar
     *	use recommended over-relazation parameter
     */
    fprintf(fp, "1  -1  0.0  0.0\t!MSP, no block, bipolar defaults\n");


    /* line 4 - igs, ifield
     *	use Gummel-Scharfetter
     *	use field-dependent mobility model based on psi
     */
    fprintf(fp, "1  2\t\t!Gummel-Scharfetter, field dependent mobility(psi)\n");

    /* line 5 - ibgnrw, isrh, iauger
     *	use bandgap narrowing, SRH recombination and Auger recombination
     *	  for bipolar
     */
    fprintf(fp, "1  1  1\t\t!Bandgap Narrowing, SRH, Auger\n");

    /* line 6 - izsum, alter
     *	no zero row-sum error array in incomplete factorization
     *	control amount of matrix diagonal modification
     */
    fprintf(fp, "0  1.0\t\t!no zero-sum error, def. diag. modification\n");

    /* line 7 - mask(x,y,z)
     * 	this is used to control the domain partitioning algorithms allowed
     *	  for use in solving the equations.  by specifying a mask, a
     *    dissection is disabled in a particular direction
     */
    fprintf(fp, "0  0  0\t\t!partitioning allowed in all directions\n");

    /* mesh specification cards.  in this case we use xnd, ynd, and znd
     *	to fill in the appropriate lines.  
     */

    /* line 8 - imesh2, und(1), und(2), und(3), mr
     *	specify that mesh will specified as in PISCES
     *  specify number of mesh cards following in each direction (x,y,z)
     *	speiify the maximum grid spacing ration between two neighboring
     *	  planes
     */
  fprintf(fp, "1  %d  %d  %d  2.0\t!format, #x-planes, #y-planes, #z-planes\n",
	xnd[xnum-1].localnum, ynd[ynum-1].localnum, znd[znum-1].localnum);

    /* x planes */
    fprintf(fp, "%g  %d  %g\t!x-loc, node-x, ratio-x\n", 
	xnd[0].loc, xnd[0].num, xnd[0].rat);
    for(i = 1; i < xnum; i++) 
	fprintf(fp, "%g  %d  %g\t!x-loc, node-x, ratio-x\n", 
		     xnd[i].loc, xnd[i].num - xnd[i-1].num, xnd[i].rat);

    /* y planes */
    fprintf(fp, "%g  %d  %g\t!y-loc, node-y, ratio-y\n", 
	ynd[0].loc, ynd[0].num, ynd[0].rat);
    for(i = 1; i < ynum; i++) 
	fprintf(fp, "%g  %d  %g\t!y-loc, node-y, ratio-y\n", 
		     ynd[i].loc, ynd[i].num - ynd[i-1].num, ynd[i].rat);

    /* z planes */
    fprintf(fp, "%g  %d  %g\t!z-loc, node-z, ratio-z\n", 
	znd[0].loc, znd[0].num, znd[0].rat);
    for(i = 1; i < znum; i++) 
	fprintf(fp, "%g  %d  %g\t!z-loc, node-z, ratio-z\n", 
		     znd[i].loc, znd[i].num - znd[i-1].num, znd[i].rat);


    /* Domain specification */
    /* nregn - number of regions (different materials) */
    fprintf(fp, "%d\t\t!number of regions\n", num_regions);
    for (i = 0; i < num_regions; i++)  {
	fprintf(fp, "%d  %d  %g  %g\t!regtyp, nplane, eps, ni0\n",
	    reg[i].type, reg[i].nplane, reg[i].eps, reg[i].ni0);
	fprintf(fp, "%d %d %d %d %d %d\t!xlo xhi ylo yhi zlo zhi\n",
	    reg[i].xlo, reg[i].xhi, reg[i].ylo, reg[i].yhi,
	    reg[i].zlo, reg[i].zhi);
    }

    /* Interface specification */
    /*   assume for now that interface is flat */
    fprintf(fp, "0\t\t!flat interface\n");

    /* Electrode specification */
    fprintf(fp, "%d\t\t!# of electrodes\n", nelect);

    for (i = 0; i < nelect; i++)  {
	/* elecnt - 1 ohmic contact
	 *	    2 contact with work function difference
	 *
 	 * eletyp - 0 contact is on an insulator
	 * 	    1 contact is on a semiconductor
	 *
	 * eleshp - 0 rectilinear electrode
	 *	    1 sloped electrode specified by set of planes
	 *	    2 volume electrode specified by 2 sets of planes
	 */
	pl_ptr = electrode[i].pln;
	fprintf(fp, "%d %d %d\t\t!elecnt, eletyp, eleshp\n",
	    electrode[i].cnt, electrode[i].typ, electrode[i].shp); 
	if (electrode[i].cnt == 2)
	    fprintf(fp, "%g\t\t!work function\n", electrode[i].work);
	if (electrode[i].shp > 0)  {
	    fprintf(fp, "%d ", electrode[i].dir);
	    for (j = 0; j < electrode[i].shp; j++)
		fprintf(fp, "%d ", pl_ptr[j].num);
	    fprintf(fp,"\n");
	    /* print plane information */
	}
	    fprintf(fp, "%d %d %d %d %d %d\t!xlo,xhi,ylo,yhi,zlo,zhi\n",
		electrode[i].xlo, electrode[i].xhi, electrode[i].ylo, 
		electrode[i].yhi, electrode[i].zlo, electrode[i].zhi);
     }

     /* print out potentials at each electrode */
    for (i = 0; i < nelect; i++)  
	fprintf(fp, "%g\t\t!v(%d)\n", electrode[i].pot, i+1);

    num_dop_specs = 0;
    for (i = 0; i < ndop; i++)  {
	str_ptr = &(dop_data[i].dop_tag.stride_dop);
	num_dop_specs += str_ptr->num_planes;
    }

    /* print out doping information */
    fprintf(fp, "%d\t\t!number of doping specifications\n", num_dop_specs);


    /* assume that we don't use constant doping here */
    for (i = 0; i < ndop; i++)  {
	str_ptr = &(dop_data[i].dop_tag.stride_dop);
	for (j = 0; j < str_ptr->num_planes; j++)  {
	  if (  (str_ptr->xhi > str_ptr->xlo) &&
		(str_ptr->yhi > str_ptr->ylo) &&
		(str_ptr->zhi >= str_ptr->zlo) )  {
	    if ( (dop_data[i].type == N_TYPE)  
		|| (dop_data[i].type == N_GAUSSIAN)
		|| (dop_data[i].type == N_ERFC) ) 
		    fprintf(fp, "2 %g\t\t!idope, conc\n", str_ptr->peak_dop);
	    else
		fprintf(fp, "2 %g\t\t!idope, conc\n", -(str_ptr->peak_dop));
	    if (j == 0)  {
	        fprintf(fp, "%g %g %g %g %g %g\t!xlo,xhi,ylo,yhi,zlo,zhi\n",
	            str_ptr->xlo, str_ptr->xhi, 
	            str_ptr->ylo, str_ptr->yhi,
	            str_ptr->zlo, str_ptr->zhi);
	    }
	    else  {
		ar_ptr = str_ptr->planes[j-1];
	        fprintf(fp, "%g %g %g %g %g %g\t!xlo,xhi,ylo,yhi,zlo,zhi\n",
	            ar_ptr->xlo, ar_ptr->xhi, 
	            ar_ptr->ylo, ar_ptr->yhi,
	            ar_ptr->zlo, ar_ptr->zhi);
	    }
	    fprintf(fp, "%g %g %g\t!xdev,ydev,zdev\n",
	        str_ptr->xdev, str_ptr->ydev, str_ptr->zdev);
	    fprintf(fp, "%d %d %d\t\t\t!ierfcx, ierfcy, ierfcz\n",
	        str_ptr->ierfcx, str_ptr->ierfcy, str_ptr->ierfcz);
	  } /* end if str_ptr && */
	} /* end for */
    }

    /* Output and Electrical Calculation specifications */
    /* nout - set to 0 since not used */
    fprintf(fp, "0\t\t!nout\n");


    /* nbias */ 
    fprintf(fp, "1\t\t!nbias\n"); 
    fprintf(fp, "%d\t\t!bias steps\n", 
	(int)ceil((double)(maxbase - minbase)/baseincrement));

    /* bias steps  */
    fprintf(fp, "0.0 %g 0.0\t!bias increments\n", baseincrement);

    /* ivth - not used for bipolar */
    fprintf(fp, "0\t\t!ivth\n");

    /* ncur - not used for bipolar */
    fprintf(fp, "0\t\t!ncur\n");

    /* hout - output variable to host */
    /* 	1 - concentration	*/
    /*	2 - quasi-fermi levels	*/
    fprintf(fp, "1\t\t!hout\n");

    /* hord - order of output data  */
    /*	1 - x */
    /*	2 - y */
    /*	3 - z */
    fprintf(fp, "3 1 2\t\t!z-varies,x-varies,y-varies\n");

    fclose( fp );
}
