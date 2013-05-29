

static char SccsID[] = "@(#)vector.c	1.1\t4/25/89";

/*************************************************************************
 *									 *
 *     Copyright c 1984 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the SUPREM4 computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/
/*   vector.c                Version 3.13     */
/*   Last Modification : 12/2/88  15:38:29 */

#include <stdio.h>
#include <math.h>
extern double erf(), erfc();
#include <ctype.h>
#include "global.h"
#include "constant.h"
#include "geom.h"
#include "expr.h"
#include "material.h"
#include "impurity.h"
#include "diffuse.h"




/************************************************************************
 *									*
 *	vfunc( val, type ) - This routine sets up a vector array	*
 *  based on the vector function specified.  val contains the input 	*
 *  array as well as the output.					*
 *									*
 *  Original:	MEL	8/85						*
 *									*
 ************************************************************************/
char *vfunc( val, type, expr )
float *val;
int type;
struct vec_str *expr;	/*argument of the vector function*/
{
    register int i, j, imp, s;
    register int ploc;
    double tmp[MAXIMP];
    double c, cmax, cabs;
    double *sol[MAXIMP], *act[MAXIMP], *equ[MAXIMP], *foo[MAXIMP];
    double net[MAXPNT], noni[MAXPNT];

    switch( type ) {

    case ACTIVE:

	if ( expr->type != SOLVAL ) 
	    return("Active can only be taken of an impurity value");

	imp = expr->value.ival;
	s = imptosol[imp];

	if (last_temp == 0.0) 
	    return("The value is not available unless a temperature has been given\n" );

	/*first allocate and initialize the arrays*/
	init_pseudo( last_temp );
	comp_mat( last_temp );
	for(j = 0; j < n_imp; j++) {
	    sol[j] =(double *)malloc( sizeof(double) * (nn + 1) );
	    act[j] =(double *)malloc( sizeof(double) * (nn + 1) );
	    foo[j] =(double *)malloc( sizeof(double) * (nn + 1) );
	    equ[j] =(double *)malloc( sizeof(double) * (nn + 1) );
	    for(i = 0; i < nn; i++) sol[j][i] = nd[i]->sol[j];
	}

	/*compute the active concentration at each node*/
	get_active( TRUE, last_temp, sol, net, foo, act, equ, foo );

	/*put in the solution variable*/
	for(i = 0; i < nn; i++) val[i] = act[s][i];

	/*free stuff up*/
	for(j = 0; j < n_imp; j++) {
	    free( sol[j] );
	    free( act[j] );
	    free( foo[j] );
	    free( equ[j] );
	}
	break;

    case SCALE:
	/*find the max and scale by it*/
	for(i = 0, cmax = 0.0; i < nn; i++) {
	    cabs = val[i]; if (cabs < 0) cabs *= -1;
	    if ( cabs > cmax ) cmax = cabs;
	}
	if (cmax != 0)
	    for(i = 0; i < nn; i++) val[i] /= cmax;
	break;

    case GRADX:
    case GRADY:
	RoughGrad( type, val);
	break;
	
    default:
	fprintf(stderr, "Internal error: unknown vector function %d\n", type);
	break;
    }
    return( NULL );
}




/************************************************************************
 *									*
 *	constants( str, tok ) - This routine parses the string in str	*
 *  to see if it matches any of the constants defined for vector 	*
 *  expressions and real number parsing.				*
 *									*
 *  Original:	MEL	2/85						*
 *									*
 ************************************************************************/
constants( str, tok )
char *str;
struct tok_str *tok;
{
    
    /*sort of a mass case statement*/
    if ( ! strcmp( str, "Kb" ) ) {
	tok->type = RCONST;		tok->value.dval = 8.62e-5;
	return(0);
    }
    return( -1 );
}
    

/************************************************************************
 *									*
 *	sol_values( s, tok ) - This routine checks the string in s for 	*
 *  a match with one of the solution values defined.			*
 *									*
 *  Original:	MEL	2/85						*
 *									*
 ************************************************************************/
sol_values( s, tok )
char *s;
struct tok_str *tok;
{
    int len;

    if (substring( "arsenic", s)) {
	tok->type = SOLVAL;
	tok->value.ival = As;
	return(0);
    }
    else if (substring( "boron", s)) {
	tok->type = SOLVAL;
	tok->value.ival = B;
	return(0);
    }
    else if (substring( "antimony", s)) {
	tok->type = SOLVAL;
	tok->value.ival = Sb;
	return(0);
    }
    else if (substring( "phosphorus", s)) {
	tok->type = SOLVAL;
	tok->value.ival = P;
	return(0);
    }
    else if (substring("interstitial", s)) {
	tok->type = SOLVAL;
	tok->value.ival = I;
	return(0);
    }
    else if (substring( "vacancy", s)) {
	tok->type = SOLVAL;
	tok->value.ival = V;
	return(0);
    }
    else if (substring( "time", s)) {
	tok->type = SOLVAL;
	tok->value.ival = TIM;
	return(0);
    }
    else if (substring( "x", s)){
	tok->type = SOLVAL;
	tok->value.ival = X;
	return(0);
    }
    else if (substring( "y", s)){
	tok->type = SOLVAL;
	tok->value.ival = Y;
	return(0);
    }
    else if (substring( "oxygen", s)){
	tok->type = SOLVAL;
	tok->value.ival = OXY;
	return(0);
    }
    else if (substring( "trap", s)){
	tok->type = SOLVAL;
	tok->value.ival = T;
	return(0);
    }
    else if (substring( "gold", s)){
	tok->type = SOLVAL;
	tok->value.ival = Au;
	return(0);
    }
    else if (substring( "Sxx", s)){
	tok->type = SOLVAL;
	tok->value.ival = Sxx;
	return(0);
    }
    else if (substring( "Syy", s)){
	tok->type = SOLVAL;
	tok->value.ival = Syy;
	return(0);
    }
    else if (substring( "Sxy", s)){
	tok->type = SOLVAL;
	tok->value.ival = Sxy;
	return(0);
    }
    else if (substring( "x.veloc", s)|| (!strcmp( "v.x", s)) || (!strcmp( "xv", s)) || (!strcmp( "vx", s))) {
	tok->type = SOLVAL;
	tok->value.ival = XVEL;
	return(0);
    }
    else if (substring( "y.veloc", s)|| (!strcmp( "v.y", s)) || (!strcmp( "yv", s)) || (!strcmp( "vy", s))) {
	tok->type = SOLVAL;
	tok->value.ival = YVEL;
	return(0);
    }
    else if (substring( "psi", s)){
	tok->type = SOLVAL;
	tok->value.ival = Psi;
	return(0);
    }
    else if (substring( "doping", s)){
	tok->type = SOLVAL;
	tok->value.ival = DOP;
	return(0);
    }
    else if (substring( "electrons", s)){
	tok->type = SOLVAL;
	tok->value.ival = ELEC;
	return(0);
    }
    else if (substring( "ni", s)){
	tok->type = SOLVAL;
	tok->value.ival = ELI;
	return(0);
    }
    else if (substring( "ci.star", s)){
	tok->type = SOLVAL;
	tok->value.ival = CIS;
	return(0);
    }
    else if (substring( "cv.star", s)){
	tok->type = SOLVAL;
	tok->value.ival = CVS;
	return(0);
    }
    else if (substring( "cesium", s)){
	tok->type = SOLVAL;
	tok->value.ival = Cs;
	return(0);
    }
    
    return( -1 );
}




/************************************************************************
 *									*
 *	vec_func( s, tok ) - this routine checks to see if the input is	*
 *  a vector function name.						*
 *									*
 *  Original:	MEL	2/86						*
 *									*
 ************************************************************************/
vec_func( s, tok )
char *s;
struct tok_str *tok;
{
    int len;

    if (substring( "active", s)){
	tok->type = VFN;
	tok->value.ival = ACTIVE;
	return(0);
    }
    else if (substring( "scale", s)){
	tok->type = VFN;
	tok->value.ival = SCALE;
	return(0);
    }
    else if (substring( "gradx", s)){
	tok->type = VFN;
	tok->value.ival = GRADX;
	return(0);
    }
    else if (substring( "grady", s)){
	tok->type = VFN;
	tok->value.ival = GRADY;
	return(0);
    }

    return( -1 );
}

/*-----------------RoughGrad--------------------------------------------
 * Take the gradient of a vector valued function in the most obvious
 * way. Not an accurate procedure!
 *----------------------------------------------------------------------*/
#define XC 0
#define YC 1

RoughGrad( which, vector)
    int which;
    float *vector;
{
    int i, ie, j, *local;
    float *accum, *weight, v0, v1, v2, *p0, *p1, *p2, denom, gradx, grady;

    /* Initialize averaging */
    weight = salloc( float, nn);
    accum = salloc( float, nn);
    for (i = 0; i < nn; i++) weight[i] = 0;
    for (i = 0; i < nn; i++) accum[i] = 0;

    /* Loop on triangles computing gradients */
    for (ie = 0; ie < ne; ie++) {
	local = tri[ie]->nd;

	/* Compute the unique value of the triangle gradient */
	v0 = vector[ local[0]];
	v1 = vector[ local[1]];
	v2 = vector[ local[2]];
	p0 = pt[ nd[ local[0]]->pt]->cord;
	p1 = pt[ nd[ local[1]]->pt]->cord;
	p2 = pt[ nd[ local[2]]->pt]->cord;
	denom = (p1[XC]-p0[XC])*(p2[YC]-p0[YC]) - (p1[YC]-p0[YC])*(p2[XC]-p0[XC]);
	if (denom <= 0) {
	    fprintf( stderr, "Negative area triangle! (area[%d] =  %g)\n", ie, denom);
	    continue;
	}
	gradx = -( v2*(p1[YC]-p0[YC]) + v1*(p0[YC]-p2[YC]) + v0*(p2[YC]-p1[YC]));
	grady =  ( v2*(p1[XC]-p0[XC]) + v1*(p0[XC]-p2[XC]) + v0*(p2[XC]-p1[XC]));
	

	/* Add it to each node, weighted by 1/area */
	for (j = 0; j < 3; j++) {
	    float wt = 1/denom;
	    accum[ local[j]] += wt* ((which==GRADX)? gradx : grady) / denom;
	    weight[ local[ j]] += wt;
	}
    }

    /* Average nodes */
    for (i = 0; i < nn; i++)
	if (weight[ i] != 0)
	    vector[ i] = accum[i] / weight[i];
        else
	    vector[ i] = 0;

    free( weight);
    free( accum);
}


	
    
    
