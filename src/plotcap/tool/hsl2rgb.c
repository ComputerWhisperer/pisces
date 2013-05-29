/* Tue Mar  7 15:08:30 PST 1989 (dredge--stanford)
 *
 * "hsl2rgb": Convert HSL color values to RGB values.
 *
 * notes:
 *	o  This is very simple and note quite correct, but...
 * 
 * Original: Michael Eldredge -- Stanford (mar 89)
 */

#include <stdio.h>

#ifdef MAIN
/* 0 == Blue, 120 == Red, 240 == Green */

main()
	{
	float	h, s, l ;
	float	r, g, b ;

	for (;;) {
		if (isatty(0)) {
			printf("   Hue: B=0, R=120, G=240") ;
			printf("   Sat.: 0..1") ;
			printf("   Light: 0..1\n") ;
			printf(" H S L: ") ;
			}
		if (scanf("%f %f %f", &h, &s, &l) <= 0) break ;

		hsl2rgb(h, s, l,   &r, &g, &b) ;

		printf("[%g,%g,%g]\t -> %g %g %g\n",
			h, s, l,
			r, g, b) ;
		}
	}
#endif /*MAIN*/

static float	peg() ;
static float	Fmod() ;
#define Max(x, y)  ((x)>(y)? (x): (y))

/* convert Hue,Saturation,Lightness  -> Red,Green,Blue */
hsl2rgb(h,s,l,  r, g, b)
	float	h, s, l ;	/*in: */
	float*	r ;		/*out: */
	float*	g ;		/*out: */
	float*	b ;		/*out: */
	{

	float	pR, pB, pG ;
	float	light ;
	float	t ;

        h = Fmod(h, (float)360.0) ;

        /* Hue: 0..360 */
        if (h <= 120.0) {               /* between B & R */
                pR = h / 120. ;
                pB = 1.0 - pR ;
		t = Max(pR, pB) ;
		pR /= t ;
		pB /= t ;
                pG = 0.0 ;
                }
        else if (h <= 240.0) {          /* between R & G */
                pG = (h - 120.0) / 120. ;
                pR = 1.0 - pG ;
		t = Max(pG, pR) ;
		pG /= t ;
		pR /= t ;
                pB = 0.0 ;
                }
        else {
                pB = (h - 240.) / 120. ;
                pG = 1.0 - pB ;
		t = Max(pB, pG) ;
		pB /= t ;
		pG /= t ;
                pR = 0.0 ;
		}

	/* Saturation: 0..1 */
	pR *= s ;		/* tune it down by the saturation */
	pG *= s ;
	pB *= s ;

	/* Lightness: 0..0.5..1 */
	/* add white or black according to lightness */
	if (s > 0.0) light = (2.0 * l - 1.0) ;
	else         light = l ;	/* achromatic case */

	pR += light ;
	pG += light ;
	pB += light ;

	/* check range */
	*r = peg(pR, 0.0, 1.0) ;
	*g = peg(pG, 0.0, 1.0) ;
	*b = peg(pB, 0.0, 1.0) ;
	}

static float
peg(val, lo, hi)
	float	val ;
	float	lo, hi ;
	{
	if (val < lo) return lo ;
	if (val > hi) return hi ;
	return val ;
	}

static float
Fmod(num, denom)
    float num, denom;
    {
    float sign = (num > 0.0? 1.0: -1.0) ;
    if (denom == 0.0) return num ;

    if (sign < 0) num = -num ;

    return sign * (num - (int)(num/denom) * denom) ;
    }
