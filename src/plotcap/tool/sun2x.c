/* date: 11 dec 86 (mje)
 *
 * "sun2x.c": convert a sun icon image to an X icon/bitmap image.
 *
 * This very simply swap the bits in each short integer.  It is not written
 *	for speed, just simplicity.
 *
 * Usage:  <in the make file>
 *	x_prog_w_icon: NAME.icon
 *	NAME.icon: SUN.icon
 *	cc -DNEW_NAME=\"NAME\"
 *	a.out > NAME.icon
 *	rm a.out
 *
 * written:  Michael Eldredge (dec 86)
 */

#include <stdio.h>

/* We need to change Sunview bits... */
/* Load in the sun version (assuming 64x64) and convert to new ... */

static short x_bits[] = {
#	include "sunview.icon"
	} ;

#define	SIZE	(sizeof(x_bits)/sizeof(short))

short	x_new[SIZE] ;


main() {
	short	t ;
	int	s, i ;
	int	tmp ;
	
	/* foreach source bit... */
	for (s = 0 ; s < SIZE; s++) {
		t = 0 ;

		for (i = 0; i < 16; i++)
			if (x_bits[s] & (1<<i)) t |= (1 << (15-i)) ;

		x_new[s] = t ;
		}

	/* Write out the new X formatted font file */
	printf("#define %s_width  64\n", NEW_NAME) ;
	printf("#define %s_height 64\n", NEW_NAME) ;
	printf("static short %s_bits[] = {\n", NEW_NAME) ;

	for (s = 0; s < SIZE; s++) {
		if (s % 8 == 0) printf("\t") ;
		tmp = x_new[s] & 0xffff ;
		printf("0x%04x", tmp) ;
		if (s != SIZE-1) printf(",") ;
		if (s % 8 == 7)  printf("\n") ;
		}

	printf("};\n") ;
	}
