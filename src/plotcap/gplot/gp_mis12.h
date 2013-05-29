/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* date: 07 may 86 (mje)
 *
 * definitions etc. for (old) alu12.
 */

/* Some entries are really commands to the ALU.  These are all < 0 */
#define ALU_NULL	(p_ent)0
#define ALU_NOENT	(p_ent)-1
#define	ALU_RESET	(p_ent)-2
#define ALU_POST	(p_ent)-3
#define ALU_INIT	(p_ent)-4
#define NOENT		ALU_NOENT		/* Some old folks call it ~ */

/* Command to alu_mkX routines to free the space that they malloc'd */
#define	FREE_IT	 0

/* al_poki commands. (Things we may like to poke) */ 
#define AP_LUPTO 1		/* Put to output device */ 
#define AP_LUGTO 2		/* Get from output -- Probably never used */ 
#define AP_LUPTI 3		/* Put to input device */
#define AP_LUGTI 4		/* Get from output device */

/* al_mkb commands.  Which buffer to malloc up. */
#define	MKB_IBUF  1
#define MKB_OBUF  2
#define MKB_IDAT  3

/* Function decl's */
char	*alu_mkb() ;
int	*alu_mkp() ;
