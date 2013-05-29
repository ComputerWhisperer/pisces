/* Fri Aug 12 17:44:51 PDT 1988 (dredge--stanford)
 *
 * instruction definitions etc. for alu18.
 *
 * instruction format:
 *	|c |wr| ir              | mod |       reg
 *	 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0 
 *	         (7)         (1)  (2)        (6)
 *
 * written: 
 * mod: 28 mar 85 (dredge)	added floating point stuff.
 * mod: 25 nov 85 (dredge)	Converted to alu18.h (from alu12.h)
 * mod: 09 may 86 (dredge)	Float support & Use with plotcap v.3
 */

/* -------- Stuff that can easily be changed by the user --------------- */

/* We want integers and floats to be the same size, so define types where
 *   this is true.	We also need to know how big (in bytes) these
 *   types are (RAW_IBYTES).
 */
#define Int	int
#define Float	float
#define RAWI_BYTES	4

#define MAXJSS	10		/* Size of subr jump table */
#define MAXRET   8		/* Size of subr retn point stack */

/* -------- End of easy to change stuff -------------------------------- */

#define JSS1    1		/* Offset in prog of 1st jss entry */
#define JSSN    JSS1 + MAXJSS	/*  and of the last */
#define NOJUMP  0		/* Since MAXJSS is alway > 0, use 0 */



/* how to make intructions, POS values are number of bits to slide up
 */
#define CRPOS 15
#define WRPOS 14
#define IRPOS  8
#define MRPOS  6
#define RRPOS  0

#define GTCR  0100000
#define GTWR  0040000
#define GTIR  0037400
#define GTMR  0000300
#define GTRR  0000077


/* Entry will be of type "p_ent" */
#define	p_ent	Int

/* Some entries are really commands to the ALU.  These are all < 0 */
#define ALU_NULL	(p_ent)0
#define ALU_NOENT	(p_ent)-1
#define	ALU_RESET	(p_ent)-2
#define ALU_POST	(p_ent)-3
#define ALU_INIT	(p_ent)-4


/* and those for breaking apart words into the instruction, etc. */
#define gti(W)		((W) & GTIR)
#define gtw(W)		((W) & GTWR)
#define gtm(W)		((W) & GTMR)
#define gtr(W)		((W) & GTRR)
#define gtc(W)		((W) & GTCR)



/* ir: instructions.  Types are at thresholds, ie: highest ir of some type */
#define NOP	( 0 << IRPOS)
#define JSS	( 1 << IRPOS)
#define NEG	( 2 << IRPOS)
#define	ITR	( 3 << IRPOS)
#define	KPE	( 4 << IRPOS)
#define TST	( 5 << IRPOS)
#define END	( 9 << IRPOS)
#define IT_ONE	END		/* TYPE: One word instructions */

#define LOD	(10 << IRPOS)
#define ADD	(11 << IRPOS)
#define SUB	(12 << IRPOS)
#define MUL	(13 << IRPOS)
#define DIV	(14 << IRPOS)
#define IOR	(15 << IRPOS)
#define AND	(16 << IRPOS)
#define EOR	(17 << IRPOS)
#define SFL	(18 << IRPOS)
#define SFR	(19 << IRPOS)
#define RBT	(20 << IRPOS)
#define FLOD	(21 << IRPOS)
#define IT_NORM FLOD		/* TYPE: Normal usage instructions */

#define MVO	(30 << IRPOS)
#define MVI	(31 << IRPOS)
#define FMVO	(32 << IRPOS)
#define IT_IO   FMVO		/* TYPE: I/O instructructions */

/* NOTE: NONE OF THESE ARE USED... */
#define FADD	(42 << IRPOS)
#define FSUB	(43 << IRPOS)
#define FMUL	(44 << IRPOS)
#define FDIV	(45 << IRPOS)
#define FNEG	(46 << IRPOS)
#define CVFI	(48 << IRPOS)		/* convert float to int */
#define CVIF	(49 << IRPOS)		/* convert int to float */
#define IT_FLT	CVIF		/* TYPE: Floating point instructions */
/* end NOTE */

#define STR	(50 << IRPOS)
#define SPF	(51 << IRPOS)
#define	AST	(52 << IRPOS)
#define CPO	(53 << IRPOS)
#define BCC	(54 << IRPOS)
#define FSTR	(55 << IRPOS)
#define IT_MISC BCC		/* TYPE: MISC all others */
/* ----------------------------------------------------------------- */


/* rr: source/destination registers or variables (Rn, I, J, etc...) */
#define R0	( 0 << RRPOS)
#define R1	( 1 << RRPOS)
#define R2	( 2 << RRPOS)
#define R3	( 3 << RRPOS)
#define R4	( 4 << RRPOS)
#define R5	( 5 << RRPOS)
#define R6	( 6 << RRPOS)
#define R7	( 7 << RRPOS)
#define TA	( 8 << RRPOS)
#define TB	( 9 << RRPOS)
#define MAXREGS  10

#define PSUEDO_REGS (16 << RRPOS)	/* psuedo registers above this */
#define OB	(16 << RRPOS)		/* Output buffer pointer */
#define BC	(17 << RRPOS)		/* Byte counter */

#define IVAR	( 0 << RRPOS)
#define JVAR	( 1 << RRPOS)
#define CVAR	( 2 << RRPOS)
#define SVAR	( 3 << RRPOS)
#define MAXIVARS  4

#define	XVAR	( 0 << RRPOS)
#define	YVAR	( 1 << RRPOS)
#define MAXFVARS  2


/* wr: working registers. (Same as TA & TB, but for something else) */
#define WA	(00 << WRPOS)
#define WB	(01 << WRPOS)

/* mr: source mode. */
#define REG	(00 << MRPOS)
#define VAR	(01 << MRPOS)
#define IND	(02 << MRPOS)
#define DAT	(03 << MRPOS)

/* I/O instructions are a little different than the others.
 *	The mode is whether BINary, ASCii, and via an indirect register.
 *	The register is:
 *	        format|  reg
 *	       5  4  3  2  1  0
 */

/* special for io instructions. register/binary-format */
#define gtior(W)	((W) & 0000007)
#define gtiof(W)	((W) & 0000070)
#define ftob(F)		((F) >> 3)

#define mkf(F)		((F) << 3)

/* mr: for io */
#define BIN	(00 << MRPOS)
#define ASC	(01 << MRPOS)
#define IIO	(02 << MRPOS)		/* Indirection bit */

/* rr: for io -- same R0 - R7 */

/* fr: for io */
#define RAWF	mkf(0)
#define BYTE	mkf(1)
#define WORD	mkf(2)
#define LONG	mkf(4)

/* CPO (and some other) instruction(s) have some peculiarities */
/*  If data is small enough to fit into the register field put it
 *    in the register field of the instruction   (MODE = REG)
 *    else we must put it in the following word  (MODE = DAT)
 */
#define IS_SMALL(D)	(D <= GTRR)

/* JSS's use the working reg field to indicate JSS.@ for JSS.* */
#define JSS_STAR	WB

/* Condition codes */
#define CC_GT	01
#define CC_EQ	02
#define	CC_LT	04

/* Macro to decide what type of instruction we have */
#define irtype(IR) \
	( IR <= IT_ONE  ? IT_ONE  : \
	( IR <= IT_NORM ? IT_NORM : \
	( IR <= IT_IO   ? IT_IO   : \
	( IR <= IT_FLT  ? IT_FLT  : \
		          IT_MISC ))))

/* Convert integers to floating point, and the other way too */
#define F_I(I)	*((Float *)(&(I)))
#define I_F(F)	*((Int	*)(&(F)))

/* Header word fields */
#define STAB_LOCAL   0x0100
#define STAB_FUNCS   0x0200
#define HEAD_TRACE   0x0400

