/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/*
 * Mon Sep 18 11:43:34 PDT 1989 (dredge--stanford)
 * 
 * "gp_com.h" : common variables for gplot software. These are the
 *              external definitions.
 *
 * written:  Michael Eldredge  (may 84)
 * modified: Michael Eldredge  (mar 85)
 * modified: Michael Eldredge  (may 86) Added REVCO support.
 * modified: MJE (dec 86) Added NLINE, NFILS, NPEN ints. & g_{pen,fil}typ
 * modified: Michael Eldredge  (jan 87) Made CLP.. floats (from ints)
 * modified: MJE (feb 87) moved Plast from gp_work() to here.
 * modified: MJE (feb 87) added caps PAUS.
 */

#ifndef REV_CODE
/* someone made a mistake! (check the makefile) */
#define REV_CODE 0000
#endif

/* if this is the common file and not the definitions file, then we will 
 *  want to be able to initialize some of these values.  So define 
 *   a ``function'' to do this IS(); else this function is NULL.
 */
#ifdef COMMONFILE
#  define EXTERN
#  define IS(V) = V
#else
#  define EXTERN extern
#  define IS(V)
#endif
/* Note that we will undefine EXTERN at the end of this file */

#ifndef p_ent
#   define p_ent int
#endif

EXTERN	int	gp_revcode IS(REV_CODE) ;


/* one big struct to hold all 'plotcap' values.  This makes io easier */
EXTERN struct _allcapabs { 

	 /* composit capability from several Bools */
	 int    dev_type ;             /* T_VECT || T_RAST || T_SAVE */

	 /* BOOL capabilities */
	 Bool   IGPO , ICLR , RREV  ;

	 /* INT  capabilities */
	 int    PUNX,  PIXX;           /* pix per unit in X, total pix in X */
	 int    PUNY,  PIXY;
	 int    BFSZ,  INSZ ;
	 int    PIVL,  RPXB;        /* for rast: init pix val, pix per byte*/
	 int    ORGX  , ORGY  ;     /* orgins (initial pixel offset) */
	 int	NLINE, NPEN, NFILS;	/* colors, also? */

	 /* FLOAT capabilities */
         float  SCALX  , ROTAX , TRANX ; /* scale,rotote and offset */
         float  SCALY  , ROTAY , TRANY ;
         float  ANGLE ;                      /* rotation angle */
	 float	USCAL ;			/* input units to plotdev units */
	 float  CLPLX , CLPLY ;     /* low  bound clipping values */
	 float  CLPHX , CLPHY ;     /* high bound clipping values */

	 /* CHAR capabilities */
	 char plt_fil[NAMRLN];    /* file name for plot output */
	
	 /* PROG capabilites */
	 p_ent  DRAW,  MOVE,  PCLR ;  /* move, draw, clear */
	 p_ent  INIT,  PEND,  PLOP,  PLCL,  BFOP,  BFCL;
	 p_ent  LINE,  USR1,  USR2,  PEN ;  
	 p_ent  MSET,  MCLR,  MCMP ;
	 p_ent  DRWST, DRWCL, DRWCM ;
	 p_ent  SFORM, UNKN;
	 p_ent  AREA , FILS;
	 p_ent	ATOG , GTOA;		/* context switching */
	 p_ent	CLOC ;
	 p_ent	PAUS ;

	 } g_caps ;

#define  C g_caps
#define  plot_type C.dev_type
#define  plot_file C.plt_fil

/* Global pointers to the ``program'' and input data. And how full they are */
EXTERN	int	*g_prog ;
EXTERN	char	*g_idat ;
EXTERN	int	 g_poff , g_ioff ;


/* Alterable copies of some capability values */
EXTERN	float	 g_clplx ,  g_clply ;
EXTERN	float	 g_clphx ,  g_clphy ;
EXTERN	Bool	 g_logclp IS(F) ;	/* on logical or physical axis? */
EXTERN  Bool	 g_dobreak IS(T) ;	/* break on draws out of bounds */
EXTERN	fpoint2d g_logbox[4] ;		/* logical bounding box */
EXTERN	fpoint2d g_phsbox[4] ;		/* pysical bounding box */

EXTERN	float	 g_scalx ,  g_scaly ;
EXTERN	float	 g_tranx ,  g_trany ;
EXTERN	float	 g_rotax ,  g_rotay , g_angle ;

/* Transformation constants */
EXTERN	float	 g_XXcon , g_XYcon , g_Xcon ;
EXTERN	float	 g_YXcon , g_YYcon , g_Ycon ;

/* Setup/NotSetup Flags.  Device set, File set, everything set */
EXTERN	Bool	g_allset IS(F) ;
EXTERN	Bool	g_devset IS(F) ;
EXTERN	Bool	g_filset IS(F) ;
EXTERN	int	g_agmode IS(MODE_G) ;		/* alpha or graphics mode */

/* Input values for this point and the previous one */
EXTERN	int	*g_ivar1 ;
EXTERN	int	*g_ivar0 ;
EXTERN	float	*g_fvar1 ;
EXTERN	float	*g_fvar0 ;


/* The big pixel array for RASTer plotting, and some other values */
EXTERN   char   *pixels IS(0);
EXTERN   int     tot_bytes,  bytes_line;


/* File infromation */
EXTERN	int	 lu_oplt IS(LUODEF) ;

/* Auto save parameters */
EXTERN	int	 lu_asav IS(-1) ;	/* Lu for auto-saving */
EXTERN	Bool	 asav_on IS(F) ;	/* Are we auto-saving */

/* */
EXTERN	int	 g_lintyp IS(1) ;	/* Current draw mode Line type */
EXTERN	Bool	 soft_line IS(F) ;
EXTERN	Bool	 hwline_ok IS(F) ;
EXTERN	int	 g_drawmd IS(G_MSET) ;
EXTERN	Bool	 something IS(F) ;
EXTERN	int	 g_pentyp IS(1), g_filtyp IS(0) ;
EXTERN	Bool	 soft_pen IS(F) ;
EXTERN	Bool	 hwpen_ok IS(F) ;

EXTERN	int	 soft_area IS(F);	/* software area filling */

/* Things that can be turned on and off */
EXTERN	Bool	 sv_igpo  ;

EXTERN	float	 g_lastX ,  g_lastY ;
EXTERN	fpoint2d g_Plast
# ifdef COMMONFILE
		= { 0.0, 0.0 }
#endif
		;
EXTERN	float	g_xshint IS(0.0), g_yshint IS(0.0) ;

/* JUNK */
EXTERN	int	 xfrm_ord IS(XF_DEF) ;	/* xformation order */
EXTERN	int	 last_cmd, last_ent ;
EXTERN	char	*punq_loc IS(0) ;	/* Make a unique file name ? */

#ifdef DEBUG

EXTERN	FILE	*fpbug ;
EXTERN	int	 bug_level ;
EXTERN	int	 db_writeok IS(T) ;

#endif

#ifdef DO_STATS

EXTERN  struct _gp_stats {
	char	 st_dnam[10] ;		/* Device set up */
	int	 st_type ;		/*  type of device that was setup */
	int	 st_uid  ;		/* who is using this stuff */
	long	 st_time ;		/* when */

	int	 st_psiz, st_pmax ;	/* Program sizes */
	int	 st_dsiz, st_dmax ;	/* Idat sizes */

	int	 st_stsz, st_stmx ;	/* Stab sizes */
	int	 st_sesz, st_semx ;	/* Stab enties sizes */

	} gp_stats ;

#endif


/* undefine it incase someone else has the same idea */
#undef EXTERN
/* ----------------------------------------------------------------------- */
