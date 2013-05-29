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
 * Mon Sep 18 11:45:07 PDT 1989 (dredge--stanford)
 *
 * "gp_caps.h" : capabilites used by gplot from the plotcap file.
 *	These are broken down into the different types of capabilities
 *	and stuck in structures to allow easy access.
 * Original: Michael Eldredge (dec 86)
 * Modified: MJE (jul 87) some compilers don't like incomplete struct
 *	defintions, so the ``-1'' entries are now filled out.
 * Modified: mje (sep 89) Added PAUS capabilities.
 */


/* Basic Device types possible */
static struct {
	int  ptyp;
	char *nam;
	} *tcp, Tcap[] = {

	{ T_VECT , "VECT" } ,   /* vector type device */
	{ T_RAST , "RAST" } ,   /* rastor type device */
	{ T_SAVE , "SAVE" } ,   /* psuedo-device, saves plotcalls to file */
#ifdef DEBUG
	{ T_DEBUG, "DEBUG"} ,   /* psuedo-device, for debugging */
#endif
	{ -1 , "" }
	};

/* Bool type caps */
static struct {
	int   ptyp;
	char  *nam;
	Bool  *loc;
	} *bcp , Bcap[] = {

	{ T_VECT , "IGPO" , &(C.IGPO) } ,
	{ T_VECT , "ICLR" , &(C.ICLR) } ,
	{ T_RAST , "RREV" , &(C.RREV) } ,
	{ -1, "", NULL }
	};

/* numeric type caps, first: is value to be int for float number?? */
/* INT types */
static struct {
	int   ptyp;
	char  *nam;
	int   *loc;
	int    def;
	} *icp , Icap[] = {

	{ T_ANY  , "PUNX" , &C.PUNX ,  0    } ,
	{ T_ANY  , "PUNY" , &C.PUNY ,  0    } ,
	{ T_ANY  , "PIXX" , &C.PIXX ,  0    } ,
	{ T_ANY  , "PIXY" , &C.PIXY ,  0    } ,
	{ (T_VECT | T_SAVE) , 
		   "BFSZ" , &C.BFSZ ,  OSIZDEF } ,
	{ T_VECT , "INSZ" , &C.INSZ ,  ISIZDEF } ,

	{ T_RAST , "PIVL" , &C.PIVL ,  0    } ,
	{ T_RAST , "RPXB" , &C.RPXB ,  8    } ,
	{ T_ANY  , "ORGX" , &C.ORGX ,  0    } ,
	{ T_ANY  , "ORGY" , &C.ORGY ,  0    } ,

	{ T_ANY  , "NLINE", &C.NLINE, NHUGE } ,	/* default is big! */
	{ T_ANY  , "NPEN" , &C.NPEN , NHUGE } ,	/* default is big! */
	{ T_ANY  , "NFILS", &C.NFILS, NHUGE } ,	/* default is big! */

	{ T_ANY  , "_STAB", &stabsiz,  STABSIZE } ,
	{ T_ANY  , "_SENT", &stabent,  STABENTS } ,
	{ -1, "", NULL, 0 }
	};

/* FLOAT types */
static struct {
	int   ptyp;
	char  *nam;
	float *loc;
	float  def;
	} *fcp , Fcap[] = {

	{ T_ANY  , "SCALX" , &C.SCALX    ,  1.0 } ,
	{ T_ANY  , "SCALY" , &C.SCALY    ,  1.0 } ,
	{ T_ANY  , "TRANX" , &C.TRANX    ,  0.0 } ,
	{ T_ANY  , "TRANY" , &C.TRANY    ,  0.0 } ,
	{ T_ANY  , "ROTAX" , &C.ROTAX    ,  0.0 } ,
	{ T_ANY  , "ROTAY" , &C.ROTAY    ,  0.0 } ,
	{ T_ANY  , "ANGLE" , &C.ANGLE    ,  0.0 } ,

	{ T_ANY  , "CLPLX", &C.CLPLX,  -1.0   } ,
	{ T_ANY  , "CLPLY", &C.CLPLY,  -1.0   } ,
	{ T_ANY  , "CLPHX", &C.CLPHX,  -1.0   } ,
	{ T_ANY  , "CLPHY", &C.CLPHY,  -1.0   } ,
	
	{ T_ANY  , "USCAL" , &C.USCAL    ,  1.0 } ,
	{ -1, "", NULL, 0.0 }
	};

/* string type caps */
static struct {
	int   ptyp;
	char  *nam;
	char  *loc;
	char  *def;
	} *scp , Scap[] = {

	{ T_ANY  , "FILE" , plot_file   ,  ""  } ,
	{ -1 , "", "", "" }
	};

/* program type caps */
static struct {
	int   ptyp;
	char  *nam;
	p_ent *loc;
	p_ent  def;
	} *pcp , Pcap[] = {

	{ T_VECT , "DRAW" , &C.DRAW , NOENT } ,   /* draw setting bits */
	{ T_VECT , "MOVE" , &C.MOVE , NOENT } ,
	{ T_VECT , "PCLR" , &C.PCLR , NOENT } ,

	{ T_ANY  , "INIT" , &C.INIT , NOENT } ,
	{ T_ANY  , "PEND" , &C.PEND , NOENT } ,
	{ T_ANY  , "PLOP" , &C.PLOP , NOENT } ,
	{ T_ANY  , "PLCL" , &C.PLCL , NOENT } ,
	{ T_ANY  , "BFOP" , &C.BFOP , NOENT } ,
	{ T_ANY  , "BFCL" , &C.BFCL , NOENT } ,

	{ T_SAVE , "SFORM", &C.SFORM, NOENT } ,  /* save format */

	/* drawing modes. First how to toggle to new mode. Next how to draw in
	 *	a given mode.  Can use which ever is appropriate.
	 */
	{ T_VECT , "MSET" , &C.MSET , NOENT } ,  /* toggle mode to: set bits */
	{ T_VECT , "MCLR" , &C.MCLR , NOENT } ,  /*     clear bits */
	{ T_VECT , "MCMP" , &C.MCMP , NOENT } ,  /*        complement bits */
	{ T_VECT , "DRWCL", &C.DRWCL, NOENT } ,  /* DRAW-CLEAR sequence */
	{ T_VECT , "DRWCM", &C.DRWCM, NOENT } ,  /* DRAW-COMPENENT sequence */
	{ T_VECT , "LINE" , &C.LINE , NOENT } ,  /* hardware line types */

	/* unsupported (not strongly recommended) caps */
	{ T_VECT , "PEN"  , &C.PEN  , NOENT } ,
	{ T_ANY  , "USR1" , &C.USR1 , NOENT } ,
	{ T_ANY  , "USR2" , &C.USR2 , NOENT } ,

	/* Hardware area fill and fill pattern select caps */
	{ T_VECT , "AREA" , &C.AREA , NOENT } ,  /* area filling .... */
	{ T_VECT , "FILS" , &C.FILS , NOENT } ,  /* filling select .. */

	/* Context swithing capabilities. Graphics mode to/from Alpha mode */
	{ T_VECT , "ATOG" , &C.ATOG , NOENT } ,
	{ T_VECT , "GTOA" , &C.GTOA , NOENT } ,

	/* Input functions.... (ie: get cursor location) */
	{ T_VECT , "CLOC" , &C.CLOC , NOENT } ,
	{ T_ANY  , "PAUS" , &C.PAUS , NOENT } ,

	{ T_ANY  , "UNKN" , &C.UNKN , NOENT } ,
	{ -1, "", NULL, 0 }
	};
