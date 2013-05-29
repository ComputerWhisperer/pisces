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
 * Tue Jan 30 15:20:10 PST 1990 (dredge--stanford)
 * "dp_com.h" : common variables for dplot software. These are the
 *              external definitions.
 */

/* if this is the actual common file, we need to init some values */
#ifdef COMNFILE
#  define  IS(V)  = V
#else
#  define  IS(V)
#endif


 /* Rev code for dplot */
 EXTERN  float  dp_rcode IS( REV_CODE ) ;
 EXTERN  char	dp_prmt[PRMTSZ] ;

 /* Some default values for parsing, etc. */
 EXTERN  char	cmd_char IS(CMD_CHAR) , cmt_char IS(CMT_CHAR) ;
 EXTERN  char	sys_char IS(SYS_CHAR) , sin_char IS(SIN_CHAR) ;

 /* plot points, P0: last point and P1: new point */
 EXTERN  float  x0, x1;
 EXTERN  float  y0, y1;
 EXTERN  float  newX, newY;
 EXTERN  int    pen;
 EXTERN  float  rateX, rateY;
 EXTERN  float  addtoX IS(0.) , addtoY IS(0.) ;     /* off set data */
 EXTERN  float  mulbyX IS(1.) , mulbyY IS(1.) ;     /* scale the data */
 EXTERN	 bool	absX   IS(F ) , absY   IS(F ) ;	    /* take abs first */

 /* plot corners */
 EXTERN  float  plotX0, plotX1 ;
 EXTERN  float  plotY0, plotY1 ;
 EXTERN  float  sizeX , sizeY  ;	/* size (in inches, etc.) of device */

 EXTERN  float  p_cornX  IS(CORNX) , p_cornY  IS(CORNX) ;
 EXTERN  bool   p_cornXp IS(BYPRC) , p_cornYp IS(BYPRC) ;
 EXTERN  float  p_lenX   IS(PLENX) , p_lenY   IS(PLENY) ;
 EXTERN  bool   p_lenXp  IS(BYPRC) , p_lenYp  IS(BYPRC) ;
 EXTERN  float  p_maxX   IS(PMAXX) , p_maxY   IS(PMAXY) ;
 EXTERN  float  p_aspect IS(ASPECT);

 /* Scale, Rotate, and Translate values. */
 EXTERN  float  scaleX, rotateX, transX;
 EXTERN  float  scaleY, rotateY, transY;
 EXTERN  float  rotateA;

 /* number of columns in list, X's column number, Y's col number, current 
  *   column.
  */
 EXTERN  short  ncols, colX, colY, colcnt;
 EXTERN  bool   logXplot, logYplot;
 EXTERN  float  minX, maxX;
 EXTERN  float  minY, maxY;
 EXTERN  int    skipover, stopafter;
 EXTERN  char   title[BUFFSIZE], labelX[BUFFSIZE], labelY[BUFFSIZE];
 EXTERN  float  l_siz IS(SYMB_SIZE);		/* label size */
 EXTERN  float  s_siz IS(SYMB_SIZE);		/* little symbols */
 EXTERN  char   s_sym[1];
 EXTERN  int    lin_typ;
 EXTERN  int    by_how;

 /* Pen thicknesses... */
 EXTERN	 int	pen_axis IS(PEN_AXIS) ;
 EXTERN	 int	pen_data IS(PEN_DATA) ;
 EXTERN	 int	pen_symb IS(PEN_SYMB) ;

 /* Delete these segments, please */
 /* Note: this is simple for now.  Just delete certain types of segments,
  *	not "delete the 5th segment" or anything fancy....
  */
 EXTERN	 int	seg_list[ MAX_SEG_DEL ] ;
 EXTERN	 int	seg_count IS(0) ;
 EXTERN	 int	seg_depth IS(0) ;

 /* locations for $line.label $symb.label; set by $goto.{x,y} */
 EXTERN  float  xlloc IS(0.0), ylloc IS(0.0) ;
 EXTERN  bool	llocs_rel IS(F) ;	/* relative (data) values? */

#ifdef Some_Day
 /* scan a file  to find current mins and maxs */
 EXTERN  bool JustScan;           /* just scanning values for min/max */
 EXTERN  float x_min, x_max, y_min, y_max; /* scanned values */
#endif

 /* ???? */
 EXTERN  int  lin_count IS(0);          /* total line count of all files */
 EXTERN  int  pnt_count IS(0);		/* total point count */
 EXTERN  bool gotX, gotY;               /* found x,y yet ? */
 EXTERN  bool NewPlot IS(T) ;		/* used any plotting functions yet? */
 EXTERN  bool haveERR;
 EXTERN  int  ErrCode;
 EXTERN  FILE *fplog;
 EXTERN  int  log_lev;    	/* just for fun... */
 EXTERN  int  log_val IS(0) ;	/* so we can log very verbose stuff... */
 EXTERN  char ttynam[20]; /* more just for fun stuff ... */
