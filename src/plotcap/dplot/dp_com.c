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
 * Wed Sep 20 12:51:41 PDT 1989 (dredge--stanford)
 * "dp_com.c" : common storage for dplot software.  
 * Modified: mje -- stanford (sep 89) include version code here.
 */

#include <stdio.h>
#include "dp_def.h"

#define COMNFILE
#undef  EXTERN
#define EXTERN

#include "dp_vers.h"
#include "dp_com.h"
