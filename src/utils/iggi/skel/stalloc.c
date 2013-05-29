/*---------------------------------------------------------------------
 *
 * stalloc.c - define static storage.
 *
 * Copyright c 1985 The board of trustees of the Leland Stanford
 *                  Junior University. All rights reserved.
 * This subroutine may not be used outside of the SUPREM4 computer
 * program without the prior written consent of Stanford University.
 *
 * Original: CSR
 *---------------------------------------------------------------------*/
#define STATIC_ALLOCATION_TIME

#include <general.h>
#include "dbase.h"
#include "skelp.h"
#include "menu.h"


global_init()
{
  /*...This is basically a BLOCK DATA. */
    root.node = 0;
    root.reg = 0;
    root.tri = 0;
    root.edge = 0;

    debug1 = debug2 = 0;

    menu_wide = 7;
    menu_deep = 2;
    verbose = 1;
    check = 1;

    DefCw = DefCh = 0.13;
    DsNode = DsEdge = 1; DsReg = 0;
    DsAxis = 1;
    DsTri = DsObt = 0;
    DsFill = 0;
    ClrElec = 2; ClrEdge = 4; ClrReg=7; ClrTri = 1;
}
