/* $Header: /users/suprem/ig2/skel/RCS/griph.h,v 1.1 85/05/16 19:57:19 conor Exp $*/

/*----------------------------------------------------------------------
 *
 * griph.h - include file for grid graphics functions.
 *
 * Copyright c 1985 The board of trustees of the Leland Stanford
 *                  Junior University. All rights reserved.
 * This subroutine may not be used outside of the SUPREM4 computer
 * program without the prior written consent of Stanford University.
 *
 * Original: CSR Jan 85.
 *---------------------------------------------------------------------*/

char *wreset();
void draw_node(), shade_tri(), draw_node(), draw_tri(), draw_edge(), 
     draw_reg(), draw_mesh();

struct Snode *npick();
struct Sedge *epick();
struct Sreg  *rpick();
