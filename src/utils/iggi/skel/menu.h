/* $Header: /users/suprem/ig2/skel/RCS/menu.h,v 1.1 85/05/16 19:58:10 conor Exp $*/

/*----------------------------------------------------------------------
 *
 * menu.h - defines all the functions in user.c and the associated menu
 *          entries.
 *
 * Copyright c 1985 The board of trustees of the Leland Stanford
 *                  Junior University. All rights reserved.
 * This subroutine may not be used outside of the SUPREM4 computer
 * program without the prior written consent of Stanford University.
 *
 * Original: CSR Jan 85
 *---------------------------------------------------------------------*/
#ifdef STATIC_ALLOCATION_TIME
#define EXT
#else
#define EXT extern
#endif

typedef char * (*PFC)();	/* Pointer to a function returning strings. */

struct mitem 
	{
	char *text;		/* Identifying command.   */
	PFC func;		/* Associated function    */
	struct mitem *sub_menu;	/* Or sub-menu            */
	int active;		/* Currently up?          */
	struct WINDOW w;	/* Associated screen box. */
	};
		
EXT int menu_deep, menu_wide;	/* These must be intialized */

/* Set up menu tables backwards so everything is recognized - like Pascal. */

/* First list all functions. */
char *urmesh(), *uwmesh(), *ugmesh(),
     *ucr_node(), *ucr_reg(), 
     *uds_node(), *uds_reg(), *uds_all(),
     *ual_node(), *ual_edge(), *ual_reg(), 
     *usp_reg(), *usp_edge(),
     *ujoin_reg(), *ujoin_edge(),
     *uwi_refr(), *uwi_zin(), *uwi_zout(), *uwi_pan(), *uwi_pop(),
     *umv_node(), *umv_edge(), *umv_reg(), *umv_org(), *umv_block(),
     *uop_node(), *uop_reg(), *uop_edge(), *uop_tri(), 
     *uop_fill(), *uop_obt(), *uop_bgrid(), *uop_axis(),
     *allover(), *uescape();

#define NW {0,0,0,1,0,1,0,1,0,1,0,0,0,0,0,0,0,0,0}

#ifdef STATIC_ALLOCATION_TIME
EXT struct mitem file_menu[] = 
	{
	{"Read", urmesh, 0, 0, NW},
	{"Write", uwmesh, 0, 0, NW},
	{"Dplot", ugmesh, 0, 0, NW},
	{"Quit", 0, 0, 0, NW},
	{"", 0, 0, 0, NW}
	};

EXT struct mitem create_menu[] = 
	{
	{"Node", ucr_node, 0, 0, NW},
	{"Region", ucr_reg, 0, 0, NW},
	{"Quit", 0, 0, 0, NW},
	{"", 0, 0, 0, NW}
	};

EXT struct mitem destroy_menu[] = 
	{
	{"Node", uds_node, 0, 0, NW},
	{"Region", uds_reg, 0, 0, NW},
	{"All", uds_all, 0, 0, NW},
	{"Quit", 0, 0, 0, NW},
	{"", 0, 0, 0, NW}
	};

EXT struct mitem alter_menu[] = 
	{
	{"Node", ual_node, 0, 0, NW},
	{"Edge", ual_edge, 0, 0, NW},
	{"Region", ual_reg, 0, 0, NW},
	{"Quit", 0, 0, 0, NW},
	{"", 0, 0, 0, NW}
	};

EXT struct mitem split_menu[] =
	{
	{"Region", usp_reg, 0, 0, NW},
	{"Edge", usp_edge, 0, 0, NW},
	{"Quit", 0, 0, 0, NW},
	{"", 0, 0, 0, NW}
	};

EXT struct mitem join_menu[] =
	{
	{"Region", ujoin_reg, 0, 0, NW},
        {"Edge", ujoin_edge, 0, 0, NW},
	{"Quit", 0, 0, 0, NW},
	{"", 0, 0, 0, NW}
	};

EXT struct mitem wind_menu[] = 
	{
	{"Fresh", uwi_refr, 0, 0, NW},
	{"In", uwi_zin, 0, 0, NW},
	{"Out", uwi_zout, 0, 0, NW},
	{"Pan", uwi_pan, 0, 0, NW},
	{"Reset", uwi_pop, 0, 0, NW},
	{"Quit", 0, 0, 0, NW},
	{"", 0, 0, 0, NW}
	};

EXT struct mitem really_quit[] = 
	{
	{"Yes", allover, 0, 0, NW},
	{"No ", 0, 0, 0, NW},
	{"", 0, 0, 0, NW}
	};

EXT struct mitem move_menu[] =
	{
	{"Node", umv_node, 0, 0, NW},
	{"Edge", umv_edge, 0, 0, NW},
	{"Region",umv_reg, 0, 0, NW},
	{"Origin",umv_org, 0, 0, NW},
	{"Block", umv_block, 0, 0, NW},
	{"Quit", 0, 0, 0, NW},
	{"", 0, 0, 0, NW}
	};

EXT struct mitem option_menu[] =
	{
	{"Node", uop_node, 0, 0, NW},
	{"Edge", uop_edge, 0, 0, NW},
	{"Region", uop_reg, 0, 0, NW},
	{"Triangle", uop_tri, 0, 0, NW},
	{"Stretch", uop_fill, 0, 0, NW},
	{"Obtuse", uop_obt, 0, 0, NW},
	{"Grid", uop_bgrid, 0, 0, NW},
	{"Axis", uop_axis, 0, 0, NW},
	{"Quit", 0, 0, 0, NW},
	{"", 0, 0, 0, NW},
	};

EXT struct mitem main_menu[] = 
	{
	{"File", 0, file_menu, 0, NW},
	{"Create", 0, create_menu, 0, NW},
	{"Destroy", 0, destroy_menu, 0, NW},
	{"Alter", 0, alter_menu, 0, NW},
	{"Split", 0, split_menu, 0, NW},
	{"Join", 0, join_menu, 0, NW},
	{"Move",   0, move_menu, 0, NW},
	{"Window", 0, wind_menu, 0, NW},
	{"Options", 0, option_menu, 0, NW},
	{"!Shell", uescape, 0, 0, NW},
	{"Quit", 0, really_quit, 0, NW},
	{"", 0, 0, 0, NW}
	};
#else
EXT struct mitem main_menu[];
#endif
