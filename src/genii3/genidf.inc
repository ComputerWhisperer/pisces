c---------------------------------------------------------------------
c Fri Jun  2 15:35:06 PDT 1989 (dredge--stanford)
c-----------------------------------------------------------------------
c   Copyright 1980 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c-----------------------------------------------------------------------
c
c DEFINITIONS.
c This is the genii definitions file.
c
c MOD: Changed for FORTRAN 77.    (MJE 18feb83)
c modified: Michael Eldredge (jul 87) re-organize for "subr"ed version.
c
c---------------------------------------------------------------------
c THINGS THAT ARE EASY-TO-CHANGE:

c These should be defined as the preconnected input and output LUs.
       integer TTYIN, TTYOUT
       parameter (TTYIN=5, TTYOUT=6)

c These are just default and can easily be reset by the user with the
c  genilu() subroutine.  But we need some defaults in case never set.
       integer DLUINP, DLUOUT, DLUPRS, DLUKYF, DLUKYU
       parameter (DLUINP=10)
       parameter (DLUOUT=11)
       parameter (DLUPRS=12)
       parameter (DLUKYF=13)
       parameter (DLUKYU=14)


c chars/file-namr , max-number-cards , max-total-parms
       integer CPNAM, CARD, PARM
       parameter (CPNAM= 8)
       parameter (CARD= 60)
       parameter (PARM=800)
c
c chars/cval , cvals/card, logicals/card, reals/card
       integer CPCVL, CVPC, LPC, RPC
       parameter (CPCVL=20)
       parameter (CVPC = 8)
       parameter (LPC  =60)
       parameter (RPC  =60)
c
c max errs reported.
       integer ERMAX, ERMESLN
       parameter (ERMAX=20)
       parameter (ERMESLN=20)
c
c Filename length, input line size, max prompt string size
       integer NAMRLN, LINELN, PRMTSZ
       parameter (NAMRLN=128)
       parameter (LINELN=132)
       parameter (PRMTSZ= 20)
c
c max number of input files ("include" depth)
       integer  MXINCL
       parameter (MXINCL= 5)

c end of Things easy-to-change.
c
c---------------------------------------------------------------------

c Legal word separator
c  white spaces: {space,tab}
        character*(2) SPACE
	parameter (SPACE=' 	')
c  eparm: end of parameter {space,tab,comma}
        character*(3) EPARM
	parameter (EPARM=' 	,')
c  eword: end of word      {space,tab,comma,equals}
        character*(4) EWORD
	parameter (EWORD=' 	,=')

c type of parameter defines.
        integer NUMTYPE, LOGTYPE,CHRTYPE
	parameter (NUMTYPE=1,LOGTYPE=2,CHRTYPE=3)
c
c.. for file opens... give names to open codes....
        integer OLDFILE,NEWFILE,UNKNOWN,SCRATCH
	parameter(OLDFILE=1,NEWFILE=2,UNKNOWN=3,SCRATCH=4)
c
c.. and give names to the other open parameters....
        logical  FORMED,UNFORMED,EXCLUSIVE,NONEXCLUS,APPEND,NOAPPEND
	parameter (FORMED=   .true.)
	parameter (UNFORMED= .false.)
	parameter (EXCLUSIVE=.true.)
	parameter (NONEXCLUS=.false.)
	parameter (APPEND=   .true.)
	parameter (NOAPPEND= .false.)
c
c.. keyid if fatal errors found by genii.
        integer FATALERRORS
	parameter (FATALERRORS= -1)
c---------------------------------------------------------------------
