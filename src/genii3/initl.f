      SUBROUTINE INITL(inpfil, filprs, keyfil, ukyfil, outlu)
      include 'genidf.inc'
      character*(*) inpfil
      character*(*) filprs
      character*(*) keyfil
      character*(*) ukyfil
      integer       outlu

c Thu Sep 14 16:49:02 PDT 1989 (dredge--stanford)
c-----------------------------------------------------------------------
c   Copyright 1980 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c-----------------------------------------------------------------------
c
c   WRITTEN BY: Stephen E. Hansen
c   DATE: June 5, 1979
c
c   MODIFICATIONS:
c
c   Michael Eldredge  Aug. 22, 1980
c     Convert to RATFOR, Use PARKY
c   Michael ELdredge  June 30, 1981
c     Use FORTRAN-77 I/O
c   Stephen E. Hansen  Jan. 26, 1982
c     Restructure, use new PARKY
c   Michael Eldredge (jun 80)  UNIX conversion to Fortran-77.
c      Use of character variables.
c   Michael Eldredge (jul 87)  Convert to library package.
c   Michael Eldredge -- Stanford (sep 89) Convert to ANSI-F77
c
c   NAME: INITL
c
c   FUNCTION: Initializes GENII card and parameter specifications.
c
c   TYPE OF ROUTINE: SUBROUTINE
c
c   CALLING SEQUENCE:
c
c      CALL INITL( ... )
c
c   PARAMETERS:
c
c     (INPUT)
c
c     (OUTPUT)
c
c
c   ERROR CONDITIONS:
c
c    008 : A parameter specification follows a card alias.
c    010 : The number of card types exceeds available storage.
c    011 : The number of parameters exceeds available storage.
c    012 : Card aliases a previously undefined card id number.
c    015 : First non-comment line in key file is not a card name
c    030 : Error detected on open of unformatted key file.
c    031 : Unexpected End-Of-File on read from unformatted key file.
c    032 : Error detected on read from unformatted key file.
c    034 : Error detected on open/create of unformatted key file.
c    035 : Error detected on write to unformatted key file.
c    036 : Error detected on open of input file.
c    037 : Error detected on open/create of parsed input data file.
c    042 : Error detected on close of unformatted key file.
c    043 : Error detected on close of formatted key file.
c
c   ALGORITHM:
c
c     1)  Check if the unformatted key file exists, if so, then read
c         in the card and parameter data from that file.
c
c     2)  If the unformatted key file does not exist, then read and
c         parse the card and parameter data from the formatted key
c         file.  Then create an unformatted key file and dump the
c         card and paramter data to it.
c
c     3)  Open the input file and a file to receive the parsed input
c         data
c
c---------------------------------------------------------------------
c     common area
c----------------------------------------------------------------------
      include 'common.inc'
c---------------------------------------------------------------------
c     LOCAL VARIABLES
c---------------------------------------------------------------------
      logical opnerr,closer,lfld4,lcard,alias
      integer i,j,ierr,np,npt,jk,jkeys,ll
      integer mxRs,mxLs,mxCs
      integer  nRs, nLs, nCs
c... temp (mapped) versions of file names
      character*(NAMRLN) fkymap, ukymap
      integer field2,field3
      character*(CPNAM) field1
      character*(CPCVL) cfld4
      real    rfld4
      logical ljunk
c
c----------------------------------------------------------------------
c..FUNCTIONS
c----------------------------------------------------------------------
      logical fexist
      logical gtval
c----------------------------------------------------------------------
c
c   start of initl.
c
c----------------------------------------------------------------------
c.. clear and init the error processing routines.
      call gerst(0,0,0,' ',0)

c NEVER really call gtval(), just reference it. MJE.
      if (linum .eq. -33 .and. linum .eq. -34) ljunk=gtval(i)

c First: copy in the file name for genii's use.
c We'll just be saving this one.
      call csetv(prsfil, NAMRLN, ' ')
c Never pass intrinsic names a parameters!!! Most compilers donot like it
      ll=LEN(filprs)
      ll=MIN0(ll,NAMRLN)
      call cpack(filprs, prsfil, ll)
c for messages.
      luout = outlu


c.. Does the unformatted parameter specification file exist?
c.. if the file does exist, all we have to do is read in the
c.. data as it is.
      if (fexist(ukyfil)) then
c.. first open up the file.
c.. status=old,form=unformatted,no-append.
          call opnfl(lukyu,ukyfil,opnerr,ierr,
     +               OLDFILE,UNFORMED,NONEXCLUS,NOAPPEND)
c
c.. if there was an error during the open.
          if (opnerr) then
c.. flag the error.
              ll=LEN(ukyfil)
              call gerst(030,0,ierr,ukyfil,ll)
c
c.. and leave now.
              return
          endif

c.. if the file was opened successfully, get the card specification
c.. data from the file.
          read (lukyu,iostat=ierr,err=91,end=92)
     +                   nkeys,keyids,nparms,parptr

          do 1100 i=1,CARD
1100          read (lukyu,iostat=ierr,err=91,end=92) keys(i)
          do 1110 i=1,PARM
1110          read (lukyu,iostat=ierr,err=91,end=92) parms(i)
          read (lukyu,iostat=ierr,err=91,end=92) parloc
          read (lukyu,iostat=ierr,err=91,end=92) partyp
          do 1120 i=1,PARM
              read (lukyu,iostat=ierr,err=91,end=92)
     +                cdfalt(i),rdfalt(i),ldfalt(i)
1120      continue

c.. that should be all, close the file.
          call clsfl(lukyu,closer,ierr)
c
c.. if an error was detected on the close, then flag it.
          if (closer)  call gerst(042,0,ierr,err,0)

c--------------------------------------------------------------------
c
c.. if file containing the parameter specifications in a
c.. compact format is not available, then we must create one
c.. from the file containing the information in fixed format.
c
c--------------------------------------------------------------------
      else
c.. this expansion is done transparently in opnfl(), but I can't
c... stand this info message printing without the actual
c....destination names
          call xgeten(keyfil, fkymap)
          if (fkymap(1:1) .eq. ' ') fkymap = keyfil
          call xgeten(ukyfil, ukymap)
          if (ukymap(1:1) .eq. ' ') ukymap = ukyfil

c.. let the user know what is happening...
          i = index(fkymap,' ')
          if (i.eq.0) i = LEN(fkymap)
          j = index(ukymap,' ')
          if (j.eq.0) j = LEN(ukymap)
          write(luttyo, 2101) fkymap(1:i), ukymap(1:j)
2101      format(//20x,'Building New Binary Key File',
     +          /10x,a,
     +          /5x,' --> ',a,
     +          /20x,'   Please wait...'//)

c.. open up the formatted specification file.
c.. status=old,form=formatted,no-append.
          call opnfl(lukyf,keyfil,opnerr,ierr,
     +               OLDFILE,FORMED,EXCLUSIVE,NOAPPEND)
c
c.. if an error was detected on the open.
          if (opnerr) then
c.. flag an error.
              call gerst(033,0,ierr,err,0)
              return
          endif

c.. read the card and parameter specificatin from the formatted
c.. file.

c.. initialize a few varibles.
c.. set the line count to zero.
          linum=0
c.. set the number of cards to zero.
          nkeys=0
c.. initialize Number Parameters (Tot)
          npt  =0
c.. number of parms per card.
          np   =0
c.. max number of each used on any given card
          mxRs = 0
          mxLs = 0
          mxCs = 0
          nRs = 0
          nLs = 0
          nCs = 0

c.. bring in the lines from the formatted key file.
c.. until an end of file is reached.
c.. call parky to get the next line from the file, it will
c.. parse the line into the various fields and determine
c.. whether the line describes a card or a parameter.
1400          call parky(field1,CPNAM,field2,field3,
     +                   rfld4,lfld4,cfld4,CPCVL,lcard)
c
c.. if an error was detected, then split.
              if (errflg) goto 1500

c.. if it is a card or an end-of-file.
              if (lcard .or. eofflg) then
c.. if we just got a card line and the previous card was
c.. not an alias, then let's save the number of parameters
c.. used by the previous card.
                  if (nkeys .gt. 0 .and. .not.alias) then
c.. save the number of parameters.
                      nparms(nkeys)=np
c.. if there were no parameters then don't point to any.
                      if (np .eq. 0)  parptr(nkeys)=0
                      if (nRs.gt.mxRs) mxRs=nRs
                      if (nLs.gt.mxLs) mxLs=nLs
                      if (nCs.gt.mxCs) mxCs=nCs
                  endif

c
c.. now, if an end-of-file was detected, split.
                  if (eofflg) goto 1500

c
c.. incrment the card count
                  nkeys=nkeys+1

c
c.. if the number of cards exceeds the amount of storage.
                  if (nkeys .gt. CARD) then
c.. too many card types.
                      call gerst(010,linum,CARD,field1,CPNAM)
                      goto 1500
                  endif
c
c.. set the parameter count for this card to zero.
                  np=0

c
c.. put the card name into the keys array.
                  keys(nkeys) = field1

c
c.. save the cards id number in keyids
                  keyids(nkeys)=field2

c
c.. if the third parameter not zero, then this card name is
c.. an alias, find the card it is aliasing.
                  alias=(field3 .ne. 0)
                  if (alias) then
c.. number of keys to search
                      jkeys=nkeys-1
                      do 1420 jk=1,jkeys
                          if (iabs(field3).eq.keyids(jk))goto 1421
1420                  continue
c
c.. error: no keyid with that number
1421                  if (jk .gt. jkeys) then
                          call gerst(012,linum,keyids(nkeys),
     +                                         field1,CPNAM)
                          goto 1500
                      endif

c
c.. have this card use the parameters of the alias.
                      nparms(nkeys)=nparms(jk)
                      parptr(nkeys)=parptr(jk)
                  endif


c
c.. if this not a card line then it is a parameter line.
              else
c.. if these parameters follow an aliased card.
                  if (alias) then
c.. flag an error.
                      call gerst(008,linum,0,field1,CPNAM)
                      goto 1500
                  endif
c
c.. increment the parameter count.
                  npt=npt+1

c
c.. if the number of cards seen so far is zero.
                  if (nkeys .eq. 0) then
c.. flag an error.
                      call gerst(015,linum,0,field1,CPNAM)
                      goto 1500
                  endif

c
c.. if the number of parameters exceeds available storage.
                  if (npt .gt. PARM) then
                      call gerst(011,linum,PARM,field1,CPNAM)
                      goto 1500
                  endif
c
c.. another parmeter on this card.
                  np=np+1
c
c.. if this is the first parameter on this card then have the
c.. cards parameter pointer point to it.
                  if (np .eq. 1) then
                      parptr(nkeys)=npt
                      nRs = 0
                      nLs = 0
                      nCs = 0
                  endif

c
c.. move the parameter name into the parms array.
                  parms(npt) = field1

c
c.. get the parameter type and its default.
                  partyp(npt)=field2
                  if (field2 .eq. NUMTYPE) then
                      rdfalt(npt)=rfld4
                      nRs = nRs + 1
                  else if (field2 .eq. LOGTYPE) then
                      ldfalt(npt)=lfld4
                      nLs = nLs + 1
                  else if (field2 .eq. CHRTYPE) then
                      call cpack(cfld4,cdfalt(npt),CPCVL)
                      nCs = nCs + 1
                  endif
c
c.. get the parameter value array location.
                  parloc(npt)=field3
              endif
c.. end of repeat.
          goto 1400
c
c.. close the formatted key file, we're done with it.
1500      call clsfl(lukyf,closer,ierr)
c.. not any more...
          eofflg=.false.
c
c.. if an error was detected on the close of the file then flag it.
          if (closer)  call gerst(043,0,ierr,err,0)

c.. Some information:
          write(luttyo, 2113) 'CARDS:',nkeys,CARD,nkeys*100/CARD
2113      format(15x,a12,' used ',i4,' of ',i4,3x,'(',i2,'%)')
          write(luttyo, 2113) 'PARMS:',npt  ,PARM,npt  *100/PARM

          write(luttyo, 2113) '    Numbers ', mxRs,RPC,mxRs*100/RPC
          write(luttyo, 2113) '    Logicals', mxLs,LPC,mxLs*100/LPC
          write(luttyo, 2113) '    Strings ',mxCs,CVPC,mxCs*100/CVPC
c          write(luttyo, 2113) '    Strings ', CVPC
c2115      format(15x,a,' per command: ',i3)
          write(luttyo, 2117) CPCVL
2117      format(15x,'    Characters per string: ',i3)

c
c.. if there havn't been any errors so far.
          if (.not.errflg) then
c.. create and open the unformatted key file.
c.. status=new,form=unformatted,no-append.
              call opnfl(lukyu,ukyfil,opnerr,ierr,
     +                   NEWFILE,UNFORMED,NONEXCLUS,NOAPPEND)
c
c.. if there was an error on the open.
              if (opnerr) then
c.. flag the error.
                  call gerst(034,0,ierr,err,0)
                  return
              endif

c.....dump the card specification data to the file.
              write (lukyu,iostat=ierr,err=94)
     +                     nkeys,keyids,nparms,parptr

              do 3100 i=1,CARD
3100              write (lukyu,iostat=ierr,err=94) keys(i)
              do 3110 i=1,PARM
3110              write (lukyu,iostat=ierr,err=94) parms(i)
              write (lukyu,iostat=ierr,err=94) parloc
              write (lukyu,iostat=ierr,err=94) partyp
              do 3120 i=1,PARM
                  write (lukyu,iostat=ierr,err=94)
     +                    cdfalt(i),rdfalt(i),ldfalt(i)
3120          continue
c
c.. close the unformatted key file.
              call clsfl(lukyu,closer,ierr)
c
c.. if an error was detected on the close, then flag it.
              if (closer)  call gerst(042,0,ierr,err,0)
          endif

c.. let the user know we're done with this...
          write(luttyo, 2102)
2102      format(//20x,'...New Binary Key File DONE.'//)
      endif


c
c.. now we just open the input and output files and
c.. reset the line counter and we're done!

      opinp = .false.
      opprs = .false.
c.. if no errors so far.
      if (.not.errflg .and. .not.iactiv) then
c.. set the line count to zero.
          linum=0

c  Open up the input file.
          call opnfl(luinp,inpfil,opnerr,ierr,
     +               OLDFILE,FORMED,EXCLUSIVE,NOAPPEND)

c.. if an error was detected on the open.
          if (opnerr) then
c.. flag an error.
              ll=LEN(inpfil)
              call gerst(036,0,ierr,inpfil,ll)
              goto 95
           endif
          opinp=.true.

c.. open up the file that gets the parsed input data.
c.. status=unknown,form=unformatted,no-append.
          call opnfl(luprs,prsfil,opnerr,ierr,
     +               UNKNOWN,FORMED,EXCLUSIVE,NOAPPEND)
c
c.. if an error was detected on the open.
          if (opnerr) then
c.. flag an error.
              ll=LEN(prsfil)
              call gerst(037,0,ierr,prsfil,ll)
              goto 95
          endif
          opprs=.true.

      else
c interactive input!
          luinp = luttyi
c don't close it later!
          opinp = .false.
      endif
c
c.. that's it for us.
      return

c--------------------------------------------------------------------
c
c.. the errors get stuck down here.
c
c--------------------------------------------------------------------
c.. error on read from unformatted key file.
91    call gerst(032,0,ierr,err,0)
      goto 95

c.. unexpected end of file on read from unformatted key file.
92    call gerst(031,0,0,err,0)
      goto 95

c.. error on write to unformatted key file.
94    call gerst(035,0,ierr,err,0)

c.. close the file and return.
95    call clsfl(lukyu,closer,ierr)
      return

c.. of initl.
      end
