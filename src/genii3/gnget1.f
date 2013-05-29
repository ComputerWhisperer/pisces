      LOGICAL FUNCTION GNGET1(lerr)
      include 'genidf.inc'
      logical lerr
c Wed Nov  8 22:57:32 PST 1989 (dredge--stanford)
c-----------------------------------------------------------------------
c   Copyright 1989 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c-----------------------------------------------------------------------
c
c "gnget1": Get the next user input line.
c
c Calling sequence:
c
c     <no_eof> = GNGET1(lerr)
c
c Where:
c     lerr logical  - True if error
c
c Original: Michael Eldredge -- Stanford (feb 89)
c
c-----------------------------------------------------------------------
c   common
c-----------------------------------------------------------------------
      include 'common.inc'
c-----------------------------------------------------------------------
c   local variables
c-----------------------------------------------------------------------
c...to peek or not to peek:
      logical GETONE, PEEK
      parameter (GETONE=.false., PEEK=.true.)

      logical   iscont,excont,didget
      integer   ityp,iloc
      integer   newkey,np,iptr,i,indx
      integer   lpf,lpl
      character*(LINELN) line
      logical pushd
      SAVE    line, pushd
      character blnk

      data blnk /' '/
      data pushd /.false./
c-----------------------------------------------------------------------
c   start of gnget1
c-----------------------------------------------------------------------

c.. not at EOF and no errors (yet)
      gnget1 = .true.
      lerr   = .false.

c.. GET A LINE FROM THE INPUT DECK
      iscont=.false.
c.. lpf=0  sez: read a line
c.. lpf=1  sez: already have on read in, just start parsing @ch#1
      lpf=0
      if (pushd) lpf=1
      pushd=.false.
      call getln(prmpt,prmln,line,lpf,lpl,GETONE,
     +     iscont,excont,newkey,didget)
      if (iscont) call gerst(040,linum,0,line(lpf:lpf),lpl-lpf+1)
      if (eofflg) goto 900
      if (errflg) goto 910


c.. INIT CARD INFORMATION and DEFAULTS:
c....keyid of new line, number of parms and pointer to 1st of this
c.....cards parameters.
      keyid=newkey
      np=nparms(keyptr)
      iptr=parptr(keyptr)

c.. initialize all of the data arrays for the new card.
      do 340 i=1,CVPC
340   call csetv(cval(i),CPCVL,blnk)
      call lsetv(cspecd,CVPC,.false.)
      call rsetv(rval  ,RPC ,0.)
      call lsetv(rspecd,RPC ,.false.)
      call lsetv(lval  ,LPC ,.false.)
      call lsetv(lspecd,LPC ,.false.)
c.. not a commment card
      tspecd=.false.
      call csetv(tval,LINELN,blnk)

c.. move in the defaults
c.. now for each possible parameter
c-->    for (indx=iptr ; indx < iptr+np ; indx=indx+1)
      do 370 indx = iptr, iptr+np-1
c.. location and type of the default
         iloc=parloc(indx)
         ityp=partyp(indx)

c.. if it is a numerical parameter
         if (ityp .eq. NUMTYPE) then
             rval(iloc)=rdfalt(indx)
c.. if it is a logical parameter
         else if (ityp .eq. LOGTYPE) then
             lval(iloc)=ldfalt(indx)
c.. if it is a character value parameter
         else if (ityp .eq. CHRTYPE) then
             call cpack(cdfalt(indx),cval(iloc),CPCVL)
         endif
370   continue


c.. If we get a continuation card, then come back here.
100   continue

c.. if no parameters, move in the char string otherwise
c.. call parse and have it move the values in.
c comment cards all get moved in.
      if (np.eq.0 .and. lpf.le.lpl)
     +    call cpack(line(lpf:lpf),tval,lpl-lpf+1)
      if (np.ne.0) then
          call parse(parms(iptr),partyp(iptr),parloc(iptr),
     +               np,line,lpf,lpl)
	  if (iactiv) then
              if (eofflg) goto 900
              if (errflg) goto 910
	  endif
      endif

c.. if expecting a continue line, get it -- interactive or not.
      if (excont) then
          lpf=0
          iscont=excont
          call getln(prmpt2,prm2ln,line,lpf,lpl,
     +               GETONE,iscont,excont,newkey,didget)
          goto 100
      endif

c.. if not interactive mode, read ahead to find (old style) continues.
c mje: was 'else if', but the goto 100 above means we don't need it
      if (.not.iactiv) then
c.. already read a new one, don't reread/parse line()
          lpf=0
          iscont=excont
          call getln(prmpt2,prm2ln,line,lpf,lpl,
     +               PEEK,iscont,excont,newkey,didget)
          if (.not.iscont) then
              if (didget) pushd=.true.
              goto 500
          endif
          goto 100
      endif

c... clean exit, no errors
      goto 500

c.. someone saw an eof
900   gnget1=.false.
      goto 500
c.. someone saw an error
910   lerr=.true.
      keyid = -2
c.. normal exit
500   continue
      if (iactiv) call gener
      return
      end
