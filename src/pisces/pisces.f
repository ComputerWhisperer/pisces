cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Sun Sep  2 21:54:00 PDT 1990 (anderson--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE PISCES(INFILE,LUOFIL,KEYFIL,UKYFIL,HADERR)
      include 'p2conf.h'
      character*(*) infile
      integer       luofil
      character*(*) keyfil, ukyfil
      logical       haderr
c
c-----------------------------------------------------------------------
c
c  The following is the actual main procedure routine in the PISCES
c  modeling program.  It first calls the GENII input processor to 
c  read the user input file and then calls appropriate routines to
c  interpret and act on each input line.
c
c          ****** PISCES-IIB ******
c
c Usage:
c  call pisces(infile, luofil, keyfil, ukyfil, haderr)
c
c Where:
c  infile  character*(*) - Input file name to be opened.
c  luofil  integer       - Lu to already open output file.
c  keyfil  character*(*) - Genii key file (text version).
c  ukyfil  character*(*) - Genii unformatted key file (binary version).
c  haderr  logical       - True if errors found, False if all went ok
c ----------
c
c  Original : C.H.Price     Stanford University          May, 1982
c  Revision : MRP           Stanford University          Nov, 1983
c  Revision : DYC           Monte Carlo                  Jun, 1986
c  Modified : Michael Eldredge -- Stanford               Oct, 1987
c       Added new GENII.  Re Org.
c  Modified : Michael Eldredge -- Stanford               May, 1988
c       Split main into: main and subr pisces to isolate main
c       and the system depenencies.
c  Modified : G.Anderson (photogeneration added)        June, 1989
c  Modified : MJE -- stanford                           Aug, 1989
c       new xmktmp calling seq.
c  Modified : MJE -- stanford                           Mar, 1990
c       don't include the version info, call out for it.  Makes
c       recompiles MUCH quicker when the version changes (ie:
c       this file does not have to be recompiled).
c
c  Copyright c 1981 The board of trustees of the Leland Stanford 
c                   Junior University. All rights reserved.
c  This subroutine may not be used outside of the PISCES computer
c  program without the prior written consent of Stanford University. 
c
c-----------------------------------------------------------------------
c 
c                   common areas
c
      include     'blank.h'
      include     'names.h'
      include     'stat.h'
      include     'key.h'
      include     'logunt.h'
      include     'plot.h'
      include     'setup.h'
      include     'impact.h'
c------------------------------------------------------------------
c
c 
c                   type declarations 
c 
      integer     i,j
      integer     lu
      character*1 chstar(60)
      character*40 tmpfil
      real   rdummy
      character*40 vers
      character*40 rels
      integer      szvers
      integer      szrels
c
c
c -- FUNCTIONS
      logical genii, gtkey

c -- DATA
      data chstar/60*' '/

c 
c****************************************************************** 
c 
      haderr = .false.

c...Clear error flags and  array
      call erset(0,0,0) 
c
c...bring into common from parameters.
      inpfil = infile
      luout=luofil
      luinf=luout 
      luplt=luout 
      ludia=luout 

c..if no input file given, then this must be interactive
c...(erset needs to know so it won't abort on errors)
      if (inpfil(1:1).eq.' ') isiact=.true.
c
c ==============================================================
c...Print the banner message.

c....get time of day and print head
      call xclock(3,idaytm,rdummy)
c
c....get the version information
      call p2vers(vers,rels)
      call sqshst(vers,szvers)
      call sqshst(rels,szrels)
c
c.....setup titles for graphics.
      i = INDEX(rels,' ')
      j = INDEX(rels,'.')
      if (j.gt.0 .and. j.lt.i) i=j
      if (i.gt.0) then
        i=i-1
      else
        i=1
      endif
      header = 'PISCES-II'
      hdvers = vers(1:szvers) // rels(1:i)
c
c      lu = lutty
      lu = luout
2     write(lu, 521)
521   format(//)
      call censtr(lu,
     + '********* PISCES-II Device Modeling Program *********', 72)
      call censtr(lu,
     +  '(Version '//vers(1:szvers)//'-'//rels(1:szrels)//')',
     +   72)
      write(lu, 521)
      call censtr(lu, 'Date and time = '//idaytm, 72)
      write(lu, 521)
c      lu = luout
c      if (lu.ne.lutty) goto 2
c
c 
c ==============================================================
c...Parse the input file

c....Let genii know which LUs to use.
      call gnilus(lugtmp, 4+NINCLS, INCKEY)
c
c....Let the parser temp file be temp only.
      call xmktmp(tmpfil, LEN(tmpfil), 'p2prs', 5)

c....Try to parse the input file.
      if (.not.genii(inpfil, tmpfil, keyfil, ukyfil, luout))
     +    goto 9999
c....for error reporting.
      linum = 0
c
c ==============================================================
c...Initialize line number read and max. exponential (exponential
c...limiting criterion)
      lcrdrd=.false.
      dcexp=dexp(expcut)
      exci=1.d0/excrit
c
c...Initialize Monte Carlo constants
c
      call initpm
c 
c ==============================================================
c---------------
c   Main loop
c---------------
c
c...Note: ALL keyids must be in the following if-elseif-endif
c.....Statment.  Even if the key is supposed to be detected
c.....in a remote ...CK() routine; the key should be in the
c.....very last elseif check near the end.  The result will be
c.....to flag the key as out-of-proper-order.


c...Get next card, unless already read
10    if (.not.lcrdrd) eofflg = .not.gtkey(keyid)
      lcrdrd=.false.
      if(eofflg.or.(keyid.eq.kend)) then
         if(.not.lbpt1.and.lsetpr) then
            if(.not.lminit) call minit
            if(.not.lscale) call mater
            call setpr
         endif
         write(luout,21)
21       format(//' ')
         haderr = .false.
c...quitting time.....
         goto 9000
      endif
      if(errflg) goto 9999
c
c...Segment branch point

c...nonpositive keys indicate a (non fatal) error from GENII, assume
c...that genii let the user know and go get the next line.
      if (keyid.le.0) then
       goto 10
      else if(keyid.eq.ktitle) then
       goto 100 
      else if(keyid.eq.kcomme) then 
       goto 10
      else if(keyid.eq.kmesh) then
        call mshgen 
      else if(keyid.eq.krgrid) then
        call regrid
      else if(keyid.eq.ksymbo) then 
        call symgen 
      else if((keyid.eq.kmater).or.(keyid.eq.kconta).or.
     +        (keyid.eq.kintf)) then
        if(.not.lminit) call minit
        call setmat
      else if(keyid.eq.kmodel) then
        if(.not.lminit) call minit
        call modck
      else if(keyid.eq.kmeth) then
        call method
      else if(keyid.eq.kcheck) then
        call solchk
      else if(keyid.eq.klogj) then
        call logchk
      else if(keyid.eq.kload) then 
         if(.not.lbpt1) then
            if(.not.lminit) call minit
            if(.not.lscale) call mater
            if(lsetpr) call setpr
            lbpt1=.true.
         endif
         if(.not.errflg) call ldsol
      else if(keyid.eq.kimpct) then
         call impcck
         if ((.not.errflg).and.lmont) call mcint(mntnam)
      else if(keyid.eq.ksolve) then 
         if(.not.lbpt1) then
            if(.not.lminit) call minit
            if(.not.lscale) call mater
            if(lsetpr) call setpr
            lbpt1=.true.
         endif
       if(.not.errflg) call solvr 
      else if(keyid.eq.kprint) then 
         call printr 
      else if(keyid.eq.kplot1) then 
         call plottr 
      else if(keyid.eq.kplot2) then 
        call plottr 
      else if(keyid.eq.koptn) then
        call optck
      else if(keyid.eq.kxtrac) then
        call extrac
c
c...For Monte Carlo
c
      else if(keyid.eq.kwindo) then
         if(.not.lminit) call minit
         if(.not.lscale) call mater
         call windck
      else if(keyid.eq.kmcmsh) then
         call mcmshc
      else if(keyid.eq.kmcmat) then
         call mcmatc
      else if(keyid.eq.kmcsol) then
         call mcsolc
      else if(keyid.eq.kmcout) then
         call mcoutc
c
c...Ooops! All of these are handled elsewhere.  If we hit them here, they
c.....were given in the wrong order.  Flag the error.
      else if (keyid.eq.kxmesh  .or.  keyid.eq.kymesh
     +   .or.  keyid.eq.kregio  .or.  keyid.eq.kdopin
     +   .or.  keyid.eq.kelect  .or.  keyid.eq.kcntur
     +   .or.  keyid.eq.kspred  .or.  keyid.eq.kelim
     +   .or.  keyid.eq.kvectp
     +   .or.  keyid.eq.kcompo  .or.  keyid.eq.keline) then
         call erset(2,linum,keyid)
c
c...Oops! unrecognized KEYID.  Something is wrong!  A card was added to
c.......the key file, but there is no code for it!
      else
       call erset(1,linum,keyid) 
      endif
c 
c...Check for errors and handle
      if(errflg.or.wrnflg) call erite
      if(errflg) goto 9999
c
c...Get the next card processed
      goto 10
c
c...Title card - transfer title into title & write out 
100   call gttval(ititle, LEN(ititle))
      i=LEN(ititle)
115   i=i-1
      if(ititle(i:i).eq.' ') goto 115
      do 116 j=1,i
116   chstar(j)='*'
      write(luout,1000) chstar,ititle,chstar
1000  format(//,1x,60a1,/,1x,a60,/,1x,60a1)
c 
      goto 10 
c------------------------------------------------
c Returns:
c...Error
9999  haderr = .true.
c.....FALLTHROUGH
c
c...clean up and return
c.....Close the genii tmp file (-1:means close and delete!)
9000  call gniend
      call fpend
      RETURN
      end



ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c squish string
c
      SUBROUTINE SQSHST(str,strsz)
        character*(*)  str
        integer        strsz
c
c remove leading and intermediate spaces and return the final size.
c
        integer i,j,n,s

        n = 0
        s = 1
        j = 1
        do 20 i = 1,LEN(str)
            if (str(i:i).ne.' ') then
                if (j.ne.i) then
                    str(j:j) = str(i:i)
                endif
                n=j
                j = j+1
                s = 0
            else
                s = s +1
                if (s.le.1) j = j+1
            endif
20     continue

       strsz=n
       return
       end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Tue Apr  3 09:54:16 PDT 1990 (dredge--stanford)
c
      BLOCK DATA BLOCK
      include 'p2conf.h'
c
c----------------------------------------------------------------------
c 
c     copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
c----------------------------------------------------------------------
c 
c                   common areas
c 
      include     'blank.h'
      include     'stat.h'
      include     'key.h'
      include     'logunt.h'
      include     'names.h'
      include     'symb.h'
      include     'setup.h'
      include     'sol.h'
      include     'plot.h'
      include     'impact.h'
      include     'symme.h'
      include     'photo.h'
      include     'trfldmob.h'
c****************************************************************** 
c 
c                   data area 
c
c...BLANK
      data lcpu,ldebug/2*.false./,nxymax/400/,
     +     maxdbl/1.d38/,mindbl/1.d-38/,maxexp/80.d0/,
     +     ndop/20/,ndreg/0/,expcut/41.d0/,excrit/1.d5/,nlump/0/,
     +     lgmaxd/87.498d0/,lgmind/-87.498d0/
      data isiact/.false./
c 
c...LOGUNT (lucom)
c...GENII will take a range of 4+NINCLS (make sure lugtmp is the
c...largest value used.
      data lutty/6/,  luout/7/,  luinf/7/, ludia/7/,  luplt/8/,
     +     lusup/9/,  lutmp/10/, lucpu/11/,lutmp2/12/,lulog/13/,
     +     luxtra/15/,lulog2/16/,
     +     lugtmp/17/
c
c...STAT (stcom)
      data lmshcd/.false./,lregcd/.false./,lelecd/.false./, 
     +     lxmscd/.false./,lymscd/.false./,ldopcd/.false./, 
     +     lmshdn/.false./,lsymdn/.false./,lmthdn/.false./,
     +     lmodcd/.false./,lbpt1 /.false./,lscale/.false./,
     +     lbicep/.false./,lmatcd/.false./,lminit/.false./
c
c...KEY (kycom)
      data ktitle/1/,kcomme/2/,kend/3/,kmesh/4/,kxmesh/5/,kymesh/6/,
     +     kregio/7/,kdopin/8/,kelect/9/,kprint/10/,
     +     ksymbo/11/,kmeth/12/,kmodel/13/,kmater/14/,ksolve/15/,
     +     kconta/16/,kplot1/17/,kplot2/18/,kcntur/19/, 
     +     kspred/20/,kintf/21/,kelim/22/,kload/23/,
     +     krgrid/24/,kvectp/25/,kxtrac/26/,
     +     kcheck/27/,klogj/28/,koptn/29/,kwindo/30/,kmcmsh/31/,
     +     kmcmat/32/,kmcsol/33/,kmcout/34/,kcompo/35/,keline/36/,
     +     kimpct/37/
c
c...NAMES (dcbco)
      data inpfil/'piscin.dat                              '/
c
c...SYMB (symco)
      data npdim/MAXPT/,nedim/MAXTRI/,nbdim/MAXCON/,
     +     iadim/MAXIA/,ildim/MAXILU/,iudim/MAXILU/,nadim/MAXIA/,
     +     mapdim/MAXMAP/,
     +     ipcdim/MAXEQN/,ixbdim/MAXEQN/,iydim/MAXIY/,
     +     adim/MAXADJ/,ludim/MAXLU/,
     +     mp1fn/MAXPT/,mp2fn/MAXPT/
c..  Gummel related
      data adimg/MAXAG/,ludimg/MAXLUG/,mpgum/MAXPT/
c
c...SETUP (setco)
      data lcntnu/.false./,bias/MAXCNT*0.d0/,lsol1/.false./,
     +     lsol2/.false./,temp/300./,mattyp/MAXREG*0/,epsmat/MAXREG*0./,
     +     nspar/MAXPAR/,semmat/MAXPAR*0./,workf/MAXCNT*0./,egap/0./,
     +     lboltz/.true./,boltzk/8.617e-5/,qcharg/1.602e-19/,
     +     eps0/8.854e-14/
      data ldiff/.false./,llogj/.false./,
     +     cresis/MAXCNT*0.d0/,ccapac/MAXCNT*0.d0/,
     +     lsrh,lauger,lconmb,lconm2,lfldmb,lconlt/6*.false./,
     +     schotk/MAXCNT*.false./,
     +     lresis/MAXCNT*.false./,lcnres/MAXCNT*.false./,
     +     lcurbc/MAXCNT*.false./,lflow/.false./,
     +     nintrf/0/,nintmx/MAXINF/,lbarl/MAXCNT*.false./,
     +     barla/MAXCNT*0.d0/,barlb/MAXCNT*0.d0/,lsrfmb/.false./
c
c...SOL (solco)
      data lpoiss/.false./,itmode/2/,laccel/.false./,nfact/0.5d0/,
     +     acstrt,acstop,acstep/.4,.6,.04/,dvlmt1/0.1d0/,
     +     dvlmt/-999./,amp/MAXCNT*0.d0/
      data namp/MAXCNT*0.d0,MAXCNT*0.d0/
      data ctolx0/1.d0/,acontn/5.d-1/,
     +     ptolg/1.d-26/,ctolg/5.d-18/,ptolx,ctolx/2*1.d-5/,
     +     lmultp/.true./,itlmt/15/,maxinn/40/,lucrit/1.d-3/,
     +     dvlmt2/1.d0/,gloops/-1/,cupdt/0.1d0/,mxiccg/25/,
     +     mudamp,ddamp,kdamp,fdamp,ldamp/1.d-12,0.5d0,0.d0,10.d0,10/,
     +     pdamp/2.d0/,stime,stime0/2*0.0/,lprntj/.false./,
     +     lxnorm/.true./,lgnorm/.false./,lcontn/.false./,nrloop/2/,
     +     lu1cri/3.d-3/,lu2cri/3.d-2/,lscl/.false./,
     +     lfixqf/.false./,nbias,pbias/2*0./,ldbug2/.false./,
     +     icgflg/.false./,ldjr/.false./,luauto/.false./,
     +     dvmax,dnmax,dpmax,gvmax,gnmax,gpmax/6*0.d0/,
     +     lrstrt/.true./,nback/6/,pscal/1.d0/,flcri/-1./,
     +     l2nd/.true./,ltauto/.true./,l2norm/.true./,
     +     timtol/5.d-3/,dtmin/1.d-25/,lexqf/.false./
c
c...PLOT (pltco)
      data xpwid0/3.000/,ypwid0/3.000/,idepln/2/,juncln/5/,ibndln/1/,
     +     igrdln/1/,ielcln/4/,initpl/.false./,ipendn/2/,
     +     lntmax/11/,rndoff/1e-5/,n1max/1000/,
     +     lrx/0./,lry/0./,linsid/.false./,
     +     iarchg/39/,iparea/38/,ncolor/10/,gpsize/102/,
     +     lxcomp,lycomp/2*.false./,
     +     palett/6,7,8,9,10,11,12,13,14,15,10*0/
c...... user requested values: set all these to 0.0 to get defaults
      data uxpwid/0.0/, uypwid/0.0/, uxoff/0.0/, uyoff/0.0/
c
c...Carrier-carrier scattering and IMPACT IONIZATION(impact)
      data labbas/.false./,limpct/.false./
c
c...SYMMETRICAL COORDINATION
      data lcyl/.false./
c...WIDTH SPECIFIED
      data lwidth/.false./,width/1.0/
c...Photogeneration(photo)
      data lphgen/.false./

      end
