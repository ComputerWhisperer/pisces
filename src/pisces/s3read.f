cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1988 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Sun Sep  2 21:54:00 PDT 1990 (anderson--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE S3READ(ftype,supfil,imptyp,inodes)
      include 'p2conf.h'
c
c--------------------------------------------------------------------
c
c     The following routine reads doping concentrations from a
c     SUPREM-III (or gen'l ascii) output file (supfil).  The value
c     of ftype indicates the type of file.  The index 
c     imptyp indicates which impurity is to be extracted.
c
c                  ftype  =  1  old suprem3 structure file
c                            2  suprem3 unformatted (binary) export
c                            3  suprem3 formatted (ascii) export
c                            4  gen'l ascii data file
c
c                  imptyp =  1  Boron
c                            2  Phosphorus
c                            3  Arsenic
c                            4  Antimony
c
c     Original : MRP         Stanford University          Feb, 1984
c     Revised  : MRP    (user defined ascii files)        Jan, 1985
c     Revised  : SEH    (add formatted export files)      Sep, 1988
c
c     Copyright c 1984 and 1988 by the Board of Trustees of the Leland
c                      Stanford Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
c-----------------------------------------------------------------------
c
c            common area
c
      include     'blank.h'
      include     'logunt.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'doptmp.h'
      integer TMPPAD(1282989)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c-----------------------------------------------------------------------
c
c            local declarations
c-----------------------------------------------------------------------
c
      logical ldum
      integer i,j,ftype,imptyp,idum,nlayrs,top(10),bot(10),inodes
      integer maxmat,maximp,maxlay,maxgrd,maxm1,ierr
      integer nimps,nnodes,impt(10),idximp,idxsub,matt(10)
      real rdum,ynow,yi,thick(10),endlyr
      character*20 supfil,adum
      character*2 fdatyp
c
c...Array bounds for SUPREM-III (if changed, remember to change 
c...array bounds in common/com.doptmp for consup and ysup)
      data maxmat/10/,maximp/4/,maxlay/10/,maxgrd/MAXSUP/
c-----------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
      nnodes=maxgrd
      if (ftype .eq. 2) goto 500
      if (ftype .eq. 3) goto 600
      if (ftype .eq. 4) goto 200
c
c------------------
c  OLD SUPREM-III
c------------------
c
c...Initialization
      maxm1=maxmat+1
c
c...Open/rewind file
      call fopcl(1,supfil,20,lusup,.false.,ierr)
      if(errflg) return
c
c...Read file type and code
      read(lusup,err=999,end=999) fdatyp
      read(lusup,err=999,end=999) idum
c
c...Read structural info
      read(lusup,err=999,end=999) nlayrs,(ldum,i=1,maxm1)
      read(lusup,err=999,end=999) (idum,idum,top(i),bot(i),
     +                             idum,rdum,rdum,rdum,rdum,rdum,
     +                             (rdum,rdum,j=1,maximp),
     +                             i=1,maxlay)
      read(lusup,err=999,end=999) (ldum,i=1,maximp)
      idxsub=1
c
c...Read grid-space and concentration array 
      read(lusup,err=999,end=999) (ysup(i),i=1,maxgrd)
      do 10 j=1,imptyp
10    read(lusup,err=999,end=999) (consup(i),i=1,maxgrd)
c
c...Close file and proceed
      call fopcl(0,supfil,20,lusup,.false.,ierr)
c
c...Calculate real depth array and shift data to tops of arrays.
c...Remember: SUPREM stores depth as increments, not actual location.
      inodes=bot(idxsub)-top(idxsub)
      j=top(idxsub)
      ynow=0.
      do 20 i=1,inodes
      yi=ynow
      consup(i)=consup(j)
      ynow=ynow+ysup(j)
      ysup(i)=yi*1.e-4
      j=j+1
20    continue
      j=inodes+1
      do 30 i=j,maxgrd
      ysup(i)=0.
      consup(i)=0.
30    continue
c
      return
c
c------------------
c  NEW SUPREM-III
c------------------
c...Export format (binary version)
c
c...Open/rewind file
500   call fopcl(1,supfil,20,lusup,.false.,ierr)
      if(errflg) return
c
c...Read no. layers, no. impurities and no. points
      read(lusup,err=999,end=999) nlayrs,nimps,nnodes
c
c...Read structure and find substrate layer index 
c...(layer code - 1=Si,2=SiO2,3=poly,4=Si3N4,5=Al)
      read(lusup,err=999,end=999) (matt(i),thick(i),top(i),i=1,nlayrs)
      read(lusup,err=999,end=999) (adum,i=1,nlayrs)
      read(lusup,err=999,end=999) (idum,rdum,i=1,nlayrs)
      do 501 idxsub=1,nlayrs
      if(matt(idxsub).eq.1) goto 502
501   continue
502   endlyr=thick(idxsub)
c
c...Read impurity code array and find impurity index
      read(lusup,err=999,end=999) (impt(i),i=1,nimps)
      do 505 idximp=1,nimps
      if(impt(idximp).eq.imptyp) goto 506
505   continue
c
c...More silly stuff
506   read(lusup,err=999,end=999) (adum,i=1,nimps)
      read(lusup,err=999,end=999) ((rdum,rdum,j=1,nimps),i=1,nlayrs)
c
c...Read grid space and conc array for desired impurity only
c...Note: for new fmt we read active (not chemical) conc.  Yeah!
      read(lusup,err=999,end=999) (ysup(i),i=1,nnodes)
      read(lusup,err=999,end=999) (rdum,i=1,nnodes)
      do 510 j=1,idximp
      read(lusup,err=999,end=999) (rdum,i=1,nnodes)
      read(lusup,err=999,end=999) (consup(i),i=1,nnodes)
510   continue
c
      goto 615
c.................................................................
c
c...Export format (ascii version)
c
c...Open/rewind file
600   call fopcl(2,supfil,20,lusup,.false.,ierr)
      if(errflg) return
c
c...Read no. layers, no. impurities and no. points
      read(lusup,*,err=999,end=999) nlayrs,nimps,nnodes
c
c...Read structure and find substrate layer index 
c...(layer code - 1=Si,2=SiO2,3=poly,4=Si3N4,5=Al)
      do 601 i=1,nlayrs
      read(lusup,*,err=999,end=999) adum
      read(lusup,*,err=999,end=999) matt(i),thick(i),top(i),idum,rdum
601   continue
      do 602 idxsub=1,nlayrs
      if(matt(idxsub).eq.1) goto 603
602   continue
603   endlyr=thick(idxsub)
c
c...Read impurity code array and find impurity index
      do 604 i=1,nimps
      read(lusup,*,err=999,end=999) adum
      read(lusup,*,err=999,end=999) impt(i)
604   continue
      do 605 idximp=1,nimps
      if(impt(idximp).eq.imptyp) goto 606
605   continue
c
c...More stuff
606   do 607 i=1,nlayrs
      read(lusup,*,err=999,end=999) (rdum,rdum,j=1,nimps)
607   continue
c
c...Read grid space and conc array for desired impurity only
c...Note: for new fmt we read active (not chemical) conc.  Yeah!
      do 610 i=1,nnodes
      read(lusup,*,err=999,end=999) ysup(i),rdum,(rdum,rdum,j=1,idximp)
      consup(i)=rdum
610   continue
c
c...Close file and proceed
615   call fopcl(0,supfil,20,lusup,.false.,ierr)
c
c...Calculate real depth array and shift data to tops of arrays.
      j=top(idxsub)
      ynow=0.
      inodes=0
620   if((ynow.gt.endlyr).or.(j.gt.nnodes)) goto 625
      inodes=inodes+1
      yi=ynow
      consup(inodes)=consup(j)
      ynow=ynow+ysup(j)
      ysup(inodes)=yi*1.e-4
      j=j+1
      goto 620
c
625   j=inodes+1
      do 630 i=j,maxgrd
      ysup(i)=0.
      consup(i)=0.
630   continue
c
      return
c
c---------------
c  ASCII INPUT
c---------------
c
c...Open/rewind file
200   call fopcl(2,supfil,20,lusup,.false.,ierr)
      if(errflg) return
c
c...Read depth and concentration
      i=1
210   read(lusup,*,err=998,end=220) ysup(i),consup(i)
      i=i+1
      goto 210
220   inodes=i-1
c
c...Close file and proceed
      call fopcl(0,supfil,20,lusup,.false.,ierr)
c
c...Shift top to 0.
      ynow=ysup(1)
      do 120 i=1,inodes
      ysup(i)=(ysup(i)-ynow)*1.e-4
120   continue
      j=inodes+1
      do 130 i=j,maxgrd
      ysup(i)=0.
      consup(i)=0.
130   continue
c
      return
c
c...Sorry, error detected on read
998   call erset(281,linum,0)
      return
999   call erset(291,linum,0)
      return
      end
