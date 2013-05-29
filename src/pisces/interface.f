cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Tue May 30 16:00:38 PDT 1989 (dredge--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE INTFCK
      include 'p2conf.h'
c 
c     This routine gets the info off the interface card.
c
c     Possible parameters are:
c          fixed charge (cm-2)
c          interface recombination velocity (electrons)
c          interface recombination velocity (holes)
c
c     Original:  MRP  May,1985
c 
c     copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. all rights reserved.
c     this subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      include     'blank.h'
      include     'setup.h'
c 
c FUNCTIONS:
      logical isrval
      real    gtrval
c
c------------------------------------------------------------------
c*************
c**  Start  **
c*************
c
c...Increment interface index.  
c...Don't allow more than we can store.
      nintrf=nintrf+1
      if(nintrf.lt.nintmx) goto 10
      call erset(196,linum,nintmx)
      return
c 
c...Get limits & convert to centimeters
10    call devlts(ssdata(nintrf,1),ssdata(nintrf,2),
     +            ssdata(nintrf,3),ssdata(nintrf,4))
      if(isrval(1)) ssdata(nintrf,1)=gtrval(1)*1.e-4
      if(isrval(2)) ssdata(nintrf,2)=gtrval(2)*1.e-4
      if(isrval(3)) ssdata(nintrf,3)=gtrval(3)*1.e-4
      if(isrval(4)) ssdata(nintrf,4)=gtrval(4)*1.e-4
c 
c...Get parameters (5=qss,6=sn,7=sp)
      ssdata(nintrf,5)=0.
      ssdata(nintrf,6)=0.
      ssdata(nintrf,7)=0.
      if(isrval(5)) ssdata(nintrf,5)=gtrval(5)
      if(isrval(6)) ssdata(nintrf,6)=gtrval(6)
      if(isrval(7)) ssdata(nintrf,7)=gtrval(7)
c
c...Bye
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE SETQSS
      include 'p2conf.h'
c
c     Set interface parameters.
c
c     Original: Mark R. Pinto   Stanford University  May,1985
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'setup.h'
      include     'emaco.h'
c
c------------------------------------------------------------------
      integer i,j
      real xx,yy
c------------------------------------------------------------------
c
c...Assign values.
c...Dont forget to scale qss by dcscl.
      do 100 i=1,np
         xx=cord(1,i)
         yy=cord(2,i)
         qss(i)=0.
c        snintf(i)=ssdata(j,6)
c        spintf(i)=ssdata(j,7)
c...next 3 lines added by mje.  It looks like some bad arrary
c....refs have been happening here.
         snintf(i)=0.
         spintf(i)=0.
         if (nintrf.le.0) goto 100
         do 200 j=1,nintrf
            if((xx.ge.ssdata(j,1)).and.(xx.le.ssdata(j,2)).and.
     +         (yy.ge.ssdata(j,3)).and.(yy.le.ssdata(j,4))) then
               qss(i)=ssdata(j,5)*dcscli
               snintf(i)=ssdata(j,6)
               spintf(i)=ssdata(j,7)
            endif
200      continue
100   continue
c 
c...Done
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE INTFLN
      include 'p2conf.h'
c 
c     date code 10/14/83 CSR
c     mod        6/30/85 MRP
c
c     Calculate the interface length assoc. with each node. 
c 
c     copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. all rights reserved.
c     this subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      include     'blank.h'
      include     'setup.h'
      include     'emaco.h'
c
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'doptmp.h'
      integer TMPPAD(1282989)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
      integer ie,nel,j,jp,nd,tmod,offset
c------------------------------------------------------------------
c 
c*************
c**  Start  **
c*************
c
c...Initialize
      do 10 j=1,np
10       dintf(j)=0.
c 
c...Indirect approach : scan triangles (insulator only)
c...and make contributions when nodes are in window.
      do 500 ie=1,ne
      if (mattyp(imat(ie)).gt.0) goto 500
c
      do 499 j=1,3
         nel=nextel(j,ie)
         if(nel.le.0) goto 499
         if(mattyp(imat(nel)).lt.0) goto 499
c
c...so far, ie is oxide and neighbour j of ie is semi
c...  => nop(j+1,ie) and nop(j+2,ie) are on interface
         do 200 offset=1,2
            jp=tmod(j+offset)
            nd=nop(jp,ie)
            if(tsides(j,ie).eq.0.) write(6,*) 'Lost lengths somewhere!'
            dintf(nd)=dintf(nd)+0.5*tsides(j,ie)
200      continue

499   continue
500   continue
c 
c...Done
      return
      end 
