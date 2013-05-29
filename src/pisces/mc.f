cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fri Mar  9 14:31:59 PST 1990 (dredge--stanford)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE INITPM
c
c     Initialize the constants for PIACES-MC
c
c     Original : D. Y. Cheng    Stanford University       Oct, 1986
c
c     Copyright c 1981 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
c                   common areas
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE WINDCK
c
c     Get parameters from the WINDOW card
c
c     Original : D. Y. Cheng    Stanford University       Oct, 1986
c
c     Copyright c 1981 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE MCMSHC
c
c     Get parameters from the MCMESH card
c
c     Original : D. Y. Cheng    Stanford University       Oct, 1986
c
c     Copyright c 1981 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE MCSOLC
c
c     Get parameters from the MCSOLVE card
c
c     Original : D. Y. Cheng    Stanford University       Oct, 1986
c
c     Copyright c 1981 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE MCMATC
c
c     Get parameters from the MCMATERL card
c
c     Original : D. Y. Cheng    Stanford University       Oct, 1986
c
c     Copyright c 1981 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE MCOUTC
c
c     Get parameters from the MCOUTPUT card
c
c     Original : D. Y. Cheng    Stanford University       Oct, 1986
c
c     Copyright c 1981 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
      return
      end
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
Cmje: called in solver.F, but commented out. So comment it out
Cmje: here.
Cc
C      SUBROUTINE WATCH (tbias)
C      double precision tbias(*)
Cc
Cc     Monitor the E field in the device. If it is too high,
Cc     switch to Monte Carlo.
Cc
Cc     Original D. Y. Cheng     Stanford University     Jun, 1986
Cc
Cc     Copyright c 1981 The board of trustees of the Leland Stanford 
Cc                      Junior University.  All rights reserved.
Cc     This subroutine may not be used outside of the PISCES computer
Cc     program without the prior written consent of Stanford University. 
Cc
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
Cc
Cc                   start 
C      return
C      end
