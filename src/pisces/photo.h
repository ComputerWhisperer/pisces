cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c#######################################################################
c
c     Common block /PHOTO/
c
c     8/89 G. Anderson
c	based on work by K. Kosai at Hughes SBRC
c     Variables for calculation of photogeneration.  The light is
c     assumed to be a plane wave travelling in x direction,
c     and uniformly illuminating the device.  The photogeneration rate 
c     is calculated in SUBROUTINE RECOMB as
c
c       	Gphot(x,y) = flux*abs_coeff*EXP(-abs_coef*ABS(pos))
c
c     where pos is y coefficient of current node; photogeneration
c     decays exponentially from y=0 as photons penetrate deeper into
c     the device.  At all locations where y is negative, 
c     the photogeneration rate is zero.  All variables are entered on
c     the MODELS card, and may be changed at any time.
c
c#######################################################################
c
c     1  abscof,  ! Absorption coefficient in 1/cm. Default = 1.0e12
c     2  flux,      ! Incident photon flux in 1/(cm^2 sec).  Multiplied
c                    by dcsli*qkt to scale.  This is the flux at the
c                    plane of incidence defined by pgorig.  Default = 1.0e3
c     
c      LOGICAL
c     1  lphgen,    ! = .TRUE. if photogeneration is to be calculated in
c                    subroutine RECOMB.
c
      REAL abscof, flux    
c
      LOGICAL lphgen
c
      COMMON /PHOTO/ abscof, flux, lphgen
