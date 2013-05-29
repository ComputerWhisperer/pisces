ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Tue Sep 11 03:26:16 PDT 1990 (anderson--stanford)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      REAL FUNCTION USRMOB1(hflag,conc1,temper)
c
c     User definable concentration-dependent mobility model    
c
c-----this example: new mobility model, Ref.: Arora, Hauser, Roulston,
c                               IEEE ED-29,pp 292-295
c
c     Analytic function to calculate low-field mobility based on
c     total impurity concentration and temperature.
c
c              hflag......true if holes
c              conc1......total concentration
c              temper.....temperature
c
c     
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include    'usrmob1.h'
c
c--Passed parameters
c
      logical hflag
      real conc1,temper
c
c--Local variables
c
      real tmp,den,ualpha
c
c
      tmp=temper/300.
      if(hflag) then
         ualpha=0.88*TMP**uhp
         den=1.+abs((conc1/(ucp*TMP**ugp)))**ualpha
         usrmob1=umu1p*(tmp**uap) + umu2p*(tmp**ubp)/den
      else
         ualpha=0.88*TMP**uhn
         den=1.+abs((conc1/(ucn*tmp**ugn)))**ualpha
         usrmob1=umu1n*(tmp**uan) + umu2n*(tmp**ubn)/den
      endif
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE INITUM1
c
c     Initialize user-definable mobility model parameters
c
c-----this example: new mobility model, Ref.: Arora, Hauser, Roulston,
c                               IEEE ED-29,pp 292-295
c
c     Analytic function to calculate low-field mobility based on
c     total impurity concentration and temperature.
c     
c     This subroutine is called from the procedure which reads parameters
c     on the "models" card whenever the flag "luser1" is set.  It is
c     intended to be the place where the user can get his/her own input
c     parameters from the "models" statement and use them to control
c     the initialization of the internal model parameters.
c
c     The isrval(#) function checks for the presence of numerical parameter 
c     number # on the models card. 
c     The islval(#) function checks for the presence of logical parameter 
c     number # on the models card. 
c     The gtrval(#) function returns the value of numerical parameter 
c     number # on the models card. 
c     The gtlval(#) function returns the value of logical parameter 
c     number # on the models card. 
c
c     Parameter numbers are associated with parameter names as defined
c     in the PISCES key file "pisc.key".  Each unique parameter name
c     must have its own number; if two names have the same number, they
c     are treated as synonyms by the parser.
c
c     The default names "parm1" etc. in the keyfile are generic.  The
c     user may change them to more meaningful names as desired.
c
      include    'usrmob1.h'
c
c-----Function declarations
c
      logical isrval, islval, gtlval
      real    gtrval
c
c-----Data initialization.  Variables are declared in the "include" file
c
      data uan,ubn,ucn,ugn,uhn/ -0.57, -2.33, 1.26e17, 2.4, -0.146/,
     +     uap,ubp,ucp,ugp,uhp/ -0.57, -2.23, 2.35e17, 2.4, -0.146/,
     +     umu1n,umu2n,umu1p,umu2p/ 88., 1252., 54.3, 407./
c
      if (isrval(15)) umu1n=gtrval(15)
      if (isrval(16)) umu2n=gtrval(16)
      if (isrval(17)) umu1p=gtrval(17)
      if (isrval(18)) umu2p=gtrval(18)
c
      return
      end
