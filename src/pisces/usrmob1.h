cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Tue Sep 11 03:26:16 PDT 1990 (anderson--stanford)
c-----------------------------------------------------------------------
c 
c     common for user-definable concentration-dependent mobility model
c            
c     remember NOT to mix character and other data within the same
c     data block.  If you need to user character data start a new
c     common data block.
c 
      common /user1/ ubn,ubp,ucn,ucp,uan,uap,ugn,ugp,uhn,uhp,
     +               umu1n,umu2n,umu1p,umu2p
c 
      double precision  ubn,ubp,ucn,ucp,uan,uap,ugn,ugp,uhn,uhp,
     +                  umu1n,umu2n,umu1p,umu2p
