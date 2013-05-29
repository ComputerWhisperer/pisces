c Wed Sep 13 13:44:11 PDT 1989 (dredge--stanford)
c-----------------------------------------------------------------------
c   Copyright 1980 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c-----------------------------------------------------------------------
c
c These routines are all used to set arrays (buffers) to some given
c  value.  They are all the same, except one is for real values, one
c   for logical and one for character.
c
c written: Michael Eldredge (apr 83)
c----------------------------------------------------------------------

      subroutine csetv(cbuf,leng,cval)
        character*(*)   cbuf
        character    cval
        integer      leng
c ----
      integer      i

c set a character buffer to cval.

      do 10 i=1,leng
10    cbuf(i:i)=cval

      return
      end

      subroutine rsetv(rbuf,leng,rval)
      integer leng
      real    rbuf(leng),rval

c ---
      integer i
c set a real buffer to 'rval'.

      do 10 i=1,leng
10    rbuf(i)=rval

      return
      end

      subroutine lsetv(lbuf,leng,lval)
      integer leng
      logical lbuf(leng),lval

c ---
      integer i

c set a logical buffer to 'lval'.

      do 10 i=1,leng
10    lbuf(i)=lval

      return
      end
