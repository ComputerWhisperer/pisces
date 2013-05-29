      subroutine cpack(strn1,strn2,nchar)
        integer nchar
        character*(*) strn1, strn2
c Wed Sep 13 09:37:06 PDT 1989 (dredge--stanford)
c-----------------------------------------------------------------------
c   Copyright 1980 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c-----------------------------------------------------------------------
c
c   WRITTEN BY: Michael J. Eldredge                      DATE: 01-Jul-83
c   MODIFICATIONS:
c   BY:  <name>                                          DATE: dd-mmm-yy
c
c   NAME: cpack
c   FUNCTION: Takes the STRN1 character string and copies it into the
c             STRN2 character string.
c
c   TYPE OF ROUTINE:  SUBROUTINE
c   CALLING SEQUENCE:
c    call cpack(strn1,strn2,nchar)
c
c   PARAMETERS (INPUT):
c    strn1*(*)     char     Input character string.
c    nchar         integer  The number of characters to move.
c
c   PARAMETERS (OUTPUT):
c    strn2*(*)     char     Output character string.
c
c   ERROR CONDITIONS: (None.)
c
c   ROUTINES USED: (None.)
c   INCLUDE FILES: (None.)
c
c   NOTES
c     The version of f77 on UNIX uses a slightly different sub-string
c     delimitations vs ANSI standard Fortran-77. As:
c     UNIX f77:   strng(char1 : nchars)    # first and number of chars.
c     ANSI    :   strng(char1 : char2 )    # first and last
c
c-----------------------------------------------------------------------
c   start of cpack
c-----------------------------------------------------------------------
c.. pack it in..
c... (boy is this simple!!)
      strn2(1:nchar) = strn1(1:nchar)
c
      return
      end
