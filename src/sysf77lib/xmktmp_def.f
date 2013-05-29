Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
C    Copyright c 1988 The Board of Trustees of the Leland Stanford
C    Junior University. All rights reserved.  This routine may not
C    be used without the prior written consent of Stanford University.
C
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc



C =======================================================================
C  "XMKTMP": Return a unique file name.
C 
C   Usage:
C      call xmktmp(str, LEN(str))
C 
C   Notes:
C      + Format is set here.  The user program need not know
C 	 what form we are using.
C      + We just increment an integer, cat it together with a
C        file template, and make sure the file does note exist.
C        If it does, repeat.
C 
C   Original: Michael Eldredge -- Stanford University (may 88)

c -----------------------------------------------------------------------
      SUBROUTINE XMKTMP(str, strln)
      character*(*)  str
      integer        strln

      integer  i,j
      logical  lexist
      character*(20) cbuf
      integer  cnt
      data     cnt/0/

c...build a file name
10    cnt = cnt + 1
      write(cbuf, 11) cnt
11    format(i)
c
c...delimit numeric string.
      do 20 i = 1,LEN(cbuf)
	 if (cbuf(i:i).ne.' ') goto 21
20    continue
21    do 24 j = i,LEN(cbuf)
	 if (cbuf(j:j).eq.' ') goto 25
24    continue
25    j = j - 1
c
c...build the (possible) file name.
C ========== FILE NAME TEMPLATE ==========
      str = 'p2b' // cbuf(i:j) // '.tmp'
c
      inquire(file=str, exist=lexist, err=99)
      if (lexist) goto 10
c
99    continue
      return
      end
