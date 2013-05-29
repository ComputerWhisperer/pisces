cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fri Mar  9 14:29:46 PST 1990 (dredge--stanford)
      SUBROUTINE INCNM(name)
      character*(*) name 
c---------------------------------------------------------------------
c 
c     incnm : Increments an ascii string of up to LEN(name)
c             characters by 1 
c             character, starting with the last character and possibly
c             extending the incrementation to characters on the left
c             of the last character.
c             The characters are incremented using the sequence 0-9,a-z.
c             when a character is incremented beyond "z" the character
c             is set to "0" and the next character to the left is 
c             incremented.
c 
c     copyright c 1984 The Board of Trustees of Leland Stanford
c                      Junior University. all rights reserved.
c     this subroutine may not be used outside of the pisces computer
c     program without the prior written consent of Stanford University. 
c 
c 
c---------------------------------------------------------------------
c 
c     local variables 
c 
c---------------------------------------------------------------------
      integer i 
      logical flag
c 
c---------------------------------------------------------------------
c 
c     start of incnm
c 
c---------------------------------------------------------------------
c.....find the last non-blank character in the name
      do 100 i=LEN(name),1,-1
      if (name(i:i).ne.' ') goto 200
100     continue
c.....check for special characters in last digit and handle their  
c.....incrementing.
200   flag = .true.
10    if (.not.flag) goto 20
      flag = .false.
      if (name(i:i).eq.'z') then
        name(i:i) = '0'
        flag = .true.
        else if (name(i:i).eq.'Z') then
             name(i:i) = 'a'
        else if (name(i:i).eq.'9') then
             name(i:i) = 'A'
        else 
          name(i:i) = char(ichar(name(i:i))+1)
        endif
        i = i - 1
      if (i.eq.0) flag =.false.
        goto 10
20      continue
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE EXTNM(name,namett)
      character*(*) name,namett 
c---------------------------------------------------------------------
c 
c     extnm : adds the string 'tt' to a mesh file name, to be used
c             by regrid to store the triangle tree.
c 
c     copyright c 1984 The Board of Trustees of Leland Stanford
c                      Junior University. all rights reserved.
c     this subroutine may not be used outside of the pisces computer
c     program without the prior written consent of Stanford University. 
c 
c 
c---------------------------------------------------------------------
c 
c     local variables 
c 
c---------------------------------------------------------------------
      integer i,j
c 
c---------------------------------------------------------------------
c 
c     start of extnm
c 
c---------------------------------------------------------------------
c
c.....copy it over
      do 50 j=1,LEN(name)
   50    namett(j:j)=name(j:j)

c.....find the last non-blank character in the name
      do 100 i=20,1,-1
      if (name(i:i).ne.' ') goto 200
  100 continue
c
c......Extend the name unless it's too long, in which case increment it.
  200 continue
      if (i.lt.19) then
       j=i+1
       namett(j:j)='t'
       j=j+1
       namett(j:j)='t'
      else
       call incnm(namett)
      endif
c
c...... Wish everything was so easy...
      return
      end 
