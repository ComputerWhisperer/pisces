cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fri Mar  9 14:33:13 PST 1990 (dredge--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
        SUBROUTINE  MD(n, ia,ja, iysiz, max, v,l,
     +                 head,last,next, mark, flag)
c
c  description
c
c    md finds a minimum degree ordering of the rows and columns of a
c    symmetric matrix m stored in (ia,ja,a) format.
c
c
c  additional parameters
c
c    max  - declared dimension of the one-dimensional arrays v and l;
c           max must be at least  n+2k,  where k is the number of
c           nonzeroes in the strict upper triangle of m
c
c    iysiz- actual size of v and l (added by MRP)
c
c    v    - integer one-dimensional work array;  dimension = max
c
c    l    - integer one-dimensional work array;  dimension = max
c
c    head - integer one-dimensional work array;  dimension = n
c
c    last - integer one-dimensional array used to return the permutation
c           of the rows and columns of m corresponding to the minimum
c           degree ordering;  dimension = n
c
c    next - integer one-dimensional array used to return the inverse of
c           the permutation returned in last;  dimension = n
c
c    mark - integer one-dimensional work array (may be the same as v);
c           dimension = n
c
c    flag - integer error flag;  values and their meanings are -
c             0      no errors detected
c             11n+1  insufficient storage in md
c
c
c  definitions of internal parameters
c
c    ---------+---------------------------------------------------------
c    v(s)     z value field of list entry
c    ---------+---------------------------------------------------------
c    l(s)     z link field of list entry  (0 => end of list)
c    ---------+---------------------------------------------------------
c    l(vi)    z pointer to element list of uneliminated vertex vi
c    ---------+---------------------------------------------------------
c    l(ej)    z pointer to boundary list of active element ej
c    ---------+---------------------------------------------------------
c    head(d)  z vj => vj head of d-list d
c             z  0 => no vertex in d-list d
c
c
c             z                  vi uneliminated vertex
c             z          vi in ek           z       vi not in ek
c    ---------+-----------------------------+---------------------------
c    next(vi) z undefined but nonnegative   z vj => vj next in d-list
c             z                             z  0 => vi tail of d-list
c    ---------+-----------------------------+---------------------------
c    last(vi) z (not set until mdp)         z -d => vi head of d-list d
c             z-vk => compute degree        z vj => vj last in d-list
c             z ej => vi prototype of ej    z  0 => vi not in any d-list
c             z  0 => do not compute degree z
c    ---------+-----------------------------+---------------------------
c    mark(vi) z mark(vk)                    z nonnegative tag < mark(vk)
c
c
c             z                   vi eliminated vertex
c             z      ei active element      z           otherwise
c    ---------+-----------------------------+---------------------------
c    next(vi) z -j => vi was j-th vertex    z -j => vi was j-th vertex
c             z       to be eliminated      z       to be eliminated
c    ---------+-----------------------------+---------------------------
c    last(vi) z  m => size of ei = m        z undefined
c    ---------+-----------------------------+---------------------------
c    mark(vi) z -m => overlap count of ei   z undefined
c             z       with ek = m           z
c             z otherwise nonnegative tag   z
c             z       < mark(vk)            z
c
c-----------------------------------------------------------------------
c
        integer  ia(1)
        integer  v(1), l(1),  head(1), last(1), next(1),
     *     mark(1),  flag,  tag, dmin, vk,ek, tail, n, max, k, iysiz
        INTeger   ja(1)
        equivalence  (vk,ek)
c
c----initialization
        tag = 0
        call  mdi
     *     (n, ia,ja, max,v,l, head,last,next, mark,tag, flag, iysiz)
        if (flag.ne.0)  return
c
        k = 0
        dmin = 1
c
c----while  k < n  do
   1    if (k.ge.n)  go to 4
c
c------search for vertex of minimum degree
   2      if (head(dmin).gt.0)  go to 3
            dmin = dmin + 1
            go to 2
c
c------remove vertex vk of minimum degree from degree list
   3      vk = head(dmin)
          head(dmin) = next(vk)
          if (head(dmin).gt.0)  last(head(dmin)) = -dmin
c
c------number vertex vk, adjust tag, and tag vk
          k = k+1
          next(vk) = -k
          last(ek) = dmin - 1
          tag = tag + last(ek)
          mark(vk) = tag
c
c------form element ek from uneliminated neighbors of vk
          call  mdm
     *       (vk,tail, v,l, last,next, mark)
c
c------purge inactive elements and do mass elimination
          call  mdp
     *       (k,ek,tail, v,l, head,last,next, mark)
c
c------update degrees of uneliminated vertices in ek
          call  mdu
     *       (ek,dmin, v,l, head,last,next, mark)
c
          go to 1
c
c----generate inverse permutation from permutation
   4    do 5 k=1,n
          next(k) = -next(k)
   5      last(next(k)) = k
c
        return
        end
  


c------------------------------------------------------------------
        SUBROUTINE  MDI(n, ia,ja, max,v,l,
     +                 head,last,next, mark,tag, flag, sfs)
        integer  ia(1), v(1), l(1),  head(1), last(1), next(1),
     *     mark(1), tag,  flag,  sfs, vi,dvi, vj, n, j, 
     *     jmin, jmax, max
        INTeger   ja(1)
c
c----initialize degrees, element lists, and degree lists
        do 1 vi=1,n
          mark(vi) = 1
          l(vi) = 0
   1      head(vi) = 0
        sfs = n+1
c
c----create nonzero structure
c----for each nonzero entry a(vi,vj) in strict upper triangle
        do 3 vi=1,n
          jmin = ia(vi)
          jmax = ia(vi+1) - 1
          if (jmin.gt.jmax)  go to 3
          do 2 j=jmin,jmax
            vj = ja(j)
            if (vi.ge.vj)  go to 2
              if (sfs.ge.max)  go to 101
c
c------enter vj in element list for vi
              mark(vi) = mark(vi) + 1
              v(sfs) = vj
              l(sfs) = l(vi)
              l(vi) = sfs
              sfs = sfs+1
c
c------enter vi in element list for vj
              mark(vj) = mark(vj) + 1
              v(sfs) = vi
              l(sfs) = l(vj)
              l(vj) = sfs
              sfs = sfs+1
   2        continue
   3      continue
c
c----create degree lists and initialize mark vector
        do 4 vi=1,n
          dvi = mark(vi)
          next(vi) = head(dvi)
          head(dvi) = vi
          last(vi) = -dvi
          if (next(vi).gt.0)  last(next(vi)) = vi
   4      mark(vi) = tag
c
        return
c
c ** error -- insufficient storage
 101    flag = 9*n + vi
        return
        end
  


        SUBROUTINE  MDM(vk,tail, v,l, last,next, mark)
        integer  vk, tail,  v(1), l(1),   last(1), next(1),   mark(1),
     *     tag, s,ls,vs,es, b,lb,vb, blp,blpmax
        equivalence  (vs, es)
c
c----initialize tag and list of uneliminated neighbors
        tag = mark(vk)
        tail = vk
c
c----for each vertex/element vs/es in element list of vk
        ls = l(vk)
   1    s = ls
        if (s.eq.0)  go to 5
          ls = l(s)
          vs = v(s)
          if (next(vs).lt.0)  go to 2
c
c------if vs is uneliminated vertex, then tag and append to list of
c------uneliminated neighbors
            mark(vs) = tag
            l(tail) = s
            tail = s
            go to 4
c
c------if es is active element, then ...
c--------for each vertex vb in boundary list of element es
   2        lb = l(es)
            blpmax = last(es)
            do 3 blp=1,blpmax
              b = lb
              lb = l(b)
              vb = v(b)
c
c----------if vb is untagged vertex, then tag and append to list of
c----------uneliminated neighbors
              if (mark(vb).ge.tag)  go to 3
                mark(vb) = tag
                l(tail) = b
                tail = b
   3          continue
c
c--------mark es inactive
            mark(es) = tag
c
   4      go to 1
c
c----terminate list of uneliminated neighbors
   5    l(tail) = 0
c
        return
        end
  


c------------------------------------------------------------------
        SUBROUTINE  MDP(k,ek,tail, v,l, head,last,next, mark)
        integer  ek, tail,  v(1), l(1),  head(1), last(1), next(1),
     *     mark(1),  tag, free, li,vi,lvi,evi, s,ls,es, ilp,ilpmax,
     *     k, i
c
c----initialize tag
        tag = mark(ek)
c
c----for each vertex vi in ek
        li = ek
        ilpmax = last(ek)
        if (ilpmax.le.0)  go to 12
        do 11 ilp=1,ilpmax
          i = li
          li = l(i)
          vi = v(li)
c
c------remove vi from degree list
          if (last(vi).eq.0)  go to 3
            if (last(vi).gt.0)  go to 1
              head(-last(vi)) = next(vi)
              go to 2
   1          next(last(vi)) = next(vi)
   2        if (next(vi).gt.0)  last(next(vi)) = last(vi)
c
c------remove inactive items from element list of vi
   3      ls = vi
   4      s = ls
          ls = l(s)
          if (ls.eq.0)  go to 6
            es = v(ls)
            if (mark(es).lt.tag)  go to 5
              free = ls
              l(s) = l(ls)
              ls = s
   5        go to 4
c
c------if vi is interior vertex, then remove from list and eliminate
   6      lvi = l(vi)
          if (lvi.ne.0)  go to 7
            l(i) = l(li)
            li = i
c
            k = k+1
            next(vi) = -k
            last(ek) = last(ek) - 1
            go to 11
c
c------else ...
c--------classify vertex vi
   7        if (l(lvi).ne.0)  go to 9
              evi = v(lvi)
              if (next(evi).ge.0)  go to 9
                if (mark(evi).lt.0)  go to 8
c
c----------if vi is prototype vertex, then mark as such, initialize
c----------overlap count for corresponding element, and move vi to end
c----------of boundary list
                  last(vi) = evi
                  mark(evi) = -1
                  l(tail) = li
                  tail = li
                  l(i) = l(li)
                  li = i
                  go to 10
c
c----------else if vi is duplicate vertex, then mark as such and adjust
c----------overlap count for corresponding element
   8              last(vi) = 0
                  mark(evi) = mark(evi) - 1
                  go to 10
c
c----------else mark vi to compute degree
   9              last(vi) = -ek
c
c--------insert ek in element list of vi
  10        v(free) = ek
            l(free) = l(vi)
            l(vi) = free
  11      continue
c
c----terminate boundary list
  12    l(tail) = 0
c
        return
        end
  


c------------------------------------------------------------------
        SUBROUTINE  MDU(ek,dmin, v,l, head,last,next, mark)
        integer  ek, dmin,  v(1), l(1),  head(1), last(1), next(1),
     *     mark(1),  tag, vi,evi,dvi, s,vs,es, b,vb, ilp,ilpmax,
     *     blp,blpmax, i
        equivalence  (vs, es)
c
c----initialize tag
        tag = mark(ek) - last(ek)
c
c----for each vertex vi in ek
        i = ek
        ilpmax = last(ek)
        if (ilpmax.le.0)  go to 11
        do 10 ilp=1,ilpmax
          i = l(i)
          vi = v(i)
          if (last(vi))  1, 10, 8
c
c------if vi neither prototype nor duplicate vertex, then merge elements
c------to compute degree
   1        tag = tag + 1
            dvi = last(ek)
c
c--------for each vertex/element vs/es in element list of vi
            s = l(vi)
   2        s = l(s)
            if (s.eq.0)  go to 9
              vs = v(s)
              if (next(vs).lt.0)  go to 3
c
c----------if vs is uneliminated vertex, then tag and adjust degree
                mark(vs) = tag
                dvi = dvi + 1
                go to 5
c
c----------if es is active element, then expand
c------------check for outmatched vertex
   3            if (mark(es).lt.0)  go to 6
c
c------------for each vertex vb in es
                b = es
                blpmax = last(es)
                do 4 blp=1,blpmax
                  b = l(b)
                  vb = v(b)
c
c--------------if vb is untagged, then tag and adjust degree
                  if (mark(vb).ge.tag)  go to 4
                    mark(vb) = tag
                    dvi = dvi + 1
   4              continue
c
   5          go to 2
c
c------else if vi is outmatched vertex, then adjust overlaps but do not
c------compute degree
   6        last(vi) = 0
            mark(es) = mark(es) - 1
   7        s = l(s)
            if (s.eq.0)  go to 10
              es = v(s)
              if (mark(es).lt.0)  mark(es) = mark(es) - 1
              go to 7
c
c------else if vi is prototype vertex, then calculate degree by
c------inclusion/exclusion and reset overlap count
   8        evi = last(vi)
            dvi = last(ek) + last(evi) + mark(evi)
            mark(evi) = 0
c
c------insert vi in appropriate degree list
   9      next(vi) = head(dvi)
          head(dvi) = vi
          last(vi) = -dvi
          if (next(vi).gt.0)  last(next(vi)) = vi
          if (dvi.lt.dmin)  dmin = dvi
c
  10      continue
c
  11    return
        end

