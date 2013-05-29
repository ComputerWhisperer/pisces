cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Tue Sep 11 03:26:16 PDT 1990 (anderson--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE ERITE
      include 'p2conf.h'
c     date code: mar. 17, 1980
c---------------------------------------------------------------------
c 
c     erite outputs all of the currently logged errors in the err 
c     array.
c 
c     Original : Stephen E. Hansen                       Aug, 1979
c     Revision : MRP                                     Mar, 1984
c 
c     Copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c---------------------------------------------------------------------
c 
c     common area 
c 
      include     'logunt.h'
      include     'blank.h'
      include     'eritco.h'
c
c---------------------------------------------------------------------
c 
c     local variables 
c 
c---------------------------------------------------------------------
      integer i,index,lu,numer,line,ierval,nerrmx,errmin,errmax,j
      character*140 cherr
      data nerrmx/20/ , errmin/-330/ , errmax/330/
c---------------------------------------------------------------------
c 
c     start of erite
c 
c---------------------------------------------------------------------
      index=0 
c......this is the start of the loop.  index will point to the next 
c......location in the err array and the error and line number, along 
c......with an optional parameter are extracted.
10010 index=index+1 
c 
c......if we've gone through the entire err array then let's quit.
      if(index.gt.nerrmx) goto 10600
c 
c......get the error/warning number.
      numer=err(1,index)
c 
c......if the value of the current location in err is zero, then we've
c......output the last of the errors detected.
      if(numer.eq.0) goto 10600 
c 
c......if error number was not zero, then get the line number and the 
c......optional parameter.
      line =err(2,index)
      ierval=err(3,index) 
c 
c......set lu to the output lu #. 
      lu=luout
c 
c......check if this is to be a warning or an error.
10020 if(numer.gt.0) goto 10030 
c 
c......since it is negative, this is only a warning.
      write(lu,10121) line
      goto 10100
10121 format(/' ** Warning in line #',i4) 
c 
c......since it was greater than zero, this is a deadly error.
10030 write(lu,10131) line
10131 format(/' ** Error in line #',i4) 
c 
c......next, check to see that the error or warning number is inside
c......the valid range. 
10100 if(numer.ge.errmin .and. numer.le.errmax) goto 10200
c 
c......oops!  it wasn't.  somebody goofed.
      write(lu,10101) numer 
      goto 10500
10101 format(//' Invalid error number detected!', 
     +         '  The error number was ',i12/ 
     +         ' Please notify the person responsible for the', 
     +         ' maintenance of this program.') 
c 
c---------------------------------------------------------------------
c 
c      now, depending on the error number, let's output the appropriate 
c      error or warning message.
c 
c---------------------------------------------------------------------
c......first, since it might be a negative warning number, we must
c......take the absolute value of numer.
10200 numer = iabs(numer)
      call inita(numer,cherr)
      if (ierval.ne.0) then
      write(lu,cherr) ierval
      else
      write(lu,cherr) numer
      endif
c 
c......check to see if we've sent this error message to the tty yet.
10500 if(lu.eq.lutty) goto 10010
c 
c......nope, let's do it now. 
      lu=lutty
      goto 10020
c......all done. clear warning flag and error array. 
c......(Hey Steve! you forgot to reset the pointer!!!)
10600 wrnflg=.false.
      do 10610 i=1,index-1
      do 10610 j=1,3
      err(j,i)=0
10610 continue
      indxer=0
c......bye, bye!!!
      return
      end
c 
c
      SUBROUTINE INITA(numer,cr)
      character*140 cr
      integer numer,i
c---------------------------------------------------------------------
c 
c     here we see the number of different ways the user can screw up. 
c
c     Original : CHP, SEH
c     Revised  : MRP, CSR
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c---------------------------------------------------------------------
c
      i=numer
      if(i.gt.400) goto 1004
      if(i.gt.300) goto 1003
      if(i.gt.200) goto 1002
      if(i.gt.100) goto 1001
      goto (001,002,003,004,005,006,007,008,009,010,
     +      011,012,013,014,015,016,017,018,019,020,
     +      021,022,023,024,025,026,027,028,029,030,
     +      031,032,033,034,035,036,037,038,039,040,
     +      041,042,043,044,045,046,047,048,049,050,
     +      051,052,053,054,055,056,057,058,059,060,
     +      061,062,063,064,065,066,067,068,069,070,
     +      071,072,073,074,075,076,077,078,079,080,
     +      081,082,083,084,085,086,087,088,089,090,
     +      091,092,093,094,095,096,097,098,099,100) i
1001  continue
      goto (101,102,103,104,105,106,107,108,109,110,
     +      111,112,113,114,115,116,117,118,119,120,
     +      121,122,123,124,125,126,127,128,129,130,
     +      131,132,133,134,135,136,137,138,139,140,
     +      141,142,143,144,145,146,147,148,149,150,
     +      151,152,153,154,155,156,157,158,159,160,
     +      161,162,163,164,165,166,167,168,169,170,
     +      171,172,173,174,175,176,177,178,179,180,
     +      181,182,183,184,185,186,187,188,189,190,
     +      191,192,193,194,195,196,197,198,199,200) i-100
1002  continue
      goto (201,202,203,204,205,206,207,208,209,210,
     +      211,212,213,214,215,216,217,218,219,220,
     +      221,222,223,224,225,226,227,228,229,230,
     +      231,232,233,234,235,236,237,238,239,240,
     +      241,242,243,244,245,246,247,248,249,250,
     +      251,252,253,254,255,256,257,258,259,260,
     +      261,262,263,264,265,266,267,268,269,270,
     +      271,272,273,274,275,276,277,278,279,280,
     +      281,282,283,284,285,286,287,288,289,290,
     +      291,292,293,294,295,296,297,298,299,300) i-200
1003  continue
      goto (301,302,303,304,305,306,307,308,309,310,
     +      311,312,313,314,315,316,317,318,319,320,
     +      321,322,323,324,325,326,327,328,329,330) i-300
1004  write(*,*) 'Disaster in INITA - all ships destroyed!'
      write(*,*) 'Error code = ',i
      stop
c
001   cr='(1x,''Invalid card type detected.'',i4)'
      return
002   cr='(1x,''Card type specified is out of order.'',i4)' 
      return
003   cr='(1x,''Illegal lu specified for structure input file.'',i4)'
      return
004   cr='(1x,''Error #'',i4,'' detected on opening the specified'',' 
     +       // ' '' file.'')'
      return
005   cr='(1x,''Error #'',i4,'' detected while reading the specified'','
     +       // ' '' file.'')'
      return
006   cr='(1x,''Error #'',i4,'' is undefined, call out the bug'
     +       // ' catcher!'')'
      return
007   cr='(1x,''Unexpected end-of-file detected in input file'',i4)' 
      return
008   cr='(1x,''Error #'',i4,'' detected when purging,'
     +       // ' specified structure''/'' or solution file.'')' 
      return
009   cr='(1x,''Error#'',i4,'' detected while writing the specified '
     +       // 'file.'')'
      return
010   cr='(1x,''Unexpected end-of-file detected in input file.'',i4)' 
      return
011   cr='(1x,''x or y mesh card in wrong place!'',i4)' 
      return
012   cr='(1x,''x or y nodes or locations not in ascending order!'',i4)' 
      return
013   cr='(1x,''Second mesh card encountered'',i4)' 
      return
014   cr='(1x,''nx and ny must both be specified and lie between 1 &'' '
     +       // ',i4,'' !'')'
      return
015   cr='(1x,''Mesh type not specified (rect, etc.)!'',i4)'
      return
016   cr='(1x,''Region or electrode numbers must lie'
     +       // ' between 1 and'',i4,'' !'')'
      return
017   cr='(1x,''Region or electrode limits too large, too small, or' 
     +       // ' reversed'',i4)' 
      return
018   cr='(1x,''Region or electrode cards missing from rect.mesh'
     +       // ' section'',i4)'
      return
019   cr='(1x,''No region card before this doping card!'',i4)'
      return
020   cr='(1x,''Concentration negative or not specified!'',i4)' 
      return
021   cr='(1x,''None or more than 1 profile types specified!'',i4)' 
      return
022   cr='(1x,''Std. dev. or origin cards not specified!'',i4)' 
      return
023   cr='(1x,''Expected an x. or y.mesh card here!'',i4)'
      return
024   cr='(1x,''Node number out of bounds, location not specified, or' 
     +       // ' ratio not within .2 to .5'',i4)'
      return
025   cr='(1x,''Mesh print requested before mesh specified!'',i4)'
      return
026   cr='(1x,''Neither or both dopant types specified!'',i4)'
      return
027   cr='(1x,''Error #'',i6,'' in vmsp routine in symbolic!'')' 
      return
028   cr='(1x,''Error #'',i6,'' in unblok routine in symbolic!'')' 
      return
029   cr='(1x,''Symbolic arrays too small, check printout!'',i4)' 
      return
030   cr='(1x,''Can not specify both nested dissection and minimum '
     +       // 'degree parameters!'',i4)'
      return
031   cr='(1x,''No input file given for irregular mesh!'',i4)'
      return
032   cr='(1x,''Ambigous material type definition for region #'',i1)'
      return
033   cr='(1x,''No quantity for specified for vector plot'',i4)'
      return
034   cr='(1x,''No electrode specified for bias stepping!'',i4)'
      return
035   cr='(1x,''Specified temperature out of range!'',i4)'
      return
036   cr='(1x,''Invalid parameter specified for an insulator!'',i4)'
      return
037   cr='(1x,''Conflicting specifications in condition card!'',i4)'
      return
038   cr='(1x,''No material type specified for region #'',i4,)'
      return
039   cr='(1x,''Region #'',i4,'' has no permittivity or has not been'
     +       // ' assigned'')'
      return
040   cr='(1x,annot specify x,y-component with solution diff'
      return
041   cr='(1x,''No energy gap specified for the semiconductor'',i4)'
      return
042   cr='(1x,''No mobility specified for the semiconductor!'',i4)'
      return
043   cr='(1x,''No lifetimes specified for the semiconductor!'',i4)' 
      return
044   cr='(1x,''No saturation velocity specified for the semiconductor'
     +       // ' material!'',i4)'
      return
045   cr='(1x,''Symbolic card must precede method card!'',i4)'
      return
046   cr='(1x,''No initial guess present - must solve for initial bias'
     +      // ' point''/''or load a solution.'',i4)'
      return
047   cr='(1x,''Projection not possible!,'',i4)'
      return
048   cr='(1x,''Can only step voltage or current, not both!'',i4)'
      return
049   cr='(1x,''Contact number given ('',i4,'') is out of range!'')' 
      return
050   cr='(1x,''Ambiguous quantity defined for vector plot'',i4)'
      return
051   cr='(1x,''Only 1 previous solution is present!'',i4)' 
      return
052   cr='(1x,''No previous solution is present!'',i4)' 
      return
053   cr='(1x,''Can not change temperature during run!'',i4)'
      return
054   cr='(1x,''Method card encountered before mesh done!'',i4)' 
      return
055   cr='(1x,''Solve card before symbolic done!'',i4)' 
      return
056   cr='(1x,''Two of the biases on electrode (#'',i4,' 
     +       // ' '') are identical.''/'' The biases must' 
     +       // ' all be different for projection!'')' 
      return
057   cr='(1x,''Previous solution will be' 
     +       // ' used as initial guess for this bias point!'',i4)'
      return
058   cr='(1x,''Work function not specified for contact number'',i4)'
      return
059   cr='(1x,''Cannot do single poisson when doing poisson only.' 
     +       // '  multiple will be''/'' set for you!'',i4)'
      return
060   cr='(1x,''Cannot do acceleration with multiple Poisson.'//
     +        'It will be turned off!'',i4)' 
      return
061   cr='(1x,''Numerical value of pivot #'',i4,'' is approximately '
     +       //'zero in vmnp routine - no solution possible.'' '
     +       //'/''Try a different bias point.'')'
      return
062   cr='(1x,''Error in nbc sorting routine in mesh generation!'',i4)'
      return
063   cr='(1x,''Error in obtuse triangle calculations in geom' 
     +       // ' subroutine of mesh''/'' for element #'',i5,)'
      return
064   cr='(1x,''Material number incorrect. There are'',i4,'
     +       // ' '' materials!'')'
      return
065   cr='(1x,''Cannot project with current sources!'',i4)'
      return
066   cr='(1x,''Valid solution not present!'',i4)'
      return
067   cr='(1x,''Setup #2 data not present!'',i4)' 
      return
068   cr='(1x,''Setup #1 data not present!'',i4)' 
      return
069   cr='(1x,''Too many nodes generated!'',i5,)' 
      return
070   cr='(1x,''ne must be less than'',i5,)' 
      return
071   cr='(1x,''nb must be less than'',i5,)' 
      return
072   cr='(1x,''Element#'',i5,'' has not been assigned any region '
     +       // 'number'')'
      return
073   cr='(1x,''I do not know this bug number was for'',i4)' 
      return
074   cr='(1x,''Bias/time step value is zero!'',i4)' 
      return
075   cr='(1x,''Incrementing of solution file name extended beyond'
     +       //' last 2 characters,''/'' file name unchanged!'',i4)' 
      return
076   cr='(1x,''* Operator requested program termination! *'',i4)'
      return
077   cr='(1x,''* Operator requested early solution termination! *'' '
     +       // ',i4)' 
      return
078   cr='(1x,''Must have had mesh card before any plotting!'',i4)' 
      return
079   cr='(1x,''Warning #'',i4,'' - more than one doping output file'
     +       // ' - ignored'')'
      return
080   cr='(1x,''One of the min values is greater than the max values'
     +       // ' in plot.2d card!'',i4)' 
      return
081   cr='(1x,''min.val not specified in contour card!'',i4)' 
      return
082   cr='(1x,''del.val in contour card is less than zero.  it has'
     +       // ' been set to zero for you!'',i4)'
      return
083   cr='(1x,''max.val in contour card is less than min.val.  it' 
     +       // ' has been set to min.val for you!'',i4)' 
      return
084   cr='(1x,''del.val is zero but max.val does not equal min.val.' 
     +       // '  max.val set equal to min.val!'',i4)' 
      return
085   cr='(1x,''Exactly one function type may be specified.  you have'
     +       // ' specified'',i4,''!'')' 
      return
086   cr='(1x,''Error #'',i4,'' detected on attempt to read '
     +       // 'from the input file.'')'
      return
087   cr='(1x,''Error #'',i4,'' detected on attempt to read ,' 
     +       // 'from the input file.'')'
      return
088   cr='(1x,''Solution must be present for depletion edges'',i4)' 
      return
089   cr='(1x,''Solution must be present for specified contour!'',i4)'
      return
090   cr='(1x,''Solution must be done for field lines!'',i4)'
      return
091   cr='(1x,''Line endpoints not fully specified!'',i4)'
      return
092   cr='(1x,''Line must be completey within device bounds!'',i4)' 
      return
093   cr='(1x,''Ionization coefficients must be positive!'',i4)'
      return
094   cr='(1x,''Line type must be positive.  sign changed for you!'' '
     +       // ',i4)'
      return
095   cr='(1x,''Line type out of range(1 -'',i4,'').  set to 1 for'
     +       // ' you!'')'
      return
096   cr='(1x,''Error #'',i5,'' on attempt to open or create plot'
     +       // ' file!'')'
      return
097   cr='(1x,''Plot file already exits as non plot file!'',i4)'
      return
098   cr='(1x,''Must have mesh card but no doping before spread!'',i4)'
      return
099   cr='(1x,''Warning #'',i4,'' - doping output file not specified'
     +      // ' on first DOPING card'')'
      return
100   cr='(1x,''Width must be specified and be non-negative!'',i4)' 
      return
101   cr='(1x,''Upper and lower must both be specified!'',i4)'
      return
102   cr='(1x,''Error in value or order of upper or lower parms.'',i4)'
      return
103   cr='(1x,''Only one of thickness/y.lower/fix.ylower may '
     +      //'be specified!'',i4)' 
      return
104   cr='(1x,''One of thickness/y.lower/fix.ylower must be '
     +      //'specified!'',i4)'
      return
105   cr='(1x,''The valid range for volume.ratio is zero to one!'',i4)' 
      return
106   cr='(1x,''Thickness must be positive!'',i4)'
      return
107   cr='(1x,''Error in computation of lower location.''/''Must be >'
     +       // ' upper loc. and <= bottom loc.!'',i4)'
      return
108   cr='(1x,''Encroachment parameter must be >= .1!'',i4)'
      return
109   cr='(1x,''Insulator or semiconductor must be specified on region'
     +       // ' card!'',i4)'
      return
110   cr='(1x,''Backround doping has same polarity as gaussian at '
     +       // 'junction depth!'',i4)'
      return
111   cr='(1x,''x.charac length and y.charac must be positive!'',i4)' 
      return
112   cr='(1x,''Either conc or char. length must be specified!'',i4)'
      return
113   cr='(1x,''Either junction or char. length must be specified!'','
     +        // 'i4)' 
      return
114   cr='(1x,''junction must be > y.bottom or < y.top !'',i4)'
      return
115   cr='(1x,''Either dose or concentration must be specified and'
     +       // ' be positive!'',i4)' 
      return
116   cr='(1x,''ratio.lateral is negative or not specified!'',i4)' 
      return
117   cr='(1x,''Rectangular and description modes incompatible'',i4)'
      return
118   cr='(1x,''Too many elements generated'',i4)'
      return
119   cr='(1x,''x or y limits outside of bounds or in wrong order!'','
     +        // 'i4)' 
      return
120   cr='(1x,''Concentration must be specified on qf card!'',i4)' 
      return
121   cr='(1x,''Must have x.left <= x.right and y.top <= y.bottom!'','
     +        // 'i4)' 
      return
122   cr='(1x,''Specified junction lies outside silicon!'',i4)'
      return
123   cr='(1x,''Junction already exists near requested junction!'',i4)'
      return
124   cr='(1x,''Doping is zero at node adjacent to junction!'',i4)' 
      return
125   cr='(1x,''Mobility constants yield zero mobility at y=0!'',i4)'
      return
126   cr='(1x,''Too many junction depth specifiers'',i4)'
      return
127   cr='(1x,''Irregular region boundary not 6-multiple! '',i4)'
      return
128   cr='(1x,''Electrodes overlap!'',i4)'
      return
129   cr='(1x,''Nested dissection can only be done with a rectangular'
     +       // ' mesh - ignored'',i4)'
      return
130   cr='(1x,''Electrode #'',i4,'' contacts both sides of a '
     +       // 'junction'')'
      return
131   cr='(1x,''Backround conc. at junc. depth is higher than '
     +       // 'specified peak conc. for gaussian!'',i4)'
      return
132   cr='(1x,''P-type region has two different biases applied''/ '
     +       // ' '' - lower bias used as hole qf!'',i4)'
      return
133   cr='(1x,''N-type region has two different biases applied ''/'
     +       // ' '' - higher bias used as electron qf!'',i4)'
      return
134   cr='(1x,''junction must > x.right or < x.left !'',i4)'
      return
135   cr='(1x,''Can only use ELIMINATE card for rectangular grids!'','
     +        // 'i4)'
      return
136   cr='(1x,''SUPREM_IV input file is incomplete!'',i4)'
      return
137   cr='(1x,''Can only expand rectangular/SUPREM_IV grids!'',i4)'
      return
138   cr='(1x,''Can only do a mirrored expansion in x-direction!'',i4)'
      return
139   cr='(1x,''Mirrored width greater than width of device!'',i4)'
      return
140   cr='(1x,''New xmax inside current device bounds!'',i4)'
      return
141   cr='(1x,''New xmin inside current device bounds!'',i4)'
      return
142   cr='(1x,''New ymax inside current device bounds!'',i4)'
      return
143   cr='(1x,''New ymin inside current device bounds!'',i4)'
      return
144   cr='(1x,''Only one semiconductor allowed!'',i4)'
      return
145   cr='(1x,''Materials must be defined before MODEL card!'',i4)'
      return
146   cr='(1x,''Ambiguous error criterion specified!'',i4)'
      return
147   cr='(1x,''No function specified.'',i6)'
      return
148   cr='(1x,''Must supply two files in order to get difference!'','
     +        // 'i4)'
      return
149   cr='(1x,''Can not do vector plots on current/recomb.'
     +       // ' differences!'',i4)'
      return
150   cr='(1x,''For logarithmic values, del.val must be > 0'',i4)'
      return
151   cr='(1x,''Triangle tree (*tt) for this mesh file not found'',i5)' 
      return
152   cr='(1x,''Error #'',i4,'' in reading/writing triangle tree'')'
      return
153   cr='(1x,''Regrid - triangle tree does not match this mesh!'',i5)'
      return
154   cr='(1x,''Regrid - invalid or no function step given.'',i5)'
      return
155   cr='(1x,''Error #'',i4,'' - error on attempted read from doping'
     +       // ' file'')'
      return
156   cr='(1x,''Maximum of 10 colors available, you chose'',i4)'
      return
157   cr='(1x,''Unknown plot device.'',i5)'
      return
158   cr='(1x,''Plot size/offset must be greater than zero!'',i5)'
      return
159   cr='(1x,''Internal error in color plot.'',i5)'
      return
160   cr='(1x,''Exactly one parameter may be chosen.  You have'
     +       // ' specified'',i4,''!'')' 
      return
161   cr='(1x,''Must load or calculate a solution before extracting'
     +       // ' this parameter! '',i4)'
      return
162   cr='(1x,''Must load mesh before extracting parameters (fool!)'')'
      return
163   cr='(1x,''No contact or region combination specified'',i5)'
      return
164   cr='(1x,''No such contact/region combination : '',i5)'
      return
165   cr='(1x,''Previous solution(s) not written with CURRENT'
     +       // ' option - ''/'
     +       // ' ''cannot calculate field/recomb./current!'',i4)'
      return
166   cr='(1x,''Stack overflow for continuation method - n>'',i2)'
      return
167   cr='(1x,''Mobility coefficient must be > 0!'',i4)'
      return
168   cr='(1x,''Ambiguous substrate doping type!'',i4)'
      return
169   cr='(1x,''No substrate doping type specified!'',i4)'
      return
170   cr='(1x,''Must specify middle point location!'',i4)'
      return
171   cr='(1x,''Must specify middle point index!'',i4)'
      return
172   cr='(1x,''Middle index is not between upper and lower!'',i4)'
      return
173   cr='(1x,''Middle location is not between upper and lower!'',i4)'
      return
174   cr='(1x,''Could not obtain equilibrium potential for node'',i6)'
      return
175   cr='(1x,''Slotboom variable set is no longer supported.'',i4)'
      return
176   cr='(1x,''Sorry, Fermi-Dirac statistics are not available '
     +       // 'in this version!'',i4)'
      return
177   cr='(1x,''Solution must be loaded to regrid on this quantity!'' '
     +       // ',i4)'
      return
178   cr='(1x,''No region number given on region card!'',i4)'
      return
179   cr='(1x,''Number of electrodes in log file does not agree'
     +       // ' with present mesh!'',i4)'
      return
180   cr='(1x,''Too many nodes for Gummel method, maximum'',i6)'
      return
181   cr='(1x,''Too many nodes for knot iteration, maximum'',i6)'
      return
182   cr='(1x,''Too many nodes for block iteration, maximum'',i6)'
      return
183   cr='(1x,''Too many nodes for 1-carrier full Newton, maximum'','
     +        // 'i6)'
      return
184   cr='(1x,''Too many nodes for 2-carrier full Newton, maximum'','
     +        // 'i6)'
      return
185   cr='(1x,''No plot quantity specified for an axis of iv plot!'','
     +        // 'i4)'
      return
186   cr='(1x,''Sorry, this version cannot do knots with < 2 '
     +       // 'carriers!'',i4)'
      return
187   cr='(1x,''Error - too many triangles meet at a node!'',i4,'
     +       // ' ''(Triangle tree being lost?)'' )'
      return
188   cr='(1x,''Error - ambiguous instructions for region #'',i2)'
      return
189   cr='(1x,''Impurity type ignored for ascii or 2D doping '
     +       // 'files!'',i4)'
      return
190   cr='(1x,''Electrode specified for bias stepping is invalid!'',i4)'
      return
191   cr='(1x,''Electrodes with different bias steps found '
     +       // 'on attempted project!'',i4)'
      return
192   cr='(1x,''Ambiguous profile type specified!'',i4)'
      return
193   cr='(1x,''Electrode #'',i2,'' is totally embedded - flowlines '
     +       // 'not possible!'')'
      return
194   cr='(1x,''Cannot perform ac analysis with resistive'
     +      // ' or capacitive elements!'',i4)'
      return
195   continue
      cr='(1x,'
      return
196   cr='(1x,''More than '',i3,'' interface cards specified!'')'
      return
197   cr='(1x,''Error in eliminate specification'',i4)'
      return
198   cr='(1x,''Error #'',i4,'' - no. of points/electrodes in'
     +       // ' solution file is inconsistent with mesh'')'
      return
199   cr='(1x,''Warning #'',i4,'' - temperature in solution file is'
     +       // ' inconsistent'')'
      return
200   cr='(1x,''Error #'',i4,'' is undefined, call out the bug'
     +       // ' catcher!'')'
      return
201   cr='(1x,''* Invalid card type specification.'',i4)' 
      return
202   cr='(1x,''* Card type is ambiguous.'',i4)'
      return
203   cr='(1x,''* Invalid parameter specification.'',i4)' 
      return
204   cr='(1x,''* Parameter name is ambiguous.'',i4)' 
      return
205   cr='(1x,''* Invalid real or integer specification.'',i4)' 
      return
206   cr='(1x,''* Invalid logical specification.'',i4)' 
      return
207   cr='(1x,''* Ambiguous logical specification.'',i4)' 
      return
208   cr='(1x,''* Parameter value or equal sign not found.'',i4)' 
      return
209   cr='(1x,''* First input line read is a continuation line.'',i4)'
      return
210   cr='(1x,''* The number of card types in the specification'
     +       // ' file exceeds '',i4)' 
      return
211   cr='(1x,''* The number of parameters in the specification'','
     +       // ' '' file exceeds '',i4)' 
      return
212   cr='(1x,''During initialization, the card whose id#'','
     +       // ' '' is'',i4/'' referenced an undefined parameter'
     +       // ' list.'')'
      return
213   cr='(1x,''* Unexpected end-of-file detected in formatted'',' 
     +       // ' ''specification file.'',i4)'
      return
214   cr='(1x,''* Card id# was not specified in the formatted card'
     +       // ' specification file.'',i4)'
      return
215   cr='(1x,''* The number of parameters in the card was not'','
     +       // ' '' specified.'',i4)'
      return
216   cr='(1x,''* The parameters value type was not specified.'',i4)' 
      return
217   cr='(1x,''* The parameters value location was not specified.'','
     +       // 'i4)'
      return
218   cr='(1x,''* The parameter value type in the specification'
     +       // ' file'',' 
     +       // ' '' is invalid.'',i4)'
      return
219   cr='(1x,''* The parameter value location in specification file'
     +       // ' is out of range.'',i4)'
      return
220   cr='(1x,''* Error #'',i4,'' detected on attempt to open'
     +       // ' the unformatted specification file.'',i4)'
      return
221   cr='(1x,''Unformatted specification file is incorrect type.'','
     +       // 'i4)' 
      return
222   cr='(1x,''Error #'',i4,'' detected on read of unformatted'
     +       // ' specification file.'')'
      return
223   cr='(1x,''* Error #'',i4,'' detected on attempt to open'
     +       // ' the formatted specification file.'')'
      return
224   cr='(1x,''* Error #'',i4,'' detected on attempt to create'
     +       // ' the unformatted card specification file.'')' 
      return
225   cr='(1x,''* Error #'',i4,'' detected on attempt to write to'
     +       // ' the unformatted card specification file.'')' 
      return
226   cr='(1x,''* Error #'',i4,'' detected on attempted open of'
     +       // ' the input file.'')'
      return
227   cr='(1x,''* Error #'',i4,'' detected on attempted open of'
     +       // ' the output file.'')' 
      return
228   cr='(1x,''* Error #'',i4,'' detected on read from file.'')'
      return
229   cr='(1x,''* Error #'',i4,'' detected on write to parsed'
     +       // ' output file.'')' 
      return
230   cr='(1x,''Error #'',i4,'' is undefined, get the bug catcher!'')'
      return
231   cr='(1x,''Low limit out of range, set to one'',i4)' 
      return
232   cr='(1x,''High limit out of range, set to max'',i4)'
      return
233   cr='(1x,''Low limit greater than large limit'',i4)'
      return
234   cr='(1x,''Can not specify time step and voltage step'',i4)'
      return
235   cr='(1x,''Illegal time step specified'',i4)'
      return
236   cr='(1x,''Can not time step on initial bias point'',i4)'
      return
237   cr='(1x,''Multiple directions defined for eliminate.'',i4)' 
      return
238   cr='(1x,''Not all area covered by generated elements'',i4)' 
      return
239   cr='(1x,''Illegal element defined for auto generator'',i4)' 
      return
240   cr='(1x,''Error '',i4,'' no region specified on doping card'')'
      return
241   cr='(1x,''Error '',i4,'' no such region'')'
      return
242   cr='(1x,''No input geometry/description file specified '',i4)'
      return
243   cr='(1x,''Can not project time-dependent bias point '',i4)'
      return
244   cr='(1x,''No direction defined for eliminate. '',i4)'
      return
245   cr='(1x,''Small pivot had to be fixed during incomplete '
     +         //' factorization - ICCG is in difficulty.'',i3)'
      return
246   cr='(1x,''Incomplete factorization failed after '',i3,'
     +         //' '' retries - Turn off ICCG'')'
      return
247   cr='(1x,''Storage overflow ('',i6,'' blocks) during incomplete '
     +        //'factorization - use no fill in or turn off knots.'')'
      return
248   cr='(1x,''Cannot specify stopping time and no. of steps'',i4)'
      return
249   cr='(1x,''Lumped-element or current bc specification must '','
     +        //' ''precede symbolic card'',i4)'
      return
250   cr='(1x,''Must use full (or knot) Newton for'','
     +        //' '' transient solutions'',i4)'
      return
251   cr='(1x,''Contact cannot have SRV and lumped elements'',i4)'
      return
252   cr='(1x,''Full Newton reqd with lumped elmnts,'','
     +        // ' '' contact resis. or current bc'',i4)'
      return
253   cr='(1x,''Can only use 2nd order scheme with 2 carriers'',i4)'
      return
254   cr='(1x,''Can only use auto time-step with 2nd order'',i4)'
      return
255   cr='(1x,''External lumped elements not allowed with contact'','
     +        // ' '' resistance'',i4)'
      return
256   cr='(1x,''Contact cannot have SRV and contact resistance'',i4)'
      return
257   cr='(1x,''Too many permitter nodes ('',i5,'') in geometry'','
     +        // ' '' file.'')'
      return
258   cr='(1x,''Mesh defn must precede symbolic factorization'',i4)'
      return
259   cr='(1x,''Error in 1-D plot : no data!'',i4)'
      return
260   cr='(1x,''Error '',i4,'' 1-D start or end not in device. '')'
      return
261   cr='(1x,''Error '',i4,'' 1-D start=end! '')'
      return
262   cr='(1x,''Error '',i4,'' 1-D plots single variable only'')'
      return
263   cr='(1x,''Error '',i4,'' x or y bound of 1-D plot missing'')'
      return
264   cr='(1x,''Maximum number of inner loops exceeded in ac '','
     +        // ' ''analysis!'',i4)'
      return
265   cr='(1x,''No electrodes specified in geometry file!'',i4)'
      return
266   cr='(1x,''Materials may only be printed after a MODEL, LOAD '','
     +        // ' ''or SOLVE card - sorry!'',i4)'
      return
267   cr='(1x,''Warning '',i4, '' 1-D plot overflow - values'
     +        // ' discarded'')'   
      return
268   cr='(1x,''Warning : isolated electrode node #'',i5,'
     +        // ' '' - length = 0'')'
      return
269   cr='(1x,''Warning '',i4, '' too many points for spline - limited'
     +       // ' to maximum allowed'')'
      return
270   cr='(1x,''No small signal bias applied for ac analysis!'',i4)'
      return
271   cr='(1x,''Surface recombination velocity >= 0 please!'')'
      return
272   cr='(1x,''Triangle '',i4,'' has a zero side.'')'
      return
273   cr='(1x,''Only Schottky contacts may have finite SRV'',i5)'
      return
274   cr='(1x,''No frequency specified for ac analysis!'',i4)'
      return
275   cr='(1x,''Cannot do ac analysis with <2 carriers!'',i4)'
      return
276   cr='(1x,''Only one small signal bias may be applied for ac '','
     +       // ' ''analysis!'',i4)'
      return
277   cr='(1x,''Illegal electrode specified for ac analysis!'',i4)'
      return
278   cr='(1x,''Must use full Newton for ac analysis!'',i4)'
      return
279   cr='(1x,''Not enough storage for minimum degree calculation.'','
     +       // 'i4)'
      return
280   cr='(1x,''Isolated schottky node found '',i4)'
      return
281   cr='(1x,''Error on attempted read of ascii doping file '',i4)'
      return
282   cr='(1x,''No solution method specified on SYMBOLIC card :'', i4)'
      return
283   cr='(1x,''Ambiguous impurity types specified for doping'
     +       // ' input '',i4)'
      return
284   cr='(1x,''No impurity type specified for SUPREM-III input '',i4)'
      return
285   cr='(1x,''No doping specified. '')'
      return
286   cr='(1x,''Huge outward field at Schottky  ignored, contact'',i4)'
      return
287   cr='(1x,''Inward normal field at Schottky ignored, contact'',i4)'
      return
288   cr='(1x,''Maximum no. of damping loops exceeded'',i4)'
      return
289   cr='(1x,''File not found or file system error.'',i4)'
      return
290   cr='(1x,''Error #'',i4,'' - symbolic file is inconsistent'
     +       // ' with mesh/method defn'')'
      return
291   cr='(1x,''Error #'',i4,'' - error on attempted read of'
     +       // ' SUPREM-III input file'')'
      return
292   cr='(1x,''Error #'',i4,'' - electrons and holes specified,'
     +       // ' but only one carrier'')'
      return
293   cr='(1x,''Warning #'',i4,'' - Newton parameters specified'
     +       // ' for Gummel method''/13x,'' - parameters ignored'')'
      return
294   cr='(1x,''Error #'',i4,'' - more than one method specified'')'
      return
295   cr='(1x,''Error #'',i4,'' - parameter out of range on METHOD '
     +       // ' card'')'
      return
296   cr='(1x,''Warning #'',i4,'' - cannot switch to Gummel or block-'
     +       // 'iterative methods from full Newton''/'
     +       // '14x,''- parameter ignored'')'
      return
297   cr='(1x,''Warning #'',i4,'' - cannot use qf variables with'
     +       // ' Gummel method'')'
      return
298   cr='(1x,''Warning #'',i4,'' - Gummel parameters specified'
     +       // ' for Newton method''/13x,'' - parameters ignored'')'
      return
299   cr='(1x,''Warning #'',i4,'' - ambiguous solution methods'
     +       // ' specified - ignored'')'
      return
300   cr='(1x,''Multiple frequencies found for same dc point in ac'
     +      // ' file!'',i4)'
      return
301   cr='(1x,''Multiple dc points found for same freqency in ac'
     +      // ' file!'',i4)'
      return
302   cr='(1x,''No initial time-step available!'',i4)'
      return
303   continue
      cr='(1x,''Negative mean free path was defined!'',i4)'
      return
304   continue
      cr='(1x,''No photon flux specified for photogeneration!'',i4)'
      return
305   continue
      cr='(1x,''No absorption coefficient specified for'
     +      // ' photogeneration!'',i4)'
      return
306   continue
      cr='(1x,''Warning #'',i4,'' - negative photon flux given for'
     +       // ' photogeneration - absolute value used'')'
      return
307   continue
      cr='(1x,''Warning #'',i4,'' - negative absorption coefficient'
     +       // ' given for photogeneration - absolute value used'')'
      return
308   continue
      cr='(1x,''Inconsistant value of ACC.SF or INV.SF!'',i4)'
      return
309   continue
      cr='(1x,''Inconsistant value of OX.LEFT, OX.RIGHT,'
     +       // ' OR OX.BOTTOM!'',i4)'
      return
310   continue
      cr='(1x,''Only one of CCSMOB, ANALYTIC, ARORA or USER1 may '
     +       // 'be specified'',/,'' - all concentration dependent '
     +       // 'mobility models cleared '',i4)'
      return
311   continue
      cr='(1x,''Warning #'',i4,'' - both ARORA and ANALYTIC '
     +       // 'specified  - ARORA model assumed.'')'
      return
312   continue
      cr='(1x,''Warning #'',i4,'' - both USER1 and either ARORA or '
     +       // 'ANALYTIC specified'',/,''  - USER1 model assumed.'')'
      return
313   continue
      cr='(1x,'
      return
314   continue
      cr='(1x,'
      return
315   continue
      cr='(1x,'
      return
316   continue
      cr='(1x,'
      return
317   continue
      cr='(1x,'
      return
318   continue
      cr='(1x,'
      return
319   continue
      cr='(1x,'
      return
320   continue
      cr='(1x,'
      return
321   continue
      cr='(1x,'
      return
322   continue
      cr='(1x,'
      return
323   continue
      cr='(1x,'
      return
324   continue
      cr='(1x,'
      return
325   continue
      cr='(1x,'
      return
326   continue
      cr='(1x,'
      return
327   continue
      cr='(1x,'
      return
328   continue
      cr='(1x,'
      return
329   continue
      cr='(1x,'
      return
330   continue
      cr='(1x,'
      return
c
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE ERSET(numer,line,ierval) 
      include 'p2conf.h'
        integer numer,line,ierval 
c     date code: apr. 07, 1980
c---------------------------------------------------------------------
c 
c     erset logs the specified error, line number and optional error
c     parameter.
c 
c 
c     original : stephen e. hansen                       aug. 28, 1979
c     revision : MRP                                     mar, 1984
c 
c     copyright c 1981 the board of trustees of the leland stanford 
c                      junior university. all rights reserved.
c     this subroutine may not be used outside of the pisces computer
c     program without the prior written consent of stanford university. 
c 
c 
c---------------------------------------------------------------------
c 
c     common area 
c 
      include     'eritco.h'
      include     'blank.h'
c
      integer index
c---------------------------------------------------------------------
c 
c     start of erset
c 
c---------------------------------------------------------------------
c......if the error number is zero, reset the error/warning counter 
c......the err array and the error and warning flags. 
      if(numer.ne.0) goto 20
      do 10 index=1,20
      err(1,index)=0
      err(2,index)=0
10    err(3,index)=0
      indxer=0 
      errflg=.false.
      wrnflg=.false.
      return
c 
c......increment the error array pointer. 
20    indxer=indxer+1 
      if(indxer.gt.20) goto 30 
c 
c......put the error number, the line number and the optional parameter 
c......into the err array.
      err(1,indxer)=numer
      err(2,indxer)=line 
      err(3,indxer)=ierval 
c 
c......if this was an error set the error flag, if it was a warning 
c......then set the warning flag. 
30    errflg=(numer.gt.0 .and. .not.isiact).or.errflg 
      wrnflg=(numer.lt.0 .or. (isiact .and. numer.gt.0)).or.wrnflg 
c 
c......that's all.  simple wasn't it? 
      return
      end 
