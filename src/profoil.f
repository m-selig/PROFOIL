
      PROGRAM PROFOIL

C**********************************************************************
C     PROFOIL    Version 2.0   March 2022
C     
C     Multi-Point Inverse
C     Airfoil Design
C     
C     Copyright (c) 1990-2022 Michael Selig
C     Copyright (c) 1995 Ashok Gopalarathnam
C**********************************************************************

C**********************************************************************
C...  Read the input data from profoil.in
C     ILINE is the current line number of the input file.
C**********************************************************************

      INCLUDE 'PROFOIL.INC'
      LOGICAL LGCL, LEND
 1000 FORMAT(A,'0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0')
 1005 FORMAT(2X,' Airfoil/Comment: ',A)
C-----set the constants
      CALL SETUP
C-----set the default values
      CALL DEFLT
C-----check some of the variables
      CALL PROOF
C=========================================================================
C-----open the input file profoil.in (see setup.f)
      OPEN(UNIT = LU10, FILE = FILE10, status = 'old')
      REWIND LU10
C=========================================================================
      ILINE = 0
      LEND = FF
C---  do until iteration
 10   CONTINUE
      READ(LU10,'(A)') LINE
      ILINE = ILINE + 1
      IF (LINE(1:1) .EQ. '!') THEN
C---  skip this line
      ELSEIF (LINE(1:1) .EQ. '#') THEN
C---  skip this line
      ELSEIF (LINE(1:8) .EQ. ' FXPR100') THEN
C... Stuff that can be removed.
        READ(LU10,*)
        DO JPT = 1, 81
          READ(LU10,*) X,Y
          WRITE(24,1340) X/100000., Y/100000.
 1340     FORMAT(1X,2F8.5)
        ENDDO
        CLOSE(24)
        call stoptst
      ELSEIF (LINE(1:1) .EQ. ' ') THEN
C---  skip this line
      ELSEIF (LINE(1:1) .EQ. '*') THEN
C---  finished reading in data from input file unit LU10
        CLOSE(LU10)
        LEND = TT
C--------AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
      ELSEIF (LINE(1:8) .EQ. 'ADDALFAS') THEN
C     add number beyond alfas to alfas  (in FOIL line )
        LADDALF = .NOT. LADDALF
      ELSEIF (LINE(1:17) .EQ. 'AIRFOIL_AREA_FILE') THEN
        WRITE(TLINE, 1000) LINE(18:50)
        READ(TLINE,*) CHORDO
        IF(CHORDO .EQ. 0.) CHORDO = 1.
        IAREAMD = 0
        CALL AFAREA(IAREAMD)
      ELSEIF (LINE(1:8) .EQ. 'AIRFOIL ') THEN
        WRITE(LU06,*)
        WRITE(LU06,*) '  ********************************************'
        WRITE(LU06,1005) LINE(9:50)
        WRITE(LU06,*) '  ********************************************'
        WRITE(LU06,*)
      ELSEIF (LINE(1:6) .EQ. 'ALFASP') THEN
        WRITE(TLINE, 1000) LINE(7:50)
        READ(TLINE,*) IALF
        IF (IALF .GT. NALF) THEN
          WRITE(LU06,*) 'Error 101: IALF > NALF (profoil.f)'
          CALL STOPTST
        ENDIF
        LALFASP = FF
        IF(IALF.GT.0) LALFASP = TT
        DO JALF = 1, IALF
          READ(LU10,*) ALFA(JALF)
        ENDDO
C--------BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
      ELSEIF (LINE(1:4) .EQ. 'BLAN') THEN
        CALL BLAN
      ELSEIF (LINE(1:9) .EQ. 'BUMPALFAS') THEN
        WRITE(TLINE, 1000) LINE(10:50)
        READ(TLINE,*) ICASE, DELTA
        CALL ALFAINC(ICASE,DELTA)
C--------CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      ELSEIF (LINE(1:4) .EQ. 'CASE') THEN
        WRITE(TLINE, 1000) LINE(5:50)
        READ(TLINE,*) JFOIL
        WRITE(LU06,*) '  *************************'
        WRITE(LU06,*) '  Running case:', JFOIL 
        WRITE(LU06,*) '  *************************'
      ELSEIF (LINE(1:6) .EQ. 'CHORDO') THEN
        WRITE(TLINE, 1000) LINE(7:50)
        READ(TLINE,*) CHORDO
      ELSEIF (LINE(1:6) .EQ. 'CLAMP1') THEN
        LCLAMP1 = TT
        WRITE(TLINE, 1000) LINE(7:50)
        READ(TLINE,*)CLAMP1(1), CLAMP1(2), CLAMP1(3), CLAMP1(4),
     $       CLAMP1(5), CLAMP1(6), CLAMP1(7), CLAMP1(8),
     $       CLAMP1(9), CLAMP1(10),CLAMP1(11),CLAMP1(12),
     $       CLAMP1(13),CLAMP1(14),CLAMP1(15),CLAMP1(16)
      ELSEIF (LINE(1:6) .EQ. 'CLAMP2') THEN
        LCLAMP2 = TT
        WRITE(TLINE, 1000) LINE(7:50)
        READ(TLINE,*) CLAMPVS
      ELSEIF (LINE(1:5) .EQ. 'COORD') THEN
        WRITE(TLINE, 1000) LINE(6:50)
        READ(TLINE,*) IARGPU 
        IF(IARGPU .GT. NARGP) THEN
          WRITE(LU06,*) 'Error 104: IARGPU .GT. NARGP (profoil.f)'
          call stoptst
        ENDIF
C--------DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
      ELSEIF (LINE(1:5) .EQ. 'DEBUG') THEN
        WRITE(LU06,*) ' DEBUG keyword not available anymore '
        call stoptst
      ELSEIF (LINE(1:5) .EQ. 'DELAN') THEN
        WRITE(TLINE, 1000) LINE(6:50)
        READ(TLINE,*) DELAN
      ELSEIF (LINE(1:9) .EQ. 'DELVSCALE') THEN
        WRITE(TLINE, 1000) LINE(10:50)
        READ(TLINE,*) JSEG, TEMP
        DELVSC(JSEG) = TEMP
        LDELVSC(JSEG) = TT
      ELSEIF (LINE(1:4) .EQ. 'DELV') THEN
        LLDELV = TT
        WRITE(TLINE, 1000) LINE(5:50)
        READ(TLINE,*) ICASE, JSEG, VALUE
        LDELV(JSEG)  = TT
        IDVTP(JSEG)  = ICASE
        IF (LDELVSC(JSEG)) THEN
          SCALE = DELVSC(JSEG)
        ELSE
          SCALE = 1.
        ENDIF
        IF (IDVTP(JSEG) .EQ. 1) THEN
C---  linear velocity distribution for entire segment
          VTILDE(JSEG) = VALUE * SCALE
        ELSEIF (IDVTP(JSEG) .EQ. 2) THEN
C---  parabolic velocity distribution for entire segment
          VTILDE(JSEG) = VALUE * SCALE
        ELSEIF (IDVTP(JSEG) .EQ. 3) THEN
C---  linear in x/c approximately for entire segment
          VTILDE(JSEG) = VALUE *SCALE
        ELSEIF (IDVTP(JSEG) .EQ. 11 
     $         .OR. IDVTP(JSEG) .EQ. 12
     $         .OR. IDVTP(JSEG) .EQ. 13
     $         .OR. IDVTP(JSEG) .EQ. 14
     $         .OR. IDVTP(JSEG) .EQ. 15
     $         .OR. IDVTP(JSEG) .EQ. 16) THEN
C---  11: linear velocity distribution given for subsegments
C---  via WGHTPHI and SSDELV 
C---  12: quadratic velocity distribution given for subsegments
C---  via WGHTPHI and SSDELV 
C---  13: cubic velocity distribution given for subsegments
C---  via WGHTPHI and SSDELV P- smooth
C---  14: cubic velocity distribution given for subsegments
C---  via WGHTPHI and SSDELV P+ smooth 
C---  15: cubic velocity distribution given for subsegments
C---  via WGHTPHI and SSDELV P- and P+ smooth 
C---  16: cubic velocity distribution given for subsegments
C---  via WGHTPHI and SSDELV for free ends
          LWGHT = TT
          ISPSEG = ISPSEG + 1
          KSPSEG(JSEG) = ISPSEG
          IF(KSPSEG(JSEG) .GT. NSPSEG) THEN
            WRITE(LU06,*) ' Error 105.5: ISPSEG > NSPSEG (profoil.f)'
            call stoptst
          ENDIF
          KSBSEG(JSEG) = INT(VALUE+.2)
          IF(KSBSEG(JSEG) .GT. NSBSEG) THEN
            WRITE(LU06,*) ' Error 105: ISBSEG > NSBSEG (profoil.f)'
            call stoptst
          ENDIF
          DO 200 JSBSEG = 1, KSBSEG(JSEG)
            READ(LU10,*) WGHTPHI(ISPSEG,JSBSEG), SSDELV(ISPSEG,JSBSEG)
            SSDELV(ISPSEG, JSBSEG) = SSDELV(ISPSEG, JSBSEG) * SCALE
 200      CONTINUE
          IF(DABS(WGHTPHI(ISPSEG,KSBSEG(JSEG))-1.).GT.0.001)THEN
            WRITE(LU06,*)  
     $           ' Error 107: WGHTPHI(ISBSEG) .NE. 1 (profoil.f)'
            call stoptst
          ENDIF
        ELSE
          WRITE(LU06,*)  
     $         ' Error 108: This IDVTP(.) not valid (profoil.f)'
          call stoptst
        ENDIF
      ELSEIF (LINE(1:4) .EQ. 'DUMP') THEN
        CALL DUMP
C--------EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
      ELSEIF (LINE(1:4) .EQ. 'ECHO') THEN
C---  do echo out convergence history
        LECHO = TT
      ELSEIF (LINE(1:3) .EQ. 'ETC') THEN
        WRITE(TLINE, 1000) LINE(4:50)
        READ(TLINE,*) EM, ER
C--------FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      ELSEIF (LINE(1:6) .EQ. 'FINISH') THEN
C---  Evaluate: airfoil coordinates and normalize, moment, 
C     Q integrals, airfoil surface slope.
        CALL ARGHP
        CALL PHARM
        WRITE(LU06,*)
        CALL QHARM
        CALL COORD
        CALL NORM
        CALL ARCSLEN
        IF(LTHICKE) THEN
           CALL THICKE(XCRD,YCRD,IARGP+1,THKMAX,XCMAX,YTHK)
        ELSE
           CALL THICK
        ENDIF
        if (lteflap) call flapit
        CALL FSA2B2
        CALL MOMENT(0.D0,CM)
        CALL FSQ
        CALL THET
        WRITE(LU06,1002)
 1002   FORMAT(2X,' ********************** AIRFOIL DESIGN IS FINISHED ',
     $       '**********************')
        WRITE(LU06,*) 
        CALL RESET('PRINT__')
        WRITE(TLINE, 1000) LINE(7:50)
        READ(TLINE,*) IPRT(1),IPRT(2),IPRT(3),IPRT(4),IPRT(5),
     $       IPRT(6),IPRT(7),IPRT(8),IPRT(9),IPRT(10)
        DO 320 I = 1, 10
          IF(IPRT(I) .EQ. 10) THEN
            LPRT10 = TT
          ELSEIF(IPRT(I) .EQ. 20) THEN 
            LPRT20 = TT
          ELSEIF(IPRT(I) .EQ. 30) THEN 
            LPRT30 = TT
          ELSEIF(IPRT(I) .EQ. 40) THEN 
            LPRT40 = TT
          ELSEIF(IPRT(I) .EQ. 50) THEN 
            LPRT50 = TT
          ELSEIF(IPRT(I) .EQ. 60) THEN 
            LPRT60 = TT
          ELSEIF(IPRT(I) .EQ. 70) THEN 
            LPRT70 = TT
          ELSEIF(IPRT(I) .EQ. 80) THEN 
            LPRT80 = TT
          ELSEIF(IPRT(I) .EQ. 90) THEN 
            LPRT90 = TT
          ELSEIF(IPRT(I) .EQ. 100) THEN 
            LPRT100 = TT
          ELSEIF(IPRT(I) .EQ. 101) THEN 
            LPRT101 = TT
          ELSE
          ENDIF
 320    CONTINUE
        CALL FORDAT
      ELSEIF (LINE(1:4) .EQ. 'FLIP') THEN
        LFLIP = TT
      ELSEIF (LINE(1:4) .EQ. 'FOIL') THEN
        ISEG = ISEG + 1
        IF(ISEG .GT. NSEG) THEN
          WRITE(LU06,*) ' Error 103: ISEG > NSEG (profoil.f)'
          CALL STOPTST
        ENDIF
        IF(ISEG .GT. 100) THEN
c...   This is due to a conflict w/ another variable in another subroutine ... cannot remember now.
          WRITE(LU06,*) ' Error 103: ISEG > 100 (profoil.f)'
          CALL STOPTST
        ENDIF
        WRITE(TLINE, 1000) LINE(5:50)
        READ(TLINE,*) AM(ISEG), ALFAS(ISEG), DUMMY, ADDVAL
        IF(LADDALF) THEN
          ALFAS(ISEG) = ALFAS(ISEG) + ADDVAL
        ENDIF
      ELSEIF (LINE(1:3) .EQ. 'FTE') THEN
        WRITE(TLINE, 1000) LINE(4:50)
        READ(TLINE,*) ANGLE, PHIEPP(1), PHIEPP(2) 
        LFTE = TT
C---  convert angle to epsilon in radians
        EPP = DTOR*ANGLE/PI
        WRITE(LU06,*) '  *******************'
        WRITE(LU06,*) '  * Finite TE Angle *'
        WRITE(LU06,*) '  *******************'
        WRITE(LU06,*)
      ELSEIF(LINE(1:11) .EQ. 'FTRANSNEWT1') THEN
C---  fixed transition for current NEWT1 line 
        LLFXTR1(IEQU1) = TT
        WRITE(TLINE, 1000) LINE(12:50)
        READ(TLINE,*) JSEGTR1(IEQU1), ITMPL
        CALL ITOL(ITMPL,LGCL)
        LLBOSTR(IEQU1) = LGCL
      ELSEIF(LINE(1:11) .EQ. 'FTRANSNEWT2') THEN
C---  fixed transition for current NEWT2 line 
        LLFXTR2(IADJS) = TT
        WRITE(TLINE, 1000) LINE(12:50)
        READ(TLINE,*) JSEGTR2(IADJS), ITMPL
        CALL ITOL(ITMPL,LGCL)
        LLBETR(IADJS) = LGCL
      ELSEIF(LINE(1:10) .EQ. 'FTRANSSBLA') THEN
C---  fixed transition for current select boundary layer analysis
        LFXTR = TT
        WRITE(TLINE, 1000) LINE(11:50)
        READ(TLINE,*) JSEGTR, ITMPL
        CALL ITOL(ITMPL,LGCL)
        LBOSTR = LGCL
C--------GGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG
      ELSEIF (LINE(1:13) .EQ. 'GENSYM_REPORT') THEN
        LGENSYM_REPORT = TT
      ELSEIF (LINE(1:12) .EQ. 'GENSYM_XF_YF') THEN
        WRITE(TLINE, 1000) LINE(13:50)
        READ(TLINE,*) XF, YF
        LGENSYM = TT
        LGENSYM_XF_YF = TT
      ELSEIF (LINE(1:17) .EQ. 'GENSYM_XF_YHALFTC') THEN
        WRITE(TLINE, 1000) LINE(18:50)
        READ(TLINE,*) XF, YHALFTC
        LGENSYM = TT
      ELSEIF (LINE(1:6) .EQ. 'GENSYM') THEN
        call gensym
C--------HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH
C--------IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII

      ELSEIF(LINE(1:3) .EQ. 'IBL') THEN
C
C     Added AG, 24 Oct 2001
C     to compute boundary layer for a given velocity distribution
C
        WRITE(TLINE, 1000) LINE(4:50)
        READ(TLINE,*) RINFAN
        RINF = RINFAN
        OPEN(UNIT = 36,FILE = 'ibl.in',status = 'old')
        REWIND 36
        read(36,*) ibl
        if (ibl .gt. nbl) then
          write(lu06,*) ' '
          write(lu06,*) ' IBL = ',IBL,'   NBL = ',NBL
          write(lu06,*) ' IBL cannot be greater than NBL '
          write(lu06,*) ' ERROR: in ibl.in '
          stop
        endif
        do jbl = 1,ibl
          read(36,*) ii, sbl(jbl), vbl(jbl)
        enddo
        lfxtr = .false.
        CALL probl
        CALL drln
        write(lu06,*) ' '
        write(lu06,*) ' writing 37'
        write(lu06,*) ' '
        do jbl = 1,ibl
          write(37,233) jbl, sbl(jbl), vbl(jbl),
     &                  h12bl(jbl), drlnbl(jbl)
        enddo
 233  format(i3,2x,4(f9.6,2x),f16.12)

      ELSEIF (LINE(1:4) .EQ. 'IDES') THEN
        ISTAGE = ISTAGE + 1
        IF(.NOT. LSKIP) CALL ITERATE
      ELSEIF (LINE(1:5) .EQ. 'IFREQ') THEN
C---  number of frequencies to track for N-values
        WRITE(TLINE, 1000) LINE(6:50)
        READ(TLINE,*) IFREQ
      ELSEIF (LINE(1:3) .EQ. 'ILE') THEN
        WRITE(TLINE, 1000) LINE(4:50)
        READ(TLINE,*) ILE
      ELSEIF (LINE(1:7) .EQ. 'ITERMAX') THEN
        WRITE(TLINE, 1000) LINE(8:50)
        READ(TLINE,*) ITERMAX
        LMANUAL = FF
C--------JJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJ
C--------KKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKK
C--------LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
      ELSEIF (LINE(1:8) .EQ. 'LENGTH_B') THEN
        WRITE(TLINE, 1000) LINE(9:50)
        READ(TLINE,*) LENGTH_B
C--------MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
      ELSEIF (LINE(1:11) .EQ. 'MAKE_MIRROR') THEN
        if(lldelv) then
          write(lu06,*) 
     $         'Error 111: Cannot mirror section when DELV is used'
          call stoptst
        endif
        if(lsym) then
          write(lu06,*) 
     $         'Error 111.1: Symmetric section is already mirrored'
          call stoptst
        endif
        call mirror
C--------NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
      ELSEIF (LINE(1:5) .EQ. 'NCOMP') THEN
C---  parameter related to ENN computation
        WRITE(TLINE, 1000) LINE(6:50)
        READ(TLINE,*) NCOMP
      ELSEIF (LINE(1:4) .EQ. 'NEGN') THEN
C---  track Negative N-values. 
        LNEGN = TT
      ELSEIF (LINE(1:4) .EQ. 'NEWT') THEN
        CALL NEWTIN
      ELSEIF (LINE(1:6) .EQ. 'NOECHO') THEN
C---  do not echo out convergence history
        LECHO = FF
C--------OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
C--------PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
      ELSEIF (LINE(1:5) .EQ. 'PAUSE') THEN
C---  save the jacobian (sensitivity) before scrambling in SYSNEWT
        WRITE(LU06,*) ' Enter number to resume, letter to debug',
     $        ' (if in debug mode)'
        READ(lu05,*) JUNK
      ELSEIF (LINE(1:4) .EQ. 'PHIS') THEN
        WRITE(TLINE, 1000) LINE(5:50)
        READ(TLINE,*) AS(1), AS(2)
      ELSEIF (LINE(1:9) .EQ. 'PROTECTED') THEN
        WRITE(LU06,*) '***************************************',
     $       '***************************' 
        WRITE(LU06,*) '*  This is a final airfoil design and is',
     $       ' protected: DO NOT MODIFY *'
        WRITE(LU06,*) '***************************************',
     $       '***************************' 
C--------QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C--------RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
      ELSEIF (LINE(1:4) .EQ. 'RBLA') THEN
C---  reset boundary layer analysis
        CALL RESET('BLA____')
      ELSEIF (LINE(1:7) .EQ. 'RCLAMP1') THEN
C---  reset CLAMP1 values 
        CALL RESET('CLAMP1_')
      ELSEIF (LINE(1:7) .EQ. 'RCLAMP2') THEN
C---  reset CLAMP2 value
        CALL RESET('CLAMP2_')
      ELSEIF (LINE(1:4) .EQ. 'REC') THEN
        WRITE(TLINE, 1000) LINE(4:50)
        READ(TLINE,*) AKA(1), AKA(2)
      ELSEIF (LINE(1:5) .EQ. 'RELAX') THEN
        WRITE(TLINE, 1000) LINE(6:50)
        READ(TLINE,*) RELAX 
      ELSEIF (LINE(1:8) .EQ. 'RESETALL') THEN
C---  reset all data
        CALL DEFLT
      ELSEIF (LINE(1:5) .EQ. 'RFOIL') THEN
C---  reset airfoil design data
        CALL RESET('FOIL___')
      ELSEIF (LINE(1:5) .EQ. 'RNEWT') THEN
C---  reset Newton iteration data
        CALL RESET('NEWT___')
C--------SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
      ELSEIF(LINE(1:4) .EQ. 'SBLA') THEN
C---  setup
        LENN  = FF
        LDRLN = FF
C---  Select boundary layer analysis 
        WRITE(TLINE, 1000) LINE(6:50)
        READ(TLINE,*) JSEGAN, ITMPL, ALFAAN,
     $       RINFAN,
     $       MPRTBL(1), MPRTBL(2), MPRTBL(3), 
     $       MPRTBL(4), MPRTBL(5), MPRTBL(6), 
     $       MPRTBL(7), MPRTBL(8), MPRTBL(9),
     $       MPRTBL(10),MPRTBL(11),MPRTBL(12)
        CALL ITOL(ITMPL,LGCL)
        LBOSAN = LGCL
C---  Determine number of files to print for the select analysis
        JPRTBL = 0
 310    CONTINUE
        JPRTBL = JPRTBL + 1
        IF(MPRTBL(JPRTBL) .NE. 0) GOTO 310
        IPRTBL = JPRTBL - 1 
        DO 315 JPRTBL = 1, IPRTBL
          IF(MPRTBL(JPRTBL) .EQ. 19) LENN  = TT
          IF(MPRTBL(JPRTBL) .EQ. 18) LDRLN = TT
 315    CONTINUE
      ELSEIF (LINE(1:12) .EQ. 'SCALEVELDIST') THEN
        LSCALEV = TT
        WRITE(TLINE, 1000) LINE(13:50)
        READ(TLINE,*) PROPO
      ELSEIF (LINE(1:5) .EQ. 'SENSI') THEN
C---  save the jacobian (sensitivity) before scrambling in SYSNEWT
        LSENSI = TT
      ELSEIF (LINE(1:4) .EQ. 'SIGN') THEN
        WRITE(TLINE, 1000) LINE(5:50)
        READ(TLINE,*) SIGNPER 
      ELSEIF(LINE(1:18) .EQ. 'SKIP_UNKNOWN_WORDS') THEN
        LSKIPQ = .NOT. LSKIPQ
      ELSEIF (LINE(1:6) .EQ. 'SNEWT2') THEN
        LSNEWT2 = TT
      ELSEIF (LINE(1:4) .EQ. 'STAT') THEN
C---  print out the statistics
        CALL STATS
      ELSEIF (LINE(1:7) .EQ. 'STOPTST') THEN
        call stoptst
      ELSEIF (LINE(1:10) .EQ. 'SYM_TOGGLE') THEN
        LSYM = .NOT. LSYM
      ELSEIF (LINE(1:6) .EQ. 'SYMTRY') THEN
        CALL SYMTRY
      ELSEIF (LINE(1:3) .EQ. 'SYM') THEN
        LSYM = TT
C--------TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
ccc   ELSEIF (LINE(1:16) .EQ. 'THICKNESS_EPPLER') THEN
ccc   WRITE(lu06,*) 'THICKNESS EPPLER option does not compute max camber'
ccc   LTHICKE = TT
      ELSEIF (LINE(1:6) .EQ. 'TEFLAP') THEN
        WRITE(TLINE, 1000) LINE(7:50)
        READ(TLINE,*) iflapmode, xhinge, deltaflap
        lteflap = tt
      ELSEIF (LINE(1:7) .EQ. 'TOLSPEC') THEN
        LMANUAL = FF
        WRITE(TLINE, 1000) LINE(8:50)
        READ(TLINE,*) TOLSPEC
      ELSEIF(LINE(1:4) .EQ. 'TEST') THEN
C---  Test for e^n method (not implemented on github)
        WRITE(TLINE, 1000) LINE(6:50)
        READ(TLINE,*) JSEGAN, ITMPL, ALFAAN, RINFAN
        CALL ITOL(ITMPL,LGCL)
        LBOSAN = LGCL
        LENN = TT
        CALL BLAN
        CALL ENVLPE(IBL)
        WRITE(33,333) VTILDE(4), ENNMAX, SBL(IBL), SKS, IBL
        WRITE(34,333) VTILDE(4), H12BL(IBL), SBL(IBL), SKS, IBL
 333    FORMAT(2X,4F10.5,I5)
        LENN = FF
C--------UUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUU
C--------VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
      ELSEIF (LINE(1:17) .EQ. 'FORCE_ZERO_VEL_PT' ) THEN
        LVELF0 = TT
      ELSEIF (LINE(1:7) .EQ. 'VELDIST') THEN
C---  Determine the velocity distribution.
        WRITE(LU06,*) 
        CALL RESET('PRINT__')
        WRITE(TLINE, 1000) LINE(8:50)
        READ(TLINE,*) IPRT(1),IPRT(2),IPRT(3),IPRT(4),IPRT(5),
     $       IPRT(6),IPRT(7),IPRT(8),IPRT(9),IPRT(10)
        DO 300 I = 1, 10
          IF(IPRT(I) .EQ. 10) THEN
            LPRT10 = TT
          ELSEIF(IPRT(I) .EQ. 20) THEN 
            LPRT20 = TT
          ELSEIF(IPRT(I) .EQ. 30) THEN 
            LPRT30 = TT
          ELSEIF(IPRT(I) .EQ. 40) THEN 
            LPRT40 = TT
          ELSEIF(IPRT(I) .EQ. 50) THEN 
            LPRT50 = TT
          ELSEIF(IPRT(I) .EQ. 60) THEN 
            LPRT60 = TT
          ELSEIF(IPRT(I) .EQ. 70) THEN 
            LPRT70 = TT
          ELSEIF(IPRT(I) .EQ. 80) THEN 
            LPRT80 = TT
          ELSEIF(IPRT(I) .EQ. 85) THEN 
            LPRT85 = TT
          ELSEIF(IPRT(I) .EQ. 90) THEN 
            LPRT90 = TT
          ELSEIF(IPRT(I) .EQ. 100) THEN 
            LPRT100 = TT
          ELSE
          ENDIF
 300    CONTINUE
        CALL VELDIST
      ELSEIF (LINE(1:7) .EQ. 'VELAM') THEN
C---  Determine the velocity at the points AM(.) and AS(.)
        CALL VELAM
      ELSEIF (LINE(1:4) .EQ. 'VLEV') THEN
        WRITE(TLINE, 1000) LINE(5:50)
        READ(TLINE,*) IVEL, VS(IVEL)
C--------WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW
      ELSEIF (LINE(1:5) .EQ. 'WEDGE') THEN
        WRITE(TLINE, 1000) LINE(6:50)
        READ(TLINE,*) WEDGET
        LWEDGE = TT
C--------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C--------YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY
C--------ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ
      ELSE 
        IF (.NOT. LSKIPQ) THEN
          WRITE(LU06,*) 
     $         ' Error 114: Word is not in PROFOIL dictionary or'
          WRITE(LU06,*) '        line is not expected. (profoil.f)'
          WRITE(LU06,1001) ILINE, LINE(1:10)
 1001     FORMAT(/2X,'Error in line number:',2X,I3/
     $         2X,'line>>',A,'...'//)
          call stoptst
        ELSE
          WRITE(LU06,*) ' Warning: Word is not in PROFOIL dictionary or'
          WRITE(LU06,*) '          line #',ILINE, ' is not expected.'
          WRITE(LU06,*) '          (profoil.f)'
        ENDIF
      ENDIF
      IF(.NOT. LEND) GOTO 10
C---  until iteration
      CALL STATS
      END ! PROFOIL

