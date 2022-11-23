
      SUBROUTINE NEWTIN
C***********************************************************************
C...  Newton line input subroutine
C
C     Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      LOGICAL LGCL
 1000 FORMAT(A,'0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0')
 1001 FORMAT('Error in line>>',A,'...')
      IF (LINE(1:5) .EQ. 'NEWT1') THEN
        LNEWT1 = TT
        LITON = TT
        IEQU1 = IEQU1 + 1
        IF(IEQU1 .GT. NEQU1) THEN
          WRITE(lu06,*) ' Error 115: IEQU1 > NEQU1 (newtin.f)'
          call stoptst
        ENDIF
C--------whether or not the current NEWT line is from a boundary layer RX,
C     assume that there is no fixed transition.
        LLFXTR1(IEQU1) = FF
        IF (LINE(1:7) .EQ. 'NEWT1G0') THEN
C-----------Global-variable Rx (see notes 10-14-90 p. 1+)
          WRITE(TLINE, 1000) LINE(8:50)
          READ(TLINE,*) IFTP1(IEQU1), 
     &         FNEWT1(IEQU1),
     &         ITP1(IEQU1), ITP2(IEQU1), CLAMP1(IEQU1)
C-----------set appropriate iteration logicals 
          IF(IFTP1(IEQU1) .EQ. 100 .OR.
     &         IFTP1(IEQU1) .EQ. 184 .OR.
     &         IFTP1(IEQU1) .EQ. 185 .OR.
     &         IFTP1(IEQU1) .EQ. 190 .OR.
     &         IFTP1(IEQU1) .EQ. 191 .OR.
     &         IFTP1(IEQU1) .EQ. 192 .OR.
     &         IFTP1(IEQU1) .EQ. 193 .OR.
     &         IFTP1(IEQU1) .EQ. 194 .OR.
     &         IFTP1(IEQU1) .EQ. 195 .OR.
     &         IFTP1(IEQU1) .EQ. 196 .OR.
     &         IFTP1(IEQU1) .EQ. 197 .OR.
     &         IFTP1(IEQU1) .EQ. 198 .OR.
     &         IFTP1(IEQU1) .EQ. 199) THEN
C--------------do nothing               
          ELSEIF(IFTP1(IEQU1) .EQ. 101) THEN
C--------------zero-lift pitching-moment coefficient Rx
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
            LTHICK = TT
            LTHICKE= TT
            LMOMENT= TT
          ELSEIF(IFTP1(IEQU1) .EQ. 102) THEN
C--------------maximum thickness ratio Rx
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
            LTHICK = TT
            LTHICKE= TT
          ELSEIF(IFTP1(IEQU1) .EQ. 103) THEN
C--------------zero-lift angle-of-attack w.r.t chordline Rx
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
          ELSEIF(IFTP1(IEQU1) .EQ. 104) THEN
C--------------location of maximum thickness
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
            LTHICK = TT
            LTHICKE= TT
          ELSEIF(IFTP1(IEQU1) .EQ. 105) THEN
C--------------maximum camber
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
            LTHICK = TT
            LTHICKE= TT
          ELSEIF(IFTP1(IEQU1) .EQ. 106) THEN
C--------------leading edge radius
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
            LTHICK = TT
            LTHICKE= TT
          ELSEIF(IFTP1(IEQU1) .EQ. 107) THEN
C--------------maximum thickness ratio according to Eppler's routine
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
            LTHICK = TT
            LTHICKE= TT
          ELSEIF(IFTP1(IEQU1) .EQ. 121) THEN
C--------------t/c_max for gensym airfoil
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
            LGENSYM= TT
          ELSEIF(IFTP1(IEQU1) .EQ. 122) THEN
C--------------flap angle for gensym airfoil
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
            LGENSYM= TT
          ELSEIF(IFTP1(IEQU1) .EQ. 135) THEN
C--------------area calculation
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            if(.not. lsym)  then
              write(lu06,*)
     $             'Error 105 (newtin.f): Check afarea.f code before',
     $             ' continuing.  See the notes in the code.  Bug?',
     $             ' (newtin.f)'
              call stoptst
            endif
          ELSEIF(IFTP1(IEQU1) .EQ. 146) THEN
C--------------volume calculation
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            if(.not. lsym)  then
              write(lu06,*)
     $             'Error 103 (newtin.f): Requires a symmetric',
     $             ' airfoil with the LSYM line used.',
     $             ' (newtin.f)'
              call stoptst
            endif
          ELSEIF(IFTP1(IEQU1) .EQ. 155) THEN
C--------------trailing edge "thickness" in x (TEX)
            LTEZERO = FF
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
          ELSEIF(IFTP1(IEQU1) .EQ. 156) THEN
C--------------trailing edge thickness in y (TEY)
            LTEZERO = FF
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
          ELSEIF(IFTP1(IEQU1) .EQ. 157) THEN
C--------------trailing edge midpoint in x
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
          ELSEIF(IFTP1(IEQU1) .EQ. 158) THEN
C--------------trailing edge midpoint in y
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
          ELSE
            WRITE(lu06,*) 
     $           ' Error 116: Newton 1 type unknown (newtin.f)'
            call stoptst
          ENDIF
        ELSEIF (LINE(1:7) .EQ. 'NEWT1G1') THEN
C-----------Global-variable Rx with one condition
          WRITE(TLINE, 1000) LINE(8:50)
          READ(TLINE,*) IFTP1(IEQU1), 
     &         COND1(IEQU1), FNEWT1(IEQU1), 
     &         ITP1(IEQU1),  ITP2(IEQU1), CLAMP1(IEQU1)
C-----------set appropriate iteration logicals 
          IF(IFTP1(IEQU1) .EQ. 200) THEN
C--------------do nothing               
          ELSEIF(IFTP1(IEQU1) .EQ. 201) THEN
C--------------do nothing               
          ELSEIF(IFTP1(IEQU1) .EQ. 202) THEN
C--------------do nothing               
          ELSEIF(IFTP1(IEQU1) .EQ. 203) THEN
C--------------do nothing               
          ELSEIF(IFTP1(IEQU1) .EQ. 205) THEN
C--------------thickness at a given location
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
          ELSEIF(IFTP1(IEQU1) .EQ. 206) THEN
C--------------camber at a given location
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
          ELSE
            WRITE(lu06,*) 
     $           ' Error 117: Newton 1 type unknown (newtin.f)'
            call stoptst
          ENDIF
        ELSEIF (LINE(1:7) .EQ. 'NEWT1S0') THEN
C------------Segment Rx w/ no condition
          WRITE(TLINE, 1000) LINE(8:50)
          READ(TLINE,*) IFTP1(IEQU1), 
     &         JSEGIX1(IEQU1), ITMPL, FNEWT1(IEQU1), 
     &         ITP1(IEQU1), ITP2(IEQU1), CLAMP1(IEQU1)
          CALL ITOL(ITMPL,LGCL)
          LLBOS(IEQU1) = LGCL
C-----------set appropriate iteration logicals
          IF(IFTP1(IEQU1) .EQ. 400) THEN
C--------------x/c location Rx
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
          ELSEIF(IFTP1(IEQU1) .EQ. 401) THEN
C--------------s/c location Rx
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
          ELSEIF(IFTP1(IEQU1) .EQ. 402) THEN
C--------------y/c location Rx
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
          ELSEIF(IFTP1(IEQU1) .EQ. 403) THEN
C--------------desired angle of airfoil point from trailing edge
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
          ELSE
            WRITE(lu06,*) 
     $           ' Error 118: Newton 1 type unknown (newtin.f)'
            call stoptst
          ENDIF
        ELSEIF (LINE(1:7) .EQ. 'NEWT1S1') THEN
C-----------Segment Rx w/ one condition
          WRITE(TLINE, 1000) LINE(8:50)
          READ(TLINE,*) IFTP1(IEQU1), 
     &         JSEGIX1(IEQU1), ITMPL, COND1(IEQU1), 
     &         FNEWT1(IEQU1), 
     &         ITP1(IEQU1), ITP2(IEQU1), CLAMP1(IEQU1)
          CALL ITOL(ITMPL,LGCL)
          LLBOS(IEQU1) = LGCL
C-----------set appropriate iteration logicals
          IF(IFTP1(IEQU1) .EQ. 500) THEN
C--------------H_12 @ alfa^* for given Rinf
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
          ELSEIF(IFTP1(IEQU1) .EQ. 501) THEN
C--------------R_delta2 @ alfa^* for given Rinf
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
          ELSEIF(IFTP1(IEQU1) .EQ. 502) THEN
C--------------TC @ alfa^* for given Rinf
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
          ELSEIF(IFTP1(IEQU1) .EQ. 503) THEN
C--------------n_ENN @ alfa^* for given Rinf
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
          ELSEIF(IFTP1(IEQU1) .EQ. 504) THEN
C--------------n_Drela @ alfa^* for given Rinf
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
          ELSEIF(IFTP1(IEQU1) .EQ. 505) THEN
C--------------H_32 @ alfa^* for given Rinf
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
          ELSEIF(IFTP1(IEQU1) .EQ. 506) THEN
C--------------c_f @ alfa^* for given Rinf
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
          ELSE
            WRITE(lu06,*) 
     $           ' Error 119: Newton 1 type unknown (newtin.f)'
            call stoptst
          ENDIF
        ELSEIF (LINE(1:7) .EQ. 'NEWT1S2') THEN
C-----------Segment Rx w/ two conditions
          WRITE(TLINE, 1000) LINE(8:50)
          READ(TLINE,*) IFTP1(IEQU1), 
     &         JSEGIX1(IEQU1), ITMPL, 
     &         COND1(IEQU1), COND2(IEQU1), 
     &         FNEWT1(IEQU1), 
     &         ITP1(IEQU1), ITP2(IEQU1), CLAMP1(IEQU1)
          CALL ITOL(ITMPL,LGCL)
          LLBOS(IEQU1) = LGCL
C-----------set appropriate iteration logicals
          IF(IFTP1(IEQU1) .EQ. 600) THEN
C--------------H_12 for given Rinf and alfa
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
          ELSEIF(IFTP1(IEQU1) .EQ. 601) THEN
C--------------H_32 for given Rinf and alfa
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
          ELSE
            WRITE(lu06,*) 
     $           ' Error 120: Newton 1 type unknown (newtin.f)'
            call stoptst
          ENDIF
            ELSE
              WRITE(lu06,*) 
     $             ' Error 122: Newton 1 type unknown (newtin.f)'
          call stoptst
        ENDIF             
        IF(CLAMP1(IEQU1) .GT. 0.) LCLAMP1 = TT
      ELSEIF (LINE(1:5) .EQ. 'NEWT2') THEN
        LNEWT2 = TT
        LADJSS = TT
        IADJS = IADJS + 1
        IF(IADJS .GT. NADJS) THEN
          WRITE(lu06,*) ' Error 125: iadjs > nadjs (newtin.f)'
          call stoptst
        ENDIF
C--------whether or not the current NEWT line is from a boundary layer RX,
C     assume that there is no fixed transition.
        LLFXTR2(IADJS) = FF
        IF (LINE(1:8) .EQ. 'NEWT2SD0') THEN
C-----------Segment distribution 
          WRITE(TLINE, 1000) LINE(9:50)
          READ(TLINE,*) IFTP2(IADJS),JSEGIX2(IADJS),ITMPL,
     &         KADJSBS(IADJS)
          IF(KADJSBS(IADJS) .GT. NADJSBS) THEN
            WRITE(lu06,*) ' Error 126: iadjsbs > nadjsbs (newtin.f)'
            call stoptst
          ENDIF
          CALL ITOL(ITMPL,LGCL)
          LLBE(IADJS) = LGCL
C-----------prescribed distribution 
          DO 210 JADJSBS = 1, KADJSBS(IADJS)
            READ(10,*) SSS(IADJS,JADJSBS), SSF(IADJS,JADJSBS)
 210      CONTINUE
C-----------set appropriate iteration logicals
          IF (IFTP2(IADJS) .EQ. 100) THEN
C--------------prescribed v*(s) @ alfa^*
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
          ELSE
            WRITE(lu06,*) 
     $           ' Error 127: Newton 2 type unknown (newtin.f)'
            call stoptst
          ENDIF
        ELSEIF (LINE(1:8) .EQ. 'NEWT2SD1') THEN
C-----------Segment distribution with one condition
          WRITE(TLINE, 1000) LINE(9:50)
          READ(TLINE,*) IFTP2(IADJS), JSEGIX2(IADJS), ITMPL,
     &         R1(IADJS), KADJSBS(IADJS)
          IF(KADJSBS(IADJS) .GT. NADJSBS) THEN
            WRITE(lu06,*) ' Error 128: IADJSBS > NADJSBS (newtin.f)'
            call stoptst
          ENDIF
          CALL ITOL(ITMPL,LGCL)
          LLBE(IADJS) = LGCL
C-----------prescribed distribution 
          DO 220 JADJSBS = 1, KADJSBS(IADJS)
            READ(10,*) SSS(IADJS,JADJSBS), SSF(IADJS,JADJSBS)
 220      CONTINUE
C-----------set appropriate iteration logicals
          IF (IFTP2(IADJS) .EQ. 200) THEN
C--------------prescribed H_12(s) @ alfa^* for given R
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
          ELSEIF (IFTP2(IADJS) .EQ. 201) THEN
C--------------prescribed TC(s) @ alfa^* for given R
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
          ELSEIF (IFTP2(IADJS) .EQ. 202) THEN
C--------------prescribed n(s)_ENN @ alfa^* for given R
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
          ELSEIF (IFTP2(IADJS) .EQ. 203) THEN
C--------------prescribed n(s)_Drela @ alfa^* for given R
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
          ELSEIF (IFTP2(IADJS) .EQ. 204) THEN
C--------------prescribed H_32(s) @ alfa^* for given R
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
          ELSEIF (IFTP2(IADJS) .EQ. 205) THEN
C--------------prescribed cf(s) @ alfa^* for given R
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
          ELSE
            WRITE(lu06,*) 
     $           ' Error 129: Newton 2 type unknown (newtin.f)'
            call stoptst
          ENDIF
        ELSEIF (LINE(1:8) .EQ. 'NEWT2SD2') THEN
C-----------Segment distribution with two conditions
C           Used for relative velocity specification on ME airfoil (700)
C           and H32 BL development on isolated airfoil (300)
          WRITE(TLINE, 1000) LINE(9:50)
          READ(TLINE,*) IFTP2(IADJS), JSEGIX2(IADJS), ITMPL,
     $         R1(IADJS), R2(IADJS), KADJSBS(IADJS)
          IF(KADJSBS(IADJS) .GT. NADJSBS) THEN
            WRITE(lu06,*) ' Error 130: iadjsbs > nadjsbs (newtin.f)'
            call stoptst
          ENDIF
          CALL ITOL(ITMPL,LGCL)
          LLBE(IADJS) = LGCL
C-----------prescribed distribution 
          DO JADJSBS = 1, KADJSBS(IADJS)
            READ(10,*) SSS(IADJS,JADJSBS), SSF(IADJS,JADJSBS)
          ENDDO
C-----------set appropriate iteration logicals
          IF (IFTP2(IADJS) .EQ. 300) THEN
C-----------prescribed H_32(s) for given alfa and R
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
          ELSE
            WRITE(lu06,*) 
     $           ' Error 132: newt2sd2 type unknown (newtin.f)'
            call stoptst
          ENDIF
        ELSEIF (LINE(1:8) .EQ. 'NEWT2RD1') THEN
C-----------Ramp distribution with one condition
          WRITE(TLINE, 1000) LINE(9:50)
          READ(TLINE,*) IFTP2(IADJS), JSEGIX2(IADJS), ITMPL,
     &         R1(IADJS), KADJSBS(IADJS)
          IF(KADJSBS(IADJS) .GT. NADJSBS) THEN
            WRITE(lu06,*) ' Error 136: IADJSBS > NADJSBS (newtin.f)'
            STOP
          ENDIF
          CALL ITOL(ITMPL,LGCL)
          LLBE(IADJS) = LGCL
C-----------prescribed relative angle distribution 
          DO 230 JADJSBS = 1, KADJSBS(IADJS)
            READ(10,*) SSS(IADJS,JADJSBS), SSF(IADJS,JADJSBS)
 230      CONTINUE
C-----------set appropriate iteration logicals
          IF (IFTP2(IADJS) .EQ. 500) THEN
C--------------bubble ramp
            LARGHP = TT
            LHP    = TT
            LHQ    = TT
            LCOORD = TT
            LNORM  = TT
            LSLENS = TT
          ELSE
            WRITE(lu06,*) 
     $           ' Error 137: Newton 2 type unknown (newtin.f)'
            call stoptst
          ENDIF
        ELSE
          WRITE(lu06,*) 
     $         ' Error 138: line >>newt2* not valid (newtin.f)'
          WRITE(lu06,1001) LINE(1:60)
          call stoptst
        ENDIF
C--------set up Newton equation indices for segment/ramp distributions
        JSEG = JSEGIX2(IADJS)
        DO 400 JSBSEG = 1, KSBSEG(JSEG)
          IEQU2 = IEQU2 + 1
          IF(IEQU2 .GT. NEQU2) THEN
            WRITE(lu06,*) ' Error 139: iequ2 > nequ2 (newtin.f)'
            call stoptst
          ENDIF
          JVARIDX(IEQU2,1) = JSEG
          JVARIDX(IEQU2,2) = JSBSEG
 400    CONTINUE
      ENDIF
      RETURN
      END ! NEWTIN

