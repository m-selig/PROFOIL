
      SUBROUTINE SYMTRY
C***********************************************************************
C.... airfoil is symmetric fill 
C.... in other data arrays
C     JCT     counter which moves you from the leading edge back
C     JCT2    counter for subsegments on special segments
C     JMSEG   JSEG_MASTER mirror segment on the top    (master side)
C     JSEG    JSEG_SLAVE  is the segment on the bottom (slave side)
C     IMSPSEG mirror special segment
C     JSPSEG_MASTER the values to mirror
C     JSBSEG_MASTER the values to mirror
C    
C     Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      ISEG = ILE * 2
      if(iseg .gt. nseg) then
        write(lu06,*) ' Error 105: iseg > nseg (symtry.f)'
        call stoptst
      endif
C-----determine the number of special segments for one side
C     half will be on top, and half on bottom
C     count the ones on top, then increment ISPSEG later in this
C     subroutine.
C
C     more fortran-if checks than necessary, but for clarity they are included
      IF(LWGHT) THEN
        ISPSEG = 0
        DO JSEG = 1, ILE
          IF(LDELV(JSEG)) THEN
            IF(IDVTP(JSEG) .GE. 11) THEN
              ISPSEG = ISPSEG + 1
            ENDIF
          ENDIF
        ENDDO
      ENDIF
      AM2 = AM(ILE) * 2.
C-----Do main arc limits and design angle of attacks
      JCT = 0
      DO 100 JSEG = ILE+1, ISEG
        JCT = JCT + 1
        JMSEG = ILE - JCT + 1
        IF(JSEG .EQ. ISEG) THEN
          AM(JSEG) = AM2
        ELSE
          AM(JSEG) = AM2 - AM(JMSEG-1)
        ENDIF
        ALFAS(JSEG) = -ALFAS(JMSEG)
 100  CONTINUE
C-----Do recovery, closure and FTE angle arc limits
      AKA(2) = AKA(1)
      AS(2)  = AM2 - AS(1) 
      AW(2)  = AM2 - AW(1) 
      IF(LFTE) PHIEPP(2) = AM2 - PHIEPP(1)
C-----Do special segment vtildes
      JCT = 0
      DO 200 JSEG_SLAVE = ILE+1, ISEG
        JCT = JCT + 1
        JSEG_MASTER = ILE - JCT + 1
        IF(LDELV(JSEG_MASTER)) THEN
          IF(IDVTP(JSEG_MASTER) .EQ. 1)THEN
            VTILDE(JSEG_SLAVE) = -VTILDE(JSEG_MASTER)
            IDVTP(JSEG_SLAVE)  = 1
            LDELV(JSEG_SLAVE)  = TT
          ELSEIF(IDVTP(JSEG_MASTER) .EQ. 2) THEN
            WRITE(LU06,*) 
     $           'Error 100: Cannot mirror parabolic vtilde (symtry.f)'
            CALL STOPTST
          ELSEIF(IDVTP(JSEG_MASTER) .EQ. 3) THEN
            VTILDE(JSEG_SLAVE) = -VTILDE(JSEG_MASTER)
            IDVTP(JSEG_SLAVE)  = 3
            LDELV(JSEG_SLAVE)  = TT
          ELSEIF(IDVTP(JSEG_MASTER) .GE. 11)THEN
C...  starting from the LE and moving along the l.s. to the TE
C     ISPSEG gets increment each time one is mirrored from the top
C     (master) to the bottom (slave)
            ISPSEG = ISPSEG + 1
            if (ispseg .gt. nspseg) then
              write(LU06,*) ' Error 106: ispseg > nspseg (symtry.f)'
              call stoptst
            endif
            KSPSEG(JSEG_SLAVE) = ISPSEG
            JSPSEG_MASTER = KSPSEG(JSEG_MASTER)
            KSBSEG(JSEG_SLAVE) = KSBSEG(JSEG_MASTER)
            ISBSEG = KSBSEG(JSEG_SLAVE)
            JCT2 = 0
            DO 300 JSBSEG = 1, ISBSEG
              JCT2 = JCT2 + 1
              JSBSEG_MASTER = ISBSEG - JCT2
              IF(JSBSEG_MASTER .EQ. 0) THEN
                WGHTPHI(ISPSEG,JSBSEG) = 1.
                SSDELV(ISPSEG,JSBSEG) = -SSDELV(JSPSEG_MASTER,ISBSEG)
              ELSE
                WGHTPHI(ISPSEG,JSBSEG) = 
     $               1. - WGHTPHI(JSPSEG_MASTER,JSBSEG_MASTER)
                SSDELV(ISPSEG,JSBSEG) = -SSDELV(JSPSEG_MASTER,ISBSEG)
     $               + SSDELV(JSPSEG_MASTER,JSBSEG_MASTER)
              ENDIF
 300        CONTINUE
            IF(IDVTP(JSEG_MASTER) .EQ. 11)THEN
              IDVTP(JSEG_SLAVE) = 11
              LDELV(JSEG_SLAVE) = TT
            ELSEIF(IDVTP(JSEG_MASTER) .EQ. 12)THEN
              IDVTP(JSEG_SLAVE) = 12
              LDELV(JSEG_SLAVE) = TT
            ELSEIF(IDVTP(JSEG_MASTER) .EQ. 13)THEN
              IDVTP(JSEG_SLAVE) = 13
              LDELV(JSEG_SLAVE) = TT
            ELSEIF(IDVTP(JSEG_MASTER) .EQ. 14)THEN
              IDVTP(JSEG_SLAVE) = 14
              LDELV(JSEG_SLAVE) = TT
            ELSEIF(IDVTP(JSEG_MASTER) .EQ. 15)THEN
              IDVTP(JSEG_SLAVE) = 15
              LDELV(JSEG_SLAVE) = TT
            ELSEIF(IDVTP(JSEG_MASTER) .EQ. 16)THEN
              IDVTP(JSEG_SLAVE) = 16
              LDELV(JSEG_SLAVE) = TT
            ELSE
              WRITE(LU06,*) 
     $             ' Error 221: a non-symmetric vtilde is used ',
     $             '(symtry.f).'
              STOP
            ENDIF
          ELSE
            WRITE(LU06,*) 
     $           ' Error 222: a non-symmetric vtilde is used ',
     $           '(symtry.f).'
            STOP
          ENDIF
        ENDIF
 200  CONTINUE
      RETURN
      END ! SYMTRY

