
      SUBROUTINE CHECK(JM)
C***********************************************************************
C...  Run a [check] on the design parameters
C
C     Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      LOGICAL LSTOP
      INTEGER JM
C...  JM = 0  vel not yet computed from the input data, check input only.
C     JM > 0  check all
C     JM = 10 check only the velocity levels and return
      LSTOP = FF
C...  check v*
      IF(JM .EQ. 0) THEN
        IF(VS(IVEL) .LE. 0.) THEN
          WRITE(LU06,*) 
     $         '  Error 1000: A value of VS(IVEL) is negative (check.f)'
          LSTOP = TT
        ENDIF
      ELSE 
        DO 100 I = 1, ISEG
          IF(VS(I) .LE. 0.) THEN
            WRITE(LU06,*) 
     $           '  Error 1001: A value of VS(.) is negative (check.f)'
            LSTOP = TT
          ENDIF
 100    CONTINUE
      ENDIF
      IF(JM .EQ. 10) RETURN
C...  check phi at the leading edge 
C     thesis notes PROFOIL vol. 1 4-27-90 P. 14
      AMAX = 2.*ALFAS(ILE)*DTOR + PI
      AMIN = 2.*ALFAS(ILE+1)*DTOR + PI
      IF(AMAX .EQ. AMIN) WRITE(LU06,*) 
     $     ' Error 1002: Check ILE (check).f '
      ATST = AM(ILE) * DTOR
      IF (AMAX .LT. ATST .OR. ATST .LT. AMIN) THEN
        WRITE(LU06,1000) AMIN*RTOD, ATST*RTOD, AMAX*RTOD 
 1000   FORMAT(/1X,4X,
     $       'Error 1003: PHIMIN < PHILE < PHIMAX is not true',
     $       ' (check.f)',
     $       /1X,4X,F6.1,2X,F6.1,2X,F6.1)
        AM(ILE) = 0.5 * (AMIN + AMAX) * RTOD
        WRITE(LU06,1001) AM(ILE)
 1001   FORMAT(/1X,4X,'Automatic error recovery attempted',
     &       /1X,4X,'AM(ILE) = ',F10.5)
      ENDIF
C...  check k upper and lower
      IF (AKA(1) .EQ. 0 .OR. AKA(1) .LT. -1.) THEN
        WRITE(LU06,*) '  Error 1004: AKA(1) EQ 0 OR LT -1 (check.f)'
        LSTOP = TT
      ENDIF       
      IF (AKA(2) .EQ. 0 .OR. AKA(2) .LT. -1.) THEN
        WRITE(LU06,*) '  Error 1005: AKA(2) EQ 0 OR LT -1 (check.f)'
        LSTOP = TT
      ENDIF       
C...  check phi at the trailing edge
      IF((AM(ISEG)-360) .GT. TINY) THEN
        WRITE(LU06,*) '  Error 1006: AM(ISEG) NE 360 (check.f)'
        LSTOP = TT
      ENDIF
C...  check sequence of phi's to make sure that they do not overlap
      DO 50 JSEG = 1, ISEG-1
        IF (AM(JSEG) .GT. AM(JSEG+1)) THEN
          WRITE(LU06,*) '  Error 1007: AM(.) overlap (check.f)', JSEG
          DO 51 JSEG1 = 1, ISEG
            WRITE(LU06,1034) JSEG1, AM(JSEG1)/SCLF
 1034       FORMAT(2X,'AM(',I3, ') = ',F8.4)
 51       CONTINUE
          LSTOP = TT
        ENDIF
 50   CONTINUE
C...  check closure phi's
      IF  ((AS(1) .GT. AM(1))      .OR. (AS(1) .LT.   0.) 
     &     .OR.(AS(2) .LT. AM(ISEG-1)) .OR. (AS(2) .GT. 360.)) THEN
        WRITE(LU06,*) 
     $       '  Error 1008: AS(1) OR AS(2) out of bounds (check.f)'
        LSTOP = TT
      ENDIF
C...  stop excution if a error is detected 
      IF (LSTOP) STOP
      RETURN
      END ! CHECK

