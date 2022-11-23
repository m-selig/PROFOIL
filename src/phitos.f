
      SUBROUTINE PHITOS(JSEG)
C***********************************************************************
C...Determine the local arc length SJSBSEG(.) from SSPHI(.) 
C   for the adj special segment.
C   JSPSEG from calling subroutine
C   ISBSEG from calling subroutine
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER JSEG 
      JPT = 0 
      LWHERE = FF
C-----Determine location of AM(JSEG-1) between JPT-1 and JPT
C     and interpolate to get arc length S at beginning of segment JSEG.
  100 CONTINUE
         JPT = JPT + 1
         IF(AM(JSEG-1) .LE. APHI(JPT)) LWHERE = TT
      IF(.NOT. LWHERE) GOTO 100
      SBOS = (SLENS(JPT) - SLENS(JPT-1))/(APHI(JPT) - APHI(JPT-1))
     &      * (AM(JSEG-1) - APHI(JPT-1)) + SLENS(JPT-1)

C-----determine local arc length SJSBSEG(.) from SSPHI(..)
      DO 200 JSBSEG = 1, ISBSEG
         PHILOC = AM(JSEG-1) + SSPHI(JSPSEG,JSBSEG)
         LWHERE = FF
         JPT = JPT - 1
C--------determine location of PHILOC between JPT-1 and JPT
  210    CONTINUE
            JPT = JPT + 1
            IF(PHILOC .LE. APHI(JPT)) LWHERE = TT
         IF(.NOT. LWHERE) GOTO 210
C--------determine local arc length
         SJSBSEG(JSBSEG) = (SLENS(JPT) - SLENS(JPT-1))/
     &                     ( APHI(JPT) -  APHI(JPT-1))
     &                   * (    PHILOC -  APHI(JPT-1))
     &                   +               SLENS(JPT-1) - SBOS
  200 CONTINUE
      IF(.NOT. LBE) THEN
C--------Switch order from (BOS to EOS) to (EOS to BOS) on local s.
C        In other words, flip the local origin from the BOS to the EOS.
C        Note: this does not change SBOS
         DO 400 JSBSEG = 1, ISBSEG-1
            JSBSEGT = ISBSEG - JSBSEG
            STEMP(JSBSEG) = SJSBSEG(ISBSEG) - SJSBSEG(JSBSEGT)
  400    CONTINUE
         DO 500 JSBSEG = 1, ISBSEG-1
            SJSBSEG(JSBSEG) = STEMP(JSBSEG)
  500    CONTINUE
      ENDIF
      RETURN
      END ! PHITOS

