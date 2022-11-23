
      SUBROUTINE SETPHI
C***********************************************************************
C...[Set] subsegment [phi] locations for adjustable special segments
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      DO 100 JSEG = 2, ISEG-1
         IF(IDVTP(JSEG) .GE. 11) THEN
            AMLEN = AM(JSEG) - AM(JSEG-1)
            ISBSEG = KSBSEG(JSEG)
            JSPSEG = KSPSEG(JSEG)
            DO 200 JSBSEG = 1, ISBSEG
              SSPHI(JSPSEG,JSBSEG) = AMLEN * WGHTPHI(JSPSEG,JSBSEG)
  200       CONTINUE
         ENDIF
  100 CONTINUE
      RETURN
      END ! SETPHI

