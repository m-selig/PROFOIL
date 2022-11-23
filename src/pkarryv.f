
      SUBROUTINE PKARRYV(JSEG)
C***********************************************************************
C...Pack VPACK(IPACK)
C   JSPSEG from calling routine
C   ISBSEG from calling routine
C   IPACK  from calling routine
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER JSEG
      IF(LBE) THEN
         VPACK(1) = VS(JSEG)
         DO 100 JPACK = 2, IPACK
            JSBSEG = JPACK - 1
            VPACK(JPACK) = VS(JSEG) + SSDELV(JSPSEG,JSBSEG)
  100    CONTINUE
      ELSE
         DO 200 JPACK = 1, IPACK-1
            JSBSEG = ISBSEG - JPACK + 1
            VPACK(JPACK) = VS(JSEG) + SSDELV(JSPSEG,JSBSEG)
  200    CONTINUE
         VPACK(IPACK) = VS(JSEG)
      ENDIF
      RETURN
      END ! PKARRYV

