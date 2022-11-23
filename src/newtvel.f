
      SUBROUTINE NEWTVEL
C***********************************************************************
C...Compute the actual velocity at the Newton nodes based on
C   BOS-to-EOS or EOS-to-BOS coordinate convention.
C   ISBSEG from calling subroutine.
C   JSPSEG from calling subroutine.
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      IF(LBE) THEN
         DO 100 JSBSEG = 1, ISBSEG
            FJSBSEG(JSBSEG) = SSDELV(JSPSEG,JSBSEG)
  100    CONTINUE
      ELSE
         DO 200 JSBSEG = 1, ISBSEG-1
            JSBSEGT = ISBSEG - JSBSEG
            FJSBSEG(JSBSEG)=SSDELV(JSPSEG,JSBSEGT)-SSDELV(JSPSEG,ISBSEG)
  200    CONTINUE
         FJSBSEG(ISBSEG) = - SSDELV(JSPSEG,ISBSEG)
      ENDIF
      RETURN
      END ! NEWTVEL

