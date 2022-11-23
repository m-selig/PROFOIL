
      SUBROUTINE CLAMP
C***********************************************************************
C...  Keep step size within reasonable limits and within bounds.
C     If a step size is too large, invoke a clamp and 
C     set RELAX appropriately.
C
C     Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
C...  initialize 
      RELAX = 1.
C...  perform check for NEWT1 equations
      IF(LCLAMP1) THEN
        DO 100 JVAR1 = 1, IVAR1
          IF(DABS(CLAMP1(JVAR1)) .GT. 0.0001) THEN
            DELTAJ = DABS(DELTAS(JVAR1))
            IF(DELTAJ .GT. CLAMP1(JVAR1)) THEN
              RELAXT = CLAMP1(JVAR1)/DELTAJ
              IF(RELAXT .LT. RELAX) RELAX = RELAXT
            ENDIF
          ENDIF
 100    CONTINUE
      ENDIF
C...  perform check on vtildes
      IF(LCLAMP2) THEN
        DO 200 JVAR2 = 1, IVAR2
          DELTAJ = DABS(DELTAVS(JVAR2))
          IF(DELTAJ .GT. CLAMPVS) THEN
            RELAXT = CLAMPVS/DELTAJ
            IF(RELAXT .LT. RELAX) RELAX = RELAXT
          ENDIF
 200    CONTINUE
      ENDIF
      RETURN
      END ! CLAMP

