
      SUBROUTINE SETPERV
C***********************************************************************
C...Set perturbation in the v*(phi) for Newton iteration.
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      IF(.NOT. LADJSS) RETURN
      IF(ITER .EQ. 1) THEN
         DO 100 JVAR2 = 1, IVAR2
            DELTAVP(JVAR2) = -0.0001
  100    CONTINUE
      ELSE
         DO 200 JVAR2 = 1, IVAR2
            DELTAJ = 0.1 * DELTAVS(JVAR2)
            DELTAD = DABS(DELTAJ)
            IF(DELTAD       .GT. 0.0001) THEN
               DELTAVP(JVAR2) = -0.0001
            ELSEIF(DELTAD .LT.  0.000001) THEN
               DELTAVP(JVAR2) = -0.000001
            ELSE
               DELTAVP(JVAR2) = DELTAJ
            ENDIF
  200    CONTINUE
      ENDIF
      RETURN
      END ! SETPERV

