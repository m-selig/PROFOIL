
      SUBROUTINE SAMDEL
C***********************************************************************
C...AMDEL
C
C  Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      DO 100 I = 1, ISEG
         IF (I .EQ. 1) THEN
            AMLL = AM0
         ELSE
            AMLL = AM(I-1)
         ENDIF
         AMDEL(I) = 0.5 * (AM(I) - AMLL) * DTOR
  100 CONTINUE
      RETURN
      END ! SAMDEL

