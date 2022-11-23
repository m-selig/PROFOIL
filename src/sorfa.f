
      SUBROUTINE SORFA
C***********************************************************************
C...Compute the T(.) integrals of the second integral constraint: ORFA
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'

      DO 100 I = 1, ISEG
         IF (I .EQ. 1) THEN
            AMLL = AM0
         ELSE
            AMLL = AM(I-1)
         ENDIF
         N = NINT((AM(I) - AMLL)/DELAN)
         DELAA =  (AM(I) - AMLL)/N
         F0 = T(AMLL,ALFAS(I))
         ORFA(I) = 0.
         PHI = AMLL
         DO 200 J = 1, N
            PHI = PHI + DELAA
            F1 = T(PHI,ALFAS(I))
            ORFA(I) = ORFA(I) + 0.5*(F1 + F0)*DELAA*DTOR
            F0 = F1
  200    CONTINUE
  100 CONTINUE
      RETURN
      END ! SORFA

