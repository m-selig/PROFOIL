
      SUBROUTINE SAF
C***********************************************************************
C...AFSIN & AFCOS
C
C  Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      DO 100 I = 1, ISEG
         AFCOS(I) = DCOSG(2*ALFAS(I))
         AFSIN(I) = DSING(2*ALFAS(I))
  100 CONTINUE
      RETURN
      END ! SAF

