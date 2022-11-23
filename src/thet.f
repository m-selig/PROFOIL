
      SUBROUTINE THET
C***********************************************************************
C...Solve the [sys]tem of [Newt]on equations
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      DO 100 JARGP = 1, IARGP+1
         THETAP(JARGP) = HQ(JARGP)*RTOD + APHI(JARGP)/2. +
     &                   EPP*((APHI(JARGP) - 180.0)/2.)
  100 CONTINUE
      RETURN
      END ! THET

