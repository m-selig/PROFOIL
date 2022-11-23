
      SUBROUTINE CALCPHI
C***********************************************************************
C...[Calc]ulate A[PHI](.) once and for all
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      APHI(1) = 0.
      DO 100 IPT = 1, IARGP
          APHI(IPT+1) = APHI(IPT) + DEL_PHI
  100 CONTINUE
      RETURN
      END ! CALCPHI

