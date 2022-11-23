
      SUBROUTINE JACOBI1(JVAR1)
C***********************************************************************
C...Compute the JVAR1 column of the [Jacobi]
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER JVAR1
      JVAR = JVAR1
      DO 100 JEQU1 = 1, IEQU1
         JEQU = JEQU1 
         CIJ(JEQU,JVAR) = (FNT1_0(JEQU1) - FNT1_P(JEQU1))/DELTAP(JVAR1)
  100 CONTINUE
      DO 200 JEQU2 = 1, IEQU2
         JEQU = IEQU1 + JEQU2 
         CIJ(JEQU,JVAR) = (FNT2_0(JEQU2) - FNT2_P(JEQU2))/DELTAP(JVAR1)
  200 CONTINUE
      RETURN
      END ! JACOBI1

