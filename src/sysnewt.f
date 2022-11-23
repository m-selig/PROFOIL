
      SUBROUTINE SYSNEWT
C***********************************************************************
C...Solve the [sys]tem of [Newt]on equations
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'

C---- LINPACK_D
C     Factor the matrix
C     call dgeco (   a,  lda,    n, ipvt, rcond, z )
      call dgeco ( CIJ, NEQU, IEQU, ipvt, rcond, z )
C     Solve the linear system given the right hand side
C     job = 0
C     call dgesl (   a,  lda,    n, ipvt,  b, job )
      call dgesl ( CIJ, NEQU, IEQU, ipvt, DI, 0 )
      
      DO 100 JVAR1 = 1, IVAR1
         JVAR = JVAR1
         DELTAS(JVAR1) = DI(JVAR)
  100 CONTINUE
      DO 200 JVAR2 = 1, IVAR2
         JVAR = IVAR1 + JVAR2
         DELTAVS(JVAR2) = DI(JVAR)
  200 CONTINUE
      RETURN
      END ! SYSNEWT

