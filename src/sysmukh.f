
      SUBROUTINE SYSMUKH
C***********************************************************************
C...Solves for the knowns UMU(.) and HKH(.) to determine the airfoil
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'

C---- LINPACK_D
C     Factor the matrix
C     call dgeco ( a, lda, n, ipvt, rcond, z )
      call dgeco ( AIJ, 4, 4, ipvt, rcond, z )
C     Solve the linear system given the right hand side
C     job = 0
C     call dgesl ( a, lda, n, ipvt, b, job )
      call dgesl ( AIJ, 4, 4, ipvt, BI, 0 )

      UMU(1) = BI(1)
      UMU(2) = BI(2)
      HKH(1) = BI(3)
      HKH(2) = BI(4)
      RETURN
      END ! SYSMUKH

