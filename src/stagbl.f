
      SUBROUTINE STAGBL(JBLST)
C***********************************************************************
C---Boundary layer stagnation point solution.
C   JBLST is point at which to start integral BL calculations on return.
C   See note 10-18-90 p. 2
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER JBLST
      JBL = 1
      U_X1 = FU_X(SBL(JBL),VBL(JBL),SBL(JBL+1),VBL(JBL+1))
100   CONTINUE
         JBL = JBL + 1
         U_X2 = FU_X(SBL(JBL),VBL(JBL),SBL(JBL+1),VBL(JBL+1))
      IF(U_X2 .GT. U_X1) THEN
         U_X1 = U_X2
         GO TO 100
      ENDIF
      JBLST = JBL
C-----JBLST is point at which to start integral BL calculations
C     and U_X1 is the stagnation point velocity gradient.
C
C-----first point
      H32BL(1) = 1.62566
      H12BL(1) = FH12(H32BL(1))
      D2BL(1)  = 0.29004 * DSQRT(1./(RINF * U_X1))
      D3BL(1)  = D2BL(1) * H32BL(1)
      RD2BL(1) = 0.
      CFBL(1)  = 0.
      CDBL(1)  = 0.
C-----remaining point(s) 
      DO 200 JBL = 1, JBLST-1
         H32BL(JBL+1) = H32BL(JBL)
         H12BL(JBL+1) = H12BL(JBL)
         D2BL(JBL+1)  = D2BL(JBL)
         D3BL(JBL+1)  = D2BL(JBL+1) * H32BL(JBL+1)
         RD2BL(JBL+1) = FRD2(RINF,VBL(JBL+1),D2BL(JBL+1))
         CFBL(JBL+1)  = FCF(H12BL(JBL+1),RD2BL(JBL+1))
         CDBL(JBL+1)  = FCDDISS(H12BL(JBL+1),H32BL(JBL+1),RD2BL(JBL+1))
  200 CONTINUE
      RETURN
      END ! STAGBL

