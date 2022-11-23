
      SUBROUTINE FINITE
C***********************************************************************
C...Compute the rhs terms for FTE angle and add to BI(.)
C   See notes starting "5-22-90" p. 14
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      PHIB = PHIEPP(1)
      PHIE = PHIEPP(2)
      DLOG2 = DLOG(2.D0)
      DLOGB = DLOG(2.D0 * DSING(0.5D0 * PHIB))
      DLOGE = DLOG(2.D0 * DSING(0.5D0 * PHIE))
C-----BI(1) terms---continuity equation
      BFTE1 = DLOGE - DLOGB
C-----BI(2) terms---1st int constraint
      BFTE2 = FTEI1(PHIB, PHIE, DELAN, DTOR, DLOG2)
      BFTE2 = BFTE2 + DTOR * (PHIB     - 0.D0) * DLOGB
      BFTE2 = BFTE2 + DTOR * (360.D0   - PHIE) * DLOGE
C-----BI(3) terms---2nd int constraint
      BFTE3 = PI
      BFTE3 = BFTE3 + FTEI2(PHIB, PHIE, DTOR, DLOG2)
      BFTE3 = BFTE3 + (DSING(PHIB) -  0.) * DLOGB 
      BFTE3 = BFTE3 + (0. -  DSING(PHIE)) * DLOGE 
C-----BI(4) terms---3rd int constraint
      BFTE4 = FTEI3(PHIB, PHIE, DLOG2)
      BFTE4 = BFTE4 + (-DCOSG(PHIB) + 1.) * DLOGB
      BFTE4 = BFTE4 + (-1. + DCOSG(PHIE)) * DLOGE
C-----adding in all FTE terms
      BI(1) = BI(1) + EPP * BFTE1
      BI(2) = BI(2) + EPP * BFTE2
      BI(3) = BI(3) + EPP * BFTE3
      BI(4) = BI(4) + EPP * BFTE4
      RETURN
      END ! FINITE

