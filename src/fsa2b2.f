
      SUBROUTINE FSA2B2
C***********************************************************************
C...Moment integrals FSA2 and FSB2
C   See notes 7-13-90 p.10 and 7-15-90 p. 1
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
C-----Using simpson's rule and ignoring the corner discontinuity in slope
C     at the leading edge
      SUM = 0.
      DO 100 JPT = 2, IARGP-2,2
         F1 = HP(JPT)   * DCOSG(2. * APHI(JPT))
         F2 = HP(JPT+1) * DCOSG(2. * APHI(JPT+1))
         SUM = SUM + 4.*F1 + 2.*F2
  100 CONTINUE
      F0 =  HP(1)       * DCOSG(2. * APHI(1))
      F3 =  HP(IARGP)   * DCOSG(2. * APHI(IARGP))
      F4 =  HP(IARGP+1) * DCOSG(2. * APHI(IARGP+1))
      FSA2 = DEL_PHI * DTOR * (SUM + F0 + 4.*F3 + F4)/(3.*PI)
      SUM = 0.
      DO 200 JPT = 2, IARGP-2,2
         F1 = HP(JPT)   * DSING(2. * APHI(JPT))
         F2 = HP(JPT+1) * DSING(2. * APHI(JPT+1))
         SUM = SUM + 4.*F1 + 2.*F2
  200 CONTINUE
      F0 =  HP(1)       * DSING(2. * APHI(1))
      F3 =  HP(IARGP)   * DSING(2. * APHI(IARGP))
      F4 =  HP(IARGP+1) * DSING(2. * APHI(IARGP+1))
      FSB2 = DEL_PHI * DTOR * (SUM + F0 + 4.*F3 + F4)/(3.*PI)
      RETURN
      END ! FSA2B2

