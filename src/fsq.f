
      SUBROUTINE FSQ
C***********************************************************************
C...Integrals of Q. See notes 8-30-90.
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      SUM = 0.
      DO 100 JPT = 1, IARGP
         SUM = SUM + HQ(JPT) 
  100 CONTINUE
      FSB0 = 2. * SUM/FLOAT(IARGP)
      FCT = RTOD * PI / FLOAT(IARGP2)
      SUM = 0.
      DO 200 JPT = 1, IARGP
         SUM = SUM + HQ(JPT) * DCOSG(FLOAT(JPT-1)*FCT)
  200 CONTINUE
      FSB1 = 2. * SUM/FLOAT(IARGP)
      SUM = 0.
      DO 300 JPT = 1, IARGP
         SUM = SUM + HQ(JPT) * DSING(FLOAT(JPT-1)*FCT)
  300 CONTINUE
C-----Note that epsilon is added on to the a1 (= 1 -  epp)
C     so that the printed out coefficient should be 1 exactly
C     assuming no error.
      FSA1 = - 2. * SUM/FLOAT(IARGP) + EPP
      RETURN
      END ! FSQ

