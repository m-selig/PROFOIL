
      SUBROUTINE QHARM
C***********************************************************************
C...Compute the [harm]onic function [Q] from P(phi).
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      CALL PSMOOTH
C-----extending the smooth function P from -2*pi to 4*pi
      DO 200 JNU = 1, 2*IARGP+1
         HPSMO(JNU + IARGP) = HPSMO(JNU)
  200 CONTINUE

C-----computing the cotangent factor
      FCT = RTOD * PI/FLOAT(IARGP)
      DO 300 JMU = 1, IARGP2-1, 2
         COTAN(JMU) = 1./DTANG(FLOAT(JMU) * FCT)
  300 CONTINUE
C-----computing the harmonic function Q  by looping over nu
      FCT = 1./FLOAT(IARGP2)
      DO 400 JNU = 1, IARGP
         SUM = 0.0
C--------looping over odd mu
         DO 500 JMU = 1, IARGP2-1, 2
            DIFF = HPSMO(JNU + JMU + IARGP) 
     &           - HPSMO(JNU - JMU + IARGP)
            SUM = SUM + DIFF * COTAN(JMU)
  500    CONTINUE
         HQ(JNU) = FCT * SUM + HQ(JNU)
  400 CONTINUE
      HQ(IARGP+1) = HQ(1)
      RETURN
      END ! QHARM

