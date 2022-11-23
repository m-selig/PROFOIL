
      SUBROUTINE ENVLPE(JBL)
C***********************************************************************
C...Find the envelope value for the given location JBL
C   KBEG --- index of the beginning of the non-zero n-growth
C   KEND --- index of the end       of the non-zero n-growth
C   MAXFREQ --- index of the maximum n-value over the frequencies
C...Returns the envelope value ENNMAX
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER KBEG, KEND, MAXFREQ
      LOGICAL LITER
      LITER = TT
C-----find bounds on the non-zero n-values
      CALL BOUNDS(KBEG,KEND,JBL,LITER)
C-----find the index of the maximum.
      VALUE   = 0.
      MAXFREQ = 0
      DO 200 JFREQ = KBEG, KEND
         IF(AMPF(JFREQ, JBL) .GT. VALUE) THEN
            VALUE   = AMPF(JFREQ, JBL)
            MAXFREQ = JFREQ
         ENDIF
 200  CONTINUE
C-----Stop if maximum occurs on a boundary, interpolation not possible.
      IF(MAXFREQ .EQ. 1 .OR. MAXFREQ .EQ. IFREQ) THEN
         WRITE(lu06,*) 
     $       ' Error 146: maximum of envelope occurs on boundary',
     $       ' (envlpe.f)'
         STOP
      ENDIF
C-----Spline the data with natural BCs. See notes 7-4-91
      YP1 = 1.E10
      YPN = 1.E10
      ISPL = 0
      DO 300 JFREQ = KBEG, KEND
         ISPL = ISPL + 1
         XDM(ISPL) = FREQ(JFREQ)
         YDM(ISPL) = AMPF(JFREQ,JBL)
 300  CONTINUE
C-----Compute the second derivative of the cubic spline
      CALL SPLINE(XDM,YDM,ISPL,NSPL,YP1,YPN,Y2)
C-----Compute the cubic coefficients for two intervals about max point
C-----left interval INTV = 1, right interval INTV = 2 
      KSPL = MAXFREQ - KBEG
      INTV = 0
      DO 400 JSPL = KSPL, KSPL+1
         INTV = INTV + 1
         EDD(INTV) =   (Y2(JSPL) - Y2(JSPL+1)) 
     &               * 0.1666666666666667
     &               / (XDM(JSPL) - XDM(JSPL+1))
         ECC(INTV) = 0.5 * (Y2(JSPL) - 6. * EDD(INTV) * XDM(JSPL))
         EBB(INTV) =    (YDM(JSPL)-YDM(JSPL+1)
     &                - ECC(INTV) * (XDM(JSPL)**2-XDM(JSPL+1)**2)
     &                - EDD(INTV) * (XDM(JSPL)**3-XDM(JSPL+1)**3))
     &                / (XDM(JSPL)-XDM(JSPL+1))
         EAA(INTV) =   YDM(JSPL) 
     &               - EBB(INTV) * XDM(JSPL)
     &               - ECC(INTV) * XDM(JSPL)**2
     &               - EDD(INTV) * XDM(JSPL)**3
  400 CONTINUE
C-----Compute the max (or min) of the spline and see if it falls
C     inside of the interval. If it does save the value.
C-----left interval.
      VALUE11 = 0.
      VALUE12 = 0.
      VALUE21 = 0.
      VALUE22 = 0.
      ROOTX11 = -0.5 * (2*ECC(1) 
     &         + DSQRT(DABS((2*ECC(1))**2 - 4. * 3*EDD(1) * EBB(1))))
     &         / (3*EDD(1))
      IF(      ROOTX11 .LE. FREQ(MAXFREQ-1)
     &   .AND. ROOTX11 .GE. FREQ(MAXFREQ)  ) THEN
         VALUE11 =  EAA(1)
     &            + EBB(1) * ROOTX11
     &            + ECC(1) * ROOTX11 ** 2
     &            + EDD(1) * ROOTX11 ** 3
      ENDIF 
      ROOTX12 = -0.5 * (2*ECC(1) 
     &         - DSQRT(DABS((2*ECC(1))**2 - 4. * 3*EDD(1) * EBB(1))))
     &         / (3*EDD(1))
      IF(      ROOTX12 .LE. FREQ(MAXFREQ-1)
     &   .AND. ROOTX12 .GE. FREQ(MAXFREQ)  ) THEN
         VALUE12 =  EAA(1)
     &            + EBB(1) * ROOTX12
     &            + ECC(1) * ROOTX12 ** 2
     &            + EDD(1) * ROOTX12 ** 3
      ENDIF 
      ROOTX21 = -0.5 * (2*ECC(2) 
     &         + DSQRT(DABS((2*ECC(2))**2 - 4. * 3*EDD(2) * EBB(2))))
     &         / (3*EDD(2))
      IF(      ROOTX21 .LE. FREQ(MAXFREQ)
     &   .AND. ROOTX21 .GE. FREQ(MAXFREQ+1)) THEN
         VALUE21 =  EAA(2)
     &            + EBB(2) * ROOTX21
     &            + ECC(2) * ROOTX21 ** 2
     &            + EDD(2) * ROOTX21 ** 3
      ENDIF 
      ROOTX22 = -0.5 * (2*ECC(2) 
     &         - DSQRT(DABS((2*ECC(2))**2 - 4. * 3*EDD(2) * EBB(2))))
     &         / (3*EDD(2))
      IF(   ROOTX22 .LE. FREQ(MAXFREQ)
     &.AND. ROOTX22 .GE. FREQ(MAXFREQ+1)) THEN
         VALUE22 =  EAA(2)
     &            + EBB(2) * ROOTX22
     &            + ECC(2) * ROOTX22 ** 2
     &            + EDD(2) * ROOTX22 ** 3
      ENDIF 
      ENNMAX = 0
      IF(VALUE11 .GT. ENNMAX) ENNMAX = VALUE11
      IF(VALUE12 .GT. ENNMAX) ENNMAX = VALUE12
      IF(VALUE21 .GT. ENNMAX) ENNMAX = VALUE21
      IF(VALUE22 .GT. ENNMAX) ENNMAX = VALUE22
      IF(DABS(ENNMAX) .LT. 0.0000001) THEN
         WRITE(lu06,*) ' Error 147: envelope value not found (envlpe.f)'
      ENDIF
      RETURN
      END ! ENVLPE

