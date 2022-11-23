
      SUBROUTINE COORD
C***********************************************************************
C...  Compute the airfoil coordinates from P and Q
C     Using equations 7-11-90 p.6 and 7-12-90 p. 1
C
C     Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
C...  compute XMAP(PHI) and YMAP(PHI) 
C...  Initial values for coordinates.  These are set to zero in 
C     deflt.f.  But they may be changed through Newton iteration
C     should a offset in the TE point be desired.
      XMAP(1) = XMAPOFF
      YMAP(1) = YMAPOFF
      SX = 0.
      SY = 0.
      X_PHI(1) = 0.
      Y_PHI(1) = 0. 
      X_PHI(IARGP+1) = 0.
      Y_PHI(IARGP+1) = 0. 
      IF(LFTE) THEN
        PI2 = PI/2.
        DO 200 JPT = 2, IARGP
          SIGMA = -(2.*DSING(0.5 * APHI(JPT)))**(1.-EPP) * EXP(HP(JPT))
          ANGLE = 0.5 * APHI(JPT) - EPP * (PI2*RTOD - 0.5 * APHI(JPT))
     &         + HQ(JPT) * RTOD
          X_PHI(JPT) = SIGMA * DCOSG(ANGLE)
          Y_PHI(JPT) = SIGMA * DSING(ANGLE) 
          SX = X_PHI(JPT) + SX
          SY = Y_PHI(JPT) + SY
 200    CONTINUE
      ELSE
        DO 100 JPT = 2, IARGP
          SIGMA = -2. * DSING(0.5 * APHI(JPT)) * EXP(HP(JPT)) 
          ANGLE = 0.5 * APHI(JPT) + HQ(JPT) * RTOD
          X_PHI(JPT) = SIGMA * DCOSG(ANGLE)
          Y_PHI(JPT) = SIGMA * DSING(ANGLE) 
          SX = X_PHI(JPT) + SX
          SY = Y_PHI(JPT) + SY
 100    CONTINUE
      ENDIF
      FACTOR = 0.041666666666666666*DEL_PHI*DTOR
      IF (LTEZERO) THEN
C...  Trailing edge is closed.
C...  Otherwise TEY and/or TEX are specified by NEWT1* line
        DELSX = SX/FLOAT(IARGP-1)
        DELSY = SY/FLOAT(IARGP-1)
      ENDIF
      XMAP(2) = XMAP(1) + FACTOR
     &     * (- X_PHI(IARGP) + 13. * X_PHI(1) 
     &     + 13. * X_PHI(2) - X_PHI(3)
     &     - 11. * DELSX)
      YMAP(2) = YMAP(1) + FACTOR
     &     * (- Y_PHI(IARGP) + 13. * Y_PHI(1) 
     &     + 13. * Y_PHI(2) - Y_PHI(3)
     &     - 11. * DELSY)
      XMAP(3) = XMAP(2) + FACTOR
     &     * (- X_PHI(1) + 13. * X_PHI(2) 
     &     + 13. * X_PHI(3) - X_PHI(4)
     &     - 25. * DELSX)
      YMAP(3) = YMAP(2) + FACTOR
     &     * (- Y_PHI(1) + 13. * Y_PHI(2) 
     &     + 13. * Y_PHI(3) - Y_PHI(4)
     &     - 25. * DELSY)
      DO 300 JPT = 3, IARGP-2
        XMAP(JPT+1) = XMAP(JPT) + FACTOR
     &       * (-X_PHI(JPT-1) + 13. * X_PHI(JPT) 
     &       + 13. * X_PHI(JPT+1) - X_PHI(JPT+2)
     &       - 24. * DELSX)
        YMAP(JPT+1) = YMAP(JPT) + FACTOR
     &       * (-Y_PHI(JPT-1) + 13. * Y_PHI(JPT) 
     &       + 13. * Y_PHI(JPT+1) - Y_PHI(JPT+2)
     &       - 24. * DELSY)
 300  CONTINUE
      XMAP(IARGP) = XMAP(IARGP-1) + FACTOR
     &     * (- X_PHI(IARGP-2) + 13. * X_PHI(IARGP-1) 
     &     + 13. * X_PHI(IARGP) - X_PHI(IARGP+1)
     &     - 25. * DELSX)
      YMAP(IARGP) = YMAP(IARGP-1) + FACTOR
     &     * (- Y_PHI(IARGP-2) + 13. * Y_PHI(IARGP-1) 
     &     + 13. * Y_PHI(IARGP) - Y_PHI(IARGP+1)
     &     - 25. * DELSY)
      XMAP(IARGP+1) = XMAP(IARGP) + FACTOR
     &     * (- X_PHI(IARGP-1) + 13. * X_PHI(IARGP) 
     &     + 13. * X_PHI(IARGP+1) - X_PHI(2)
     &     - 11. * DELSX)
      YMAP(IARGP+1) = YMAP(IARGP) + FACTOR
     &     * (- Y_PHI(IARGP-1) + 13. * Y_PHI(IARGP) 
     &     + 13. * Y_PHI(IARGP+1) - Y_PHI(2)
     &     - 11. * DELSY)
      RETURN
      END ! COORD

