
      SUBROUTINE RADIUS
C***********************************************************************
C...  Compute the leading edge radius
C...  IMAX determined from NORM.F
C
C     Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      LE = 1
      DO JPT = IMAX-1, IMAX+1
        XCRDLE(LE) =  XCRD(JPT)
        YCRDLE(LE) =  YCRD(JPT)
        LE = LE + 1
      ENDDO
C---  define locations of 3 points on l.e. of airfoil
      XXX1 = XCRDLE(1)
      XXX2 = XCRDLE(2)
      XXX3 = XCRDLE(3)
      YYY1 = YCRDLE(1)
      YYY2 = YCRDLE(2)
      YYY3 = YCRDLE(3)
C--- find (x,y) locations of bisector to sector of circle
      XXX4 = 0.5 * (XXX1 + XXX2) 
      YYY4 = 0.5 * (YYY1 + YYY2)
      XXX5 = 0.5 * (XXX2 + XXX3)
      YYY5 = 0.5 * (YYY2 + YYY3)
C--- find slopes (m) of bisectors of circle
      RMMM1 = (YYY1 - YYY2) / (XXX1 - XXX2)
      RMMM2 = (YYY3 - YYY2) / (XXX3 - XXX2)
C---  determine coefficients of lines which bisect the sectors
C---  lines are of the form y = ax + b
      AAA1 = -1. / RMMM1
      AAA2 = -1. / RMMM2
      BBB1 = XXX4 / RMMM1 + YYY4
      BBB2 = XXX5 / RMMM2 + YYY5
C---  determine the locations of the center of the circle
      XCEN = (BBB2 - BBB1) / (-AAA2 + AAA1)
      YCEN = AAA1 * XCEN + BBB1
C---  find radius of circle
      RADLE = DSQRT((XCEN - XXX1)**2 + (YCEN - YYY1)**2)
      RETURN
      END ! RADIUS

