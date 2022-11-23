
      SUBROUTINE MOMENT(ALPHA,CM)
C***********************************************************************
C...Compute the moment
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      CM = 4.*PI*(FSB2 * DCOSG(2.*ALPHA)
     &             -(FSA2 + (EPP - 1.)/2.)*DSING(2.*ALPHA))/CHORD**2.
      IF(DABS(ALPHA) .LT. 0.0001) CM0 = CM
      RETURN
      END ! MOMENT

