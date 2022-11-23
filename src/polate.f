
      FUNCTION POLATE(X1,Y1,X2,Y2,XIN)
C***********************************************************************
C...  Linear Interpolation between two points
C
C     Copyright (c) 1995 Ashok Gopalarathnam
C***********************************************************************
      REAL*8 POLATE, X1, X2, Y1, Y2, XIN
      POLATE = Y1 + ( (Y2-Y1)*(XIN-X1)/(X2-X1) )
      RETURN
      END ! POLATE

