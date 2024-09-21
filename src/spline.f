
      SUBROUTINE SPLINE(X,Y,N,NSPL,YP1,YPN,Y2)
C***********************************************************************
C.... SPLINE() from Numerical Recipes Book. p. 88
C     X(.) and Y(.) are the N points to spline.
C     YP1 first derivative at X(1): if YP1 > .99E10 zero second derivative
C     YPN first derivative at X(N): if YPN > .99E10 zero second derivative
C     ---> Y2  value of second derivative for the spline
C***********************************************************************
      PARAMETER (NMAX=200)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION X(NSPL),Y(NSPL),Y2(NSPL),U(NMAX)

      RETURN
      END ! SPLINE

