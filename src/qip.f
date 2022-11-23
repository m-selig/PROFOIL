
      SUBROUTINE QIP(X,Y,A)
C***********************************************************************
C     Derived from the Eppler code.
C     Ref: Eppler and Somers, NASA TM-80210, NASA Langley, August 1980.
C     Adapted by Farooq Saeed (Aug. 1995)
C***********************************************************************

C ----------------------------------------------------------------------
C Compute the coefficients of a parabola thru 3 points
C called by subroutine DIA()
C ----------------------------------------------------------------------

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(3),Y(3),A(3)
      C1   = (Y(2)-Y(1))/(X(2)-X(1))
      A(3) = (Y(3)-Y(1)-C1*(X(3)-X(1)))/((X(3)-X(1))*(X(3)-X(2)))
      A(1) = Y(1)-C1*X(1)+A(3)*X(1)*X(2)
      A(2) = C1-A(3)*(X(1)+X(2))
      RETURN
      END ! QIP

