
      SUBROUTINE THICKE(X,Y,NP,THKP,THLC,YTHK)
C************************************************************************
C     Determine an airfoil max t/c per method in the Eppler code.
C     Uses Eppler code subroutines DIA() and QIP().
C     Ref: Eppler and Somers, NASA TM-80210, NASA Langley, August 1980.
C     THKP - t/c max
C     THLC - (x/c) @ (t/c)max
C     YTHK - (y/c) @ (t/c)max
C     Adapted by Farooq Saeed (Aug. 1995)
C************************************************************************

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION U(3),V(3),W(3),Z(3),A(3),B(3),X(1001),Y(1001)

      D = 0.
      DO 4 N = 2,NP
         NR  = NP-1
         XS  = X(N)
C...  Fortran Arithemic IF statement ...........................         
 1       IF(X(NR)-XS)3,2,2
 2       NR  = NR - 1
         GO TO 1
 3       IF(NR-N.LE.3) GO TO 5
         YD  = (Y(NR+1)-Y(NR))/(X(NR+1)-X(NR))
         DNN = (Y(N)-Y(NR)-(X(N)-X(NR))*YD)*(1.-YD*YD/2.)
         IF(DNN.LE.D) GO TO 4
         NM  = N
         NRM = NR
         YTHK= Y(N)
         THLC= XS
         YF  = YD
         D   = DNN
 4    CONTINUE
 5    IIT = 0
      IF(X(NM)*2.-X(NRM)-X(NRM+1).GT.0.) NRM = NRM+1
      XST = XS
      GO TO 8
 7    IIT = IIT+1
      IF(IIT.GT.20) GO TO 11
      DO 6 I = 1,3
         NO   = NM+2-I
         NU   = NRM-2+I
         U(I) = TA*(X(NO)+YF*Y(NO))
         V(I) = TA*(Y(NO)-YF*X(NO))
         W(I) = TA*(X(NU)+YF*Y(NU))
 6       Z(I) = TA*(Y(NU)-YF*X(NU))
      CALL QIP(U,V,A)
      CALL QIP(W,Z,B)
      IF(ABS(A(3)-B(3)).LT..0001) GO TO 11
      XST = (B(2)-A(2))/(A(3)-B(3))/2.
      YS  = A(2)+2.*A(3)*XST
      NMZ = NM
      NRMZ= NRM
      F   = 1.5
      IF(F*(U(3)-XST).LT.XST-U(2)) NM = NM-1
      IF(F*(XST-U(1)).LT.U(2)-XST) NM = NM+1
      IF(F*(W(3)-XST).LT.XST-W(2)) NRM= NRM+1
      IF(F*(XST-W(1)).LT.W(2)-XST) NRM= NRM-1
      IF(NMZ.NE.NM.OR.NRMZ.NE.NRM) GO TO 7
      IF(ABS(YS).LT..0001) GO TO 9
      YF  = YF+YS/TA
 8    TA  = 1./DSQRT(1.+YF*YF)
      GO TO 7
 9    D   = A(1)-B(1)+(A(2)-B(2)+(A(3)-B(3))*XST)*XST
      VTH = A(1)+(A(2)+A(3)*XST)*XST
      D   = VTH - B(1)-(B(2)+B(3)*XST)*XST
      THLC= TA*(XST-YF*VTH)
      YTHK= TA*(VTH+YF*XST)
 11   PROLAQ = X(1)*X(1)+Y(1)*Y(1)
      PROLAU = X(NP)*X(NP)+Y(NP)*Y(NP)
      IF(PROLAU.GT.PROLAQ) PROLAQ = PROLAU
      THKP = D/SQRT(PROLAQ)       ! c

C...Note that the THICKNESS EPPLER option does not calculate max camber
C      WRITE(lu06,*)'   (t/c)max         = ', THKP
C      WRITE(lu06,*)'   (x/c) @ (t/c)max = ', THLC
C      WRITE(lu06,*)'   (y/c) @ (t/c)max = ', YTHK
      RETURN
      END ! THICKE

