
      SUBROUTINE NORM
C***********************************************************************
C...  Reduce mapped airfoil coordinates to standard format.
C     INORMMETHOD = 1: Parabola curve fit for normalization (ee notes 6-5-90).
C     INORMMETHOD = 2: Cubic spline method (MSelig20240920)
C     IMAX:  Used in thick.f (see notes 6-7-90).
C     Initial airfoil orientation: TE at the origin airfoil facing to the left
C
C     Copyright (c) 1990-2024 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      LOGICAL LLOWER

      REAL*8 SPLINEVALUE
      EXTERNAL SPLINEVALUE

C-----determine IMAX
      RRMAX = 0.
      
      If (INORMMETHOD .EQ. 1) THEN
C-------use 3-pt parabola about max mapped point
        IMAX = 0
        DO 100 JPT = 2, IARGP
          RR =  XMAP(JPT)**2 + YMAP(JPT)**2
          IF(RR .GT. RRMAX) THEN
            RRMAX = RR
            IMAX  = JPT
          ENDIF
 100    CONTINUE
C-------translate LE, LE+1, LE-1 points to fit a parabola
        THETA = DATAN(YMAP(IMAX)/XMAP(IMAX)) * RTOD + 90.
        LE = 1
        DO 200 JPT = IMAX-1, IMAX+1
          XLE(LE) =  XMAP(JPT) * DCOSG(THETA) + YMAP(JPT) * DSING(THETA)
          YLE(LE) = -XMAP(JPT) * DSING(THETA) + YMAP(JPT) * DCOSG(THETA)
          LE = LE + 1
 200    CONTINUE
C-------fitting parabola Y = A1 * X**2 + A2 * X + A3
C                             W        0           P
C-------obtain A1, A2, A3
        W1 = YLE(1)
        W2 = YLE(2)
        W3 = YLE(3)
        O1 = XLE(1)**2
        O2 = XLE(2)**2
        O3 = XLE(3)**2
        P1 = XLE(1)
        P2 = XLE(2)
        P3 = XLE(3)
        A1 = ((W1-W2)/(P1-P2) - (W2-W3)/(P2-P3))/
     &       ((O1-O2)/(P1-P2) - (O2-O3)/(P2-P3))
        A2 = ((W2-W3)-(O2-O3)*A1)/(P2-P3)
        A3 = W3 - O3*A1 - P3*A2
        XMAX = -0.5 * A2/A1
        YMAX = XMAX**2 * A1 + XMAX * A2 + A3
C-------translate the LE point back 
        XLE(4) =   XMAX * DCOSG(-THETA) + YMAX * DSING(-THETA)
        YLE(4) = - XMAX * DSING(-THETA) + YMAX * DCOSG(-THETA)
C-------rotate airfoil from zero lift angle of attack ALFA0
        ALFA0 = DATAN(YLE(4)/XLE(4)) * RTOD
        CHORD = DSQRT(XLE(4)**2 + YLE(4)**2)
      ELSEIF (INORMMETHOD .EQ. 2) THEN
C-------Use a cubic spline (natural cubic spline / second derivative is zero at the endpoints)

C-------Get IMAX for use in thick.f
        IMAX = 0
        DO 120 JPT = 2, IARGP
          RR =  XMAP(JPT)**2 + YMAP(JPT)**2
          IF(RR .GT. RRMAX) THEN
            RRMAX = RR
            IMAX  = JPT
          ENDIF
 120    CONTINUE


        RMAXDIST = 0.D0
        DO 12 I = 1, IARGP
          TMAP(I) = DBLE(I)
 12     CONTINUE
            
C-------Calculate cubic spline coefficients for X and Y separately
        CALL SPLN(TMAP, XMAP, IARGP, AX, BX, CX, DX, NARGP)
        CALL SPLN(TMAP, YMAP, IARGP, AY, BY, CY, DY, NARGP)        
C-------Find maximum point along spline
        NSAMPLES = 10000
        DO 20 I = 1, NSAMPLES
C---------Could save time if we limit to leading edge region, which
C---------will be approximately half of IARGP.
          T = DBLE(I) / DBLE(NSAMPLES) * DBLE(IARGP-1) + 1.D0
C---------WRITE(*, *) 'ISAMPLES, T:', I, T
          SPLINEX = SPLINEVALUE(T, TMAP, IARGP, AX, BX, CX, DX)
          SPLINEY = SPLINEVALUE(T, TMAP, IARGP, AY, BY, CY, DY)
C---------WRITE(*, *) '10000 x y spline pts:', SPLINEX, SPLINEY
          DISTANCE = DSQRT(SPLINEX**2 + SPLINEY**2)
          IF (DISTANCE .GT. RMAXDIST) THEN
            RMAXDIST = DISTANCE
            RMAXX = SPLINEX
            RMAXY = SPLINEY
          ENDIF
 20     CONTINUE
        
C       Inspect max point
C       WRITE(*, *) 'Maximum distance on curve:', RMAXDIST
C       WRITE(*, *) 'Point of max distance (x, y):', RMAXX, RMAXY
C       WRITE(*, *) 'Total number of points read:', IARGP
C       PAUSE

C-------rotate airfoil from zero lift angle of attack ALFA0
        ALFA0 = DATAN(RMAXY/RMAXX) * RTOD
        CHORD = DSQRT(RMAXX**2 + RMAXY**2)
        
      ENDIF
      
C-----rotate, normalize, and translate airfoil
      DO 300 JPT = 1, IARGP+1
         XCRD(JPT) = 1. + 
     &    ( XMAP(JPT) * DCOSG(ALFA0) + YMAP(JPT) * DSING(ALFA0))/CHORD
         YCRD(JPT) = 
     &    (-XMAP(JPT) * DSING(ALFA0) + YMAP(JPT) * DCOSG(ALFA0))/CHORD
  300 CONTINUE
      TEX = XCRD(1) - XCRD(IARGP+1)
      TEY = YCRD(1) - YCRD(IARGP+1)
      XTEMIDPT = 0.5 * (XCRD(1) + XCRD(IARGP+1)) - 1.
      YTEMIDPT = 0.5 * (YCRD(1) + YCRD(IARGP+1))
C... if adding a wedge to get a finite trailing edge then do
C... see PROFOIL notes 8-5-97
      IF(LWEDGE) THEN
        LLOWER = FF
        SIGN   = 1.0
        DO 305 JPT = 1, IARGP+1
          YOFFSET = SIGN * WEDGET * XCRD(JPT) * 0.5
          IF(.NOT. LLOWER) THEN
            IF(XCRD(JPT) .LE. 0.05) THEN
C...  if ycrd is withing 5% of leading edge, check for u.s. or l.s.
              IF(YCRD(JPT) .LE. 0.0) THEN
C...  if xcrd is negative, then it is the lower surface and begin
C     subtracting the YOFFSET
                LLOWER = TT
                SIGN = -1.
              ENDIF
            ENDIF
          ENDIF
          YCRD(JPT) = YCRD(JPT) + YOFFSET
 305    CONTINUE
      ENDIF
      if (lflip) then
c...  flip the coordinates and flip the order
c     put coordinates in tmp array and change sign on y-coord
c     added 020731
        do jpt = 1, iargp+1
          xtmp(jpt) =  xcrd(jpt)
          ytmp(jpt) = -ycrd(jpt)
        enddo
        do jpt = 1, iargp+1
          xcrd(iargp+2-jpt) =  xtmp(jpt)
          ycrd(iargp+2-jpt) =  ytmp(jpt)
        enddo
      endif
      RETURN
      END ! NORM


C     Subroutine to calculate cubic spline coefficients
      SUBROUTINE SPLN(X, Y, N, A, B, C, D, MAXPTS)
      IMPLICIT NONE
      INTEGER MAXPTS, N, I
      REAL*8 X(MAXPTS), Y(MAXPTS), A(MAXPTS), B(MAXPTS), C(MAXPTS)
      REAL*8 D(MAXPTS)
      REAL*8 H(MAXPTS), ALPHA(MAXPTS), L(MAXPTS)
      REAL*8 MU(MAXPTS), Z(MAXPTS)

      DO 30 I = 1, N-1
         H(I) = X(I+1) - X(I)
   30 CONTINUE
      
      DO 40 I = 2, N-1
         ALPHA(I-1) = 3.D0 * (Y(I+1) - Y(I)) / H(I) - 
     &                3.D0 * (Y(I) - Y(I-1)) / H(I-1)
   40 CONTINUE
      
      L(1) = 2.D0 * H(1)
      MU(1) = 0.5D0
      Z(1) = ALPHA(1) / L(1)
      
      DO 50 I = 2, N-1
         L(I) = 2.D0 * (X(I+1) - X(I-1)) - H(I-1) * MU(I-1)
         MU(I) = H(I) / L(I)
         Z(I) = (ALPHA(I-1) - H(I-1) * Z(I-1)) / L(I)
   50 CONTINUE
      
      L(N) = H(N-1) * (2.D0 - MU(N-1))
      Z(N) = (ALPHA(N-1) - H(N-1) * Z(N-1)) / L(N)
      C(N) = Z(N)
      
      DO 60 I = N-1, 1, -1
         C(I) = Z(I) - MU(I) * C(I+1)
         B(I) = (Y(I+1) - Y(I)) / H(I) - H(I) * (C(I+1) + 2.D0 * C(I)) 
     &          / 3.D0
         D(I) = (C(I+1) - C(I)) / (3.D0 * H(I))
   60 CONTINUE
      
      DO 70 I = 1, N
         A(I) = Y(I)
   70 CONTINUE
      
      END SUBROUTINE SPLN
      
C     Function to calculate spline value at a given point
      REAL*8 FUNCTION SPLINEVALUE(T, X, N, A, B, C, D)
      IMPLICIT NONE
      INTEGER N, I
      REAL*8 T, X(N), A(N), B(N), C(N), D(N)
      REAL*8 DX

      I = 1
   80 IF (I .LT. N .AND. T .GT. X(I+1)) THEN
         I = I + 1
         GOTO 80
      ENDIF
      
      DX = T - X(I)
      SPLINEVALUE = A(I) + B(I)*DX + C(I)*DX**2 + D(I)*DX**3
      
      END FUNCTION SPLINEVALUE
      
      
