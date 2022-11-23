
      SUBROUTINE NORM
C***********************************************************************
C...Reduce mapped airfoil coordinates to standard format.
C   See notes 6-5-90
C   Initial airfoil orientation: TE at the origin
C                                airfoil facing to the left
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      LOGICAL LLOWER
C-----determine IMAX
      RRMAX = 0.
      IMAX = 0
      DO 100 JPT = 2, IARGP
         RR =  XMAP(JPT)**2 + YMAP(JPT)**2
         IF(RR .GT. RRMAX) THEN
            RRMAX = RR
            IMAX  = JPT
         ENDIF
  100 CONTINUE
C-----translate LE, LE+1, LE-1 points to fit a parabola
      THETA = DATAN(YMAP(IMAX)/XMAP(IMAX)) * RTOD + 90.
      LE = 1
      DO 200 JPT = IMAX-1, IMAX+1
         XLE(LE) =  XMAP(JPT) * DCOSG(THETA) + YMAP(JPT) * DSING(THETA)
         YLE(LE) = -XMAP(JPT) * DSING(THETA) + YMAP(JPT) * DCOSG(THETA)
         LE = LE + 1
  200 CONTINUE
C-----fitting parabola Y = A1 * X**2 + A2 * X + A3
C                          W        0           P
C-----obtain A1, A2, A3
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
     &     ((O1-O2)/(P1-P2) - (O2-O3)/(P2-P3))
      A2 = ((W2-W3)-(O2-O3)*A1)/(P2-P3)
      A3 = W3 - O3*A1 - P3*A2
      XMAX = -0.5 * A2/A1
      YMAX = XMAX**2 * A1 + XMAX * A2 + A3
C-----translate the LE point back 
      XLE(4) =   XMAX * DCOSG(-THETA) + YMAX * DSING(-THETA)
      YLE(4) = - XMAX * DSING(-THETA) + YMAX * DCOSG(-THETA)
C-----rotate airfoil from zero lift angle of attack ALFA0
      ALFA0 = DATAN(YLE(4)/XLE(4)) * RTOD
      CHORD = DSQRT(XLE(4)**2 + YLE(4)**2)
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

