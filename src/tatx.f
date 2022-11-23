
      FUNCTION TATX(XBYC)
C***********************************************************************
C...  Determine the airfoil thickness at a specified x/c location
C
C     Copyright (c) 1995 Ashok Gopalarathnam
C     Copyright (c) 1990-2022 Michael Selig
C***********************************************************************

      INCLUDE 'PROFOIL.INC'
      REAL*8 TATX, XBYC, YTOP, YBOT, POLATE
      INTEGER J
C
C--- Uses XCRD and YCRD arrays to linearly interpolate on top and bottom surfaces
C--- ILECTP is the index of the control point just before LE arc limit
C--- IARGP is the number of control points
C--- XCRD starts from the TE, goes to LE along the top surface, then back to
C    TE along the bottom surface.
C
C--- Getting YTOP, (y/c)upper at XBYC
      DO J = 1, ILECTP
        IF (XCRD(J) .LT. XBYC) THEN
          YTOP = POLATE(XCRD(J),YCRD(J),XCRD(J-1),YCRD(J-1),XBYC)
          GOTO 100
        ELSEIF (XCRD(J) .EQ. XBYC) THEN
          YTOP = YCRD(J)
          GOTO 100
        ENDIF
      ENDDO
C
C--- Getting YBOT, (y/c)lower at XBYC
100   DO J = ILECTP+1, IARGP
        IF (XCRD(J) .GT. XBYC) THEN
          YBOT = POLATE(XCRD(J),YCRD(J),XCRD(J-1),YCRD(J-1),XBYC)
          GOTO 101
        ELSEIF (XCRD(J) .EQ. XBYC) THEN
          YBOT = YCRD(J)
          GOTO 101
        ENDIF
      ENDDO
C
101   TATX = YTOP-YBOT
C
      RETURN
      END ! TATX

