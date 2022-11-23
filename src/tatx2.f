
      SUBROUTINE TATX2
C***********************************************************************
c...  Adapted from the function tatx
c...  Requires
c     xhinge - value of hinge x/c location from teflap input line
c...  Returns
c     yhtop - u.s. at hinge point
c     yhbot - l.s. at hinge point
c...  see MSS notes 020724 p 3
c
c     Copyright (c) 1990-2022 Michael Selig
C***********************************************************************

      INCLUDE 'PROFOIL.INC'
      REAL*8  XBYC, YTOP, YBOT, POLATE      
      INTEGER J
      XBYC = xhinge
c
c--- Uses XCRD and YCRD arrays to interpolate on top and bottom surfaces
c--- ILECTP is the index of the control point just before LE arc limit
c--- IARGP is the number of control points
c--- XCRD starts from the TE, goes to LE thro' top surface, then back to
c    TE thro' bottom surface.
c
c----Getting YTOP, (y/c)upper at XBYC
c
      DO J = 1, ILECTP
        IF (XCRD(J) .LT. XBYC) THEN
          YTOP = POLATE(XCRD(J),YCRD(J),XCRD(J-1),YCRD(J-1),XBYC)
          GOTO 100
        ELSEIF (XCRD(J) .EQ. XBYC) THEN
          YTOP = YCRD(J)
          GOTO 100
        ENDIF
      ENDDO
c
c----Getting YBOT, (y/c)lower at XBYC
c
100   DO J = ILECTP+1, IARGP
        IF (XCRD(J) .GT. XBYC) THEN
          YBOT = POLATE(XCRD(J),YCRD(J),XCRD(J-1),YCRD(J-1),XBYC)
          GOTO 101
        ELSEIF (XCRD(J) .EQ. XBYC) THEN
          YBOT = YCRD(J)
          GOTO 101
        ENDIF
      ENDDO
c
101   TATX = YTOP-YBOT
      yhtop = YTOP
      yhbot = YBOT
c
      RETURN
      END ! TATX2

