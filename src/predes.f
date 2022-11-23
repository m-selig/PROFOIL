
      SUBROUTINE PREDES
C***********************************************************************
C...Preliminary stuff to do before designing airfoil
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
C-----ERROR CHECKING: forcing the number of points to be even
      IARGP  = INT(IARGPU/2.)*2
      IARGP2 = IARGP/2
      IF(IARGPU .NE. IARGP) THEN
         WRITE(lu06,*) ' Number of points changed to:', IARGP
      ENDIF
      DEL_PHI = 360./FLOAT(IARGP)
C-----if airfoil is entered with LSYM line, then create mirror image data
      IF(LSYM) CALL SYMTRY
C-----set phi_w
      AW(1) = AM(1)
      AW(2) = AM(ISEG-1)
C-----If adjustable segments are given, determine the spline coefs for
C     prescribed Newton functions.
      IF(LSNEWT2) CALL FITSEGN
C-----check to see if AM(ISEG) is 360.
      IF(DABS(AM(ISEG)-360.) .GT. 0.01) CALL MAGAM
      RETURN
      END ! PREDES

