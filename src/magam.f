
      SUBROUTINE MAGAM
C***********************************************************************
C...Scale the design arcs limit so that AM(ISEG) = 360.
C   SCLF  scale factor
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      SCLF = 360./AM(ISEG)
      DO 100 JSEG = 1, ISEG
        AM(JSEG) = AM(JSEG) * SCLF
 100  CONTINUE
      AW(1) = AW(1) * SCLF
      AW(2) = AW(2) * SCLF
      AS(1) = AS(1) * SCLF
      AS(2) = AS(2) * SCLF
      IF(LFTE) THEN
         PHIEPP(1) = PHIEPP(1) * SCLF
         PHIEPP(2) = PHIEPP(2) * SCLF
      ENDIF
      RETURN
      END ! MAGAM

