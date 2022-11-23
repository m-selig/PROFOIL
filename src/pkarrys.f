
      SUBROUTINE PKARRYS
C***********************************************************************
C...Pack array SPACK(IPACK)
C   JSPSEG from calling routine
C   ISBSEG from calling routine
C   IPACK  from calling routine
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      IPACK = 1 + ISBSEG
      IF(LBE) THEN
         SPACK(1) = SBOS
         DO 100 JPACK = 2, IPACK
            JSBSEG = JPACK - 1
            SPACK(JPACK) = SBOS + SJSBSEG(JSBSEG)
  100    CONTINUE
      ELSE
         SPACK(1) = SBOS + SJSBSEG(ISBSEG)
         DO 200 JPACK = 2, IPACK
            JSBSEG = JPACK - 1
            SPACK(JPACK) = SBOS + SJSBSEG(ISBSEG) - SJSBSEG(JSBSEG)
  200    CONTINUE
      ENDIF
      RETURN
      END ! PKARRYS

