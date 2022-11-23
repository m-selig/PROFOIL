
      SUBROUTINE PACKER2(JSEG)
C***********************************************************************
C...Pack the arrays SBL(.) and VBL(.) for the boundary layer analysis.
C   Note that alfa^* is used for VPACK(.) array, that is,
C   the design velocity is used in the VPACK(.) array.
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER JSEG
C-----set SPACK(IPACK) array
      CALL PKARRYS
C-----set VPACK(IPACK) array
      CALL PKARRYV(JSEG)
C-----pack the arrays SBL(IBL) and VBL(IBL) and save INDXBL(IPACK)
      CALL PACK2(JSEG)
      RETURN
      END ! PACKER2

