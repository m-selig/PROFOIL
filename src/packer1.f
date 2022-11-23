
      SUBROUTINE PACKER1(JSEG)
C***********************************************************************
C...Pack the arrays SBL(.) and VBL(.) for the boundary layer analysis.
C   Note the arrays SBL(.) and VBL(.) only contain one point.
C   Requires: LBOS, ALFABL from calling routine.
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER JSEG 
C-----Pack arrays for BL analysis from AMVEL0-to-PHIEND
      AMVEL0 = AMSTAG(ALFABL)
      IF(LBOS) THEN
         PHIEND = AM(JSEG-1)
      ELSE
         PHIEND = AM(JSEG)
      ENDIF          
C-----LBOS is used to determine the design velocity at the 
C     end of the segment.
      VSSEND = VSEND(JSEG)
C-----determine LBE
      IF(AMVEL0 .LE. PHIEND) THEN
         LBE = TT
      ELSE
         LBE = FF
      ENDIF
C-----set SPACK(1) and VPACK(1) array based on PHIEND and ALFABL
      CALL PACKSV1(JSEG)
C-----pack the arrays SBL(IBL) and VBL(IBL) and save INDXBL(IPACK)
      CALL PACK1
      RETURN
      END ! PACKER1

