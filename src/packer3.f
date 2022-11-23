
      SUBROUTINE PACKER3(JSEG,JSBSEGT)
C***********************************************************************
C...Call other routines for setup to PROBL call
C   Requires: ALFABL, JSPSEG, JSBSEGT, ISBSEG, LBE from calling routine
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER JSEG,JSBSEGT
C-----Pack arrays for BL analysis from AMVEL0-to-PHIEND
      AMVEL0 = AMSTAG(ALFABL)
C-----determine PHIEND and VSSEND
      IF(LBE) THEN
         JSBSEG = JSBSEGT
         PHIEND = AM(JSEG-1) + SSPHI(JSPSEG,JSBSEG)
         VSSEND = VS(JSEG) +  SSDELV(JSPSEG,JSBSEG)
      ELSE
         IF(JSBSEGT .NE. ISBSEG) THEN
            JSBSEG = ISBSEG - JSBSEGT
            PHIEND = AM(JSEG-1) + SSPHI(JSPSEG,JSBSEG)
            VSSEND = VS(JSEG) +  SSDELV(JSPSEG,JSBSEG)
         ELSE
            PHIEND = AM(JSEG-1)
            VSSEND = VS(JSEG) 
         ENDIF
      ENDIF
C-----set SPACK(1) and VPACK(1) array
      CALL PACKSV3(JSEG)
C-----pack the arrays SBL(IBL) and VBL(IBL) and save INDXBL(IPACK)
      CALL PACK1
      RETURN
      END ! PACKER3

