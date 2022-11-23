
      SUBROUTINE PACK1
C***********************************************************************
C...Pack boundary-layer arc length and velocity array.
C...Requires: AMVEL0 and PHIEND and LBE
C   and the SPACK(.) and VPACK(.) arrays which have only one element.
C...The point SPACK(1) and VPACK(1) is at the end of the velocity dist.
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      IBL = 1
      IF(LBE) THEN
C--------pack lower surface
C--------do stagnation point
         RMNDR = MOD(AMVEL0,DEL_PHI)
         IF(RMNDR .GT. 0.000001) THEN
C-----------stagnation point does not fall on coordinate point
            KSTPK = INT(AMVEL0/DEL_PHI) + 2
            SSTAG = Y3(APHI(KSTPK),  SLENS(KSTPK),
     &                 APHI(KSTPK-1),SLENS(KSTPK-1),
     &                 AMVEL0)
            SBL(IBL) = SSTAG 
            VBL(IBL) = 0.0
         ELSE
C-----------stagnation point does fall on coordinate point
            KSTPK = INT((AMVEL0 + 0.1*DEL_PHI)/DEL_PHI) + 2
            SBL(IBL) = SLENS(KSTPK-1)
            SSTAG = SBL(1)
            VBL(IBL) = 0.0
         ENDIF
C--------do endpoint
         RMNDR = MOD(PHIEND,DEL_PHI)
         IF(RMNDR .GT. 0.000001) THEN
C-----------end point does not fall on coordinate point
            KENPK = INT(PHIEND/DEL_PHI) + 1
         ELSE
C-----------end point does fall on coordinate point
            KENPK = INT((PHIEND + 0.1*DEL_PHI)/DEL_PHI)
         ENDIF
         DO 100 JPT = KSTPK, KENPK, 1
            IBL = IBL + 1
            SBL(IBL) = SLENS(JPT)
            VBL(IBL) = 2.* DABS(DCOSG(0.5*APHI(JPT) - ALFABL)) 
     &                   * ARGPV(JPT)
  100    CONTINUE
         IBL = IBL + 1
         INDXBL(1) = IBL
         SBL(IBL) = SPACK(1)
         VBL(IBL) = VPACK(1)
      ELSE 
C--------packing upper surface
C--------do stagnation point
         RMNDR = MOD(AMVEL0,DEL_PHI)
         IF(RMNDR .GT. 0.000001) THEN
C-----------stagnation point does not fall on coordinate point
            KSTPK = INT(AMVEL0/DEL_PHI) + 1
            SSTAG = Y3(APHI(KSTPK),  SLENS(KSTPK),
     &                 APHI(KSTPK-1),SLENS(KSTPK-1),
     &                 AMVEL0)
            SBL(IBL) = SSTAG 
            VBL(IBL) = 0.0
         ELSE
C-----------stagnation point does fall on coordinate point
            KSTPK = INT((AMVEL0 + 0.1*DEL_PHI)/DEL_PHI)
            SBL(IBL) = SLENS(KSTPK+1)
            SSTAG = SBL(1)
            VBL(IBL) = 0.0
         ENDIF
C--------do endpoint
         RMNDR = MOD(PHIEND,DEL_PHI)
         IF(RMNDR .GT. 0.000001) THEN
C-----------end point does not fall on coordinate point
            KENPK = INT(PHIEND/DEL_PHI) + 2
         ELSE
C-----------end point does fall on coordinate point
            KENPK = INT((PHIEND + 0.1*DEL_PHI)/DEL_PHI) + 2
         ENDIF
         DO 200 JPT = KSTPK, KENPK, -1
            IBL = IBL + 1
            SBL(IBL) = SLENS(JPT)
            VBL(IBL) = 2.* DABS(DCOSG(0.5*APHI(JPT) - ALFABL)) 
     &                   * ARGPV(JPT)
  200    CONTINUE
         IBL = IBL + 1
         INDXBL(1) = IBL
         SBL(IBL) = SPACK(1)
         VBL(IBL) = VPACK(1)
      ENDIF
C-----Reset BL coordinates
      IF(LBE) THEN
         DO 300 JBL = 1, IBL
            SBL(JBL) = SBL(JBL) - SSTAG
  300    CONTINUE
      ELSE
         DO 400 JBL = 1, IBL
            SBL(JBL) = SSTAG - SBL(JBL)
  400    CONTINUE
      ENDIF
      RETURN
      END ! PACK1

