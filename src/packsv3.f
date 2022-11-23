
      SUBROUTINE PACKSV3(JSEG)
C***********************************************************************
C...Determine the point to be packed in the BL arc length and vel array
C   Returns: IPACK = 1
C            VPACK(1)
C            SPACK(1)
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER JSEG 
      IPACK = 1
C-----determine endpoint velocity at off-design angle of attack ALFABL
      VPACK(1) = VSSEND * DABS(DCOSG(0.5*PHIEND - ALFABL)
     &                        /DCOSG(0.5*PHIEND - ALFAS(JSEG)))
C-----determine location of PHIEND between JPT-1 and JPT
      JPT = INT((PHIEND + 0.1*DEL_PHI) / DEL_PHI) + 2
C-----interpolate to get arc length S 
      SPACK(1) = (SLENS(JPT) - SLENS(JPT-1))/(APHI(JPT) - APHI(JPT-1))
     &         * (PHIEND - APHI(JPT-1)) + SLENS(JPT-1)
      RETURN 
      END ! PACKSV3

