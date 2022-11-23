
      SUBROUTINE SVBLTR
C***********************************************************************
C...Determine SBLTR and VBLTR from JSEGTR,LBE,LBOSTR,ALFABL,and SSTAG
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
C-----determine phi location
      IF(LBOSTR) THEN
         PHITR = AM(JSEGTR-1)
      ELSE
         PHITR = AM(JSEGTR)
      ENDIF          
      JPT = INT((PHITR + 0.1*DEL_PHI) / DEL_PHI) + 2
C-----determine the arc length S at location PHITR
      STR = (SLENS(JPT) - SLENS(JPT-1))/(APHI(JPT) - APHI(JPT-1))
     &     * (PHITR - APHI(JPT-1)) + SLENS(JPT-1)
C-----get arc length in local coordinates
      IF(LBE) THEN
         SBLTR = STR - SSTAG
      ELSE
         SBLTR = SSTAG - STR
      ENDIF
C-----determine the design velocity 
      IF(LBOSTR) THEN
         VSSEND = VS(JSEGTR)
      ELSE
         VEOS = 0
         IF(LDELV(JSEGTR)) THEN
            IF(IDVTP(JSEGTR) .LE. 10) THEN
               VEOS = VTILDE(JSEGTR)
            ELSE
               VEOS   = SSDELV(KSPSEG(JSEGTR),KSBSEG(JSEGTR))
            ENDIF
         ENDIF
         VSSEND = VS(JSEGTR) + VEOS 
      ENDIF
C-----determine velocity at off-design angle of attack ALFABL
      VBLTR = VSSEND * DABS(DCOSG(0.5*PHITR - ALFABL)
     &                     /DCOSG(0.5*PHITR - ALFAS(JSEGTR)))
      RETURN
      END ! SVBLTR

