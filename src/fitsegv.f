
      SUBROUTINE FITSEGV
C***********************************************************************
C...Determine coefficients for v tilde functions
C...DP_PHIN  required P' at beginning of segment. See 8-15-90 p. 3 
C                                                 &   8-24-90 p. 3
C...DVP      required slope in the velocity to maintain continuous P'
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      DO 100 JSEG = 2, ISEG-1
        IF(LDELV(JSEG)) THEN
          IF(IDVTP(JSEG) .EQ. 1) THEN 
            VBB(JSEG) = VTILDE(JSEG) / (AM(JSEG) - AM(JSEG-1))
          ELSEIF(IDVTP(JSEG) .EQ. 3) THEN 
C-----------note that here the coefs V** are not for a polynomial
C-----------see notes 12-21-90 p. 2
            VBB(JSEG) = 0.5 * (1. + DCOSG(AM(JSEG-1) + 180. - AM(ILE)))
            XCF       = 0.5 * (1. + DCOSG(AM(JSEG)   + 180. - AM(ILE)))
            VAA(JSEG) = VTILDE(JSEG) / (XCF - VBB(JSEG))
          ENDIF
        ENDIF
  100 CONTINUE
      DO 500 JSEG = 2, ISEG-1
        IF(LDELV(JSEG)) THEN
          IF(IDVTP(JSEG) .EQ. 2) THEN 
C-----------first derivative given at BOS
            LBOS = TT
            DP_PHIN = DPN(JSEG-1)
            DVP = - VSEND(JSEG) * (DP_PHIN 
     &                + 0.5 * DTANG(0.5D0 * AM(JSEG-1) - ALFAS(JSEG)))
            VCC(JSEG) = VTILDE(JSEG)/(AM(JSEG)-AM(JSEG-1))**2 
     &                 - DVP * DTOR/(AM(JSEG)-AM(JSEG-1))
            VBB(JSEG) = DVP * DTOR
          ELSEIF(IDVTP(JSEG) .EQ. 12) THEN
C-----------parabolic for each subsegment, slope matching at BOS
            LBOS = TT
            DP_PHIN = DPN(JSEG-1)
            DVP = - VSEND(JSEG) * (DP_PHIN 
     &                + 0.5 * DTANG(0.5D0 * AM(JSEG-1) - ALFAS(JSEG)))
            JSPSEG = KSPSEG(JSEG)
            ISBSEG = KSBSEG(JSEG)
            VCCSS(JSPSEG,1) = SSDELV(JSPSEG,1)/SSPHI(JSPSEG,1)**2 
     &                        - DVP * DTOR/SSPHI(JSPSEG,1)
            VBBSS(JSPSEG,1) = DVP * DTOR
            VAASS(JSPSEG,1) = 0.
            DVP = VBBSS(JSPSEG,1)
     &        + 2. * VCCSS(JSPSEG,1) * SSPHI(JSPSEG,1)
            DO 600 JSBSEG = 2, ISBSEG
              VCCSS(JSPSEG,JSBSEG) = 
     &          -(SSDELV(JSPSEG,JSBSEG-1) - SSDELV(JSPSEG,JSBSEG))
     &          /( SSPHI(JSPSEG,JSBSEG-1) -  SSPHI(JSPSEG,JSBSEG))**2
     &          + DVP/( SSPHI(JSPSEG,JSBSEG-1) -  SSPHI(JSPSEG,JSBSEG))
              VBBSS(JSPSEG,JSBSEG) = DVP 
     &          - 2. * VCCSS(JSPSEG,JSBSEG) * SSPHI(JSPSEG,JSBSEG-1)
              VAASS(JSPSEG,JSBSEG) = SSDELV(JSPSEG,JSBSEG-1)
     &          - VBBSS(JSPSEG,JSBSEG) * SSPHI(JSPSEG,JSBSEG-1)
     &          - VCCSS(JSPSEG,JSBSEG) * SSPHI(JSPSEG,JSBSEG-1)**2
              DVP = VBBSS(JSPSEG,JSBSEG)
     &          + 2. * VCCSS(JSPSEG,JSBSEG) * SSPHI(JSPSEG,JSBSEG)
  600       CONTINUE          
          ELSEIF(IDVTP(JSEG) .EQ. 13
     &            .OR. IDVTP(JSEG) .EQ. 14 
     &            .OR. IDVTP(JSEG) .EQ. 15
     &            .OR. IDVTP(JSEG) .EQ. 16) THEN
            IF(IDVTP(JSEG) .EQ. 13) THEN
C--------------first derivative given at BOS
C--------------second derivative zero at EOS
               LBOS = TT
               DP_PHIN = DPN(JSEG-1)
               DVP = - VSEND(JSEG) * (DP_PHIN 
     &               + 0.5 * DTANG(0.5D0 * AM(JSEG-1) - ALFAS(JSEG)))
               YP1 = DVP * DTOR
               YPN = 1.E10
            ELSEIF(IDVTP(JSEG) .EQ. 14) THEN
C--------------second derivative zero at BOS
C--------------first derivative given at EOS
               LBOS = FF
               DP_PHIP = DPP(JSEG)
               DVP = - VSEND(JSEG) * (DP_PHIP 
     &               + 0.5 * DTANG(0.5D0 * AM(JSEG) - ALFAS(JSEG)))
               YP1 = 1.E10
               YPN = DVP * DTOR
            ELSEIF(IDVTP(JSEG) .EQ. 15) THEN
C--------------first derivative is given at BOS
C--------------first derivative is given at EOS
               LBOS = TT
               DP_PHIN = DPN(JSEG-1)
               DVP = - VSEND(JSEG) * (DP_PHIN 
     &               + 0.5 * DTANG(0.5D0 * AM(JSEG-1) - ALFAS(JSEG)))
               YP1 = DVP * DTOR
               LBOS = FF
               DP_PHIP = DPP(JSEG)
               DVP = - VSEND(JSEG) * (DP_PHIP 
     &               + 0.5 * DTANG(0.5D0 * AM(JSEG) - ALFAS(JSEG)))
               YPN = DVP * DTOR
            ELSEIF(IDVTP(JSEG) .EQ. 16) THEN
C--------------second derivative is zero BOS
C--------------second derivative is zero EOS
               YP1 = 1.E10
               YPN = 1.E10
            ENDIF
C-----------dump SSPHI(..) and SSDELV(..) into work arrays for SPLINE
            XDM(1) = 0.
            YDM(1) = 0.
            JSPSEG = KSPSEG(JSEG)
            ISBSEG = KSBSEG(JSEG)
            DO 700 JSBSEG = 1, ISBSEG
               XDM(JSBSEG+1) =  SSPHI(JSPSEG,JSBSEG)
               YDM(JSBSEG+1) = SSDELV(JSPSEG,JSBSEG)
  700        CONTINUE
C-----------compute the second derivative of the cubic spline
            CALL SPLINE(XDM,YDM,ISBSEG+1,NSPL,YP1,YPN,Y2)
C-----------compute the cubic coefficients
            DO 800 JSBSEG = 1, ISBSEG
               VDDSS(JSPSEG,JSBSEG) = (Y2(JSBSEG) - Y2(JSBSEG+1))
     &           * 0.1666666666666667
     &           / (XDM(JSBSEG) - XDM(JSBSEG+1))
               VCCSS(JSPSEG,JSBSEG) = 0.5 * 
     &           (Y2(JSBSEG) - 6. * VDDSS(JSPSEG,JSBSEG) * XDM(JSBSEG))
               VBBSS(JSPSEG,JSBSEG) = (YDM(JSBSEG)-YDM(JSBSEG+1)
     &       - VCCSS(JSPSEG,JSBSEG) * (XDM(JSBSEG)**2-XDM(JSBSEG+1)**2)
     &       - VDDSS(JSPSEG,JSBSEG) * (XDM(JSBSEG)**3-XDM(JSBSEG+1)**3))
     &           / (XDM(JSBSEG)-XDM(JSBSEG+1))
               VAASS(JSPSEG,JSBSEG) = YDM(JSBSEG) 
     &           - VBBSS(JSPSEG,JSBSEG) * XDM(JSBSEG)
     &           - VCCSS(JSPSEG,JSBSEG) * XDM(JSBSEG)**2
     &           - VDDSS(JSPSEG,JSBSEG) * XDM(JSBSEG)**3
  800       CONTINUE
          ENDIF
        ENDIF
  500 CONTINUE
      RETURN
      END ! FITSEGV

