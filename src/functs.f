
      FUNCTION DN_DXI(H12,D2)
C***********************************************************************
C...This function computes the integrand.
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      RL = (6.54 * H12 - 14.07)/H12**2.
      RM = (0.058 * (H12 - 4.)**2./(H12 - 1.) - 0.068)/RL
      F = DNDRT(H12) * 0.5 * (RM + 1.) * RL
      DN_DXI = F / D2
      RETURN
      END ! DN_DXI

      FUNCTION RTHETO(H12)
C*********************************************************************
C...This function computes the critical Re.
C   Copyright (c) 1990-2022 Michael Selig
C*********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      RTHETO = 10**((1.415/(H12-1.) - 0.489)
     >         * TANH(20./(H12-1.)-12.9)
     >         + 3.295/(H12-1.)+0.44)
      RETURN
      END ! RTHETO

      FUNCTION DNDRT(H12)
C*********************************************************************
C...This function computes the slope DNDRT.
C   Copyright (c) 1990-2022 Michael Selig
C*********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
C...NOT USED DNDRT=0.01*SQRT((2.4*H12-3.7+2.5*TANH(1.5*H12-3.1))**2.+0.25)
      DNDRT = 0.01
     &         * SQRT((2.4*H12-3.7+2.5*TANH(1.5*H12-4.65))**2.+0.25)
      RETURN
      END


      FUNCTION Y3(X1,Y1,X2,Y2,X3)
C***********************************************************************
C...This function does linear interpolation 
C   'X1,Y1'
C   'X2,Y2'
C   'X3   ---> Y3'
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      Y3 = Y1 + (Y2-Y1)*(X3-X1)/(X2-X1)
      RETURN
      END ! Y3


      FUNCTION FTEI1(PHIB, PHIE, DELAN, DTOR, DLOG2)
C***********************************************************************
C...Evaluate [f]inite [t]railing [e]dge angle [i]ntegral 
C   on "5-22-90" p.14  for the [1]st integral constraint
C   Must be done numerically
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
C-----odd number of points for Simpson integration
C     even number of intervals
      N = 1 + 2 * INT(0.5 * (PHIE-PHIB)/DELAN)
      DELAA = (PHIE - PHIB)/FLOAT(N-1)
      SUM   = 0.
      ANGLE = PHIB
      DO 100 JPT = 2, N-3, 2
         ANGLE = ANGLE + 2. * DELAA
         F1 = DLOG(DSING(0.5D0 * (ANGLE - DELAA)))
         F2 = DLOG(DSING(0.5D0 *  ANGLE         ))
         SUM = SUM + 4.*F1 + 2.*F2
  100 CONTINUE
      F0 = DLOG(DSING(0.5D0 *  PHIB         ))
      F3 = DLOG(DSING(0.5D0 * (PHIE - DELAA)))
      F4 = DLOG(DSING(0.5D0 *  PHIE         ))
      FTEI1 = DELAA * DTOR * (SUM + F0 + 4.*F3 + F4)/3.
      FTEI1 = FTEI1 + DLOG2 * DTOR * (PHIE - PHIB)
      RETURN
      END ! FTEI1


      FUNCTION FTEI2(PHIB, PHIE, DTOR, DLOG2)
C***********************************************************************
C...Evaluate integral on "5-22-90" p.12 for the 2nd integral constraint
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      VEND = DSING(PHIE) * (DLOG(DSING(0.5*PHIE)) - 0.5)
     &       - 0.5 * DTOR * PHIE
     &       + DLOG2 * DSING(PHIE)
      VBEG = DSING(PHIB) * (DLOG(DSING(0.5*PHIB)) - 0.5)
     &       - 0.5 * DTOR * PHIB
     &       + DLOG2 * DSING(PHIB)
      FTEI2 = VEND - VBEG
      RETURN
      END ! FTEI2


      FUNCTION FTEI3(PHIB, PHIE, DLOG2)
C***********************************************************************
C...Evaluate integral on "5-22-90" p.12 for the 3rd integral constraint
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      VEND = DCOSG(PHIE) * (-DLOG(DSING(0.5*PHIE)) + 0.5 - DLOG2)
     &       +  DLOG(DSING(0.5*PHIE)) 
      VBEG = DCOSG(PHIB) * (-DLOG(DSING(0.5*PHIB)) + 0.5 - DLOG2)
     &       +  DLOG(DSING(0.5*PHIB))
      FTEI3 = VEND - VBEG
      RETURN
      END ! FTEI3

      FUNCTION VSEND(JSEG)
C***********************************************************************
C...Determine the design velocity at either the BOS and EOS
C   depending on LBOS
C   LBOS = TT --> returns VSEND = V^*(BOS)
C   LBOS = FF --> returns VSEND = V^*(EOS)
C...Sets JSPSEG and ISBSEG to values for JSEG
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      IF(LBOS) THEN
         VSEND = VS(JSEG)
      ELSE
         VEOS = 0
         IF(LDELV(JSEG)) THEN
            IF(IDVTP(JSEG) .LE. 10) THEN
               VEOS = VTILDE(JSEG)
            ELSE
               JSPSEG = KSPSEG(JSEG)
               ISBSEG = KSBSEG(JSEG)
               VEOS   = SSDELV(JSPSEG,ISBSEG)
            ENDIF
         ENDIF
         VSEND = VS(JSEG) + VEOS 
      ENDIF
      RETURN
      END ! VSEND

      FUNCTION DELTAV(JSEG, PHI)
C***********************************************************************
C   Compute v tilde.
C...JSEG              current segment
C   PHI               current value of phi
C   IDVTP(JSEG) =
C   0   no delta v* term
C   1   linear velocity slope for entire segment given via vtilde
C   2   parabolic velocity distribution for entire segment
C       (slope matching is performed for the preceeding segment)
C   3   linear in x/c approximately for entire segment thru vtilde
C
C   11  linear velocity distribution for each subsegment of a segment
C   12   parabolic velocity distribution for each subsegment of a segment
C       (slope matching is performed for the preceeding 
C        segment and/or subsegment)
C   13-16   cubic velocity distribution for each subsegment of a segment
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER JSEG
      PHILOC = PHI - AM(JSEG-1)
      IF (IDVTP(JSEG) .EQ. 0) THEN
         DELTAV = 0.0
      ELSEIF (IDVTP(JSEG) .EQ. 1) THEN
         DELTAV = VBB(JSEG) * PHILOC 
      ELSEIF (IDVTP(JSEG) .EQ. 2) THEN
         DELTAV =   VBB(JSEG) * PHILOC 
     &            + VCC(JSEG) * PHILOC ** 2
      ELSEIF (IDVTP(JSEG) .EQ. 3) THEN
C--------see notes 12-21-90 p. 2
         XC     = 0.5 * (1. + DCOSG(PHI + 180. - AM(ILE)))
         DELTAV = VAA(JSEG) * (XC - VBB(JSEG))
      ELSEIF (IDVTP(JSEG) .GE. 11) THEN
C--------see notes 6-11-90. Locate position of PHILOC.
         IF(.NOT. LEXTRA) THEN
            IF(.NOT. LWHERE) THEN
C--------------determine the sub-segment index JSBSEG
C--------------check to see if on first sub-segment of the spec segment
               JSBSEG = 1
C--------------see notes 6-13-90
               IF(PHILOC .LT. SSPHI(JSPSEG,JSBSEG)) THEN
                  LWHERE = TT         
               ELSE
                  JSBSEG = 2
C-----------------"do until"
  10              CONTINUE
                     IF((SSPHI(JSPSEG,JSBSEG-1) .LE.  PHILOC) .AND.
     &                  (PHILOC .LE.    SSPHI(JSPSEG,JSBSEG))) THEN
                        LWHERE = TT
                     ELSE 
                        JSBSEG = JSBSEG + 1
                        IF(JSBSEG .GT. ISBSEG)THEN
                           LWHERE = TT
                           LEXTRA = TT
                           JSBSEG = ISBSEG
                        ENDIF
                     ENDIF
                  IF(.NOT. LWHERE) GOTO 10
               ENDIF
            ELSE
               IF(PHILOC .GT. SSPHI(JSPSEG,JSBSEG)) THEN
                  LWHERE = FF
                  JSBSEG = JSBSEG + 1
  20              CONTINUE
                     IF((SSPHI(JSPSEG,JSBSEG-1) .LE.  PHILOC) .AND.
     &                  (PHILOC .LE.    SSPHI(JSPSEG,JSBSEG)) .AND.
     &                  (JSBSEG .LE. ISBSEG)) THEN
                        LWHERE = TT
                     ELSE 
                        JSBSEG = JSBSEG + 1
                        IF(JSBSEG .GT. ISBSEG)THEN
                           LWHERE = TT
                           LEXTRA = TT
                           JSBSEG = ISBSEG
                        ENDIF
                     ENDIF
                  IF(.NOT. LWHERE) GOTO 20
               ENDIF
            ENDIF
         ENDIF
C--------interpolate between JSBSEG-1 and JSBSEG 
         IF(IDVTP(JSEG) .EQ. 11) THEN
            IF(JSBSEG .GT. 1) THEN
               DELTAV=((SSDELV(JSPSEG,JSBSEG)-SSDELV(JSPSEG,JSBSEG-1))/
     &                 ( SSPHI(JSPSEG,JSBSEG)- SSPHI(JSPSEG,JSBSEG-1)))
     &                *( PHILOC              - SSPHI(JSPSEG,JSBSEG-1))
     &                + SSDELV(JSPSEG,JSBSEG-1) 
            ELSE
               DELTAV =  SSDELV(JSPSEG,JSBSEG)/SSPHI(JSPSEG,JSBSEG)
     &                  * PHILOC
            ENDIF
         ELSEIF(IDVTP(JSEG) .EQ. 12) THEN
            DELTAV =   VAASS(JSPSEG,JSBSEG) 
     &               + VBBSS(JSPSEG,JSBSEG) * PHILOC 
     &               + VCCSS(JSPSEG,JSBSEG) * PHILOC ** 2
         ELSEIF(IDVTP(JSEG) .EQ. 13
     &          .OR. IDVTP(JSEG) .EQ. 14
     &          .OR. IDVTP(JSEG) .EQ. 15
     &          .OR. IDVTP(JSEG) .EQ. 16) THEN
            DELTAV =   VAASS(JSPSEG,JSBSEG) 
     &               + VBBSS(JSPSEG,JSBSEG) * PHILOC 
     &               + VCCSS(JSPSEG,JSBSEG) * PHILOC ** 2
     &               + VDDSS(JSPSEG,JSBSEG) * PHILOC ** 3
         ENDIF
      ELSE
         WRITE(lu06,*) 
     $       '  Error 150: This idvtp(jseg) is not valid (deltav.f)'
      ENDIF
      RETURN
      END ! FUNCTION DELTAV

 
      FUNCTION UU(JSEG, PHI)
C***********************************************************************
C   Compute 1 + (v tilde)/v*
C...see SUB DELTAV for definitions.
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER JSEG
      ARG = 1. + DELTAV(JSEG,PHI)/VS(JSEG)
      IF(ARG .LE. 0.) THEN
        WRITE(lu06,*) 
     $       '  Error 151: ARG OF LOG IS NEGATIVE IN FUNCTION U (uu.f)'
        STOP
      ENDIF
      UU = DLOG(ARG)
      RETURN
      END !FUNCTION UU



      FUNCTION DPN(JSEG)
C***********************************************************************
C...Compute slope in P at location AM(JSEG) minus. See 8-15-90 p. 3
C   Note that this function does reset JSPSEG and ISBSEG to JSEG
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER JSEG
C-----slope contribution due to design angle-of-attack
      DP_PHIN = - 0.5 * DTANG(0.5D0 * AM(JSEG) - ALFAS(JSEG))
C-----slope contribution due to non-constant design velocity 
      IF (LDELV(JSEG)) THEN
        IF(IDVTP(JSEG) .EQ. 1) THEN
          DP_PHIN = DP_PHIN 
     &      - VBB(JSEG) * RTOD /(VS(JSEG) + VTILDE(JSEG))
        ELSEIF(IDVTP(JSEG) .EQ. 2) THEN
          DVP = VBB(JSEG)
     &      + 2. * VCC(JSEG) * (AM(JSEG)-AM(JSEG-1))
          DP_PHIN = DP_PHIN 
     &      - DVP * RTOD / (VS(JSEG) + VTILDE(JSEG))
        ELSEIF(IDVTP(JSEG) .EQ. 3) THEN
C---------see notes 12-21-90 p. 2
          DVP = VAA(JSEG) * 0.5 * (-DSING(AM(JSEG) + 180. - AM(ILE)))
          DP_PHIN = DP_PHIN 
     &      - DVP / (VS(JSEG) + VTILDE(JSEG))
        ELSEIF(IDVTP(JSEG) .EQ. 11) THEN
          JSPSEG = KSPSEG(JSEG)
          ISBSEG = KSBSEG(JSEG)
          DP_PHIN = DP_PHIN 
     &      - (SSDELV(JSPSEG,ISBSEG) - SSDELV(JSPSEG,ISBSEG-1))*RTOD
     &       /( SSPHI(JSPSEG,ISBSEG) -  SSPHI(JSPSEG,ISBSEG-1))
     &       /(VS(JSEG) + SSDELV(JSPSEG,ISBSEG))
        ELSEIF(IDVTP(JSEG) .EQ. 12) THEN
          JSPSEG = KSPSEG(JSEG)
          ISBSEG = KSBSEG(JSEG)
          DVP = VBBSS(JSPSEG,ISBSEG)
     &      + 2. * VCCSS(JSPSEG,ISBSEG) * SSPHI(JSPSEG,ISBSEG)
          DP_PHIN = DP_PHIN 
     &      - DVP * RTOD / (VS(JSEG) + SSDELV(JSPSEG,ISBSEG))
        ELSEIF(IDVTP(JSEG) .EQ. 13
     &          .OR. IDVTP(JSEG) .EQ. 14
     &          .OR. IDVTP(JSEG) .EQ. 15
     &          .OR. IDVTP(JSEG) .EQ. 16) THEN
          JSPSEG = KSPSEG(JSEG)
          ISBSEG = KSBSEG(JSEG)
          DVP = VBBSS(JSPSEG,ISBSEG)
     &      + 2. * VCCSS(JSPSEG,ISBSEG) * SSPHI(JSPSEG,ISBSEG)
     &      + 3. * VDDSS(JSPSEG,ISBSEG) * SSPHI(JSPSEG,ISBSEG)**2
          DP_PHIN = DP_PHIN 
     &      - DVP * RTOD / (VS(JSEG) + SSDELV(JSPSEG,ISBSEG))
        ENDIF
      ENDIF
      IF(JSEG .EQ. 1) THEN
        DP_PHIN = DP_PHIN 
     &    - UMU(1) * AKA(1) * DSING(AW(1))/(1 + DCOSG(AW(1)))
      ENDIF
      DPN = DP_PHIN
      RETURN
      END ! FUNCTION DPN

      FUNCTION DPP(JSEG)
C***********************************************************************
C...Compute slope in P at location JSEG plus. See 8-15-90 p. 3
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER JSEG
C-----slope contribution due to design angle-of-attack
      DP_PHIP = - 0.5 * DTANG(0.5D0 * AM(JSEG) - ALFAS(JSEG+1))
C-----slope contribution due to non-constant design velocity 
      IF (LDELV(JSEG+1)) THEN
        IF(IDVTP(JSEG+1) .EQ. 1) THEN
          DP_PHIP = DP_PHIP 
     &      - VBB(JSEG+1) * RTOD /VS(JSEG+1)
        ELSEIF(IDVTP(JSEG+1) .EQ. 2) THEN
          DVP = VBB(JSEG+1)
          DP_PHIP = DP_PHIP 
     &      - DVP * RTOD / VS(JSEG+1)
        ELSEIF(IDVTP(JSEG+1) .EQ. 3) THEN
C---------see notes 12-21-90 p. 2
          DVP = VAA(JSEG+1) * 0.5 * (-DSING(AM(JSEG) + 180. - AM(ILE)))
          DP_PHIP = DP_PHIP 
     &      - DVP / VS(JSEG+1)
        ELSEIF(IDVTP(JSEG+1) .EQ. 11) THEN
          JSPSEGT = KSPSEG(JSEG+1)
          DP_PHIP = DP_PHIP 
     &      - (SSDELV(JSPSEGT,1)*RTOD/SSPHI(JSPSEGT,1))/VS(JSEG+1)
        ELSEIF(IDVTP(JSEG+1) .EQ. 12) THEN
          JSPSEGT = KSPSEG(JSEG+1)
          DVP = VBBSS(JSPSEGT,1)
          DP_PHIN = DP_PHIN - DVP * RTOD / VS(JSEG+1) 
        ELSEIF(IDVTP(JSEG+1) .EQ. 13
     &         .OR. IDVTP(JSEG+1) .EQ. 14
     &         .OR. IDVTP(JSEG+1) .EQ. 15
     &         .OR. IDVTP(JSEG+1) .EQ. 16) THEN
          JSPSEGT = KSPSEG(JSEG+1)
          DVP = VBBSS(JSPSEGT,1)
          DP_PHIN = DP_PHIN - DVP * RTOD / VS(JSEG+1) 
        ENDIF
      ENDIF
      IF(JSEG .EQ. (ISEG-1) ) THEN
        DP_PHIP = DP_PHIP 
     &    - UMU(2) * AKA(2) * DSING(AW(2))/(1 + DCOSG(AW(2)))
      ENDIF
      DPP = DP_PHIP
      RETURN
      END ! FUNCTION DPP
 

      FUNCTION W_W(AKA,AM,AW)
C***********************************************************************
C...Eppler's assumed form of the main recovery
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      W_W = 1. + AKA * ((DCOSG(AM) - DCOSG(AW))/
     &                  (1.        + DCOSG(AW)))
      RETURN
      END ! FUNCTION W_W

      FUNCTION W_S(AM,AS)
C***********************************************************************
C...Eppler's assumed form of the closure 
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      W_S = 1. - 0.36 * (((DCOSG(AM) - DCOSG(AS))/
     &                   (1.         - DCOSG(AS)))**2)
      RETURN
      END ! FUNCTION W_S

      FUNCTION W_F(AM,PHIEPP)
C***********************************************************************
C...Finite TE contribution
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      W_F = DSING(0.5D0*AM) / DSING(0.5D0*PHIEPP)
      RETURN
      END ! FUNCTION W_F

      FUNCTION T(A,B)
C***********************************************************************
C...T
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      T = DLOG(DABS(DCOSG(A/(2.D0)-B)))
      RETURN
      END ! FUNCTION T(A,B)

      FUNCTION TTT(A,B,C,D)
C***********************************************************************
C...TTT
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      T1 = DLOG(DABS(DCOSG(A/(2.D0)-B)))
      T2 = DLOG(DABS(DCOSG(C/(2.D0)-D)))
      TTT = T1 - T2
      RETURN
      END ! FUNCTION TTT(A,B,C,D)

      FUNCTION AMSTAG(ALFA)
C***********************************************************************
C...Compute the leading edge stagnation point.
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      AMSTAG = 180. + 2. * ALFA
      RETURN
      END ! AMSTAG

      FUNCTION FU_X(XI, UI, XF, UF)
C***********************************************************************
C...Compute the velocity gradient for interval
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      FU_X = (UF - UI) / (XF - XI)
      RETURN
      END ! FU_X

      FUNCTION FRD2(RINF, U, D2)
C***********************************************************************
C...Momentum thickness based on momentum thickness
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      FRD2 = RINF * U * D2
      RETURN
      END ! FR2D

      FUNCTION FH12(H32)
C***********************************************************************
C...Compute H12 from H32 for laminar boundary layer.
C...See notes 11-14-91 p. 3 and 10-17-91 p.6.
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      IF(H32 .EQ. 1.515) THEN
         FH12 = 4.
      ELSEIF(H32 .GT. 1.515) THEN
         FH12= -5.9671 + 6.578943894*H32 
     &                   - DSQRT(43.28254848555*(0.907-H32)**2.-16.)
      ELSE
C--------fictitious correlation for H_32 less than 1.515
C        this means that if H12 is greater than 4, then the BL solution
c        is nonphysical.
         FH12 = 4 + 7. * DSQRT(1.515-H32)
      ENDIF
      RETURN
      END ! FH12

      FUNCTION FTH12(H32)
C***********************************************************************
C...Compute H12 from H32 for turbulent boundary layer.
C...See notes 11-14-91 p. 3 and 10-17-91 p.6.
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      FTH12 = (11. * H32 + 15.)/(48. * H32 - 59.)
      RETURN
      END ! FTH12

      FUNCTION FCF(H12, RD2)
C***********************************************************************
C...Compute cf from H12 and Rdelta2 via Drela Correlation
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      IF (H12 .LT. 7.4) THEN
         FCF = (-.067+0.01977*(7.4-H12)**2./(H12-1.))/RD2
      ELSE
         FCF = (-.067+0.022*(1-1.4/(H12-6.))**2.)/RD2
      ENDIF
      RETURN
      END ! FCF

      FUNCTION FTCF(H12, RD2)
C***********************************************************************
C...Compute cf from H12 and Rdelta2 via Drela Correlation
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      FTCF = 0.045716 * ((H12 - 1) * RD2)**(-0.232) * EXP(-1.26 * H12)
      RETURN
      END ! FTCF

      FUNCTION FCDDISS(H12, H32, RD2)
C***********************************************************************
C...Compute cD from H12, H32, and Rdelta2 via Drela Correlation
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      IF (H12 .LT. 4.) THEN
         FCDDISS = H32*(0.207+0.00205*(4.-H12)**5.5)/RD2
      ELSE
         FCDDISS = H32*(0.207-0.003*(H12-4.)**2./(1+0.02*H12**2.))/RD2
      ENDIF
      RETURN
      END ! FCDDISS

      FUNCTION FTCD(H12, RD2)
C***********************************************************************
C...Compute cD from H12, H32, and Rdelta2 via Drela Correlation
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      FTCD = 0.01 * ((H12 - 1) * RD2)**(-.166666666666666) 
      RETURN
      END ! FTCD

      FUNCTION FD2_X(H12, D2, U_X, U, CF)
C***********************************************************************
C...Integral momentum equation d(delta2)/dx
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      FD2_X = -(2. + H12) * D2 * U_X / U + CF
      RETURN
      END ! FD2_X

      FUNCTION FD3_X(D3, U_X, U, CD)
C***********************************************************************
C...Integral energy equation d(delta3)/dx
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      FD3_X = - 3. * D3 * U_X / U + CD
      RETURN
      END ! FD3_X

      FUNCTION TC(H32, RD2, EM, ER)
C***********************************************************************
C...Eppler's transition criterion function
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      TC = 18.4 * H32 - 21.74 + 125. * EM * (H32 - 1.573) ** 2
     &          - 0.36 * ER - DLOG(RD2)
      RETURN
      END ! TC

