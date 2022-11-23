
      SUBROUTINE PSMOOTH
C***********************************************************************
C...This routine splits off the discontinuous slopes in the harmoic P
C    PHIK     - phi location to smooth
C    DP_PHIN  - slope on negative side
C    DP_PHIP  - slope on positive side
C    AK       - difference in slopes
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
C-----initialize HPSMO(.) and HQ(.)
      DO 50 JNU = 1, IARGP
        HPSMO(JNU) = 0.0
        HQ(JNU)    = 0.0
   50 CONTINUE
      DO 100 JSEG = 1, ISEG-1
        PHIK = AM(JSEG)
        DP_PHIN = DPN(JSEG)
        DP_PHIP = DPP(JSEG)
C-------difference in slopes at the location PHIK 
        AK = DP_PHIP - DP_PHIN
C-------Test the number.
        IF (DABS(AK) .GT. 0.02) CALL SPLIT(PHIK, AK) 
C-------split off of interior subsegments for segment JSEG
        IF(LDELV(JSEG)) THEN
          IF(IDVTP(JSEG) .EQ. 11) THEN
            JSPSEG = KSPSEG(JSEG)
            ISBSEG = KSBSEG(JSEG)
            DO 200 JSBSEG = 1, ISBSEG-1
              PHIK = AM(JSEG-1) + SSPHI(JSPSEG,JSBSEG)
C-------------slope on negative side of PHIK
C             slope contribution due to design angle-of-attack
C....note that I could remove the alfas(.) contribution from both + and -
              DP_PHIN = - 0.5 * DTANG(0.5D0 * PHIK - ALFAS(JSEG))
C-------------slope contribution due to non-constant design velocity 
              IF(JSBSEG .EQ. 1) THEN
                DP_PHIN = DP_PHIN
     &            - (SSDELV(JSPSEG,JSBSEG)*RTOD/SSPHI(JSPSEG,JSBSEG))
     &             /(VS(JSEG) + SSDELV(JSPSEG,JSBSEG))
              ELSE 
                DP_PHIN = DP_PHIN
     &           - (SSDELV(JSPSEG,JSBSEG) - SSDELV(JSPSEG,JSBSEG-1))
     &              *RTOD
     &            /( SSPHI(JSPSEG,JSBSEG) -  SSPHI(JSPSEG,JSBSEG-1))
     &            /(VS(JSEG) + SSDELV(JSPSEG,JSBSEG))
              ENDIF
C-------------slope contribution due to design angle-of-attack
              DP_PHIP = - 0.5 * DTANG(0.5D0 * PHIK - ALFAS(JSEG))
C-------------slope contribution due to non-constant design velocity 
              DP_PHIP = DP_PHIP
     &          - (SSDELV(JSPSEG,JSBSEG+1) - SSDELV(JSPSEG,JSBSEG))
     &            *RTOD
     &           /( SSPHI(JSPSEG,JSBSEG+1) -  SSPHI(JSPSEG,JSBSEG))
     &           /(VS(JSEG) + SSDELV(JSPSEG,JSBSEG))
C-------------difference in slopes at the location PHIK 
              AK = DP_PHIP - DP_PHIN
              IF (DABS(AK) .GT. 0.02) CALL SPLIT(PHIK, AK) 
  200       CONTINUE
          ENDIF
        ENDIF
  100 CONTINUE
C-----split off trailing edge
      PHIK = 0.0 
C-----slope on negative and positive side of PHIK
      DP_PHIN = - 0.5 * DTANG(0.5D0 * PHIK - ALFAS(ISEG))
      DP_PHIP = - 0.5 * DTANG(0.5D0 * PHIK - ALFAS(1))
C-----difference in slopes at the location PHIK 
      AK = DP_PHIP - DP_PHIN
      IF (DABS(AK) .GT. 0.02) CALL SPLIT(PHIK, AK) 
C-----split off FTE contribution if present
C     see notes 8-15-90 p.1 and "5-22-90" p.19
      IF(LFTE) THEN
         PHIK = PHIEPP(1)
         AK = 0. - (-0.5 * EPP / DTANG(0.5*PHIK))
         IF (DABS(AK) .GT. 0.02) CALL SPLIT(PHIK, AK) 
         PHIK = PHIEPP(2)
         AK = -0.5 * EPP / DTANG(0.5*PHIK) - 0.
         IF (DABS(AK) .GT. 0.02) CALL SPLIT(PHIK, AK) 
      ENDIF
C-----add in the harmonic P
      DO 150 JNU = 1, IARGP
        HPSMO(JNU) = HP(JNU) + HPSMO(JNU)
  150 CONTINUE
      RETURN
      END ! PSMOOTH

