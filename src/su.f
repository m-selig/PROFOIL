
      SUBROUTINE SU
C***********************************************************************
C...Compute the USIN,UCOS,URGO integrals of the 1st, 2nd and 3rd 
C   integral constraints.
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'

      DO 100 JSEG = 2, ISEG-1
         IF(LDELV(JSEG)) THEN
            IF(IDVTP(JSEG) .GE. 11) THEN
               LWHERE = FF
               LEXTRA = FF
               JSPSEG = KSPSEG(JSEG)
               ISBSEG = KSBSEG(JSEG)
            ENDIF
            N = NINT((AM(JSEG) - AM(JSEG-1))/DELAN)
            DELAA =  (AM(JSEG) - AM(JSEG-1))/FLOAT(N)
            UUU = UU(JSEG, AM(JSEG-1))
            F0         = DSING(AM(JSEG-1)) * UUU
            USIN(JSEG) = 0.
            F2         = DCOSG(AM(JSEG-1)) * UUU
            UCOS(JSEG) = 0.
            F4         = UUU
            URGO(JSEG) = 0.
            PHI = AM(JSEG-1)
            DO 200 I = 1, N
               PHI = PHI + DELAA
               UUU = UU(JSEG, PHI)
               F1 = DSING(PHI) * UUU
               USIN(JSEG) = USIN(JSEG) + 0.5*(F1 + F0)*DELAA*DTOR
               F0 = F1
               F3 = DCOSG(PHI) * UUU
               UCOS(JSEG) = UCOS(JSEG) + 0.5*(F3 + F2)*DELAA*DTOR
               F2 = F3
               F5 = UUU
               URGO(JSEG) = URGO(JSEG) + 0.5*(F5 + F4)*DELAA*DTOR
               F4 = F5
  200       CONTINUE
         ENDIF
  100 CONTINUE
      RETURN
      END ! SU

