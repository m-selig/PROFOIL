
      SUBROUTINE ARGHP
C***********************************************************************
C...Compute the [arg]ument of the [h]armonic function [P] from 
C   design parameters
C   Compute ARGPB(.)
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      LOGICAL LSEARCH
      JSEG    = 1
      LOFFCTP = TT
C-----compute the argument of the harmonic function P(PHI) 
C     at the LE arc limit whether or not it is on a control point
      AMILE   = AM(ILE)
      IF(IDVTP(ILE) .GE. 11) THEN
          LWHERE = FF
          LEXTRA = FF
          JSPSEG = KSPSEG(ILE)
          ISBSEG = KSBSEG(ILE)
      ENDIF
      ARGPILE = 0.5 * (VS(ILE) + DELTAV(ILE,AMILE))
     &              /  DABS(DCOSG(AMILE/2. - ALFAS(ILE)))

      IF(IDVTP(JSEG) .GE. 11) THEN
          LWHERE = FF
          LEXTRA = FF
          JSPSEG = KSPSEG(JSEG)
          ISBSEG = KSBSEG(JSEG)
      ENDIF
 
      DO 100 IPT = 1, IARGP+1
C--------basic velocity
         ARGPB(IPT) = 0.5 * VS(JSEG) 
     &                   / DABS(DCOSG(APHI(IPT)/2. - ALFAS(JSEG)))
C--------main recovery
         IF (APHI(IPT) .LE. AW(1)) THEN
            ARGPW = W_W(AKA(1),APHI(IPT),AW(1))**(-UMU(1))
            ARGPB(IPT) = ARGPB(IPT) * ARGPW
         ENDIF
         IF (APHI(IPT) .GE. AW(2)) THEN
            ARGPW = W_W(AKA(2),APHI(IPT),AW(2))**(-UMU(2))
            ARGPB(IPT) = ARGPB(IPT) * ARGPW
         ENDIF
C--------closure
         IF (APHI(IPT) .LE. AS(1)) THEN
            ARGPS = W_S(APHI(IPT),AS(1))**HKH(1)
            ARGPB(IPT) = ARGPB(IPT) * ARGPS
         ENDIF
         IF (APHI(IPT) .GE. AS(2)) THEN
            ARGPS = W_S(APHI(IPT),AS(2))**HKH(2)
            ARGPB(IPT) = ARGPB(IPT) * ARGPS
         ENDIF
C--------delta v* terms
         IF (LLDELV) THEN
            IF(JSEG .GT. 1) THEN
               ARGPDV = 1. + DELTAV(JSEG, APHI(IPT))/VS(JSEG)
               ARGPB(IPT) = ARGPB(IPT) * ARGPDV
            ELSE
C--------------Added if/else/endif here 151128 because going into DELTAV with JSEG = 1 leads to asking for AM(0)
C--------------which does not exist.
               ARGPB(IPT) = ARGPB(IPT) * 1.0
            ENDIF
         ENDIF

         IF ((IPT+1) .LT. (IARGP+1)) THEN
C-----------I was having troubles with APHI(IARGP) being greater the 360.
C-----------1-24-91
            IF (APHI(IPT+1) .GT. AM(JSEG)) THEN
C--------------search for the JSEG that contains APHI(IPT+1)
               LSEARCH = FF
  250          CONTINUE
                  IF(  (AM(JSEG) .LE. APHI(IPT+1)) .AND.
     &              (APHI(IPT+1) .LE.  AM(JSEG+1))) THEN
                      LSEARCH = TT
                      JSEG = JSEG + 1
                   ELSE
                      JSEG = JSEG + 1
                   ENDIF
               IF(.NOT. LSEARCH) GO TO 250
               IF(IDVTP(JSEG) .GE. 11) THEN 
                  LWHERE = FF
                  LEXTRA = FF
                  JSPSEG = KSPSEG(JSEG)
                  ISBSEG = KSBSEG(JSEG)
               ENDIF
            ENDIF         
         ENDIF
C--------check to see if the LE arc limit falls on a control point
C        and save the index of the control point just before the LE arc limit
         IF (APHI(IPT) .EQ. AMILE) LOFFCTP = FF
         IF (APHI(IPT) .LT. AMILE) ILECTP = IPT 
  100 CONTINUE
      IF(LFTE) THEN
C--------Yes FTE angle.
         CALL ARGPFTE
      ELSE
C--------No FTE angle.
         DO 200 JPT = 1, IARGP+1
            ARGPV(JPT) = ARGPB(JPT)         
            ARGP(JPT)  = ARGPB(JPT)         
  200    CONTINUE
      ENDIF
      RETURN
      END ! ARGHP

