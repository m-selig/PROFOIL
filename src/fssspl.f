
      SUBROUTINE FSSSPL(JADJS)
C***********************************************************************
C...Calculate local f*(s)_j at the Newton nodes (--> SSFNN(.))
C   for the local arc lengths given in SJSBSEG(.) using the splines.
C   ISBSEG from calling subroutine
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      IADJSBS = KADJSBS(JADJS)
      LWHERE = FF
      LEXTRA = FF
C-----[] TODO: I think I need something like the following 
C     if-block in FUNCTION DELTAV
      IF(IADJSBS .EQ. 1) THEN
        LEXTRA = TT
        JADJSBS = 1
      ENDIF
C-----Find the index of JADJSBS where the local arc length of the
C     subsegment SJSBSEG(.) is between point JADJSBS-1 and JADJSBS.
      DO 100 JSBSEG = 1, ISBSEG 
        SLOC = SJSBSEG(JSBSEG) 
        IF(.NOT. LEXTRA) THEN
          IF(.NOT. LWHERE) THEN
            JADJSBS = 1
            IF(SLOC .LT. SSS(JADJS,JADJSBS))THEN
              LWHERE = TT
            ELSE
              JADJSBS = 2
   10         CONTINUE
                IF((SSS(JADJS,JADJSBS-1) .LE. SLOC) .AND.
     &             (SLOC .LE.   SSS(JADJS,JADJSBS))) THEN
                  LWHERE = TT
                ELSE
                  JADJSBS = JADJSBS + 1
                  IF(JADJSBS .GT. IADJSBS) THEN
                    LWHERE = TT
                    LEXTRA = TT
                    JADJSBS = IADJSBS 
                  ENDIF
                ENDIF
              IF(.NOT. LWHERE) GOTO 10
            ENDIF
          ELSE
            IF(SLOC .GT. SSS(JADJS,JADJSBS)) THEN
              LWHERE = FF
              JADJSBS = JADJSBS + 1
   20         CONTINUE
                IF((SSS(JADJS,JADJSBS-1) .LE. SLOC) .AND.
     &             (SLOC .LE.   SSS(JADJS,JADJSBS))) THEN
                  LWHERE = TT
                ELSE
                  JADJSBS = JADJSBS + 1
                  IF(JADJSBS .GT. IADJSBS) THEN
                    LWHERE = TT
                    LEXTRA = TT
                    JADJSBS = IADJSBS 
                  ENDIF
                ENDIF
              IF(.NOT. LWHERE) GOTO 20
            ENDIF
          ENDIF
        ENDIF
C-------Location found for SJSBSEG(JSBSEG), now interpolating.
        IF(JADJSBS .GT. 1) THEN
          IF(LSNEWT2) THEN
C---------use spline coefficients
          SSFNN(JSBSEG) =   FAASS(JADJS,JADJSBS) 
     &                    + FBBSS(JADJS,JADJSBS) * SLOC 
     &                    + FCCSS(JADJS,JADJSBS) * SLOC ** 2
     &                    + FDDSS(JADJS,JADJSBS) * SLOC ** 3
          ELSE
C---------linear interpolation or extrapolation between JADJSBS-1 and JADJSBS.
          SSFNN(JSBSEG) = (SSF(JADJS,JADJSBS) - SSF(JADJS,JADJSBS-1))/
     &                    (SSS(JADJS,JADJSBS) - SSS(JADJS,JADJSBS-1))
     &                  * (SLOC               - SSS(JADJS,JADJSBS-1))
     &                  +                       SSF(JADJS,JADJSBS-1)
          ENDIF
        ELSE
C---------straight line fit from origin to one point given.
          SSFNN(JSBSEG) = (SSF(JADJS,1)/SSS(JADJS,1)) * SLOC
        ENDIF
  100 CONTINUE
      RETURN
      END ! FSSSPL

