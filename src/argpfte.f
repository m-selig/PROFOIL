
      SUBROUTINE ARGPFTE
C***********************************************************************
C...Include terms due to FTE angle.
C   See notes "5-22-90" p.15
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
C-----generate HP(ILE) from ARGILE
      FCTILE = (2. * DSING(0.5D0 * AMILE)) ** (-EPP)
      ARGPILE = ARGPILE * FCTILE
C-----generate ARGP(.) and ARGPV(.) from ARGPB(.)
      FCTBB    = (2. * DSING(0.5D0 * PHIEPP(1))) ** (-EPP)
      FCTEE    = (2. * DSING(0.5D0 * PHIEPP(2))) ** (-EPP)
      DO 100 JPT = 2, IARGP
         FCTMM = (2. * DSING(0.5D0 * APHI(JPT))) ** (-EPP)
         IF(APHI(JPT) .LE. PHIEPP(1)) THEN
            ARGPV(JPT) = ARGPB(JPT) * FCTBB/FCTMM
            ARGP(JPT)  = ARGPB(JPT) * FCTBB
         ELSEIF(APHI(JPT) .GE. PHIEPP(2)) THEN
            ARGPV(JPT) = ARGPB(JPT) * FCTEE/FCTMM
            ARGP(JPT)  = ARGPB(JPT) * FCTEE
         ELSE
            ARGPV(JPT) = ARGPB(JPT) 
            ARGP(JPT)  = ARGPB(JPT) * FCTMM
         ENDIF
  100 CONTINUE
C-----now doing the endpoints separately
      IF(DABS(EPP) .LT. 0.00000001) THEN
         ARGPV(1)       = ARGPB(1)
         ARGPV(IARGP+1) = ARGPB(IARGP+1)
      ELSE
         ARGPV(1)       = 0.
         ARGPV(IARGP+1) = 0.
      ENDIF
      ARGP(1)        = ARGPB(1)       * FCTBB
      ARGP(IARGP+1)  = ARGPB(IARGP+1) * FCTEE
      RETURN
      END ! ARGPFTE

