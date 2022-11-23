
      SUBROUTINE INITENN
C***********************************************************************
C...Initilization required before ENN calculation
C   of a given Bl development.
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
C-----initilize
      NNN = 0
      NMIN = 0
      ICRIT = 0
      NDRCRIT = 0
      NDRTEMP = 0
      DO 50 JFREQ = 1, IFREQ
         EN(JFREQ)    = 0.
         NCRIT(JFREQ) = 0
         NTEMP(JFREQ) = 0
  50  CONTINUE
      RETURN
      END ! INITENN

