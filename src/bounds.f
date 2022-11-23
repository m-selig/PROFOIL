
      SUBROUTINE BOUNDS(KBEG,KEND,JBL,LITER)
C***********************************************************************
C...find indice bounds on AMPF for given bl location
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER KBEG,KEND
      LOGICAL LBEG, LITER
C-----Find the bounds of the non-zero data.
      LBEG  = FF
      KBEG  = 0
      KEND  = 0
      JFREQ = 0
C-----'do until' KBEG and KEND are set
 100  CONTINUE
         JFREQ = JFREQ + 1
         IF(.NOT. LBEG) THEN
C-----------find KBEG
            IF(AMPF(JFREQ,JBL) .NE. 0.) THEN
               LBEG = TT
               KBEG = JFREQ
            ENDIF
         ELSE
C-----------find KEND
            IF(DABS(AMPF(JFREQ,JBL)) .LT. 0.00001) THEN
               KEND = JFREQ
            ENDIF
         ENDIF
      IF(JFREQ .LT. IFREQ .AND. KEND .EQ. 0) GOTO 100
C-----perform diagnotics if in iteration mode
      IF(LITER) THEN
C--------Stop calculation if KBEG = 0 --> all zeros
         IF(KBEG .EQ. 0) THEN
            WRITE(lu06,*) ' Error 142: kbeg = 0 in envlpe (bounds.f)'
            STOP
         ENDIF
C--------Give warning if beginning of non-zero data is at the beginning
         IF(KBEG .EQ. 1) THEN
            WRITE(lu06,*) ' Warning 143: kbeg = 1 (bounds.f)'
         ENDIF
C--------Give warning if end of non-zero data is at the end and set KEND
         IF(KEND .EQ. 0) THEN
            KEND = IFREQ
            WRITE(lu06,*) 
     $           ' Warning 144: kend = ifreq (bounds.f)'
         ENDIF
      ENDIF
      RETURN
      END ! BOUNDS

