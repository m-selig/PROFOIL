
      SUBROUTINE SETPER
C***********************************************************************
C...  Adds a small perturbation to the variable ITP1(JVAR1) ITP2(JVAR1)
C
C     Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER JVAR1
      DO 100 JVAR1 = 1, IVAR1
        IF (ITP1(JVAR1) .EQ. 0) THEN
C...  misc single parameters  ITP1 = O
          IF (ITP2(JVAR1) .EQ. 1) THEN
C...  DELSX
              DELTAP(JVAR1) = 0.000001
          ELSEIF (ITP2(JVAR1) .EQ. 2) THEN
C...  DELSY
              DELTAP(JVAR1) = 0.000001
            ELSEIF (ITP2(JVAR1) .EQ. 3) THEN
C...  XMAPOFF
              DELTAP(JVAR1) = 0.000001
          ELSEIF (ITP2(JVAR1) .EQ. 4) THEN
C...  YMAPOFF
              DELTAP(JVAR1) = 0.000001
          ELSE
            WRITE(LU06,*) 'Error 206: ICASE not found (setper.f)'
          ENDIF
        ELSEIF (ITP1(JVAR1) .EQ. 1) THEN
C...  phi  ITP1 = 1
          IF (ITP2(JVAR1) .LT. ISEG) THEN
            DELTAP(JVAR1) = SIGNPER * 0.0001 
          ELSEIF (ITP2(JVAR1) .LT. 1000) THEN
            ICASE = ITP2(JVAR1)/100
            IF (ICASE .EQ. 1 .OR. ICASE .EQ. 2 .OR. ICASE
     $           .EQ. 3 .OR. ICASE .EQ. 4  .OR. ICASE .EQ. 8) THEN 
              DELTAP(JVAR1) = SIGNPER * 0.0001
            ELSE
              WRITE(LU06,*) 'Error 207: ICASE not found (setper.f)'
            ENDIF
          ELSE
            WRITE(LU06,*) 'Error 208: ICASE not found (setper.f)'
          ENDIF
        ELSEIF (ITP1(JVAR1) .EQ. 2) THEN
C...  phiw  ITP1 = 2
          ICASE = ITP2(JVAR1)/100
          IF (ICASE .EQ. 1 .OR. ICASE .EQ. 3 .OR. ICASE .EQ. 4) THEN 
            DELTAP(JVAR1) = SIGNPER * 0.001 
          ELSEIF (ICASE .EQ. 2) THEN
            DELTAP(JVAR1) = SIGNPER * 0.001 
          ELSE
            WRITE(LU06,*) 'Error 209: ICASE not found (setper.f)'
          ENDIF
        ELSEIF (ITP1(JVAR1) .EQ. 3) THEN
C...  phis  ITP1 = 3
          ICASE = ITP2(JVAR1)/100
          IF (ICASE .EQ. 1 .OR. ICASE .EQ. 3 .OR. ICASE .EQ. 4) THEN 
            DELTAP(JVAR1) = SIGNPER * 0.001 
          ELSEIF (ICASE .EQ. 2) THEN
            DELTAP(JVAR1) = SIGNPER * 0.001 
          ELSE
            WRITE(LU06,*) 'Error 210: ICASE not found (setper.f)'
          ENDIF
        ELSEIF (ITP1(JVAR1) .EQ. 4) THEN
C...  v*  ITP1 = 4
          IVEL = 1
          DELTAP(JVAR1) = 0.0001 
        ELSEIF (ITP1(JVAR1) .EQ. 5) THEN
C...  v tilde  ITP1 = 5
          DELTAP(JVAR1) = 0.0001
        ELSEIF (ITP1(JVAR1) .EQ. 6) THEN
C...  alpha*  ITP1 = 6
          IF (ITP2(JVAR1) .LE. ISEG) THEN
            DELTAP(JVAR1) = 0.001 
          ELSEIF (ITP2(JVAR1) .LT. 1000) THEN
            ICASE = ITP2(JVAR1)/100
            IF (ICASE .EQ. 1 .OR. ICASE .EQ. 3 .OR. ICASE .EQ. 4) THEN 
              DELTAP(JVAR1) = 0.001 
            ELSEIF (ICASE .EQ. 2) THEN
              DELTAP(JVAR1) = 0.001 
            ELSE
              WRITE(LU06,*) 'Error 211: ICASE not found (setper.f)'
            ENDIF
          ELSE
            WRITE(LU06,*) 'Error 212: ICASE not found (setper.f)'
          ENDIF
          IF (DELTAP(JVAR1) .EQ. 0.) DELTAP(JVAR1) = 0.05
        ELSEIF (ITP1(JVAR1) .EQ. 7) THEN
C...  K        ITP1 = 7
          ICASE = ITP2(JVAR1)/100
          IF (ICASE .EQ. 1 .OR. ICASE .EQ. 3 .OR. ICASE .EQ. 4) THEN 
            DELTAP(JVAR1) = SIGNPER * 0.001 
          ELSEIF (ICASE .EQ. 2) THEN
            DELTAP(JVAR1) = SIGNPER * 0.001 
          ELSE
            WRITE(LU06,*) 'Error 213: ICASE not found (setper.f)'
          ENDIF
        ELSE
          WRITE(LU06,*) 'Error 214: ICASE not found (setper.f)'
        ENDIF
 100  CONTINUE
      RETURN
      END ! SETPER

