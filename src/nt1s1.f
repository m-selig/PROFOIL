
      SUBROUTINE NT1S1(JEQU1)
C***********************************************************************
C--------H_12     @ alfa^* for given Rinf
C--------R_delta2 @ alfa^* for given Rinf
C--------TC       @ alfa^* for given Rinf
C--------n        @ alfa^* for given Rinf
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER JEQU1
      IF (     IFTP1(JEQU1) .EQ. 500
     &    .OR. IFTP1(JEQU1) .EQ. 501 
     &    .OR. IFTP1(JEQU1) .EQ. 502 
     &    .OR. IFTP1(JEQU1) .EQ. 503 
     &    .OR. IFTP1(JEQU1) .EQ. 504
     &    .OR. IFTP1(JEQU1) .EQ. 505
     &    .OR. IFTP1(JEQU1) .EQ. 506) THEN 
         JSEG   = JSEGIX1(JEQU1)
         LBOS   = LLBOS(JEQU1)
C--------fixed transition data
         LFXTR = LLFXTR1(JEQU1)
         IF(LFXTR) THEN
            JSEGTR = JSEGTR1(JEQU1)
            LBOSTR = LLBOSTR(JEQU1)
         ENDIF
C--------conditions:
         ALFABL = ALFAS(JSEG)
         RINF   = COND1(JEQU1)
         CALL PACKER1(JSEG)
C--------check to see if transition is fixed for current bl analysis.
C--------if so use JSEGTR, LBOSTR, and SSTAG (+) to get SBLTR and VBLTR
         IF (LFXTR) CALL SVBLTR
         CALL PROBL
         CALL NEWTBL(1,IFTP1(JEQU1))
         VALUE = BLNEWT
      ELSE
         WRITE(lu06,*) ' Error 176: Newton type not found (nt1s1.f)'
      ENDIF
      FNT1_X(JEQU1) = VALUE - FNEWT1(JEQU1)
      RETURN
      END ! NT1S1

