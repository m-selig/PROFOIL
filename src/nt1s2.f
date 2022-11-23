
      SUBROUTINE NT1S2(JEQU1)
C***********************************************************************
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER JEQU1
      IF  (IFTP1(JEQU1) .EQ. 600
     & .OR.IFTP1(JEQU1) .EQ. 601) THEN 
C--------H_12 for given Rinf and alfa
         JSEG = JSEGIX1(JEQU1)
         LBOS = LLBOS(JEQU1)
C--------fixed transition data
         LFXTR = LLFXTR1(JEQU1)
         IF(LFXTR) THEN
            JSEGTR = JSEGTR1(JEQU1)
            LBOSTR = LLBOSTR(JEQU1)
         ENDIF
C--------conditions:
         RINF   = COND1(JEQU1)
         ALFABL = COND2(JEQU1)
C--------setting up to run the BL analysis
         CALL PACKER1(JSEG)
C--------check to see if transition is fixed for current bl analysis.
C--------if so use JSEGTR, LBOSTR, and SSTAG (+) to get SBLTR and VBLTR
         IF (LFXTR) CALL SVBLTR
         CALL PROBL
         CALL NEWTBL(1,IFTP1(JEQU1))
         VALUE = BLNEWT
      ELSE
         WRITE(lu06,*) ' Error 177: Newton type not found (nt1s2.f)'
      ENDIF
      FNT1_X(JEQU1) = VALUE - FNEWT1(JEQU1)
      RETURN
      END ! NT1S2

