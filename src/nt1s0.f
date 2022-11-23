
      SUBROUTINE NT1S0(JEQU1)
C***********************************************************************
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER JEQU1
      JSEG = JSEGIX1(JEQU1)
      LBOS = LLBOS(JEQU1)
      IF(LBOS) THEN 
         PHI = AM(JSEG-1)
      ELSE
         PHI = AM(JSEG)
      ENDIF
C-----determine location of PHI between JPT-1 and JPT
      JPT = INT((PHI + DEL_PHI) / DEL_PHI) + 1
      IF (IFTP1(JEQU1) .EQ. 400) THEN 
C--------x/c is between JPT-1 and JPT
         VALUE = XCRD(JPT-1) + (XCRD(JPT) - XCRD(JPT-1))
     &                        /(APHI(JPT) - APHI(JPT-1))
     &                        *(PHI       - APHI(JPT-1))
      ELSEIF (IFTP1(JEQU1) .EQ. 401) THEN 
C--------s/c is between JPT-1 and JPT
         VALUE = SLENS(JPT-1) + (SLENS(JPT) - SLENS(JPT-1))
     &                         /(APHI(JPT)  -  APHI(JPT-1))
     &                         *(PHI        -  APHI(JPT-1))
      ELSEIF (IFTP1(JEQU1) .EQ. 402) THEN 
C--------y/c is between JPT-1 and JPT
         VALUE = YCRD(JPT-1) + (YCRD(JPT) - YCRD(JPT-1))
     &                        /(APHI(JPT) - APHI(JPT-1))
     &                        *(PHI       - APHI(JPT-1))
      ELSEIF (IFTP1(JEQU1) .EQ. 403) THEN 
C---  desired angle from trailing edge (see notes 7-22-94)
C---  y/c is between JPT-1 and JPT
         YVALUE = YCRD(JPT-1) + (YCRD(JPT) - YCRD(JPT-1))
     &                        /(APHI(JPT) - APHI(JPT-1))
     &                        *(PHI       - APHI(JPT-1))
         XVALUE = XCRD(JPT-1) + (XCRD(JPT) - XCRD(JPT-1))
     &                        /(APHI(JPT) - APHI(JPT-1))
     &                        *(PHI       - APHI(JPT-1))
C---     value of x/c from trailing edge (+) always
         XTE = 1 - XVALUE
         VALUE = RTOD * ATAN(YVALUE/XTE)
      ELSE
         WRITE(lu06,*) ' Error 175: Newton type not found (nt1s0.f)'
      ENDIF
      FNT1_X(JEQU1) = VALUE - FNEWT1(JEQU1)
      RETURN
      END ! NT1S0

