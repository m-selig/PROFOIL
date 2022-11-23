
      SUBROUTINE FNT1(JMODE)
C***********************************************************************
C...Evaluate the Newton functions
C   JMODE = -1 --> FNT1_P(.)
C   JMODE =  0 --> FNT1_0(.)
C   JMODE =  1 --> FNT1_1(.)
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER JMODE
      DO 100 JEQU1 = 1, IEQU1
         IF     (IFTP1(JEQU1) .GE. 100 .AND. IFTP1(JEQU1) .LE. 199) THEN 
            CALL NT1G0(JEQU1)
         ELSEIF (IFTP1(JEQU1) .GE. 200 .AND. IFTP1(JEQU1) .LE. 299) THEN 
            CALL NT1G1(JEQU1)
         ELSEIF (IFTP1(JEQU1) .GE. 400 .AND. IFTP1(JEQU1) .LE. 499) THEN 
            CALL NT1S0(JEQU1)
         ELSEIF (IFTP1(JEQU1) .GE. 500 .AND. IFTP1(JEQU1) .LE. 599) THEN 
            CALL NT1S1(JEQU1)
         ELSEIF (IFTP1(JEQU1) .GE. 600 .AND. IFTP1(JEQU1) .LE. 699) THEN 
            CALL NT1S2(JEQU1)
         ENDIF
         SVALUE1(JEQU1) = VALUE
         IF(JMODE .EQ. -1) THEN
            FNT1_P(JEQU1) = FNT1_X(JEQU1)
         ELSEIF(JMODE .EQ. 0) THEN
            FNT1_0(JEQU1) = FNT1_X(JEQU1)
         ELSE
            FNT1_1(JEQU1) = FNT1_X(JEQU1)
         ENDIF
  100 CONTINUE
      RETURN
      END ! FNT1

