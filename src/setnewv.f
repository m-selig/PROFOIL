
      SUBROUTINE SETNEWV(JVAR2,ISIGN1,ISIGN2)
C***********************************************************************
C...Set new v*(phi)
C   ISIGN1 = -1 --> DELTAVP
C   ISIGN1 =  1 --> DELTAVS
C   ISIGN2 = -1 --> subtract quantity
C   ISIGN2 =  1 --> add      quantity
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER JVAR2
      IF(ISIGN1 .LT. 0) THEN
         DELTAJ = DELTAVP(JVAR2)
      ELSE
         DELTAJ = DELTAVS(JVAR2)       
      ENDIF
      IF(ISIGN2 .LT. 0) THEN
         DELTAJ = -DELTAJ
      ELSE
         DELTAJ =  DELTAJ
      ENDIF
C-----determine the special segment and the subsegment indices
C     of the v*(phi)_jvar2 
      JSEG = JVARIDX(JVAR2,1)
      JSPSEG = KSPSEG(JSEG)
      JSBSEG = JVARIDX(JVAR2,2)
C-----add the delta
      SSDELV(JSPSEG,JSBSEG) = SSDELV(JSPSEG,JSBSEG) + DELTAJ     
      RETURN
      END ! SETNEWV

