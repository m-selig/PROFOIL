
      SUBROUTINE SPLIT(PHIK, AK) 
C***********************************************************************
C...Do the actual splitting off here.
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      REAL*8 PHIK, AK
      DO 100 JNU = 1, IARGP
         IF(DABS(APHI(JNU)-PHIK) .GT. 0.00001) THEN
            ARGSIN = 0.5 * (APHI(JNU) - PHIK) 
C-----------splitting off the corner discontinuity in slope at the LE and
C           adding conjugate part to Q
            SINARG = DSING(ARGSIN)
            HPSMO(JNU) = HPSMO(JNU) - AK * DABS(SINARG)
            HQ(JNU)    = HQ(JNU) - (2. * AK)/PI * SINARG
     &                   * DLOG(DABS(DTANG(0.5D0 * ARGSIN)))
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C--------------at this point HPSMO is the part to be subtracted from P(phi).
C              so it is the part with the corners.
CCC            WRITE(13,*) APHI(JNU), HPSMO(JNU)
CCC            WRITE(14,*) APHI(JNU), HQ(JNU)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
         ENDIF
  100 CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC      CLOSE (13)
CCC      CLOSE (14)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      RETURN
      END ! SPLIT

