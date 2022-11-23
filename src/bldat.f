
      SUBROUTINE BLDAT
C***********************************************************************
C...Print out the BL data
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      LOGICAL LPRT11, LPRT12, LPRT13, LPRT14, LPRT15, LPRT16, LPRT17, 
     &        LPRT18, LPRT19
      LPRT11 = FF
      LPRT12 = FF
      LPRT13 = FF
      LPRT14 = FF
      LPRT15 = FF
      LPRT16 = FF
      LPRT17 = FF
      LPRT18 = FF
      LPRT19 = FF
      DO 100 JPRTBL = 1, IPRTBL
         IF(MPRTBL(JPRTBL) .EQ. 11) LPRT11 = TT
         IF(MPRTBL(JPRTBL) .EQ. 12) LPRT12 = TT
         IF(MPRTBL(JPRTBL) .EQ. 13) LPRT13 = TT
         IF(MPRTBL(JPRTBL) .EQ. 14) LPRT14 = TT
         IF(MPRTBL(JPRTBL) .EQ. 15) LPRT15 = TT
         IF(MPRTBL(JPRTBL) .EQ. 16) LPRT16 = TT
         IF(MPRTBL(JPRTBL) .EQ. 17) LPRT17 = TT
         IF(MPRTBL(JPRTBL) .EQ. 18) LPRT18 = TT
         IF(MPRTBL(JPRTBL) .EQ. 19) LPRT19 = TT
 100  CONTINUE
      WRITE(lu06,*) '******************************'
      IF(LPRT11) THEN
         WRITE(lu06,*) ' v(s)       --> FOR011.DAT '
         DO 11 JBL = 1, IBL
            WRITE(11,1000) SBL(JBL), VBL(JBL)
 11     CONTINUE
      ENDIF
      IF(LPRT12) THEN
         WRITE(lu06,*) ' h12(s)     --> FOR012.DAT '
         DO 12 JBL = 1, IBL
            WRITE(12,1000) SBL(JBL), H12BL(JBL)
 12     CONTINUE
      ENDIF
      IF(LPRT13) THEN
         WRITE(lu06,*) ' H32(s)      --> FOR013.DAT '
         DO 13 JBL = 1, IBL
            WRITE(13,1000) SBL(JBL), H32BL(JBL)
 13     CONTINUE
      ENDIF
      IF(LPRT14) THEN
         WRITE(lu06,*) ' cf (test data)  --> FOR014.DAT '
ccc         WRITE(14,*) IBL
         DO 14 JBL = 1, IBL
            WRITE(14,1000) SBL(JBL), CFBL(JBL)
 14     CONTINUE
      ENDIF
      IF(LPRT15) THEN
         WRITE(lu06,*) ' d2(s)      --> FOR015.DAT '
         DO 15 JBL = 1, IBL
            WRITE(15,1000) SBL(JBL), D2BL(JBL)
 15     CONTINUE
      ENDIF
      IF(LPRT16) THEN
         WRITE(lu06,*) ' d3(s)      --> FOR016.DAT '
         DO 16 JBL = 1, IBL
            WRITE(16,1000) SBL(JBL), D3BL(JBL)
 16     CONTINUE
      ENDIF
      IF(LPRT17) THEN
         WRITE(lu06,*) ' d1(s)      --> FOR017.DAT '
         DO 17 JBL = 1, IBL
            WRITE(17,1000) SBL(JBL), D2BL(JBL) * H12BL(JBL)
 17     CONTINUE
      ENDIF
      IF(LPRT18) THEN
         WRITE(lu06,*) ' n(s)_Drela --> FOR018.DAT '
         DO 18 JBL = 1, IBL
            WRITE(18,1000) SBL(JBL), DRLNBL(JBL)
 18     CONTINUE
      ENDIF
      IF(LPRT19) THEN
         WRITE(lu06,*) ' n(s)_ENN   --> FOR019.DAT '
         DO 190 JFREQ = 1, IFREQ
            DO 191 JBL = 2, IBL
               WRITE(19,1000) SBL(JBL), AMPF(JFREQ,JBL)
 191        CONTINUE
            WRITE(19,1000) SBL(IBL), AM0
 190     CONTINUE
      ENDIF
      WRITE(lu06,*) '******************************'
 1000 FORMAT(2X,6G16.7)
      CLOSE (11)
      CLOSE (12)
      CLOSE (13)
      CLOSE (14)
      CLOSE (15)
      CLOSE (16)
      CLOSE (17)
      CLOSE (18)
      CLOSE (19)
      RETURN
      END ! BLDAT

