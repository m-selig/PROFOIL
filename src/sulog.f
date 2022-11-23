
      SUBROUTINE SULOG
C***********************************************************************
C...ULOG(.)
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      DO 100 JSEG = 2, ISEG-1
         IF(LDELV(JSEG)) THEN
            IF(IDVTP(JSEG) .GE. 11) THEN
               LWHERE = FF
               LEXTRA = FF
               JSPSEG = KSPSEG(JSEG)
               ISBSEG = KSBSEG(JSEG)
            ENDIF
            ULOG(JSEG) = UU(JSEG, AM(JSEG))
         ENDIF
  100 CONTINUE
      RETURN
      END ! SULOG

