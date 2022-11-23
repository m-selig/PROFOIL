
      SUBROUTINE DUMP
C***********************************************************************
C...Dump basic design data to file DUMP.DAT
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      WRITE(lu06,*)
      WRITE(lu06,*)'*************************'
      WRITE(lu06,*)'*** WRITING DUMP FILE ***'
      WRITE(lu06,*)'*** --> profoil.dmp   ***'
      WRITE(lu06,*)'*************************'
      WRITE(lu06,*)

      OPEN(UNIT = 21,FILE = FILE21,status = 'unknown')

      IF(LSYM) THEN
        ISEGT = ISEG/2
      ELSE
        ISEGT = ISEG
      ENDIF
      DO 100 JSEG = 1, ISEGT
         WRITE(21,1100) AM(JSEG)/SCLF, ALFAS(JSEG), JSEG
1100     FORMAT('FOIL',2X,F10.5,2X,F10.5,2X,I3)
 100  CONTINUE
      DO 500 JSEG = 1, ISEGT
         IF(LDELV(JSEG)) THEN
            JCASE = IDVTP(JSEG)
            IF (JCASE .LE. 3) THEN
               WRITE(21,1500) JCASE, JSEG, VTILDE(JSEG)
 1500          FORMAT('DELV',I3,2X,I3,2X,F10.5)
            ELSEIF (JCASE .GE. 11) THEN 
               JSPSEG = KSPSEG(JSEG) 
               ISBSEG = KSBSEG(JSEG) 
               WRITE(21,1550) JCASE, JSEG, ISBSEG
 1550          FORMAT('DELV',I3,2X,I3,4X,I3)
               DO 600 JSBSEG = 1, ISBSEG
                  WRITE(21,1600) WGHTPHI(JSPSEG,JSBSEG), 
     &                            SSDELV(JSPSEG,JSBSEG)
 1600             FORMAT(2X,F5.3,2X,F10.6)
  600          CONTINUE
            ENDIF
         ENDIF
  500 CONTINUE
      ANGLE = EPP * PI * RTOD
      WRITE(21,1200) AS(1)/SCLF, AS(2)/SCLF
1200  FORMAT('PHIS',2X,F10.5,2X,F10.5)
      IF(LFTE) THEN
         WRITE(21,1800) ANGLE, PHIEPP(1)/SCLF, PHIEPP(2)/SCLF
1800     FORMAT('FTE ',2X,F6.1,6X,F10.5,2X,F10.5)
      ENDIF
      WRITE(21,1300) AKA(1), AKA(2)
1300  FORMAT('REC ',2X,F10.5,2X,F10.5)
      WRITE(21,1400) IVEL, VS(IVEL)
1400  FORMAT('VLEV',3X,I3,8X,F10.5)
      WRITE(21,1700) ILE
1700  FORMAT('ILE ',3X,I3)
      CLOSE(21)
      RETURN
      END ! DUMP

