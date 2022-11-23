
      SUBROUTINE SWRITE(JMODE1, JMODE2)
C***********************************************************************
C...JMODE1 = 1  (JMODE2 not used)
C   JMODE1 = 2  Single parameter Rx
C               JMODE2 = 0 initial solution with residues
C               JMODE2 = 1 solution with perturbations
C   JMODE1 = 3  Segment or ramp distribution
C               JMODE2 = 0 initial solution with residues
C               JMODE2 = 1 solution with perturbations in tilde v's
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER JMODE1, JMODE2
      IF(JMODE1 .EQ. 1) THEN
         WRITE(lu06,*) '  Total airfoil design:'
         WRITE(lu06,1001) UMU(1), UMU(2), HKH(1), HKH(2), OMEGA(1), 
     &                 OMEGA(2), SKS
 1001    FORMAT(2X,7X,'UMU(1)',7X,'UMU(2)',3X,'HKH(1)',3X,'HKH(2)',
     &           1X,'OMEGA(1)',1X,'OMEGA(2)',6X,'SKS'
     &          /2X,2F13.3,5F9.3)
C--------WRITE(lu06,1002) OMEGP(1), OMEGP(2),  OMEGT(1),  OMEGT(2)
C1002    FORMAT(2X,1X,'OMEGP(1)',1X,'OMEGP(2)',1X,'OMEGT(1)',
C----&           1X,'OMEGT(2)'/2X,7F9.3)
      ELSEIF(JMODE1 .EQ. 2) THEN
         WRITE(lu06,*) '  Difference in the design variables:'
         IF(JMODE2 .EQ. 0) THEN
            DO 201 JEQU1 = 1, IEQU1
            write(lu06,2001) jequ1, fnt1_0(jequ1), svalue1(jequ1)
 2001       format(2x,'   fnt1_0(',i2,') = ',f10.5, 1x,
     $           ' value1() = ',f10.5)
  201       CONTINUE
         ELSE
            DO 202 JEQU1 = 1, IEQU1
            if (abs(clamp1(jequ1)) .lt. tiny) then
              write(lu06,2002) jequ1, fnt1_1(jequ1),
     $             svalue1(jequ1), deltas(jequ1)
               ELSE
              write(lu06,2003) jequ1, fnt1_1(jequ1), 
     $             svalue1(jequ1), deltas(jequ1),
     $             clamp1(jequ1)
               ENDIF
 2002       format(2x,'   fnt1_1(',i2,') = ',f10.5,1x,
     $           ' value1() = ', f10.5,1x,
     $           ' deltas() = ',f8.5)
 2003       format(2x,'   fnt1_1(',i2,') = ',f10.5,1x,
     $           ' value1() = ', f10.5,1x,
     $           ' deltas() = ',f8.5,1x,' clamp1() =',f6.3)
  202       CONTINUE
         ENDIF
      ELSEIF(JMODE1 .EQ. 3) THEN
         WRITE(lu06,*) '  Residue at Newton nodes:' 
         IF (IEQU2 .EQ. 0) THEN
            WRITE(lu06,*) 
     $          'Possible error 220: IEQU2 = 0, no Newton nodes',
     $          ' (swrite.f)'
         ENDIF
         IF(JMODE2 .EQ. 0) THEN
            DO 301 JEQU2 = 1, IEQU2
               WRITE(lu06,3001) JEQU2, FNT2_0(JEQU2)
 3001          FORMAT(2X,' FNT2_0(',I2,') = ',F13.8)
  301       CONTINUE
         ELSE
            DO 302 JEQU2 = 1, IEQU2
               IF (.NOT. LCLAMP2) THEN
                  WRITE(lu06,3002) JEQU2, FNT2_1(JEQU2), 
     &                  DELTAVS(JEQU2)
               ELSE
                  WRITE(lu06,3003) JEQU2, FNT2_1(JEQU2), 
     &                  DELTAVS(JEQU2), CLAMPVS
               ENDIF
 3002          FORMAT(2X,' FNT2_1(',I2,') = ',F13.8,3X,
     &                ' DELTAVS() = ',F13.8)
 3003          FORMAT(2X,' FNT2_1(',I2,') = ',F13.8,3X,
     &                ' DELTAVS() = ',F13.8,3X,' CLAMPVS  = ',F6.3)
  302       CONTINUE
         ENDIF
      ELSE    
      ENDIF
      RETURN
      END ! SWRITE

