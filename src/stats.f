
      SUBROUTINE STATS
C***********************************************************************
C...stats
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      WRITE(lu06,*) 
      IF (JFOIL .NE. 0) THEN
         WRITE(lu06,*) '***************************'
         WRITE(lu06,*) 'Finished case:', JFOIL 
         WRITE(lu06,*) '***************************'
      ENDIF
      WRITE(lu06,1001) IARGP, CHORD, -ALFA0, CM0, 100.*THKMAX,
     $     100.*THKMAXE,
     $     100.*CMBMAX,
     $     100.*XCMAXE,
     $     100.*XCMAX, SX, SY,
     $     FSB0, FSA1, FSB1
      WRITE(lu06,*)
 1001 FORMAT(2X,' **** STATISTICS ****'/
     $     2X,' IARGP  = ',  I10,/
     $     2X,' CHORD  = ',F10.3,/
     $     2X,' ALFA0  = ',F10.3,/
     $     2X,' CM0    = ',F10.4,/
     $     2X,' THKMAX = ',F9.2,'%',/
     $     2X,' THKMAXE= ',F9.2,'%',/
     $     2X,' CMBMAX = ',F9.2,'%',/
     $     2X,' XCMAX  = ',F9.2,'%',/
     $     2X,' XCMAXE = ',F9.2,'%',/
     $     2X,' SX     = ',F10.3,/
     $     2X,' SY     = ',F10.3,/
     $     2X,' B0     = ',F10.6,/
     $     2X,' A1     = ',F10.6,/
     $     2X,' B1     = ',F10.6,/
     $     2X,' ********************'/)
      IF (JFOIL .NE. 0) THEN
        WRITE(lu06,*) '*************************'
        WRITE(lu06,*) 'Case:', JFOIL 
        WRITE(lu06,*) '*************************'
      ENDIF
      IF (LTEZERO) THEN
        IF((DABS(SX) .GT. .2) .OR.
     $       (DABS(SY) .GT. .2)) THEN
C--------flash message notice to screen
          WRITE(lu06,*) '---> Check SX, SY'
        ENDIF
      ENDIF
      RETURN
      END ! STATS

