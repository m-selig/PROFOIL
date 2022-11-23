
      SUBROUTINE AFAREA(IAREAMD)
C***********************************************************************
C...  Compute the airfoil area.
C     IAREAMD = 1  use designed airfoil
C     IAREAMD = 0  use airfoil from a file
C     See PROFOIL notes 11-14-96 p 1
C     Uses: CHORDO passed through PROFOIL.INC file.
C     CHORDO is the physical chord of the airfoil.
C
C     Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      CHARACTER*80 AFFILE
      LOGICAL LOUT
 1001 FORMAT(A)
      IF (IAREAMD .EQ. 0) THEN
C...  read in the airfoil coordinates
        READ(LU10,'(A)') LINE
        WRITE(TLINE, 1001) LINE(1:50)
        READ(TLINE,'(A)') AFFILE
        IN = 9
        OPEN(UNIT = IN, FILE = AFFILE, STATUS = 'OLD')
C...  skip the first line with the airfoil name
        DO JMO = 1, 1200
          READ (IN, *, END = 999) XCRD(JMO), YCRD(JMO)
        ENDDO
        WRITE(LU06,*) 
     $       'Error 140: Did not read in all of the points in afarea.f'
 999    CONTINUE
        IMO = JMO - 1
C...        WRITE(LU06,*) 'Number of points in afarea.f = ', IMO
        CLOSE (IN)
      ELSEIF (IAREAMD .EQ. 1) THEN
        IMO = IARGP + 1
      ENDIF
C...  put coords in temp array
      DO JPT = 1, IMO
        XTMP(JPT) = XCRD(JPT)
        YTMP(JPT) = YCRD(JPT)
      ENDDO
C...  Add in LE 0,0 point by first finding the location, then adding
C     in the point.
      JMO = 1
      LOUT = FF
 1030 CONTINUE
      IF(YTMP(JMO) .LT. 0.0) THEN
        ILEPT = JMO
        LOUT  = TT
      ENDIF
      JMO = JMO + 1
      IF(.NOT. LOUT) GO TO 1030
C...  add in the point
      DO JMO = IMO, ILE, -1
        XTMP(JMO+1) = XTMP(JMO)
        YTMP(JMO+1) = YTMP(JMO)
      ENDDO
      XTMP(ILEPT) = 0.0
      YTMP(ILEPT) = 0.0
C...  increase the number of points by one
      IMO = IMO + 1
C...  scale the points
      DO JMO = 1, IMO
        XTMP(JMO) = CHORDO * XTMP(JMO)
        YTMP(JMO) = CHORDO * YTMP(JMO)
      ENDDO
C...  Get the area under the top surface using trapezodial integration
      AREAT = 0.0
      DO JMO = 1, ILEPT - 1
        DAREAT = 0.5 * (YTMP(JMO) + YTMP(JMO+1)) * 
     $       (XTMP(JMO) - XTMP(JMO + 1))
        AREAT = AREAT + DAREAT
      ENDDO
C...  Get the area under the bottom surface
      AREAB = 0.0
      DO JMO = ILEPT, IMO - 1
        DAREAB = 0.5 * (YTMP(JMO+1) + YTMP(JMO)) * 
     $       (XTMP(JMO + 1) - XTMP(JMO))
        AREAB = AREAB + DAREAB
      ENDDO
C...  Compute the area
      AREA = AREAT - AREAB
      IF(IAREAMD .EQ. 0) THEN
        WRITE(LU06,1045) AFFILE, AREA, CHORDO
 1045   FORMAT(2X,'Airfoil (', A12, ') area is', F15.8, 
     $       ' for a chord of ', F10.4)
      ENDIF
      RETURN
      END ! AFAREA

