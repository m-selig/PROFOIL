
      SUBROUTINE ECHO(ICASE)
C***********************************************************************
C...Print out the current design values
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER ICASE
C...ICASE   0  first iteration
C           1  solution from first perturbation: ITER = 0
C           2  solution from next iterations:    ITER > 1
      
      IF (ICASE .EQ. 0) THEN
         OPEN (UNIT=LU85,FILE=FILE85,STATUS='unknown')
         WRITE(LU85,1000)
 1000    FORMAT(//8X,'PROFOIL    Version 2.0   March 2022' //
     &            2X,'ITERATION 0'/)
      ELSE
         OPEN (UNIT=LU85,FILE=FILE85,STATUS='OLD',ACCESS='APPEND')
         IF (ICASE .EQ. 1) THEN
            WRITE(LU85,1001) ITER
 1001       FORMAT(20X,'perturbation solution: iteration = ',I3)
         ELSEIF (ICASE .EQ. 2) THEN
            WRITE(LU85,1002) ITER
 1002       FORMAT(2X,'NEXT SOLUTION: ITERATION = ',I3)
         ELSE 
            WRITE(LU85,*) 'Error 145: no icase (echo.f)'
         ENDIF
      ENDIF
      WRITE(LU85,100)
  100 FORMAT(2X,'JSEG',6X,'AM',1X,'ALFAS',
     &       4X,'VS',6X,'AKA',9X,'UMU',5X,'HKH',4X,'AW',4X,'AS',
     &       4X,'OMEGA')
  101 FORMAT(2X,I4,F8.1,F6.2,F6.2,F9.3,F12.3,F8.3,F6.1,F6.1,F9.2)
      JSEG = 0
      WRITE(LU85,101) JSEG, AM0
      JSEG = 1
      WRITE(LU85,101) JSEG,AM(1),ALFAS(1),VS(1), AKA(1), UMU(1), HKH(1),
     &                    AW(1), AS(1), OMEGA(1)
      DO 200 I = 2, ISEG-2
         WRITE(LU85,101) I, AM(I), ALFAS(I), VS(I)
  200 CONTINUE
      WRITE(LU85,101) ISEG-1, AM(ISEG-1), ALFAS(ISEG-1), VS(ISEG-1),
     &              AKA(2),UMU(2),HKH(2),AW(2),AS(2), OMEGA(2)
      WRITE(LU85,101) ISEG, AM(ISEG), ALFAS(ISEG), VS(ISEG)
      CLOSE(LU85)

      RETURN
      END ! ECHO

