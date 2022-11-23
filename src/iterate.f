
      SUBROUTINE ITERATE
C***********************************************************************
C...Perform iteration until convergence is reached.
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      CALL PREDES
      CALL CHECK(0)
      CALL CALCPHI
      CALL DESIGN
      IF(LECHO .AND. (ISTAGE .EQ. 1)) THEN 
         CALL ECHO(0)
      ELSEIF (LECHO) THEN
         CALL ECHO(1)
      ENDIF
      IF(LITON) THEN 
         WRITE(lu06,1002)
         WRITE(lu06,1000) ISTAGE
         WRITE(lu06,1002)
 1000    FORMAT(2X,' Initial airfoil design for stage:',I3)
 1002    FORMAT(2X,' ==========================================')
         CALL PRELIMS
         CALL NEWTSTP
         WRITE(lu06,*)
      ELSE
         CALL BASICP
           WRITE(lu06,1001)
 1001      FORMAT(2X,' Airfoil design without iteration.')
         CALL SWRITE(1,0)
      ENDIF 
      RETURN
      END ! ITERATE

