
      SUBROUTINE PRELIMS
C***********************************************************************
C...Do some preliminary setup before starting Newton iteration
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      IF(IEQU1 .GT. 0) THEN
         IVAR1 = IEQU1
      ENDIF
      IF(IEQU2 .GT. 0) THEN
         IVAR2  = IEQU2
      ENDIF
      IEQU = IEQU1 + IEQU2
      IVAR = IEQU
      RETURN
      END ! PRELIMS

