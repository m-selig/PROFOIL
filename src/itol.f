
      SUBROUTINE ITOL(ITMPL,LGCL)
C***********************************************************************
C....convert [i]nteger [to] [l]ogical
C    1 = TT; 0 = FF
C
C    Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      LOGICAL LGCL
      INTEGER ITMPL
      IF(ITMPL .EQ. 1) THEN
         LGCL = .TRUE.
      ELSE
         LGCL = .FALSE.
      ENDIF
      RETURN
      END ! ITOL

