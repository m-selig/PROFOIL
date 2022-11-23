C      
      SUBROUTINE OPTIONS
C***********************************************************************
C...  iteration options in manual mode
C...  LRET =  .TT. when selected option does not exist
C
C     Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      LOGICAL LRET
      LRET = FF
 10   CONTINUE
      WRITE(LU06,*) '     Select option:'
      WRITE(LU06,*) '  '
      WRITE(LU06,*) '       0  Return to top level'
      WRITE(LU06,*) '       1  Halve  clamp sizes'
      WRITE(LU06,*) '       2  Double clamp sizes'
      WRITE(LU06,*) '  '
      READ(LU05,*) ICASE
      IF (ICASE .EQ. 0) THEN
         LRET = TT
      ELSEIF(ICASE .EQ. 1) THEN
         DO JEQU1 = 1, IEQU1
            CLAMP1(JEQU1) = 0.5 * CLAMP1(JEQU1)
         ENDDO
         DO JADJS = 1, IADJS
            CLAMPVS = 0.5 * CLAMPVS 
         ENDDO
      ELSEIF(ICASE .EQ. 2) THEN
         DO JEQU1 = 1, IEQU1
            CLAMP1(JEQU1) = 2 * CLAMP1(JEQU1)
         ENDDO
         DO JADJS = 1, IADJS
            CLAMPVS = 2 * CLAMPVS 
         ENDDO
      ELSE
         LRET = FF
         WRITE(LU06,*) ' ...try again'
         WRITE(LU06,*) '  '
      ENDIF
      IF(.NOT. LRET) GOTO 10
      RETURN
      END ! OPTIONS

