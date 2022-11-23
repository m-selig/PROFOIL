C     
      SUBROUTINE SETLSTP
C***********************************************************************
C...  Determine whether or not to step iteration ---> LSTEP
C...  LRET =  .TT. when selected option does not exist
C...  ISTEPTO  iterate until ITER is equal to ISTEPTO
C
C     Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      LOGICAL LRET
      LRET = TT
      IF(LMANUAL) THEN
 1000    CONTINUE
         IF(.NOT.LCONSEC) THEN
C---  manual iteration mode
            WRITE(LU06,*) 'Select option:'
            WRITE(LU06,*) '  '
            WRITE(LU06,*) '   0  Stop iteration for current stage'
            WRITE(LU06,*) '   #  Number of consecutive iterations'
            WRITE(LU06,*) '  -#  skip all further newton iteration'
            WRITE(LU06,*) ' 999  To stop execution'
            WRITE(LU06,*) '  99  More options'
            WRITE(LU06,*) ' '
            READ(LU05,*) IVALUE
            IF(IVALUE .EQ. 999) THEN
               STOP
            ELSEIF(IVALUE .LT. 0) THEN
               LRET = FF
               LSTEP = FF
               LSKIP = TT
            ELSEIF(IVALUE .EQ. 0) THEN
               LRET = FF
               LSTEP = FF
            ELSEIF(IVALUE .EQ. 1) THEN
               LRET = FF
               LSTEP = TT
            ELSEIF(IVALUE .GT. 1 .AND. IVALUE .NE. 99) THEN
               LRET = FF
               WRITE(LU06,*) ' '
               WRITE(LU06,*) '      In consecutive iteration mode...'
               WRITE(LU06,*) ' '
               LCONSEC = TT
               ISTEPTO = ITER + IVALUE -1
               LSTEP = TT
            ELSEIF(IVALUE .EQ. 99) THEN
               LRET = TT
               CALL OPTIONS
            ELSE
               WRITE(LU06,*) ' ...try again'
               WRITE(LU06,*) '  '
            ENDIF
            IF(LRET) GOTO 1000
         ELSE
C---  consecutive iterations requested            
            IF(ITER .EQ. ISTEPTO) THEN
               LCONSEC = FF
            ENDIF
         ENDIF
      ELSE
C---  automatic iteration
         LSTEP = TT
         SQSUM = 0
         IF(ITER .EQ. 0) THEN
            DO JEQU1 = 1, IEQU1
               SQSUM =FNT1_0(JEQU1)**2 + SQSUM
            ENDDO
            DO JEQU2 = 1, IEQU2
               SQSUM =FNT2_0(JEQU2)**2 + SQSUM
            ENDDO
         ELSE
            DO JEQU1 = 1, IEQU1
               SQSUM =FNT1_1(JEQU1)**2 + SQSUM
            ENDDO
            DO JEQU2 = 1, IEQU2
               SQSUM =FNT2_1(JEQU2)**2 + SQSUM
            ENDDO
         ENDIF
         TOL = SQRT(SQSUM/FLOAT(IEQU1 + IEQU2))
         WRITE(lu06,999) TOL, ITER
 999     FORMAT(2X, ' RMS error  = ',F13.8,'    Iteration = ',I4)
         IF(TOL .LT. TOLSPEC .OR. ITER .GT. ITERMAX) THEN
            LSTEP = FF
         ENDIF
      ENDIF
      RETURN
      END ! SETLSTP

