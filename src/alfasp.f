
      SUBROUTINE ALFASP
C***********************************************************************
C... Prompt the user for the desired angles of attack ALFA(IALF)
C
C    Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      LOGICAL LRESUME
      LRESUME = FF
      IALF = 0
 100  CONTINUE
         WRITE(lu06,*) 
     &   '  V-dist menu : [1] list design ALFAS(.)'
         WRITE(lu06,*) 
     &   '                [2] enter angles-of-attack'
         WRITE(lu06,*) 
     &   '                [3] pass'
         READ (lu05,*) IMENU
         IF(IMENU .EQ. 1) THEN
            WRITE(lu06,*) '   JSEG    ALFAS(.)'
            DO 110 JSEG = 1, ISEG
               WRITE(lu06,1001) JSEG, ALFAS(JSEG)
 1001          FORMAT(1X,I5,2X,F12.4)
 110        CONTINUE
         ELSEIF(IMENU .EQ. 2) THEN
            WRITE(lu06,*) '  Enter number'
            READ(lu05,*) IALF
            IF(IALF .NE. 0) THEN
               WRITE(lu06,*) 
     &          '  Enter the angles of attack relative to the zll:'
               READ(lu05,*) (ALFA(I), I = 1, IALF)
            ENDIF
            LRESUME = TT
         ELSEIF(IMENU .EQ. 3) THEN
            LRESUME = TT
         ELSE
            WRITE(lu06,*) '  This number not an option. Try again.'
         ENDIF
      IF(.NOT. LRESUME) GOTO 100  
      RETURN
      END ! ALFASP

