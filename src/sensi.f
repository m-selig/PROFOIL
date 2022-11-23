
      SUBROUTINE SENSI
C***********************************************************************
C...  Save the Jacobian before calling SYSNEWT which replaces CIJ(..)
C
C     Copyright (c) 1995 Ashok Gopalarathnam
C     Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      REAL*8 CIJDIFF, CIJDIFF1, EPS
      CIJDIFF = 0.0
      EPS = 1.0D-6
      DO 100 JEQU = 1, IEQU
         DO 200 JVAR = 1, IVAR
            IF ( DABS(CIJ(JEQU,JVAR)).LT.EPS ) THEN
               CIJDIFF1 = 0.0
            ELSE
               CIJDIFF1 = (CIJ(JEQU,JVAR) - CIJ_SV(JEQU,JVAR))
               CIJDIFF1 = DABS( CIJDIFF1/CIJ(JEQU,JVAR) )
            ENDIF
            CIJDIFF = MAX( CIJDIFF,CIJDIFF1 )
            CIJ_SV(JEQU, JVAR) = CIJ(JEQU,JVAR)
 200     CONTINUE
 100  CONTINUE
      IF (CIJDIFF.LT.0.01) THEN
         LJACOBI = FF
         WRITE(lu06,*) ' '
         WRITE(lu06,*) 
     &        '  Switching Off Sensitivity and CLAMPS for',
     &        ' current stage'
         WRITE(lu06,*) ' '
      ENDIF
      RETURN
      END ! SENSI

