
      SUBROUTINE CPSENSI
C***********************************************************************
C...  Copy the CIJ_SV onto CIJ
C
C     Copyright (c) 1995 Ashok Gopalarathnam 
C     Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      DO 100 JEQU = 1, IEQU
         DO 200 JVAR = 1, IVAR
            CIJ(JEQU, JVAR) = CIJ_SV(JEQU,JVAR)
 200     CONTINUE
 100  CONTINUE
      RETURN
      END ! CPSENSI

