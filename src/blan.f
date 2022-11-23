
      SUBROUTINE BLAN
C***********************************************************************
C...Select BL analysis
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      JSEG = JSEGAN
      LBOS = LBOSAN
C-----conditions:
      ALFABL = ALFAAN
      RINF   = RINFAN
C-----setting up to run the BL analysis
      CALL PACKER1(JSEG)
C-----check to see if transition is fixed for current bl analysis.
C     ..If so use JSEGTR, LBOSTR, and SSTAG (+) to get SBLTR and VBLTR
      IF (LFXTR) CALL SVBLTR
C-----do basic boundary layer analysis 
      CALL PROBL
C-----do n_ENN calculation if desired
      IF(LENN) THEN
         CALL INITENN
         CALL FSENN
      ENDIF
C-----do n_Drela calculation if desired
      IF(LDRLN) THEN
         CALL DRLN
      ENDIF
C-----Print out selected results
      CALL BLDAT
      RETURN
      END ! BLAN

