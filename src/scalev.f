
      SUBROUTINE SCALEV
C***********************************************************************
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      DO JBL = 1, IBL
        IF(SBL(JBL) .GT. SBLTR) THEN
          VBL(JBL) = VBLTR + PROPO * (VBL(JBL) - VBLTR)
        ENDIF
      ENDDO
      END ! SCALEV

