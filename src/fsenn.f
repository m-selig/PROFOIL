
      SUBROUTINE FSENN
C***********************************************************************
C...Calculate the n-development.
C...Requires:
C      SBL(IBL)         boundary layer arc length
C      VBL(IBL)         boundary layer edge velocity
C      IBL              number of boundary layer points
C      RINF             freestream Reynolds number
C      and H12BL(.), RD2BL(.), D2BL(.)
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      COMMON/BUBFLAG/IBUBBLE,IDUMMY(6)
      IBUBBLE = 0
      DO 100 JBL = 2, IBL
C--------The very first point is the stagnation point.
         SF   = SBL(JBL)
         UF   = VBL(JBL)
         H12F = H12BL(JBL)
         D2F  = D2BL(JBL)
         RD2F = RD2BL(JBL)
         DS   = SBL(JBL) - SBL(JBL-1)
C---commented out.  Full e^n not on github.
C         CALL ENN(H12F,D2F,RD2F,UF,0.,0.,SF,DS,JBL)
C--
         DO 110 JFREQ = 1, IFREQ
            AMPF(JFREQ,JBL) = EN(JFREQ)
            XAM(JBL,JFREQ) = SF
            XAM(JBL-1,JFREQ) = SF - DS0(JFREQ)
 110     CONTINUE
 100  CONTINUE
      RETURN
      END ! FSENN

