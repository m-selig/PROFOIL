
      SUBROUTINE PROBL
C***********************************************************************
C...  Integral boundary layer subroutine for PROFOIL
C     
C...  See notes around 10-17-90
C     
C...  Includes variable step size to avoid inherent numerical instability
C     of the integral boundary layer equations.
C     
C...  Requires:
C     LFXTR    TT transition is fixed, preset from main calling routine
C     if(TT) need SBLTR and VBLTR (arc length and velocity at
C     transition location)
C     SBL(IBL) boundary layer arc length
C     VBL(IBL) boundary layer edge velocity
C     IBL      number of boundary layer points
C     RINF     freestream Reynolds number
C     
C...  Variable conventions:
C     Prefix:                                   Suffix:
C     DX*  Delta x for current interval         *I Initial
C     H32*                                      *M Middle
C     H12*                                      *F Final
C     RD2*
C     D2*  delta_2                              
C     DD2* Delta (delta_2)                      
C     D3*  delta_3
C     DD3* Delta (delta_3)
C     CF*    ( ... + c_f) term in momentum integral BL equations
C     CD*    ( ... + c_D) term in energy   ---------`'----------
C     
C...  List of required functions:
C     FH12(H32)               H12
C     FCF(H12,RD2)            skin friction cf
C     FCDDISS(H12,H32,RD2)        dissipation function cD
C     FU_X(XI,UI,XF,UF)       du/dx for interval
C     FRD2(RINF,U,D2)         Reynolds number based on delta2 and Rinf
C     FD2_X(H12,D2,U_X,U,CF)  d(delta2)/dx
C     FD3_X(D3,U_X,U,CD)      d(delta3)/dx
C...  List of local logicals:
C     LHV     TT halve the interval
C     LTR     TT transition has happened, used turbulent BL corrltns
C     LTRII   TT transition on the current interval JBL to JBL+1
C
C     Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      LOGICAL LHV, LTR, LTRII
      LTR   = FF
      LTRII = FF
C...  If LSACBL = TT, then scale the v-dist past the transition point.
C     See PROFOIL notes 11-6-96, p. 1
      IF(LSCALEV) CALL SCALEV
C...  Boundary layer stagnation point solution
C...  Return with JBLST as initial point to start integral BL calc.
      CALL STAGBL(JBLST)
      DO 100 JBL = JBLST, IBL-1
        JSTP = 0
        ISTP = 1
        NHV  = 0
        LHV  = FF
C...  [I]nitial conditions
        H32I = H32BL(JBL)
        H12I = H12BL(JBL)
        D2I  = D2BL(JBL)
        D3I  = D3BL(JBL)
        CFI  = CFBL(JBL)
        CDI  = CDBL(JBL)
        UI   = VBL(JBL)
C...  Step size and velocity gradient
        IF (LFXTR) THEN
C...  transition is fixed at some point.
C     check for transition at JBL or inside interval.
          IF (DABS(SBL(JBL) - SBLTR) .LT. 0.00001) THEN
C...  transition at JBL
            LTR = TT
            DXF  = SBL(JBL+1) - SBL(JBL)
            U_X  = FU_X(SBL(JBL),VBL(JBL),SBL(JBL+1),VBL(JBL+1))
          ELSEIF ((SBL(JBL)   + 0.00005) .LT. SBLTR .AND.
     &           (SBL(JBL+1) - 0.00005) .GT. SBLTR) THEN
C...  transition inside of interval JBL to JBL+1
            LTRII = TT
            DXF  = SBLTR - SBL(JBL)
            U_X  = FU_X(SBL(JBL),VBL(JBL),SBLTR,VBLTR)
          ELSE
C...  boundary layer is laminar on entire interval
            DXF  = SBL(JBL+1) - SBL(JBL)
            U_X  = FU_X(SBL(JBL),VBL(JBL),SBL(JBL+1),VBL(JBL+1))
          ENDIF
        ELSE
C...  transition not fixed; laminar boundary layer on interval
          DXF  = SBL(JBL+1) - SBL(JBL)
          U_X  = FU_X(SBL(JBL),VBL(JBL),SBL(JBL+1),VBL(JBL+1))
        ENDIF
 200    CONTINUE
        IF (LHV) THEN
C...  A test failed ---> halve step size
C     ---> start over at the initial point
C     ---> determine no of pts to end of interval
          NHV = NHV + 1
          DXF = DXF/2.
          ISTP = 2 * (ISTP - JSTP)
          JSTP = 0
        ENDIF
        LHV = TT
C...  [M]iddle conditions
        DXM  = DXF/2.
        DD2M = FD2_X(H12I, D2I, U_X, UI, CFI) * DXM
        D2M  = D2I + DD2M
        IF (D2M  .LE. 0.      .AND. NHV .LT. 10) GOTO 200
        DD3M = FD3_X(D3I, U_X, UI, CDI) * DXM
        D3M  = D3I + DD3M
        H32M = D3M / D2M
        IF(H32M .LT. 1.46) H32M = 1.46
        IF (H32M .GE. 2.      .AND. NHV .LT. 10) GOTO 200
        UM   = UI + DXM * U_X
        RD2M = FRD2(RINF, UM, D2M)
        IF (LTR) THEN
          H12M = FTH12(H32M)
          CFM  = FTCF(H12M, RD2M)
          CDM  = FTCD(H12M, RD2M)
        ELSE
          H12M = FH12(H32M)
          CFM  = FCF(H12M, RD2M)
          CDM  = FCDDISS(H12M, H32M, RD2M)
        ENDIF
C...  [F]inal conditions
        DD2F = FD2_X(H12M, D2M, U_X, UM, CFM) * DXF
        DD3F = FD3_X(D3M, U_X, UM, CDM) * DXF
        D2F  = D2I + DD2F
        IF (D2F .LT. 0.       .AND. NHV .LT. 10) GOTO 200
        D3F  = D3I + DD3F
        H32F = D3F / D2F
        IF(H32F .LT. 1.46) H32F = 1.46
        IF (H32F .GE. 2.      .AND. NHV .LT. 10) GOTO 200
        TEST1 = DABS(H32F-H32I)
        IF (TEST1 .GT. 0.01   .AND. NHV .LT. 10) GOTO 200
        TEST2 = DABS(H32I-2*H32M+H32F) 
        IF (TEST2 .GT. 0.0001 .AND. NHV .LT. 10) GOTO 200
C...  successful step: passed all stability tests (or NHV .GE. 10)
        UF   = UI + DXF * U_X
        RD2F = FRD2(RINF,UF,D2F)
        IF (LTR) THEN
          H12F = FTH12(H32F)
          CFF  = FTCF(H12F, RD2F)
          CDF  = FTCD(H12F, RD2F)
        ELSE
          H12F = FH12(H32F)
          CFF  = FCF(H12F,RD2F)
          CDF  = FCDDISS(H12F,H32F,RD2F)
        ENDIF
        LHV  = FF
        JSTP = JSTP + 1
        IF (JSTP .LT. ISTP) THEN
          H32I = H32F
          H12I = H12F
          D2I  = D2F
          D3I  = D3F
          CFI  = CFF
          CDI  = CDF
          UI   = UF
          GOTO 200
        ELSEIF (LTRII) THEN
          JSTP  = 0
          ISTP  = 1
          NHV   = 0
          LHV   = FF
          LFXTR = FF
          LTR   = TT
          LTRII = FF
C...  [I]nitial conditions for remainder of segment
          H32I  = H32F
          H12I  = H12F
          D2I   = D2F
          D3I   = D3F
          CFI   = CFF
          CDI   = CDF
          UI    = UF
          DXF   = SBL(JBL+1) - SBLTR
          U_X   = FU_X(SBLTR,VBLTR,SBL(JBL+1),VBL(JBL+1))
          GOTO 200
        ELSE
C...  then JSTP = ISTP; interval is finished, [F]inal conditions
          H32BL(JBL+1) = H32F
          H12BL(JBL+1) = H12F
          D2BL(JBL+1)  = D2F
          D3BL(JBL+1)  = D3F
          CFBL(JBL+1)  = CFF
          CDBL(JBL+1)  = CDF
          RD2BL(JBL+1) = RD2F
        ENDIF
 100  CONTINUE
      RETURN
      END ! PROBL

