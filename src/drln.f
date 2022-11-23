
      SUBROUTINE DRLN
C***********************************************************************
C...Calculate n development according to Drela's envelope method.
C...Variables:
C   LCRITN = .TRUE. if past critical Re according to Drela method.
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      LOGICAL LCRITN
C-----SET FLAG, flow is not critical initially
      LCRITN = FF
      DRLNBL(1) = 0.
      DO 100 JBL = 2, IBL
C--------check to see if above critical RTHETO
         IF(.NOT. LCRITN) THEN
            IF (RD2BL(JBL) .GT. RTHETO(H12BL(JBL))) THEN
C--------------Euler first step
               LCRITN = TT
               DRLNBL(JBL) = DRLNBL(JBL-1) 
     &                   + 0.5 * (0.0 + DN_DXI(H12BL(JBL),RD2BL(JBL))) 
     &                   * (SBL(JBL) - SBL(JBL-1))
            ELSE 
               DRLNBL(JBL) = DRLNBL(JBL-1) + 0.
            ENDIF
         ELSE
C-----------trapezoidal rule
            DRLNBL(JBL) = DRLNBL(JBL-1) 
     &                + 0.5 * (  DN_DXI(H12BL(JBL-1),D2BL(JBL-1))
     &                         + DN_DXI(H12BL(JBL),  D2BL(JBL)))
     &                * (SBL(JBL) - SBL(JBL-1))
         ENDIF
100   CONTINUE         
      RETURN
      END ! DRLN

