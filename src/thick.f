
      SUBROUTINE THICK
C***********************************************************************
C...Determine the airfoil thickness
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
C-----set initial us and ls indices
      JUS = IMAX - 1
      JLS = IMAX + 1
      THKMAX = 0.
      CMBMAX = 0.
   20 CONTINUE
      IF (XCRD(JUS) .LT. XCRD(JLS)) THEN
C-------interpolating for a lower surface point
        YLS = (YCRD(JLS) - YCRD(JLS-1))/(XCRD(JLS) - XCRD(JLS-1)) *
     &        (XCRD(JUS) - XCRD(JLS-1)) +            YCRD(JLS-1)
        THK = YCRD(JUS) - YLS
        CMB = YCRD(JUS) - 0.5 * THK
        IF (CMB .GT. CMBMAX) THEN
           CMBMAX = CMB
        ENDIF
        IF (THK .GT. THKMAX) THEN
           THKMAX = THK
           XCMAX = XCRD(JUS)
        ENDIF
        JUS = JUS - 1
        IF (JUS .EQ. 1) GO TO 10
      ELSE
C-------interpolating for an upper surface point
        YUS = (YCRD(JUS) - YCRD(JUS+1))/(XCRD(JUS) - XCRD(JUS+1)) *
     &        (XCRD(JLS) - XCRD(JUS+1)) +            YCRD(JUS+1)
        THK = YUS - YCRD(JLS) 
        CMB = YCRD(JLS) + 0.5 * THK
        IF (CMB .GT. CMBMAX) THEN
           CMBMAX = CMB
        ENDIF
        IF (THK .GT. THKMAX) THEN
           THKMAX = THK
           XCMAX = XCRD(JLS)
        ENDIF
        JLS = JLS + 1
        IF (JLS .EQ. IARGP) GO TO 10 
      ENDIF
      GO TO 20
   10 CONTINUE
      RETURN
      END ! THICK

