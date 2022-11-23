
      SUBROUTINE DESIGN
C***********************************************************************
C...Main airfoil design routine
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
C-----certain preliminaries
      AW(1) = AM(1)
      AW(2) = AM(ISEG-1)
C-----if airfoil is entered with LSYM line, then create mirror image data
      IF(LSYM) CALL SYMTRY
c-----lines below were used for debugging in symtry mode (LSYM = TT)
c        LSYM = .NOT. LSYM
c        CALL DUMP
c        LSYM = .NOT. LSYM
C-----solve for airfoil
      CALL COEF
      CALL SYSMUKH
      RETURN
      END ! DESIGN

