
      SUBROUTINE PHARM
C***********************************************************************
C...Compute the [harm]onic function [P] from ARGP(.)
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      HPILE   = -DLOG(ARGPILE)
      DO 200 JPT = 1, IARGP+1
         HP(JPT) = -DLOG(ARGP(JPT))
200   CONTINUE         
      RETURN
      END ! PHARM

