
      SUBROUTINE DEPEND
C***********************************************************************
C...Compute parameters that are [depend]ent on the airfoil design and that 
C   are needed to evaluate the Newton functions and/or needed for 
C   screen display.
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      CALL BASICP
      IF(LARGHP)  CALL ARGHP
      IF(LHP)     CALL PHARM
      IF(LHQ)     CALL QHARM
      IF(LCOORD)  CALL COORD
      IF(LNORM)   CALL NORM
      IF(LSLENS)  CALL ARCSLEN
      IF(LTHICKE) CALL THICKE(XCRD,YCRD,IARGP+1,THKMAXE,XCMAXE,YTHK)
      IF(LTHICK)  CALL THICK
      IF(LMOMENT) CALL FSA2B2
      IF(LMOMENT) CALL MOMENT(0.D0,CM)
      IF(LGENSYM) CALL GENSYM
      RETURN
      END ! DEPEND

