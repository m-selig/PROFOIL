
      SUBROUTINE DEFLT
C***********************************************************************
C...Set the [def]au[lt] values 
C   Note: nothing is based on the input data from unit 10.
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
C-----Initialize logicals, etc
      LMANUAL = TT
C---- reset/initialize Print logicals
      CALL RESET('PRINT__')
C---- reset/initialize Newton iteration
      CALL RESET('NEWT___')
C---- reset/initialize airfoil design
      CALL RESET('FOIL___')
C---- reset/initialize boundary layer analysis
      CALL RESET('BLA____')
C---- reset/initialize CLAMP values 
      CALL RESET('CLAMP1_')
      CALL RESET('CLAMP2_')
C---- small number, one and zero
      TINY = 0.0000002
      one  = 1d0
      zero = 0.d0
C---- for the e^n method, IBUBBLE = 0 is set in fsenn.f
      LDRLN = FF
      LENN  = FF
      NDRELA  = 0
      IFREQ   = 20
      NCOMP   = 0
      LNEGN   = FF
C---- by definition:
      AM0 = 0.
C---- initialize misc parameter:
      TOLSPEC = 0.001
      SCLF    = 1.0
      RELAX   = 1.0
      DELAN   = 0.1
      SIGNPER = 1.0
C---- default logicals:
      LSKIP   = FF
      LALFASP = FF
      LFXTR   = FF
c---- changed to FF for LECHO because I could not get the file
c     I/O to work right in echo.f
      LECHO   = FF
      LSENSI  = FF
      LADDALF = FF
      LSCALEV = FF
C---- do not skip unknown words
      LSKIPQ = FF
C---- Eppler's transition criterion default constants
      EM = 1.
      ER = 0.
C---- chord used to determine the area of the airfoil
      CHORDO = 1
C---- bulb length to determine the volume of an axisymmetric airfoil
C     revolved about the x-axis
      LENGTH_B = 1
C---- TE is closed
      LTEZERO = TT
      DELSX = 0.
      DELSY = 0.
C----initial values used to get the coordintes
      XMAPOFF = 0.
      YMAPOFF = 0.
C---- before anything happens set ILE = 0 as a check flag
      ILE = 0
C---- Do not write out the stagnation point velocity zero
C     if it falls on a coordinate point
      LVELF0 = FF
C---- Eppler's max. t/c calculation default logical
      LTHICKE = FF
C---- no trailing edge thickness wedge
      LWEDGE = FF
C---- flip airfoil over
      LFLIP = FF
C---- flapped airfoil geometry generation
      LGENSYM = FF
      LGENSYM_XF_YF  = FF
      LGENSYM_REPORT = FF

C---- trailing edge flap
      lteflap = ff

      RETURN
      END ! DEFLT     

