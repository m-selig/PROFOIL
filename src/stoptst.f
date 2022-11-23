      
      subroutine stoptst
c***********************************************************************
C...  Interactive stop prompt.
C
C     Copyright (c) 1990-2022 Michael Selig
c***********************************************************************
      include 'PROFOIL.INC'
      write(lu06,*) '  **************************************'
      write(lu06,*) '  Do you wish to stop (1 = yes; 0 = no)?'
      read(lu05,*) istop
      if(istop .eq. 1) stop
      return
      end ! stoptst

