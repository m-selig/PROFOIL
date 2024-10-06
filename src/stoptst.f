      
      subroutine stoptst
c***********************************************************************
C...  Interactive stop prompt.
C
C     Copyright (c) 1990-2022 Michael Selig
c***********************************************************************
      include 'PROFOIL.INC'
      write(lu06,*) '  **************************************'
      
c...      write(lu06,*) '  Do you wish to stop (1 = yes; 0 = no)?'
c...      read(lu05,*) istop
c...      if(istop .eq. 1) stop
      
c...  Simply stop and let user read the error message
      stop
      return
      end ! stoptst

