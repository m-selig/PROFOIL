
      subroutine flapit
c***********************************************************************
c...  Flap airfoil according to flap mode
c...  Requires:
c     iflapmode (teflap line)
c     1 ... see profoil notes 020724 p6
c     xhinge (teflap line) - x/c location of flap
c     deltaflap (teflap line) - flap deflection, positive down
c
c     Copyright (c) 1995 Ashok Gopalarathnam (partly from pan2d code)
c     Copyright (c) 1990-2022 Michael Selig
c***********************************************************************
      include 'PROFOIL.INC'
c...  have xhinge (from input)
      xpivot = xhinge
c...  get yhtop, yhbot
      call tatx2
      if (deltaflap .ge. 0) then
c...  positive flap deflection (down flap)
c     creates a gap on the upper surface so that one does not need
c     to deal w/ overlapping coordinates
        ypivot = yhbot
        sinteflap = dsing(deltaflap)
        costeflap = dcosg(deltaflap)
        do jpt = 1, iargp+1
          if (xcrd(jpt) .ge. xpivot) then
            xtemp = xcrd(jpt) - xpivot
            ytemp = ycrd(jpt) - ypivot
            xrot =  xtemp*costeflap + ytemp*sinteflap
            yrot = -xtemp*sinteflap + ytemp*costeflap
            xcrd(jpt) = xrot + xpivot
            ycrd(jpt) = yrot + ypivot
          endif
        enddo
      else
c...  negative flap deflection (up flap)
c     creates a gap on the lower surface so that one does not need
c     to deal w/ overlapping coordinates
        ypivot = yhtop
        sinteflap = dsing(deltaflap)
        costeflap = dcosg(deltaflap)
        do jpt = 1, iargp+1
          if (xcrd(jpt) .ge. xpivot) then
            xtemp = xcrd(jpt) - xpivot
            ytemp = ycrd(jpt) - ypivot
            xrot =  xtemp*costeflap + ytemp*sinteflap
            yrot = -xtemp*sinteflap + ytemp*costeflap
            xcrd(jpt) = xrot + xpivot
            ycrd(jpt) = yrot + ypivot
          endif
        enddo
      endif

      return
      end ! flapit

