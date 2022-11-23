
      subroutine gensym
C***********************************************************************
C...  Generate the gensym airfoil
C...  This routine will make more sense after reading PROFOIL notes "010703"
C...  The routine is complicated by the possibility of there being a point
C     0,0 on the xcrd,ycrd coordinates.  Things seem to be working here.
C
C     Naming Convention
C     "base" is the profoil generated airfoil 
C            from which the gensym airfoil is derived
C     "gensym" is the gensym derived airfoils
C     *gs are gensym variables
C
C     Variables:
C     xgensym,ygensym - coordinates
C     xf            - flap x/c position
C     yhalftc       - 1/2 of the thickness at the flap position once generated
C     yf            - resulting flap position, to be determined
C     fgensym       - gensym flap angle, to be determined
C     ytat          - the line defining the symmetry plane
C                     (looks like a thin airfoil theory line, hence 'tat')
C     thkmaxg       - max thickness of the gensym airfoil
C
C     Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      include 'PROFOIL.INC'
      logical lzero
c
      igensym = 0
c
c
c
c======================================================================
      if(.not. lgensym_xf_yf) then
c...  find the base airfoil thickness at xf
c     taken from tatx.f
c...  (else do nothing because yf is given)
        do jpt = 1, ilectp
          if (xcrd(jpt) .lt. xf) then
            ytop = 
     $           polate(xcrd(jpt),ycrd(jpt),xcrd(jpt-1),ycrd(jpt-1),xf)
            goto 100
          elseif (xcrd(jpt) .eq. xf) then
            ytop = ycrd(jpt)
            goto 100
          endif
        enddo
 100    continue
c...  set the position of the hingeline (xf,yhalftc)
c     xf is given as input
c     determine yf
        yf = ytop - yhalftc
      endif
c======================================================================
c
c

c
c======================================================================
c...  find the upper surface gensym coordinates
c     write out the coordinates later
c...  first point
      xtmp(1) = xcrd(1)
      ytmp(1) = ycrd(1)
c
c...  initialize for iteration loop
      jgensym = 1
c...  start at the point just past the TE (jbase = 2 is the first used below)
      jbase   = 1
 200  continue
c...  points for gensym section
      jgensym = jgensym + 1
c...  points for base section
      jbase   = jbase + 1 
c
c...  LE tests to quit
ccc      if ( (xcrd(jbase)      .eq. zero) .and. 
ccc     $     (abs(ycrd(jbase)) .lt. tiny)  ) then
      if ( xcrd(jbase)      .eq. zero ) then
c...  the airfoil has a point 0,0
c     and we are at the LE
        lzero = tt
        xtmp(jgensym) = 0.0
        ytmp(jgensym) = 0.0
        jgensym = jbase
        goto 300
      elseif (ycrd(jbase) .le. zero) then
c...  then jgensym this must be the LE point
        lzero = ff
        jgensym = jbase - 1
        goto 300
      endif
c
c...  determine the interior points
      if (xcrd(jbase) .ge. xf) then
c...  region 1
        ytat       = yf * (1-xcrd(jbase))/(1-xf)
        xtmp(jgensym) = xcrd(jbase)
        ytmp(jgensym) = ycrd(jbase) - ytat
      elseif (xcrd(jbase) .lt. xf) then
c...  region 2
        ytat       = yf * xcrd(jbase)/xf
        xtmp(jgensym) = xcrd(jbase)
        ytmp(jgensym) = ycrd(jbase) - ytat
      endif
      write(73,*) xcrd(jbase), ytat
      if (ytmp(jgensym) .le. 0.d0) then
        write(lu06,*) 'Warning 102: Negative y-value for gensym airfoil'
      endif
      goto 200
 300  continue
c
      igensym = jgensym
c======================================================================
c
c
c
c======================================================================
c...  generate the symmetric airfoil
c     this is complicated by the possibility of there being a point 0,0
c...  do the upper surface
      do jgensym = 1, igensym
        xgensym(jgensym) = xtmp(jgensym)
        ygensym(jgensym) = ytmp(jgensym)
      enddo
c...  do the lower surface
      if(lzero) then
c...  lzero = tt, includes point 0,0
        do jcount = 1, igensym-1
          jgensym = igensym + jcount
          xgensym(jgensym) =  xtmp(igensym-jcount)
          ygensym(jgensym) = -ytmp(igensym-jcount)
        enddo
      else
c...  lzero = ff, does not include point 0,0
        do jcount = 1, igensym
          jgensym = igensym + jcount
          xgensym(jgensym) =  xtmp(igensym-jcount+1)
          ygensym(jgensym) = -ytmp(igensym-jcount+1)
        enddo
      endif
cc...  do the last point at the TE
      igensym = jgensym
c      xgensym(igensym) =  xcrd(1)
c      ygensym(igensym) = -ycrd(1)
c...  determine the flap angle (beta2)
c
c      
c======================================================================
c... get the flap angle
      fgensym = datan(yf/(1-xf)) * rtod
ccc      write(lu06,*) 'fgensym = ' , fgensym
c======================================================================
c
c
c
c======================================================================
c... get the gensym airfoil thickness
      call thicke(xgensym,ygensym,igensym,thkmaxg,dumb,dumb)
c...  thkmaxg is the thickness
ccc      write(lu06,*) 'thkmaxg = ' , thkmaxg
c======================================================================
c
      if(lgensym_report) then
        write(lu06,*) 'thkmaxg = ' , thkmaxg
        write(lu06,*) 'fgensym = ' , fgensym
      endif
      return
      end ! gensym

