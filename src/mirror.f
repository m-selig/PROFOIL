
      subroutine mirror
c***********************************************************************
c...  Make a mirror image of the design data so that the airfoil when
c     designed is flipped upside down.  This will make a cambered
c     airfoil reflexed.
C
C     Copyright (c) 1990-2022 Michael Selig
c***********************************************************************
      include 'PROFOIL.INC'

      write(lu06,*)
      write(lu06,*)'***************************'
      write(lu06,*)'*** writing mirror file ***'
      write(lu06,*)'***************************'
      write(lu06,*)
c=========================================================================
      open(unit = 31, file = file31, status = 'unknown')
c=========================================================================
      write(31,1702) iargpu 
 1702 format('COORD',3x,i3)
      jcount = 1
      do 100 jseg = iseg-1, 1, -1
        write(31,1100) 
     $       am(iseg)/sclf - am(jseg)/sclf, -alfas(jseg+1), jcount
        jcount = jcount + 1
 100  continue
      write(31,1100) am(iseg)/sclf, -alfas(1), jseg
 1100 format('FOIL',2x,f10.5,2x,f10.5,2x,i3)
c...  have not yet added delv capability
c      do 500 jseg = 1, isegt
c        if(ldelv(jseg)) then
c          jcase = idvtp(jseg)
c          if (jcase .le. 3) then
c            write(31,1500) jcase, jseg, vtilde(jseg)
c 1500       format('delv',i3,2x,i3,2x,f10.5)
c          elseif (jcase .ge. 11) then 
c            jspseg = kspseg(jseg) 
c            isbseg = ksbseg(jseg) 
c            write(31,1550) jcase, jseg, isbseg
c 1550       format('delv',i3,2x,i3,4x,i3)
c            do 600 jsbseg = 1, isbseg
c              write(31,1600) wghtphi(jspseg,jsbseg), 
c     &             ssdelv(jspseg,jsbseg)
c 1600         format(2x,f5.3,2x,f10.6)
c 600        continue
c          endif
c        endif
c 500  continue
      angle = epp * pi * rtod
      write(31,1200) 
     $     am(iseg)/sclf - as(2)/sclf , am(iseg)/sclf - as(1)/sclf
 1200 format('PHIS',2x,f10.5,2x,f10.5)
      if(lfte) then
        write(31,1800) 
     $       angle, 
     $       am(iseg)/sclf - phiepp(2)/sclf, 
     $       am(iseg)/sclf - phiepp(1)/sclf
 1800   format('FTE ',2x,f6.1,6x,f10.5,2x,f10.5)
      endif
      write(31,1300) aka(2), aka(1)
 1300 format('REC ',2x,f10.5,2x,f10.5)
      write(31,1400) iseg - ivel + 1, vs(ivel)
 1400 format('VLEV',3x,i3,8x,f10.5)
      write(31,1700) iseg - ile
 1700 format('ILE ',3x,i3)
      write(31,1701)
 1701 format(
     $     'IDES',      /
     $     'FINISH 100',/
     $     'ALFASP 1',  /
     $     '10',        /
     $     'VELDIST 60',/
     $     )
      write(31,1703)
 1703 format('*')
      close(31)      
      return
      end ! mirror

