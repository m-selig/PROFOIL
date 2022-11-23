
      subroutine bulbvol
c***********************************************************************
c...  compute the volume of a bulb produced by a symmetric airfoil revolved
c     about the x-axis
c...  use the airfoil produced by profoil
c     
c...  see profoil notes 010707 p 1
c     uses: chordo passed through profoil.inc file.
c     chordo is the physical length of the bulb
c
c...  based on afarea.f
c
c     Copyright (c) 1990-2022 Michael Selig
c***********************************************************************
      include 'PROFOIL.INC'
      imo = iargp/2 + 1
c...  put coordinates in temp array
      do jpt = 1, imo
        xtmp(jpt) = xcrd(jpt)
        ytmp(jpt) = ycrd(jpt)
      enddo
c...  scale the points
      do jmo = 1, imo
        xtmp(jmo) = length_b * xtmp(jmo)
        ytmp(jmo) = length_b * ytmp(jmo)
      enddo
c...  init the volume
      volume_b = 0.0
c...  get the area under the top surface using trapezodial integration.
      do jmo = 1, imo-1
        radius = 0.5 * (ytmp(jmo) + ytmp(jmo+1))
c...  volume of the slice
        delta = pi * radius * radius * (xtmp(jmo) - xtmp(jmo + 1))
c...  summing it up
        volume_b = volume_b + delta
      enddo
      return
      end

