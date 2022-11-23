
      SUBROUTINE PACK2(JSEG)
C***********************************************************************
C...Pack the boundary layer arc length and velocity arrays.
C   See notes 10-15-90 p. 4 for conventions.
C   Note:The spacing between the Newton nodes must be greater than
C        the spacing between the coordinate points. In other words,
C        between any coordinate pairs, 
C        there should at most one Newton node.
C   [] TODO: Need an ERROR flag to this effect.
C        If ERROR is flagged, then space the closest adjacent Newton nodes 
C        further apart, or increase the number of coordinates.
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER JSEG
      AMVEL0 = AMSTAG(ALFAS(JSEG))
      IBL = 1
      IF(LBE) THEN
C--------start and end points in direction BOS to EOS
         KSTPK = INT(  AMVEL0/DEL_PHI) + 2
         KENPK = INT(AM(JSEG)/DEL_PHI) + 2
C--------first point is either ahead or on SLENS(KSTPK)
         IF(DABS(APHI(KSTPK)-AMVEL0) .LT. 0.001) THEN
C-----------points coincide
            SBL(IBL) = SLENS(KSTPK)
            SSTAG = SBL(1)
            VBL(IBL) = 0.0
            KSTPK = KSTPK + 1
         ELSE
C-----------interpolate
            SSTAG = Y3(APHI(KSTPK),  SLENS(KSTPK),
     &                 APHI(KSTPK-1),SLENS(KSTPK-1),
     &                 AMVEL0)
            SBL(IBL) = SSTAG 
            VBL(IBL) = 0.0
         ENDIF
         JPACK = 1
         IF(SPACK(JPACK) .LE. SLENS(KSTPK+1)) THEN
            WRITE(lu06,*) ' Error 179: 1st Newt node to close ',
     $          'to stag point (pack2.f)'
         ENDIF
         IBL = 2
         DO 100 JPT = KSTPK, KENPK, 1
            TEST1 = SPACK(JPACK) - SLENS(JPT)
            TEST2 = DABS(TEST1) 
            IF(TEST2 .LT. 0.001) THEN
C-------------points coincide
              SBL(IBL) = SPACK(JPACK)
              VBL(IBL) = VPACK(JPACK)
              INDXBL(JPACK) = IBL
              JPACK = JPACK + 1
              IF(JPACK .GT. IPACK) GO TO 110 
              IBL = IBL + 1
            ELSEIF(TEST1 .GT. 0.) THEN
C-------------SPACK(.) is ahead of SLENS(.)  
C             'ahead' means GT in BL flow direction
              SBL(IBL) = SLENS(JPT)
              VBL(IBL) = 2.* DABS(DCOSG(0.5*APHI(JPT) - ALFAS(JSEG))) 
     &                     * ARGPV(JPT)
              IBL = IBL + 1
            ELSE
C-------------SPACK(.) is behind SLENS(.): (TEST1 .LT. 0.) 
              SBL(IBL) = SPACK(JPACK)
              VBL(IBL) = VPACK(JPACK)
              INDXBL(JPACK) = IBL
              JPACK = JPACK + 1
              IF(JPACK .GT. IPACK) GO TO 110 
              IBL = IBL + 1
              SBL(IBL) = SLENS(JPT)
              VBL(IBL) = 2.* DABS(DCOSG(0.5*APHI(JPT) - ALFAS(JSEG))) 
     &                     * ARGPV(JPT)
              IBL = IBL + 1
            ENDIF
            IF(IBL .GT. NBL) WRITE(lu06,*) 
     $           'Error 180: IBL > NBL (pack2.f)'
  100    CONTINUE
  110    CONTINUE
         IF((JPACK-1) .NE. IPACK) THEN
            WRITE(lu06,*) 
     $          'Error 181: see comments in surboutine pack2 (pack2.f)'
         ENDIF
      ELSE
C--------start and end points in direction EOS to BOS
         KSTPK = INT(    AMVEL0/DEL_PHI) + 1
         KENPK = INT(AM(JSEG-1)/DEL_PHI) + 1
C--------first point is either ahead or on SLENS(KSTPK)
         IF(DABS(APHI(KSTPK)-AMVEL0) .LT. 0.001) THEN
C-----------points coincide
            SBL(IBL) = SLENS(KSTPK)
            SSTAG = SBL(1)
            VBL(IBL) = 0.0
            KSTPK = KSTPK-1
         ELSE
C-----------interpolate
            SSTAG = Y3(APHI(KSTPK),  SLENS(KSTPK),
     &                 APHI(KSTPK+1),SLENS(KSTPK+1),
     &                 AMVEL0)
            SBL(IBL) = SSTAG 
            VBL(IBL) = 0.0
         ENDIF
         JPACK = 1
         IF(SPACK(JPACK) .GE. SLENS(KSTPK-1)) THEN
            WRITE(lu06,*) 
     $          'Error 182: 1st Newt node to close to stag point ',
     $          '(pack2.f)'
         ENDIF
         IBL = 2
         DO 200 JPT = KSTPK, KENPK, -1
            TEST1 = SLENS(JPT) - SPACK(JPACK)
            TEST2 = DABS(TEST1) 
            IF(TEST2 .LT. 0.001) THEN
C-------------points coincide
              SBL(IBL) = SPACK(JPACK)
              VBL(IBL) = VPACK(JPACK)
              INDXBL(JPACK) = IBL
              JPACK = JPACK + 1
              IF(JPACK .GT. IPACK) GO TO 210 
              IBL = IBL + 1
            ELSEIF(TEST1 .GT. 0.) THEN
C-------------SPACK(.) is ahead of SLENS(.)
              SBL(IBL) = SLENS(JPT)
              VBL(IBL) = 2.* DABS(DCOSG(0.5*APHI(JPT) - ALFAS(JSEG))) 
     &                     * ARGPV(JPT)
              IBL = IBL + 1
            ELSE
C-------------SPACK(.) is behind SLENS(.): (TEST1 .LT. 0.) 
              SBL(IBL) = SPACK(JPACK)
              VBL(IBL) = VPACK(JPACK)
              INDXBL(JPACK) = IBL
              JPACK = JPACK + 1
              IF(JPACK .GT. IPACK) GO TO 210 
              IBL = IBL + 1
              SBL(IBL) = SLENS(JPT)
              VBL(IBL) = 2.* DABS(DCOSG(0.5*APHI(JPT) - ALFAS(JSEG))) 
     &                     * ARGPV(JPT)
              IBL = IBL + 1
            ENDIF
            IF(IBL .GT. NBL) WRITE(lu06,*) 
     $           'Error 183: ibl > nbl (pack2.f)'
  200    CONTINUE
  210    CONTINUE
         IF((JPACK-1) .NE. IPACK) THEN
            WRITE(lu06,*) 'Error 184: see comments (pack2.f)'
         ENDIF
      ENDIF
C-----Reset BL coordinates
      IF(LBE) THEN
         DO 300 JBL = 1, IBL
            SBL(JBL) = SBL(JBL) - SSTAG
  300    CONTINUE
      ELSE
         DO 400 JBL = 1, IBL
            SBL(JBL) = SSTAG - SBL(JBL)
  400    CONTINUE
      ENDIF
      RETURN
      END ! PACK2

