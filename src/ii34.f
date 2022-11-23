
      SUBROUTINE II34
C***********************************************************************
C...Analytically evaluate the [i]ntegrals of the 2nd and 3rd integral
C   constraints, which are for the [3]rd and [4]th equations in the system.
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
C-----main upper
      CALL DRAW(WC,WS,WL,AKA(1),1.D0,AW(1),1.D0,0)
      AIJ(3,1) = - WC
      AIJ(4,1) = - WS

C-----main lower
      CALL DRAW(WC,WS,WL,AKA(2),-1.D0,(360.D0-AW(2)),1.D0,0)
      AIJ(3,2) = - WC
      AIJ(4,2) = - WS

C-----closure upper
      CALL DRAW(WC, WS, WL,0.6D0,-1.D0,AS(1),1.D0,1)
      CALL DRAW(WCC,WSS,WL,-.6D0,-1.D0,AS(1),1.D0,1)
      AIJ(3,3) = WC + WCC
      AIJ(4,3) = -WS - WSS

C-----closure lower
      CALL DRAW(WC, WS, WL,0.6D0,1.D0,(360.D0-AS(2)),1.D0,1)
      CALL DRAW(WCC,WSS,WL,-.6D0,1.D0,(360.D0-AS(2)),1.D0,1)
      AIJ(3,4) = WC + WCC
      AIJ(4,4) = -WS - WSS

      RETURN
      END ! II34

