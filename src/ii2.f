
      SUBROUTINE II2
C***********************************************************************
C...Numerically evaluate the [i]ntegrals of the 1st integral constraint,
C   which is the [2]nd equation of the system.
C   DELAN is the [n]ominal [del]ta [a]ngle phi of the integration, 
C   while DELAA is that [a]ctually used. 
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'

C-----AIJ(2,1)
      N = NINT((AM(1) - AM0)/DELAN)
      DELAA =  (AM(1) - AM0)/N
      F0 = DLOG(DABS(W_W(AKA(1), AM0, AW(1))))
      AIJ(2,1) = 0.
      PHI = AM0
      DO 101 I = 1, N
         PHI = PHI + DELAA
         F1 = DLOG(DABS(W_W(AKA(1), PHI, AW(1))))
         AIJ(2,1) = AIJ(2,1) - 0.5*(F1 + F0)*DELAA*DTOR
         F0 = F1
  101 CONTINUE

C-----AIJ(2,2)
      N = NINT((AM(ISEG) - AM(ISEG-1))/DELAN)
      DELAA =  (AM(ISEG) - AM(ISEG-1))/N
      F0 = DLOG(DABS(W_W(AKA(2), AM(ISEG-1), AW(2))))
      AIJ(2,2) = 0.
      PHI = AM(ISEG-1)
      DO 102 I = 1, N
         PHI = PHI + DELAA
         F1 = DLOG(DABS(W_W(AKA(2), PHI, AW(2))))
         AIJ(2,2) = AIJ(2,2) - 0.5*(F1 + F0)*DELAA*DTOR
         F0 = F1
  102 CONTINUE

C-----AIJ(2,3)
      N = NINT((AS(1) - AM0)/DELAN)
      DELAA =  (AS(1) - AM0)/N
      F0 = DLOG(DABS(W_S(AM0, AS(1))))
      AIJ(2,3) = 0.
      PHI = AM0
      DO 103 I = 1, N
         PHI = PHI + DELAA
         F1 = DLOG(DABS(W_S(PHI, AS(1))))
         AIJ(2,3) = AIJ(2,3) + 0.5*(F1 + F0)*DELAA*DTOR
         F0 = F1
  103 CONTINUE

C-----AIJ(2,4)
      N = NINT((AM(ISEG) - AS(2))/DELAN)
      DELAA =  (AM(ISEG) - AS(2))/N
      F0 = DLOG(DABS(W_S(AS(2), AS(2))))
      AIJ(2,4) = 0.
      PHI = AS(2)
      DO 104 I = 1, N
         PHI = PHI + DELAA
         F1 = DLOG(DABS(W_S(PHI, AS(2))))
         AIJ(2,4) = AIJ(2,4) + 0.5*(F1 + F0)*DELAA*DTOR
         F0 = F1
  104 CONTINUE
      RETURN
      END ! II2

