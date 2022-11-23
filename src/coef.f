
      SUBROUTINE COEF
C***********************************************************************
C...Compute the [coef]ficients for 4X4 system to solve
C   Order of the equations: continuity equation
C                           1st integral constraint
C                           2nd integral constraint
C                           3rd integral constraint
C   AIJ(..) X(.) = BI(.) ---> X(1&2) = mu u&l, X(3&4) = kh u&l
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
C-----compute the coefficients for the continuity equation 
C     AIJ(1,*)
      AIJ(1,1) =  DLOG(DABS(W_W(AKA(1),     AM0,AW(1))))      
      AIJ(1,2) = -DLOG(DABS(W_W(AKA(2),AM(ISEG),AW(2))))      
      AIJ(1,3) = -DLOG(DABS(W_S(            AM0,AS(1))))      
      AIJ(1,4) =  DLOG(DABS(W_S(       AM(ISEG),AS(2))))      
C-----compute the coefficients for the first integral constraint
C     AIJ(2,*)
      CALL II2
C-----compute the coefficients for the third and fourth int constraints
C     AIJ(3,*) AND AIJ(4,*)
      CALL II34

C-----if there are adjustable special segments set SSPHI based on WGHTPHI
      IF (LWGHT) CALL SETPHI
      CALL VLEV
C-----determine the coefficients for the v tilde functions
      IF (LLDELV) CALL FITSEGV
      CALL CHECK(10)
      TLOG = TTT(AM(ISEG), ALFAS(ISEG), AM0, ALFAS(1))
      CALL SORFA
      CALL SOCK
      CALL SAMLOG
      CALL SAF
      CALL SAMDEL
      VLOG2 = 2.*PI*(DLOG(2./VS(1)))
      VLOG1 = DLOG(VS(1)/VS(ISEG))

      IF (LLDELV) THEN
C--------compute the delta v* terms
         CALL SULOG
         CALL SU
      ENDIF

C-----Continuity equation
      BI(1) = TLOG + VLOG1 
C-----1st integral contraint
      BI(2) = VLOG2
      DO 200 J = 1, ISEG
         BI(2) = BI(2) + ORFA(J) - AMLOG(J) - URGO(J)
  200 CONTINUE
C-----2nd integral contraint
      BI(3) = - PI
      DO 300 J = 1, ISEG
         BI(3) = BI(3) + AFSIN(J) * OCK(J) + AMDEL(J)*AFCOS(J) 
     &          + DSING(AM(J)) * ULOG(J) - UCOS(J)
  300 CONTINUE
C-----3rd integral contraint
      BI(4) = 0.0
      DO 400 J = 1, ISEG
         BI(4) = BI(4) - (1 + AFCOS(J)) * OCK(J) + AMDEL(J)*AFSIN(J)
     &          + (1. - DCOSG(AM(J))) * ULOG(J) - USIN(J)
  400 CONTINUE
C-----Add further terms to BI(.) for FTE angle
      IF(LFTE) CALL FINITE
      RETURN
      END ! COEF

