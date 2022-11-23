
      SUBROUTINE DRAW(WC,WS,WL,DK,DM,FL,AG,MA)
C***********************************************************************
C...  This subroutine was taken from the Eppler code
C     Ref: NASA-TM-80210
C***********************************************************************
      
C...MAIN PRESSURE RECOVERY FUNCTION:
C   DK = K
C   DM         UMU EXPONENT AS MULTIPLCATION
C   FL*AG      WPHI IN DEG
C...MA = 0     
C
C...CLOSURE PRESSURE RECOVERY FUNCTION:
C   DK = .6    FIRST CALL
C   DK = -.6   SECOND CALL
C   DM = -1    SIGN ONLY
C   FL*AG      SPHI IN DEG
C...MA = 1 
C
C...RETURNS:
C     WS    SINE   INTEGRAL
C     WC    COSINE INTEGRAL
C
      IMPLICIT REAL*8 (A-H, O-Z) 
      COSP = DCOSG(AG*FL)
      FK = DK
C...  Fortran Arithemic IF statement ...........................
      IF(MA)2,1,2
    1 FK= FK*(1.-COSP)/(1.+COSP)
    2 SINP = DSING(AG*FL)
C...I DO NOT KNOW WHAT [WL] IS FOR
      WL = -DM*DLOG(1.D0+FK)
      IF(FL.EQ.0.) FK = 1.D0
      BETA =(COSP-1.)/FK + COSP
      BQM1 = BETA**2 - 1.
      WUBEQ= DSQRT(ABS(BQM1))
      U= (1.+BETA)*SINP/(1.+COSP)
C...  Fortran Arithemic IF statement ...........................
      IF(BQM1)3,5,4
    3 WF = DLOG(DABS((WUBEQ+U)/(WUBEQ-U)))
C...'(LAMBA*2 - 1) WAS NEG'
      GO TO 6
    4 WF = 2.* DATAN(U/WUBEQ)
C...'(LAMBA*2 - 1) WAS POS'
      GO TO 6
    5 WF = 0.
C...'(LAMBA*2 - 1) WAS ZERO'
    6 WC =(WUBEQ*WF - SINP - BETA*FL*AG*0.01745329)
      WS = (COSP-1.)*(1.-(1./FK + 1.)*DLOG(1.D0+FK))*DM
      RETURN
      END ! DRAW

