
      SUBROUTINE SAMLOG
C***********************************************************************
C...AMLOG
C
C  Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      DO 100 I = 1, ISEG-1
         AMLOG(I) = AM(I) * DTOR * DLOG(VS(I)/VS(I+1))
  100 CONTINUE
      AMLOG(ISEG) = AM(ISEG) * DTOR * DLOG(VS(ISEG)/VS(1))
      RETURN
      END ! SAMLOG

