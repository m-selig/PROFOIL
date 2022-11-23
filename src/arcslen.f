
      SUBROUTINE ARCSLEN
C***********************************************************************
C...Compute the [arc] [l]ength [s] from ARGP ignoring phi_le
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      SLENS(1) = 0.
C-----using simple trapezoidal integration
      S_PHI1 = 0.
      DO 100 JPT = 1, IARGP
         IF(LFTE) THEN
           S_PHI2 = (2. * DABS(DSING(APHI(JPT+1)/2.))) ** (1.-EPP)
     &              * (1./ARGP(JPT+1)) 
         ELSE
           S_PHI2 = 2. * DABS(DSING(APHI(JPT+1)/2.)) * (1./ARGP(JPT+1)) 
         ENDIF
         SLENS(JPT+1) = SLENS(JPT) 
     &                     + 0.5 * (S_PHI1 + S_PHI2) * DEL_PHI * DTOR
         S_PHI1 = S_PHI2
  100 CONTINUE
      DO 200 JPT = 1, IARGP+1
CCCC--------normalize arc length to smax = 2.
CCC         SLENS(JPT) = 2. * SLENS(JPT)/SLENS(IARGP+1)
C--------normalize arc length to chord 
         SLENS(JPT) = SLENS(JPT)/CHORD 
  200 CONTINUE
      RETURN
      END ! ARCSLEN

