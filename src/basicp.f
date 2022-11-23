
      SUBROUTINE BASICP
C***********************************************************************
C...Compute some basic design dependent parameters.
C...If some parameters are large and out of bounds, they are set to
C   dummy values. This to prevent overflow and errors during SWRITE.
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      SKS    = HKH(1) + HKH(2)      
      IF((DABS(UMU(1)) .GT. 300.) 
     &         .OR. (DABS(UMU(2)) .GT. 300.)) THEN
         OMEGA(1) = 9999.
         OMEGA(2) = 9999.
C--------WRITE(lu06,*) 'Iteration on OMEGA(.) will fail'
      ELSE
         OMEGA(1) = (1. + 
     &                AKA(1)*((1. - DCOSG(AW(1)))/
     &                        (1. + DCOSG(AW(1)))))
     &                **(-UMU(1)) 
         OMEGA(2) = (1. + 
     &                AKA(2)*((1. - DCOSG(AW(2)))/
     &                        (1. + DCOSG(AW(2)))))
     &                **(-UMU(2)) 
         IF(DABS(OMEGA(1)) .GT. 9999.
     &      .OR. DABS(OMEGA(2)) .GT. 9999.) THEN
            OMEGA(1) = 9999.
            OMEGA(2) = 9999.
         ENDIF
      ENDIF
      OMEGP(1) = 2. * UMU(1) * AKA(1) / (1. + DCOSG(AW(1)))
      OMEGP(2) = 2. * UMU(2) * AKA(2) / (1. + DCOSG(AW(2)))
      IF(DABS(OMEGP(1)) .GT. 9999.) THEN
         OMEGP(1) = 9999.
C--------WRITE(lu06,*) 'Iteration on OMEGP(1) will fail'
      ENDIF
      IF(DABS(OMEGP(2)) .GT. 9999.) THEN
         OMEGP(2) = 9999.
C--------WRITE(lu06,*) 'Iteration on OMEGP(2) will fail'
      ENDIF
      IF((DABS(HKH(1)) .GT. 75) .OR. (DABS(HKH(2)) .GT. 75)) THEN
         OMEGT(1) = 0.
         OMEGT(2) = 0.
C--------WRITE(lu06,*) 'Iteration on OMEGT(.) will fail'
      ELSE
         OMEGT(1) = (1. + 
     &                AKA(1)*((1. - DCOSG(AW(1)))/
     &                        (1. + DCOSG(AW(1)))))
     &                **(-UMU(1)) * (1.-.36)**HKH(1)
         OMEGT(2) = (1. + 
     &                AKA(2)*((1. - DCOSG(AW(2)))/
     &                        (1. + DCOSG(AW(2)))))
     &                **(-UMU(2)) * (1.-.36)**HKH(2)
         IF(DABS(OMEGT(1)) .GT. 9999.
     &      .OR. DABS(OMEGT(2)) .GT. 9999.) THEN
            OMEGT(1) = 9999.
            OMEGT(2) = 9999.
         ENDIF
      ENDIF
      RETURN
      END ! BASCIP

