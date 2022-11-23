      
      SUBROUTINE VELAM
C***********************************************************************
C...  Compute the [vel]ocity distribution for given angles of attack
C     at the segment points [AM]'s
C
C     Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      LOGICAL LRESUME
      LRESUME = FF
      IALF = 0
 100  CONTINUE
      WRITE(lu06,*) 
     &     '  V-dist at AM(.) menu : [1] list design ALFAS(.)'
      WRITE(lu06,*) 
     &     '                         [2] enter angles-of-attack'
      WRITE(lu06,*) 
     &     '                         [3] pass'
      READ (lu05,*) IMENU
      IF(IMENU .EQ. 1) THEN
        WRITE(lu06,*) '   JSEG    ALFAS(.)'
        DO 110 JSEG = 1, ISEG
          WRITE(lu06,1001) JSEG, ALFAS(JSEG)
 1001     FORMAT(1X,I5,2X,F12.4)
 110    CONTINUE
      ELSEIF(IMENU .EQ. 2) THEN
        WRITE(lu06,*) '  Enter number'
        READ(lu05,*) IALF
        IF(IALF .NE. 0) THEN
          WRITE(lu06,*) 
     &         '  Enter the angles of attack relative to the zll:'
          READ(lu05,*) (ALFA(I), I = 1, IALF)
        ENDIF
        LRESUME = TT
      ELSEIF(IMENU .EQ. 3) THEN
        LRESUME = TT
      ELSE
        WRITE(lu06,*) '  This number not an option. Try again.'
      ENDIF
      IF(.NOT. LRESUME) GOTO 100  
C-----compute the velocity at the selected angles of attack
      DO 200 JALF = 1, IALF
C     
C---  compute velocity at phi_s us
        PHI = AS(1) 
        VAS1  = VS(1) 
     &       * W_S(       AS(1),AS(1))**  HKH(1) 
     &       * W_W(AKA(1),AS(1),AW(1))**(-UMU(1))
        IF (LFTE) THEN
          IF(AS(1) .LE. PHIEPP(1)) THEN
            VAS1 = VAS1 * W_F(AS(1), PHIEPP(1)) ** EPP
          ENDIF
        ENDIF
        VEL = VAS1 * DABS(DCOSG( 0.5*AS(1) - ALFA(JALF) ))
     &       /  DABS(DCOSG( 0.5*AS(1) - ALFAS(1)     ))
        JPT = INT((PHI + DEL_PHI) / DEL_PHI) + 1
        IF(JPT .GE. 2 .AND. JPT .LE. IARGP) THEN
C--   x/c is between JPT-1 and JPT (interpolated local value)
          XCLC = XCRD(JPT-1) + (XCRD(JPT) - XCRD(JPT-1))
     &         /(APHI(JPT) - APHI(JPT-1))
     &         *(PHI       - APHI(JPT-1))
C---  s/c is between JPT-1 and JPT (interpolated local value)
          SCLC = SLENS(JPT-1) + (SLENS(JPT) - SLENS(JPT-1))
     &         /(APHI(JPT)  -  APHI(JPT-1))
     &         *(PHI        -  APHI(JPT-1))
        ENDIF
        WRITE(45,*) SCLC, VEL 
        WRITE(55,*) XCLC,  VEL 
        WRITE(65,*) PHI,  VEL 
C     
C---  compute velocity at AM(1) - AM(ISEG-1)
        DO JSEG = 1, ISEG-1
          PHI = AM(JSEG)
          JPT = INT((PHI + DEL_PHI) / DEL_PHI) + 1
          IF(JPT .GE. 2 .AND. JPT .LE. IARGP) THEN
C--   x/c is between JPT-1 and JPT (interpolated local value)
            XCLC = XCRD(JPT-1) + (XCRD(JPT) - XCRD(JPT-1))
     &           /(APHI(JPT) - APHI(JPT-1))
     &           *(PHI       - APHI(JPT-1))
C---  s/c is between JPT-1 and JPT (interpolated local value)
            SCLC = SLENS(JPT-1) + (SLENS(JPT) - SLENS(JPT-1))
     &           /(APHI(JPT)  -  APHI(JPT-1))
     &           *(PHI        -  APHI(JPT-1))
C---  argpv is between JPT-1 and JPT (interpolated local value)
            ARGPVLC = ARGPV(JPT-1) + (ARGPV(JPT) - ARGPV(JPT-1))
     &           /(APHI(JPT)  -  APHI(JPT-1))
     &           *(PHI        -  APHI(JPT-1))
          ENDIF
          VEL = 2. * DABS(DCOSG(0.5*PHI - ALFA(JALF))) * ARGPVLC
          WRITE(45,*) SCLC, VEL 
          WRITE(55,*) XCLC,  VEL  
          WRITE(65,*) PHI,  VEL 
        ENDDO
C     
C---  compute velocity at phi_s ls
        PHI = AS(2) 
        VAS2  = VS(ISEG) 
     &       * W_S(       AS(2),AS(2))**  HKH(2) 
     &       * W_W(AKA(2),AS(2),AW(2))**(-UMU(2))
        IF (LFTE) THEN
          IF(AS(2) .GE. PHIEPP(2)) THEN
            VAS2 = VAS2 * W_F(AS(2), PHIEPP(2)) ** EPP
          ENDIF
        ENDIF
        VEL = VAS2 * DABS(DCOSG( 0.5*AS(2) - ALFA(JALF) ))
     &       /  DABS(DCOSG( 0.5*AS(2) - ALFAS(ISEG)  ))
C---  determine location of PHI between JPT-1 and JPT
        JPT = INT((PHI+0.1*DEL_PHI) / DEL_PHI) + 2 
        IF(JPT .GE. 2 .AND. JPT .LE. IARGP) THEN
C--   x/c is between JPT-1 and JPT (interpolated local value)
          XCLC = XCRD(JPT-1) + (XCRD(JPT) - XCRD(JPT-1))
     &         /(APHI(JPT) - APHI(JPT-1))
     &         *(PHI       - APHI(JPT-1))
C---  s/c is between JPT-1 and JPT (interpolated local value)
          SCLC = SLENS(JPT-1) + (SLENS(JPT) - SLENS(JPT-1))
     &         /(APHI(JPT)  -  APHI(JPT-1))
     &         *(PHI        -  APHI(JPT-1))
        ENDIF
        WRITE(45,*) SCLC, VEL 
        WRITE(55,*) XCLC,  VEL 
        WRITE(65,*) PHI,  VEL 
C     
C---  compute velocity at AM(ISEG)
        PHI = AM(ISEG)
C---  determine location of PHI between JPT-1 and JPT
        JPT = INT((PHI+0.1*DEL_PHI) / DEL_PHI) + 2 
        IF(JPT .GE. 2 .AND. JPT .LE. IARGP) THEN
C--   x/c is between JPT-1 and JPT (interpolated local value)
          XCLC = XCRD(JPT-1) + (XCRD(JPT) - XCRD(JPT-1))
     &         /(APHI(JPT) - APHI(JPT-1))
     &         *(PHI       - APHI(JPT-1))
C---  s/c is between JPT-1 and JPT (interpolated local value)
          SCLC = SLENS(JPT-1) + (SLENS(JPT) - SLENS(JPT-1))
     &         /(APHI(JPT)  -  APHI(JPT-1))
     &         *(PHI        -  APHI(JPT-1))
C---  argpv is between JPT-1 and JPT (interpolated local value)
          ARGPVLC = ARGPV(JPT-1) + (ARGPV(JPT) - ARGPV(JPT-1))
     &         /(APHI(JPT)  -  APHI(JPT-1))
     &         *(PHI        -  APHI(JPT-1))
        ENDIF
        VEL = 2. * DABS(DCOSG(0.5*PHI - ALFA(JALF))) * ARGPVLC
        WRITE(45,*) SCLC, VEL 
        WRITE(55,*) XCLC,  VEL  
        WRITE(65,*) PHI,  VEL 
 200  CONTINUE
      IF(IALF .NE. 0) THEN
        WRITE(lu06,*)             '*********OUTPUT**************'
        WRITE(lu06,*) '* v(s/c) at AM(.) --> FOR045.DAT *'
        WRITE(lu06,*) '* v(x/c) at AM(.) --> FOR055.DAT *'
        WRITE(lu06,*) '* v(phi) at AM(.) --> FOR065.DAT *'
        WRITE(lu06,*)             '*********OUTPUT**************'
        CLOSE (55)
        CLOSE (65)
      ENDIF
      RETURN
      END ! VELAM

