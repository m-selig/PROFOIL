
      SUBROUTINE NT1G1(JEQU1)
C***********************************************************************
c                 200 v_TE for given alfa [COND1(.)]
c                 201 velocity differential between phi_s_us and
c                     phi_s_ls  for given alfa [COND1(.)]
c                 202 velocity at phi_s_us for given alfa [COND1(.)]
c                 203 velocity at phi_s_ls for given alfa [COND1(.)]
c                |204 c_m  for given alfa
c                 205 t/c  for given x/c
c
c   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER JEQU1
      IF (IFTP1(JEQU1) .EQ. 200) THEN 
         VTE = VS(1) 
     &         * W_S(       AM0,AS(1))**  HKH(1) 
     &         * W_W(AKA(1),AM0,AW(1))**(-UMU(1))
         VALUE = VTE * DCOSG(COND1(JEQU1)) / DCOSG(ALFAS(1))
      ELSEIF (IFTP1(JEQU1) .EQ. 201) THEN 
         VAS1  = VS(1) 
     &         * W_S(       AS(1),AS(1))**  HKH(1) 
     &         * W_W(AKA(1),AS(1),AW(1))**(-UMU(1))
         VAS2  = VS(ISEG) 
     &         * W_S(       AS(2),AS(2))**  HKH(2) 
     &         * W_W(AKA(2),AS(2),AW(2))**(-UMU(2))
         IF (LFTE) THEN
            IF(AS(1) .LE. PHIEPP(1)) THEN
               VAS1 = VAS1 * W_F(AS(1), PHIEPP(1)) ** EPP
            ENDIF
            IF(AS(2) .GE. PHIEPP(2)) THEN
               VAS2 = VAS2 * W_F(AS(2), PHIEPP(2)) ** EPP
            ENDIF
         ENDIF
         VAS1 = VAS1 * DABS(DCOSG( 0.5*AS(1) - COND1(JEQU1) ))
     &              /  DABS(DCOSG( 0.5*AS(1) - ALFAS(1)     ))
         VAS2 = VAS2 * DABS(DCOSG( 0.5*AS(2) - COND1(JEQU1) ))
     &              /  DABS(DCOSG( 0.5*AS(2) - ALFAS(ISEG)  ))
         VALUE = VAS1 - VAS2
      ELSEIF (IFTP1(JEQU1) .EQ. 202) THEN 
         VAS1  = VS(1) 
     &         * W_S(       AS(1),AS(1))**  HKH(1) 
     &         * W_W(AKA(1),AS(1),AW(1))**(-UMU(1))
         IF (LFTE) THEN
            IF(AS(1) .LE. PHIEPP(1)) THEN
               VAS1 = VAS1 * W_F(AS(1), PHIEPP(1)) ** EPP
            ENDIF
         ENDIF
         VAS1 = VAS1 * DABS(DCOSG( 0.5*AS(1) - COND1(JEQU1) ))
     &              /  DABS(DCOSG( 0.5*AS(1) - ALFAS(1)     ))
         VALUE = VAS1
      ELSEIF (IFTP1(JEQU1) .EQ. 203) THEN 
         VAS2  = VS(ISEG) 
     &         * W_S(       AS(2),AS(2))**  HKH(2) 
     &         * W_W(AKA(2),AS(2),AW(2))**(-UMU(2))
         IF (LFTE) THEN
            IF(AS(2) .GE. PHIEPP(2)) THEN
               VAS2 = VAS2 * W_F(AS(2), PHIEPP(2)) ** EPP
            ENDIF
         ENDIF
         VAS2 = VAS2 * DABS(DCOSG( 0.5*AS(2) - COND1(JEQU1) ))
     &              /  DABS(DCOSG( 0.5*AS(2) - ALFAS(ISEG)  ))
         VALUE = VAS2
      ELSEIF (IFTP1(JEQU1) .EQ. 205) THEN 
	 VALUE = TATX(COND1(JEQU1))
      ELSEIF (IFTP1(JEQU1) .EQ. 206) THEN 
        xhinge = cond1(jequ1)
        call tatx2
        VALUE = 0.5 * (yhtop + yhbot)
      ELSE
         WRITE(lu06,*) ' Error 170: Newton type not found (nt1g1.f)'
      ENDIF
      FNT1_X(JEQU1) = VALUE - FNEWT1(JEQU1) 
      RETURN
      END ! NT1G1

