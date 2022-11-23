      
      SUBROUTINE NT1G0(JEQU1)
C***********************************************************************
C>>line>NEWT1G0  100 K_s
C     101 c_m0
C     102 t/c_max
C     103 alfa_0
C     104 xc_max location of maximum thickness
C     105 camber_max 
C     106 leading edge radius
C     107 maximum thickness ratio according to Eppler's routine
C     121 t/c_max for gensym airfoil
C     122 flap angle for gensym airfoil
C     134 area calculation (use CHORDO)
C     184 omega_us
C     185 omega_ls
C     190 mu_us
C     191 mu_ls
C     192 K_H_us
C     193 K_H_ls
C     194 omega_us    (indirectly through mu_us as linearizer)
C     195 omega_ls    (indirectly through mu_ls as linearizer)
C     196 omega'_us
C     197 omega'_ls
C     198 omega_T_us  (incompatible with IFTP1=200)
C     199 omega_T_ls  (incompatible with IFTP1=200)
C
C     Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER JEQU1
      IF (IFTP1(JEQU1) .EQ. 100) THEN 
        VALUE = HKH(1) + HKH(2)
        FNT1_X(JEQU1) = VALUE - FNEWT1(JEQU1)
      ELSEIF (IFTP1(JEQU1) .EQ. 101) THEN 
        VALUE = CM0
        FNT1_X(JEQU1) = VALUE - FNEWT1(JEQU1) 
      ELSEIF (IFTP1(JEQU1) .EQ. 102) THEN 
        VALUE = THKMAX
        FNT1_X(JEQU1) = VALUE - FNEWT1(JEQU1) 
      ELSEIF (IFTP1(JEQU1) .EQ. 103) THEN 
        VALUE = -ALFA0
        FNT1_X(JEQU1) = VALUE - FNEWT1(JEQU1) 
      ELSEIF (IFTP1(JEQU1) .EQ. 104) THEN 
        VALUE = XCMAX
        FNT1_X(JEQU1) = VALUE - FNEWT1(JEQU1) 
      ELSEIF (IFTP1(JEQU1) .EQ. 105) THEN 
        VALUE = CMBMAX
        FNT1_X(JEQU1) = VALUE - FNEWT1(JEQU1) 
      ELSEIF (IFTP1(JEQU1) .EQ. 106) THEN 
        CALL RADIUS
        VALUE = RADLE
        FNT1_X(JEQU1) = VALUE - FNEWT1(JEQU1) 
      ELSEIF (IFTP1(JEQU1) .EQ. 107) THEN 
        VALUE = THKMAXE
        FNT1_X(JEQU1) = VALUE - FNEWT1(JEQU1) 
      ELSEIF (IFTP1(JEQU1) .EQ. 121) THEN 
        VALUE = thkmaxg
        FNT1_X(JEQU1) = VALUE - FNEWT1(JEQU1) 
      ELSEIF (IFTP1(JEQU1) .EQ. 122) THEN 
        VALUE = fgensym
        FNT1_X(JEQU1) = VALUE - FNEWT1(JEQU1) 
      ELSEIF (IFTP1(JEQU1) .EQ. 135) THEN
        CALL AFAREA(1)
        VALUE = AREA
        FNT1_X(JEQU1) = VALUE - FNEWT1(JEQU1) 
      ELSEIF (IFTP1(JEQU1) .EQ. 146) THEN
        CALL BULBVOL
        VALUE = VOLUME_B
        FNT1_X(JEQU1) = VALUE - FNEWT1(JEQU1) 
      ELSEIF (IFTP1(JEQU1) .EQ. 155) THEN
        VALUE = TEX
        FNT1_X(JEQU1) = VALUE  - FNEWT1(JEQU1) 
      ELSEIF (IFTP1(JEQU1) .EQ. 156) THEN
        VALUE = TEY
        FNT1_X(JEQU1) = VALUE  - FNEWT1(JEQU1) 
      ELSEIF (IFTP1(JEQU1) .EQ. 157) THEN
        VALUE = XTEMIDPT
        FNT1_X(JEQU1) = VALUE  - FNEWT1(JEQU1) 
      ELSEIF (IFTP1(JEQU1) .EQ. 158) THEN
        VALUE = YTEMIDPT
        FNT1_X(JEQU1) = VALUE  - FNEWT1(JEQU1) 
      ELSEIF (IFTP1(JEQU1) .EQ. 184) THEN 
        VALUE = OMEGA(1)
        FNT1_X(JEQU1) = VALUE - FNEWT1(JEQU1) 
      ELSEIF (IFTP1(JEQU1) .EQ. 185) THEN 
        VALUE = OMEGA(2)
        FNT1_X(JEQU1) = VALUE - FNEWT1(JEQU1) 
      ELSEIF (IFTP1(JEQU1) .EQ. 190) THEN
        VALUE = UMU(1)
        FNT1_X(JEQU1) = VALUE - FNEWT1(JEQU1)
      ELSEIF (IFTP1(JEQU1) .EQ. 191) THEN 
        VALUE = UMU(2)
        FNT1_X(JEQU1) = VALUE - FNEWT1(JEQU1)
      ELSEIF (IFTP1(JEQU1) .EQ. 192) THEN 
        VALUE = HKH(1)
        FNT1_X(JEQU1) = VALUE - FNEWT1(JEQU1)
      ELSEIF (IFTP1(JEQU1) .EQ. 193) THEN 
        VALUE = HKH(2)
        FNT1_X(JEQU1) = VALUE - FNEWT1(JEQU1)
      ELSEIF (IFTP1(JEQU1) .EQ. 194) THEN 
        UMUSS = - DLOG(FNEWT1(JEQU1))/
     &       DLOG(1. +  AKA(1)*((1. - DCOSG(AW(1)))/
     &       (1. + DCOSG(AW(1)))))
        VALUE = UMU(1)
        FNT1_X(JEQU1) = VALUE - UMUSS
      ELSEIF (IFTP1(JEQU1) .EQ. 195) THEN 
        UMUSS = - DLOG(FNEWT1(JEQU1))/
     &       DLOG(1. +  AKA(2)*((1. - DCOSG(AW(2)))/
     &       (1. + DCOSG(AW(2)))))
        VALUE = UMU(2)
        FNT1_X(JEQU1) = VALUE - UMUSS
      ELSEIF (IFTP1(JEQU1) .EQ. 196) THEN 
        UMUSS = 0.5 * FNEWT1(JEQU1) * (1. + DCOSG(AW(1)))/AKA(1)
        VALUE = UMU(1)
        FNT1_X(JEQU1) = VALUE - UMUSS
      ELSEIF (IFTP1(JEQU1) .EQ. 197) THEN 
        UMUSS = 0.5 * FNEWT1(JEQU1) * (1. + DCOSG(AW(2)))/AKA(2)
        VALUE = UMU(2)
        FNT1_X(JEQU1) = VALUE - UMUSS 
      ELSEIF (IFTP1(JEQU1) .EQ. 198) THEN 
        UMUSS = (HKH(1) * DLOG(.64D0) - DLOG(FNEWT1(JEQU1)))/
     &       DLOG(1. +  AKA(1)*((1. - DCOSG(AW(1)))/
     &       (1. + DCOSG(AW(1)))))
        VALUE = UMU(1)
        FNT1_X(JEQU1) = VALUE - UMUSS 
      ELSEIF (IFTP1(JEQU1) .EQ. 199) THEN 
        UMUSS = (HKH(2) * DLOG(.64D0) - DLOG(FNEWT1(JEQU1)))/
     &       DLOG(1. +  AKA(2)*((1. - DCOSG(AW(2)))/
     &       (1. + DCOSG(AW(2)))))
        VALUE = UMU(2)
        FNT1_X(JEQU1) = VALUE - UMUSS 
      ELSE
        WRITE(lu06,*) ' Error 169: Newton type not found (nt1g0.f)'
      ENDIF
      RETURN
      END ! NT1G0

