
      SUBROUTINE VELDIST
C***********************************************************************
C...  Compute the [vel]ocity [dist]ribution for given angles of attack
C
C     Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'

C-----veldist file
      OPEN(UNIT = 60,FILE = FILE60,status = 'unknown')

      IF(.NOT.LALFASP) CALL ALFASP
C-----compute the velocity at the specified IALF angles of attack
      DO 200 JALF = 1, IALF
C--------arc limit at LE stagnation point
        AMVEL0 = AMSTAG(ALFA(JALF))
        LSTAG = FF
        DO 300 IPT = 1, IARGP+1
          IF(LSTAG) GOTO 301
          IF(APHI(IPT) .EQ. AMVEL0) THEN
C...  Stagnation point falls on a coordinate point.
C     So the stagnation point velocity distribution is not written
            WRITE(LU06,*) ' Stagnation point falls on coordinate.'
            LSTAG = TT 
C... Write the point out away if the flag below is set
            IF (LVELF0) THEN
C... Note this does not yet compute the x-stag point
            WRITE(LU06,*) 
     $             ' Skip writing of extra point to v-dist files.'
            XVEL0 =  (AMVEL0     - APHI(IPT-1))
     &           /(APHI(IPT)  - APHI(IPT-1))
     &           *(XCRD(IPT)  - XCRD(IPT-1))
     &           + XCRD(IPT-1)
            SVEL0 =  (AMVEL0     -  APHI(IPT-1))
     &           /(APHI(IPT)  -  APHI(IPT-1))
     &           *(SLENS(IPT) - SLENS(IPT-1))
     &           + SLENS(IPT-1)
              IF(LPRT40) WRITE(40,*) SVEL0,  '  0.0 '
              IF(LPRT50) WRITE(50,*) XVEL0,  '  0.0 '
              IF(LPRT60) WRITE(60,*) AMVEL0, '  0.0 '
            ENDIF
            GOTO 301
          ENDIF
          IF(APHI(IPT) .GT. AMVEL0) THEN
C... Note this does not yet compute the x-stag point
            XVEL0 =  (AMVEL0     - APHI(IPT-1))
     &           /(APHI(IPT)  - APHI(IPT-1))
     &           *(XCRD(IPT)  - XCRD(IPT-1))
     &           + XCRD(IPT-1)
            SVEL0 =  (AMVEL0     -  APHI(IPT-1))
     &           /(APHI(IPT)  -  APHI(IPT-1))
     &           *(SLENS(IPT) - SLENS(IPT-1))
     &           + SLENS(IPT-1)
            IF(LPRT40) WRITE(40,*) SVEL0,  '  0.0 '
            IF(LPRT50) WRITE(50,*) XVEL0,  '  0.0 '
            IF(LPRT60) WRITE(60,*) AMVEL0, '  0.0 '
            LSTAG = TT
          ENDIF 
 301      VEL = 2. * DABS(DCOSG(0.5*APHI(IPT) - ALFA(JALF)))
     &         * ARGPV(IPT)
          IF(LPRT40) WRITE(40,*) SLENS(IPT), VEL !, APHI(IPT)/SCLF
          IF(LPRT50) WRITE(50,*) XCRD(IPT),  VEL !, APHI(IPT)/SCLF
          IF(LPRT60) WRITE(60,*) APHI(IPT),  VEL !, APHI(IPT)/SCLF
 300    CONTINUE
 200  CONTINUE
      IF(IALF .NE. 0) THEN
        WRITE(LU06,*)             '*********OUTPUT**************'
        IF(LPRT40)  WRITE(LU06,*) '* v(s)       --> FOR040.DAT *'
        IF(LPRT50)  WRITE(LU06,*) '* v(x)       --> FOR050.DAT *'
        IF(LPRT60)  WRITE(LU06,*) '* v(phi)     --> FOR060.DAT *'
        WRITE(LU06,*)             '*********OUTPUT**************'
        CLOSE (40)
        CLOSE (50)
        CLOSE (60)
      ENDIF
      RETURN
      END ! VELDIST

