      
      SUBROUTINE NEWTSTP
C***********************************************************************
C...  Do the [Newt]on [st]e[p]s 
C
C     Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      ITER   = 0
      LCONSEC = FF
C---  set rhs of Newton system and set (0) values 
      CALL DEPEND
      IF(LNEWT1) CALL FNT1(0)
      IF(LNEWT2) CALL FNT2(0)
C---  Flag LJACOBI is used to speed up Newton iteration (Ashok Goplarathnam).
C---  This is true for the start of each stage in the iteration.
C---  The flag is hardcoded to start here, but can change in sensi.f.
      LJACOBI = TT
      DO 101 JEQU1 = 1, IEQU1
        JEQU = JEQU1 
        DI(JEQU)    = -FNT1_0(JEQU1)
 101  CONTINUE
      DO 102 JEQU2 = 1, IEQU2
        JEQU = IEQU1 + JEQU2 
        DI(JEQU)    = -FNT2_0(JEQU2)
 102  CONTINUE
C---  write total design, FNT1_0 and FNT2_0
      CALL SWRITE(1,0)
      IF(LNEWT1) CALL SWRITE(2,0)
      IF(LNEWT2) CALL SWRITE(3,0)
C---  determine whether or not to take a Newton iteration step
      CALL SETLSTP
      IF(.NOT.LSTEP) RETURN
C---  set small perturbation in Newton variables
      IF(LJACOBI) THEN
         CALL SETPER
         CALL SETPERV
      ENDIF
C     
C---  "do until" to converge airfoil---see 6-22-90 p. 3-4
 10   CONTINUE
      ITER = ITER + 1
      WRITE(lu06,1000) ITER
 1000 FORMAT(2X,' Iteration',I4)
C---  start: Newton iteration
C     compute lhs Newton system coefficients
C     loop over variables JVAR1 and JVAR2
C     step columnwise through lhs of Newton system
      IF(LJACOBI) THEN
         DO 301 JVAR1 = 1, IVAR1
            WRITE(lu06,1001) JVAR1
 1001       FORMAT(2X,' Calculating partials for airfoil design ',
     &           'parameter: ',I3)
            CALL SETNEW(JVAR1,-1,-1)
            CALL CHECK(1)
            CALL DESIGN
            IF(LECHO) CALL ECHO(1)
            CALL DEPEND
            CALL FNT1(-1)
            IF(LNEWT2) CALL FNT2(-1)
            CALL JACOBI1(JVAR1)
            CALL SETNEW(JVAR1,-1,1)
 301     CONTINUE
         DO 302 JVAR2 = 1, IVAR2
            WRITE(lu06,1002) JVAR2
 1002       FORMAT(2X,' Calculating partials for velocity at ',
     &           'Newton node:  ',I3)
            CALL SETNEWV(JVAR2,-1,-1)
            CALL CHECK(1)
            CALL DESIGN
            IF(LECHO) CALL ECHO(1)
            CALL DEPEND
            IF(LNEWT1) CALL FNT1(-1)
            CALL FNT2(-1)
            CALL JACOBI2(JVAR2)
            CALL SETNEWV(JVAR2,-1,1)
 302     CONTINUE
         IF(LSENSI) CALL SENSI
      ELSE
         CALL CPSENSI
         WRITE(lu06,*) '   Using converged Jacobian'
      ENDIF
      CALL SYSNEWT
C---  finish: Newton iteration
C     
C---  start: design of complete new airfoil (p+1)
      IF (LJACOBI) THEN
         IF(LCLAMP1 .OR. LCLAMP2) CALL CLAMP
      ELSE
         RELAX = 1.
      ENDIF
      DO 401 JVAR1 = 1, IVAR1
        DELTAS(JVAR1) = RELAX * DELTAS(JVAR1)
        CALL SETNEW(JVAR1,1,1)
 401  CONTINUE
      DO 402 JVAR2 = 1, IVAR2
        DELTAVS(JVAR2) = RELAX * DELTAVS(JVAR2)
        CALL SETNEWV(JVAR2,1,1)
 402  CONTINUE
      CALL CHECK(1)
      CALL DESIGN
      CALL DEPEND
      IF(LNEWT1) CALL FNT1(1)
      IF(LNEWT2) CALL FNT2(1)
      IF(LECHO) CALL ECHO(2)
C---  finish: design of complete new airfoil (p+1)
C     
C---  start: convergence check 
C     write total design, FNT1_1(.), FNT2_1(.), and deltas
      CALL SWRITE(1,0)
      IF(LNEWT1) CALL SWRITE(2,1)
      IF(LNEWT2) CALL SWRITE(3,1)
C---  determine whether or not to take a Newton iteration step
      CALL SETLSTP
      IF (LSTEP) THEN
        DO 601 JEQU1 = 1, IEQU1
          JEQU = JEQU1
          FNT1_0(JEQU1) =  FNT1_1(JEQU1)
          DI(JEQU)      = -FNT1_0(JEQU1)
 601    CONTINUE
        DO 602 JEQU2 = 1, IEQU2
          JEQU = IEQU1 + JEQU2
          FNT2_0(JEQU2) =  FNT2_1(JEQU2)
          DI(JEQU)      = -FNT2_0(JEQU2)
 602    CONTINUE
        CALL SETPER
        CALL SETPERV
      ENDIF
C---  finish: convergence check
      IF(LSTEP) GOTO 10
C     
      RETURN
      END ! NEWTSTP

