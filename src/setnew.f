
      SUBROUTINE SETNEW(JVAR1,ISIGN1,ISIGN2)
C***********************************************************************
C...  Adds (ISIGN2 = 1) or subtracts (ISIGN2 = -1) 
C     a small perturbation to the variable ITP1(JVAR1) ITP2(JVAR1)
C
C     Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER JVAR1
      IF(ISIGN1 .LT. 0) THEN
        DELTAJ = DELTAP(JVAR1)
      ELSE
        DELTAJ = DELTAS(JVAR1)       
      ENDIF
      IF(ISIGN2 .LT. 0) THEN
        DELTAJ = -DELTAJ
      ELSE
        DELTAJ =  DELTAJ
      ENDIF
C...
      IF (ITP1(JVAR1) .EQ. 0) THEN
C...  misc single parameters  ITP1 = O
        IF (ITP2(JVAR1) .EQ. 1) THEN
C...  DELSX
          DELSX = DELSX + DELTAJ
        ELSEIF (ITP2(JVAR1) .EQ. 2) THEN
C...  DELSY
          DELSY = DELSY + DELTAJ
        ELSEIF (ITP2(JVAR1) .EQ. 3) THEN
C...  XMAPOFF
          XMAPOFF = XMAPOFF + DELTAJ
        ELSEIF (ITP2(JVAR1) .EQ. 4) THEN
C...  YMAPOFF
          YMAPOFF = YMAPOFF + DELTAJ
        ELSE
          WRITE(LU06,*) 'Error 195: ICASE not found (setnew.f)'
        ENDIF

      ELSEIF (ITP1(JVAR1) .EQ. 1) THEN
        IF (ITP2(JVAR1) .LT. ISEG) THEN
          AM(ITP2(JVAR1)) =  AM(ITP2(JVAR1)) + DELTAJ
        ELSEIF (ITP2(JVAR1) .LT. 1000) THEN
          ICASE = ITP2(JVAR1)/100
          IF (ICASE .EQ. 1) THEN 
            DO 111 JSEG = 1, ILE-1
              AM(JSEG) = AM(JSEG) + DELTAJ
 111        CONTINUE
            AW(1) = AW(1) + DELTAJ
          ELSEIF (ICASE .EQ. 2) THEN
            DO 121 JSEG = ILE+1, ISEG-1
              AM(JSEG) = AM(JSEG) + DELTAJ
 121        CONTINUE
            AW(2) = AW(2) + DELTAJ
          ELSEIF (ICASE .EQ. 3) THEN
            DO 131 JSEG = 1, ILE-1
              AM(JSEG) = AM(JSEG) + DELTAJ
 131        CONTINUE
            DO 132 JSEG = ILE+1, ISEG-1
              AM(JSEG) = AM(JSEG) - DELTAJ
 132        CONTINUE
            AW(1) = AW(1) + DELTAJ
            AW(2) = AW(2) - DELTAJ
          ELSEIF (ICASE .EQ. 4) THEN
            DO 141 JSEG = 1, ILE-1
              AM(JSEG) = AM(JSEG) + DELTAJ
 141        CONTINUE
            DO 142 JSEG = ILE+1, ISEG-1
              AM(JSEG) = AM(JSEG) + DELTAJ
 142        CONTINUE
            AW(1) = AW(1) + DELTAJ
            AW(2) = AW(2) + DELTAJ
          ELSEIF (ICASE .EQ. 8) THEN
            DO  JSEG = 1, ISEG - 1
              AM(JSEG) = AM(JSEG) + DELTAJ
            ENDDO
            AW(1) = AW(1) + DELTAJ
            AW(2) = AW(2) + DELTAJ
          ELSE
            WRITE(LU06,*) 'Error 196: ICASE not found (setnew.f)'
          ENDIF
        ELSE
          WRITE(LU06,*) 'Error 197: ICASE not found (setnew.f)'
        ENDIF

      ELSEIF (ITP1(JVAR1) .EQ. 2) THEN
        ICASE = ITP2(JVAR1)/100
        IF (ICASE .EQ. 1) THEN 
          AM(1) = AM(1) + DELTAJ
          AW(1) = AW(1) + DELTAJ
        ELSEIF (ICASE .EQ. 2) THEN
          AM(ISEG-1) = AM(ISEG-1) + DELTAJ
          AW(2)      = AW(2)      + DELTAJ
        ELSEIF (ICASE .EQ. 3) THEN
          AM(1) = AM(1) + DELTAJ
          AW(1) = AW(1) + DELTAJ
          AM(ISEG-1) = AM(ISEG-1) - DELTAJ
          AW(2)      = AW(2)      - DELTAJ
        ELSEIF (ICASE .EQ. 4) THEN
          AM(1) = AM(1) + DELTAJ
          AW(1) = AW(1) + DELTAJ
          AM(ISEG-1) = AM(ISEG-1) + DELTAJ
          AW(2)      = AW(2)      + DELTAJ
        ELSE
          WRITE(LU06,*) 'Error 198: ICASE not found (setnew.f)'
        ENDIF

      ELSEIF (ITP1(JVAR1) .EQ. 3) THEN
        ICASE = ITP2(JVAR1)/100
        IF (ICASE .EQ. 1) THEN 
          AS(1) = AS(1) + DELTAJ
        ELSEIF (ICASE .EQ. 2) THEN
          AS(2) = AS(2) + DELTAJ
        ELSEIF (ICASE .EQ. 3) THEN
          AS(1) = AS(1) + DELTAJ
          AS(2) = AS(2) - DELTAJ
        ELSEIF (ICASE .EQ. 4) THEN
          AS(1) = AS(1) + DELTAJ
          AS(2) = AS(2) + DELTAJ
        ELSE
          WRITE(LU06,*) 'Error 199: ICASE not found (setnew.f)'
        ENDIF

      ELSEIF (ITP1(JVAR1) .EQ. 4) THEN
        VS(IVEL) = VS(IVEL) + DELTAJ

      ELSEIF (ITP1(JVAR1) .EQ. 5) THEN
        IF (ITP2(JVAR1) .LE. ISEG) THEN
          JSEG = ITP2(JVAR1)
          IF (IDVTP(JSEG) .EQ. 1) THEN
C---  linear velocity distribution for entire segment
            VTILDE(JSEG) = VTILDE(JSEG) + DELTAJ
          ELSEIF (IDVTP(JSEG) .EQ. 2) THEN
C---  parabolic velocity distribution for entire segment
            VTILDE(JSEG) = VTILDE(JSEG) + DELTAJ
          ELSEIF (IDVTP(JSEG) .EQ. 3) THEN
C---  linear in x/c approximately for entire segment
            VTILDE(JSEG) = VTILDE(JSEG) + DELTAJ
          ELSEIF (IDVTP(JSEG) .EQ. 11 
     $           .OR. IDVTP(JSEG) .EQ. 12
     $           .OR. IDVTP(JSEG) .EQ. 13
     $           .OR. IDVTP(JSEG) .EQ. 14
     $           .OR. IDVTP(JSEG) .EQ. 15
     $           .OR. IDVTP(JSEG) .EQ. 16) THEN
c...  added special segment capability 020120 (see PROFOIL notes "020120")
            jspseg = kspseg(jseg)
            isbseg = ksbseg(jseg)
c...  determine the new veos velocity
            veos_old = ssdelv(jspseg,isbseg)
            veos_new = ssdelv(jspseg,isbseg) + deltaj
c...  scale factor to apply to all subsegments
            scale_factor = veos_new/veos_old
            do jsbseg = 1, isbseg
              ssdelv(jspseg,jsbseg) = scale_factor*ssdelv(jspseg,jsbseg)
            enddo
          ENDIF
        ELSEIF (ITP2(JVAR1) .LT. 1000) THEN
          ICASE = ITP2(JVAR1)/100
          IF (ICASE .EQ. 1) THEN 
            DO 511 JSEG = 2, ILE-1
              IF(IDVTP(JSEG) .GE. 1 .AND. IDVTP(JSEG) .LE. 3) THEN
                VTILDE(JSEG) = VTILDE(JSEG) + DELTAJ
              ENDIF
 511        CONTINUE
          ELSEIF (ICASE .EQ. 2) THEN
            DO 521 JSEG = ILE+1, ISEG-1
              IF(IDVTP(JSEG) .GE. 1 .AND. IDVTP(JSEG) .LE. 3) THEN
                VTILDE(JSEG) = VTILDE(JSEG) + DELTAJ
              ENDIF
 521        CONTINUE
          ELSEIF (ICASE .EQ. 3) THEN
            DO 531 JSEG = 2, ILE-1
              IF(IDVTP(JSEG) .GE. 1 .AND. IDVTP(JSEG) .LE. 3) THEN
                VTILDE(JSEG) = VTILDE(JSEG) + DELTAJ
              ENDIF
 531        CONTINUE
            DO 532 JSEG = ILE+1, ISEG-1
              IF(IDVTP(JSEG) .GE. 1 .AND. IDVTP(JSEG) .LE. 3) THEN
                VTILDE(JSEG) = VTILDE(JSEG) - DELTAJ
              ENDIF
 532        CONTINUE
          ELSEIF (ICASE .EQ. 4) THEN
            DO 541 JSEG = 2, ILE-1
              IF(IDVTP(JSEG) .GE. 1 .AND. IDVTP(JSEG) .LE. 3) THEN
                VTILDE(JSEG) = VTILDE(JSEG) + DELTAJ
              ENDIF
 541        CONTINUE
            DO 542 JSEG = ILE+1, ISEG-1
              IF(IDVTP(JSEG) .GE. 1 .AND. IDVTP(JSEG) .LE. 3) THEN
                VTILDE(JSEG) = VTILDE(JSEG) + DELTAJ
              ENDIF
 542        CONTINUE
          ELSE
            WRITE(LU06,*) 'Error 200: ICASE not found (setnew.f)'
          ENDIF
        ELSE
          WRITE(LU06,*) 'Error 201: ICASE not found (setnew.f)'
        ENDIF

      ELSEIF (ITP1(JVAR1) .EQ. 6) THEN
        IF (ITP2(JVAR1) .LE. ISEG) THEN
          ALFAS(ITP2(JVAR1)) = ALFAS(ITP2(JVAR1)) + DELTAJ
        ELSEIF (ITP2(JVAR1) .LT. 1000) THEN
          ICASE = ITP2(JVAR1)/100
          IF (ICASE .EQ. 1) THEN 
            DO 211 I = 1, ILE
              ALFAS(I) = ALFAS(I) + DELTAJ
 211        CONTINUE
          ELSEIF (ICASE .EQ. 2) THEN
            DO 221 I = ILE+1, ISEG
              ALFAS(I) = ALFAS(I) + DELTAJ
 221        CONTINUE
          ELSEIF (ICASE .EQ. 3) THEN
            DO 231 I = 1, ILE 
              ALFAS(I) = ALFAS(I) + DELTAJ
 231        CONTINUE
            DO 232 I = ILE+1, ISEG
              ALFAS(I) = ALFAS(I) - DELTAJ
 232        CONTINUE
          ELSEIF (ICASE .EQ. 4) THEN
            DO 241 I = 1, ILE 
              ALFAS(I) = ALFAS(I) + DELTAJ
 241        CONTINUE
            DO 242 I = ILE+1, ISEG
              ALFAS(I) = ALFAS(I) + DELTAJ
 242        CONTINUE
          ELSE
            WRITE(LU06,*) 'Error 202: ICASE not found (setnew.f)'
          ENDIF
        ELSE
          WRITE(LU06,*) 'Error 203: ICASE not found (setnew.f)'
        ENDIF

      ELSEIF (ITP1(JVAR1) .EQ. 7) THEN
        ICASE = ITP2(JVAR1)/100
        IF (ICASE .EQ. 1) THEN 
          AKA(1) = AKA(1) + DELTAJ
        ELSEIF (ICASE .EQ. 2) THEN
          AKA(2) = AKA(2) + DELTAJ
        ELSEIF (ICASE .EQ. 3) THEN
          AKA(1) = AKA(1) + DELTAJ
          AKA(2) = AKA(2) - DELTAJ
        ELSEIF (ICASE .EQ. 4) THEN
          AKA(1) = AKA(1) + DELTAJ
          AKA(2) = AKA(2) + DELTAJ
        ELSE
          WRITE(LU06,*) 'Error 204: ICASE not found (setnew.f)'
        ENDIF

      ELSE
        WRITE(LU06,*) 'Error 205: ICASE not found (setnew.f)'
      ENDIF
      RETURN
      END ! SETNEW

