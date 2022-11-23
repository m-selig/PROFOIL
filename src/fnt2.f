
      SUBROUTINE FNT2(JMODE)
C***********************************************************************
C...Evaluate the Newton functions
C   JMODE = -1 --> FNT2_P(.)
C   JMODE =  0 --> FNT2_0(.)
C   JMODE =  1 --> FNT2_1(.)
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER JMODE
      IF(.NOT. LADJSS) RETURN
      JEQU2 = 0
      DO 50 JADJS = 1, IADJS
         JSEG   = JSEGIX2(JADJS)
         LBE    = LLBE(JADJS)
         JSPSEG = KSPSEG(JSEG)
         ISBSEG = KSBSEG(JSEG)
C--------fixed transition data
         LFXTR = LLFXTR2(JADJS)
         IF(LFXTR) THEN
            JSEGTR = JSEGTR2(JADJS)
            LBOSTR = LLBETR(JADJS)
         ENDIF
C--------get arc length at BOS and local arc length to Newton nodes
C        in direction from either BOS or EOS.
         CALL PHITOS(JSEG)
C--------compute SSFNN(.) at Newton nodes at the local arc lengths
         IF(LSNEWT2) THEN
            CALL FSSSPL(JADJS)
         ELSE
            CALL FSS(JADJS)
         ENDIF
         IF(IFTP2(JADJS) .EQ. 100) THEN
C-----------compute FJSBSEG(.) at Newton nodes
            CALL NEWTVEL
            DO 100 JSBSEG = 1, ISBSEG
               JEQU2 = JEQU2 + 1
               FNT2_X(JEQU2) = FJSBSEG(JSBSEG) - SSFNN(JSBSEG)
  100       CONTINUE             
         ELSEIF(     IFTP2(JADJS) .EQ. 200
     &          .OR. IFTP2(JADJS) .EQ. 201
     &          .OR. IFTP2(JADJS) .EQ. 202
     &          .OR. IFTP2(JADJS) .EQ. 203
     &          .OR. IFTP2(JADJS) .EQ. 204
     &          .OR. IFTP2(JADJS) .EQ. 205) THEN
C-----------Alfa^* is used in packing subroutines
            RINF = R1(JADJS)
C-----------Setting up to compute FJSBSEG(.) at Newton nodes
            CALL PACKER2(JSEG)
C-----------check to see if transition is fixed for current bl analysis.
C-----------if so use JSEGTR, LBOSTR, and SSTAG (+) to get SBLTR and VBLTR
            IF (LFXTR) CALL SVBLTR
C-----------Run the boundary layer analysis for SBL(.) and VBL(.) and 
C           return FJSBSEG(.) at Newton nodes
            CALL PROBL
            CALL NEWTBL(2,IFTP2(JADJS))
            DO 200 JSBSEG = 1, ISBSEG
               JEQU2 = JEQU2 + 1
               FNT2_X(JEQU2) = FJSBSEG(JSBSEG) - SSFNN(JSBSEG)
  200       CONTINUE             
         ELSEIF(IFTP2(JADJS) .EQ. 300) THEN
C-----------H32 spec for given Re and alfa
C-----------need JSEG and LBOS for packer1
C           JSEG = JSEGIX2(JADJS) ... this is set at top of do loop
            LBOS = .NOT. LBE
C-----------Note that LBE also gets re-evaluated/re-set in packer1.f
C-----------conditions:
            RINF   = R1(JADJS)
            ALFABL = R2(JADJS)
C-----------Setting up to compute FJSBSEG(.) at Newton nodes
            CALL PACKER2(JSEG)
C-----------check to see if transition is fixed for current bl analysis.
C-----------if so use JSEGTR, LBOSTR, and SSTAG (+) to get SBLTR and VBLTR
            IF (LFXTR) CALL SVBLTR
C-----------Run the boundary layer analysis for SBL(.) and VBL(.) and 
C           return FJSBSEG(.) at Newton nodes
            CALL PROBL
            CALL NEWTBL(2,IFTP2(JADJS))
            DO 302 JSBSEG = 1, ISBSEG
              JEQU2 = JEQU2 + 1
              FNT2_X(JEQU2) = FJSBSEG(JSBSEG) - SSFNN(JSBSEG)
 302        CONTINUE             
         ELSEIF(IFTP2(JADJS) .EQ. 500) THEN
C-----------Bubble ramp distribution for given Rinf
            RINF = R1(JADJS)
C-----------Alfa^* on segment in front of ramp 
C--------   is used in packing subroutines for initial ramp condition.
            IF(LBE) THEN
               JSEGI = JSEG - 1
               LBOS = FF
               ALFAI = ALFAS(JSEGI)
            ELSE
               JSEGI = JSEG + 1
               LBOS = TT
               ALFAI = ALFAS(JSEGI)
            ENDIF
C-----------compute the initial H12 for the ramp as 
C-----------the initial ramp condition: FIRC
            ALFABL = ALFAI
            CALL PACKER1(JSEGI)
            CALL PROBL
            CALL NEWTBL(1,IFTP2(JADJS))
            FIRC = BLNEWT
C-----------loop over prescribed angles of attack
            DO 500 JSBSEGT = 1, ISBSEG
               ALFABL = ALFAI + SSFNN(JSBSEGT)
               JEQU2 = JEQU2 + 1
C--------------setting up to compute BLNEWT at Newton node
               CALL PACKER3(JSEG,JSBSEGT)
C--------------run the boundary-layer analysis for SBL(.) and VBL(.) and 
C-----------   return BLNEWT at Newton node
               CALL PROBL
               CALL NEWTBL(1,IFTP2(JADJS))
               FNT2_X(JEQU2) = BLNEWT - FIRC 
  500       CONTINUE
         ELSE 
            WRITE (lu06,*) '  Error 149: iftp1(.) not found (fnt2.f)'
            STOP
         ENDIF
   50 CONTINUE
      DO 350 JEQU2 = 1, IEQU2
         IF(JMODE .EQ. -1) THEN
            FNT2_P(JEQU2) = FNT2_X(JEQU2)
         ELSEIF(JMODE .EQ. 0) THEN
            FNT2_0(JEQU2) = FNT2_X(JEQU2)
         ELSE
            FNT2_1(JEQU2) = FNT2_X(JEQU2)
         ENDIF
  350 CONTINUE
      RETURN
      END ! FNT2

