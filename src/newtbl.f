
      SUBROUTINE NEWTBL(NEWTMD,NEWTTP)
C***********************************************************************
C...Evaluate the Newton functions for Bl Rx
C
C...Requires:
C      SBL(IBL)         boundary layer arc length
C      VBL(IBL)         boundary layer edge velocity
C      IBL              number of boundary layer points
C      RINF             freestream Reynolds number
C      and H12BL(.), RD2BL(.), D2BL(.)
C
C...NEWTMD Newton mode: 1 = NEWT1*
C                       2 = NEWT2*
C
C...If NEWTMD = 1: 
C      INDXBL(1) = IBL 
C      IPACK     = 1
C   --->Returns:   BLNEWT = single dependent Newton variable 
C   
C...If NEWTMD = 2: 
C      INDXBL(IPACK) IPACK = number of Newton nodes + 1
C                    JPACK = 1 beginning of the segment
C                    JPACK = 2...IPACK Newton nodes
C   ---> Returns:  FSBSEG(IPACK-1)  dependent Newton variable at Newton nodes
C
C
C...NEWTTP  type of dependent Newton variable for either mode
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      INTEGER NEWTMD, NEWTTP
      JPACK = 1
C-----set the dependent Newton variable(s)
      IF (NEWTMD .EQ. 2) THEN
C--------do preliminary calculations first
         IF(NEWTTP .EQ. 202) THEN
            CALL INITENN
            CALL FSENN
         ENDIF
         IF(NEWTTP .EQ. 203) THEN
            CALL DRLN
         ENDIF
C--------pack the temporary arrays
         DO 130 JPACK = 1, IPACK
            JBL = INDXBL(JPACK)
            IF    (NEWTTP .EQ. 200) THEN
               STEMP(JPACK) = H12BL(JBL)
            ELSEIF(NEWTTP .EQ. 201) THEN
               STEMP(JPACK) = TC(H32BL(JBL), RD2BL(JBL), EM, ER)
            ELSEIF(NEWTTP .EQ. 202) THEN
               CALL ENVLPE(JBL)
               STEMP(JPACK) = ENNMAX
            ELSEIF(NEWTTP .EQ. 203) THEN
               STEMP(JPACK) = DRLNBL(JBL)
            ELSEIF(NEWTTP .EQ. 204) THEN
               STEMP(JPACK) = H32BL(JBL)
            ELSEIF(NEWTTP .EQ. 205) THEN
               STEMP(JPACK) = CFBL(JBL)
            ELSEIF(NEWTTP .EQ. 300) THEN
               STEMP(JPACK) = H32BL(JBL)
            ELSE
               WRITE(lu06,*) 
     $             ' Error 163: Newton type not found (newtbl.f)'
            ENDIF
  130    CONTINUE
C--------determine the relative function for segment
         DO 500 JSBSEG = 1, ISBSEG
            FJSBSEG(JSBSEG) = STEMP(JSBSEG+1) - STEMP(1)
  500    CONTINUE
      ELSEIF(NEWTMD .EQ. 1) THEN
         IF     (NEWTTP .EQ. 500) THEN
            BLNEWT = H12BL(IBL)
         ELSEIF (NEWTTP .EQ. 501) THEN
            BLNEWT = RD2BL(IBL)
         ELSEIF (NEWTTP .EQ. 502) THEN
            BLNEWT = TC(H32BL(IBL), RD2BL(IBL), EM, ER)
         ELSEIF (NEWTTP .EQ. 503) THEN
            CALL INITENN
            CALL FSENN
            CALL ENVLPE(IBL)
            BLNEWT = ENNMAX
         ELSEIF (NEWTTP .EQ. 504) THEN
            CALL DRLN
            BLNEWT = DRLNBL(IBL)
         ELSEIF (NEWTTP .EQ. 505) THEN
            BLNEWT = H32BL(IBL)
         ELSEIF (NEWTTP .EQ. 506) THEN
            BLNEWT = CFBL(IBL)
         ELSEIF (NEWTTP .EQ. 600) THEN
C-----------H_12 for given alfa and Rinf
            BLNEWT = H12BL(IBL)
         ELSEIF (NEWTTP .EQ. 601) THEN
C-----------H_32 for given alfa and Rinf
            BLNEWT = H32BL(IBL)
         ELSE
            WRITE(lu06,*) 
     $          ' Error 164: Newton type not found (newtbl.f)'
         ENDIF
      ENDIF
      RETURN
      END ! NEWTBL

