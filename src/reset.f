      
      SUBROUTINE RESET(A)
C***********************************************************************
C... Reset PRTDES logical to default values
C
C    Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      CHARACTER(len=*) A
      IF(A(1:7) .EQ. 'PRINT__') THEN
        LPRT10  = FF
        LPRT20  = FF
        LPRT30  = FF
        LPRT40  = FF
        LPRT50  = FF
        LPRT60  = FF
        LPRT70  = FF
        LPRT80  = FF
        LPRT90  = FF
        LPRT100 = FF
        LPRT101 = TT
        DO 100 JPRT = 1, 10
          IPRT(JPRT) = 0
 100    CONTINUE
      ELSEIF(A(1:7) .EQ. 'NEWT___') THEN
        LMANUAL = TT
        IEQU1   = 0
        IVAR1   = 0
        IADJS   = 0
        IEQU2   = 0
        IVAR2   = 0
        ISTAGE  = 0
        LCLAMP1 = FF
        LCLAMP2 = FF
        LITON   = FF
        LADJSS  = FF
        LSNEWT2 = FF
        LNEWT1  = FF
        LNEWT2  = FF
C---  default Newton function logicals:
        LARGHP  = FF
        LHQ     = FF
        LHP     = FF
        LCOORD  = FF
        LNORM   = FF
        LSLENS  = FF
        LTHICK  = FF
        LTHICKE = FF
        LMOMENT = FF
C... other Newton stuff
        XMAPOFF = 0
        YMAPOFF = 0
        DELSX   = 0
        DELSY   = 0
        LTEZERO = TT
      ELSEIF(A(1:7) .EQ. 'FOIL___') THEN
        JFOIL   = 0
        ISEG    = 0
        ISPSEG  = 0
        LWGHT   = FF
        LLDELV  = FF
        LFTE    = FF
        LSYM    = FF
C---  initialize array values
        DO 300 JSEG = 1, NSEG
          LDELV(JSEG)  = FF
          LDELVSC(JSEG)= FF
          KSPSEG(JSEG) = 0
          KSBSEG(JSEG) = 0
          IDVTP(JSEG)  = 0
          ULOG(JSEG)   = 0.
          URGO(JSEG)   = 0.
          UCOS(JSEG)   = 0.
          USIN(JSEG)   = 0
 300    CONTINUE
      ELSEIF(A(1:7) .EQ. 'BLA____') THEN
        IAN     = 0
      ELSEIF(A(1:7) .EQ. 'CLAMP1_') THEN
        LCLAMP1 = FF
        DO 400 JCLAMP = 1, NEQU1
          CLAMP1(JCLAMP) = 0.
 400    CONTINUE
      ELSEIF(A(1:7) .EQ. 'CLAMP2_') THEN
        LCLAMP2 = FF
        CLAMPVS = 0.
      ENDIF
      RETURN
      END ! RESET

