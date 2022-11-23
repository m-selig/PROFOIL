
      SUBROUTINE VLEV
C***********************************************************************
C...Solves for the design [v]elocities [lev]els v*(.) based on VS(IVEL)
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      DO 100 JSEG = IVEL, ISEG-1
         VEOS = 0
         IF(LDELV(JSEG)) THEN
            IF(IDVTP(JSEG) .LE. 10) THEN
               VEOS = VTILDE(JSEG)
            ELSE
               JSPSEG = KSPSEG(JSEG)
               ISBSEG = KSBSEG(JSEG)
               VEOS   = SSDELV(JSPSEG,ISBSEG)
            ENDIF
         ENDIF
         VS(JSEG+1) = (VS(JSEG) + VEOS)
     &                    * DABS(DCOSG(0.5*AM(JSEG) - ALFAS(JSEG+1))/
     &                           DCOSG(0.5*AM(JSEG) -   ALFAS(JSEG)))
  100 CONTINUE
      DO 200 JSEG = IVEL, 2, -1
         VEOS = 0
         IF(LDELV(JSEG-1)) THEN
            IF(IDVTP(JSEG-1) .LE. 10) THEN
               VEOS = VTILDE(JSEG-1)
            ELSE
               JSPSEG = KSPSEG(JSEG-1)
               ISBSEG = KSBSEG(JSEG-1)
               VEOS   = SSDELV(JSPSEG,ISBSEG)
            ENDIF
         ENDIF
         VS(JSEG-1) =  VS(JSEG)
     &                   * DABS(DCOSG(0.5*AM(JSEG-1) - ALFAS(JSEG-1))/
     &                          DCOSG(0.5*AM(JSEG-1) -   ALFAS(JSEG)))
     &                   - VEOS 
  200 CONTINUE      
      RETURN
      END ! VLEV

