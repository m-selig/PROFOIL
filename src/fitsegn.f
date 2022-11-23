
      SUBROUTINE FITSEGN
C***********************************************************************
C...Determine coefficients for prescribed Newton tilde functions.
C   This routine is very much like FITSEGV.
C   SSDELV(..) --> SSF(..)
C   SSPHI(..)  --> SSS(..)
C   Need to determine spline coefficients for IADJS number
C   of adjustable segments each with IADJSBS of adjustable subsegments.
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      DO 500 JADJS = 1, IADJS
         IADJSBS = KADJSBS(JADJS)
         IF(IADJSBS .GT. 1) THEN 

CCCC-----------Only one point is given, fit a straight line.
CCC            FBBSS(JADJS,JADJSBS) = 
CCC     &                 SSF(JADJS,JADJSBS)/ SSS(JADJS,JADJSBS)
CCC         ELSE

C-----------Fit a cubic spline to the points with zero second derivatives
C           at the endpoints.
            YP1 = 1.E10
            YPN = 1.E10
C-----------dump SSS(..) and SSF(..) into work arrays for SPLINE
            XDM(1) = 0.
            YDM(1) = 0.
            DO 700 JADJSBS = 1, IADJSBS
               XDM(JADJSBS+1) = SSS(JADJS,JADJSBS)
               YDM(JADJSBS+1) = SSF(JADJS,JADJSBS)
  700       CONTINUE
C-----------compute the second derivative of the cubic spline
            CALL SPLINE(XDM,YDM,IADJSBS+1,NSPL,YP1,YPN,Y2)
C-----------compute the cubic coefficients
            DO 800 JADJSBS = 1, IADJSBS
               FDDSS(JADJS,JADJSBS) = (Y2(JADJSBS) - Y2(JADJSBS+1))
     &           * 0.1666666666666667
     &           / (XDM(JADJSBS) - XDM(JADJSBS+1))
               FCCSS(JADJS,JADJSBS) = 0.5 * 
     &    (Y2(JADJSBS) - 6. * FDDSS(JADJS,JADJSBS) * XDM(JADJSBS))
               FBBSS(JADJS,JADJSBS) = (YDM(JADJSBS)-YDM(JADJSBS+1)
     &   - FCCSS(JADJS,JADJSBS) * (XDM(JADJSBS)**2-XDM(JADJSBS+1)**2)
     &   - FDDSS(JADJS,JADJSBS) * (XDM(JADJSBS)**3-XDM(JADJSBS+1)**3))
     &           / (XDM(JADJSBS)-XDM(JADJSBS+1))
               FAASS(JADJS,JADJSBS) = YDM(JADJSBS) 
     &           - FBBSS(JADJS,JADJSBS) * XDM(JADJSBS)
     &           - FCCSS(JADJS,JADJSBS) * XDM(JADJSBS)**2
     &           - FDDSS(JADJS,JADJSBS) * XDM(JADJSBS)**3
  800       CONTINUE
         ENDIF
  500 CONTINUE
      RETURN
      END ! FITSEGN

