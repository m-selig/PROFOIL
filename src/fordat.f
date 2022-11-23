
      SUBROUTINE FORDAT
C***********************************************************************
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      ITEST = 0

C...  xy coordinate file
      open(unit = 96, file = file96, status = 'unknown')

      DO 100 JPT = 1, IARGP+1
         IF(LPRT20) WRITE(20,1000) SLENS(JPT), THETAP(JPT)
         IF(LPRT30) WRITE(30,1000) APHI(JPT)/SCLF, SLENS(JPT)
         IF(LOFFCTP .AND. (JPT .GT. ILECTP) .AND. (ITEST .EQ. 0)) THEN
            IF(LPRT70) WRITE(70,1001) AMILE/SCLF, HPILE
            ITEST = 1
         ENDIF
         IF(LPRT70)  WRITE(70,1000)  APHI(JPT)/SCLF, HP(JPT)
         IF(LPRT80)  WRITE(80,1000)  APHI(JPT)/SCLF, HQ(JPT)
         IF(LPRT90)  WRITE(90,1002)  XMAP(JPT),YMAP(JPT) !,APHI(JPT)/SCLF
         IF(LPRT100) WRITE(95,1002)  XCRD(JPT),YCRD(JPT) !,APHI(JPT)/SCLF
         IF(TT)      WRITE(96,1004)  XCRD(JPT),YCRD(JPT) !,APHI(JPT)/SCLF
  100 CONTINUE
      if(lgensym) then
c...  write out the coordinates for gensym airfoil if designed
        write(lu06,*) 'Writing out gensym coordinates (gensym.f)'
        open(unit = 97, file = file97, status = 'unknown')
        do jgensym = 1, igensym
          write(97,1004) xgensym(jgensym), ygensym(jgensym)
        enddo
      endif
 1000 FORMAT(2X,2F12.5)
 1001 FORMAT(2X,2F12.5,'  ILE')
 1002 FORMAT(2X,2F12.5,F9.3)
 1003 FORMAT(8X,2I6)
 1004 FORMAT(2X,2F22.18)
      WRITE(lu06,*)             '****************OUTPUT****************'
      IF(LPRT20)  WRITE(lu06,*) '* theta(s)   -> fort.20              *'
      IF(LPRT30)  WRITE(lu06,*) '* s(phi)     -> fort.30              *'
      IF(LPRT70)  WRITE(lu06,*) '* P(phi)     -> fort.70              *'
      IF(LPRT80)  WRITE(lu06,*) '* Q(phi)     -> fort.80              *'
      IF(LPRT90)  WRITE(lu06,*) '* mapped x,y -> fort.90              *'
      IF(LPRT100) WRITE(lu06,*) '* norm   x,y -> fort.95              *'
C...  file96 (opened above):
      IF(TT)      WRITE(lu06,*) '* norm   x,y -> profoil.xy (high res)*'
C...  file97 (opend above):
      IF(lgensym) WRITE(lu06,*) '* gensym x,y -> gensym.xy            *'
C...  FILE85 (opened in echo.f):
      IF(LECHO)   WRITE(lu06,*) '* tra data   -> profoil_tra.txt      *'
      WRITE(lu06,*)             '**************************************'
      CLOSE (20)
      CLOSE (30)
      CLOSE (70)
      CLOSE (80)
      CLOSE (90)
      CLOSE (95)
      CLOSE (96)
      CLOSE (97)
      RETURN
      END ! FORDAT

