
      SUBROUTINE ALFAINC(ICASE, DELTA)
C***********************************************************************
C...  Add or subtract a delta to the alfas
C
C     Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      IF(ILE .EQ. 0) THEN
        WRITE(LU06,*) 
     $       'Warning: BUMPALFAS line must come after FOIL lines'
      ENDIF
      IF(DELTA .EQ. 0) THEN
        WRITE(LU06,*) 
     $       ' WARNING: BUMPALFAS has 0 (no change) for delta alfa',
     $       '(alfainc.f)'
      ENDIF
      IF (ICASE .EQ. 100) THEN 
        DO  I = 1, ILE
          ALFAS(I) = ALFAS(I) + DELTA
        ENDDO
      ELSEIF (ICASE .EQ. 200) THEN
        DO  I = ILE+1, ISEG
          ALFAS(I) = ALFAS(I) + DELTA
        ENDDO
      ELSEIF (ICASE .EQ. 300) THEN
        DO I = 1, ILE 
          ALFAS(I) = ALFAS(I) + DELTA
        ENDDO
        DO I = ILE+1, ISEG
          ALFAS(I) = ALFAS(I) - DELTA
        ENDDO
      ELSEIF (ICASE .EQ. 400) THEN
        DO  I = 1, ILE 
          ALFAS(I) = ALFAS(I) + DELTA
        ENDDO
        DO  I = ILE+1, ISEG
          ALFAS(I) = ALFAS(I) + DELTA
        ENDDO
      ELSE
        WRITE(LU06,*) ' WARNING: BUMPALFAS case not found (alfainc.f)'
      ENDIF
      RETURN
      END ! ALFAINC

