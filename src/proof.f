
      SUBROUTINE PROOF
C***********************************************************************
C...This routine proofreads a few things.
C
C   Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
      IF(NSEG    .GT.  100) WRITE(LU06,*) 
     $     ' Error 191: Parameter conflict (proof.f)'
      IF(NADJSBS .GT. NSPL) WRITE(LU06,*) 
     $     ' Error 192: Parameter conflict (proof.f)'
      IF(NSBSEG  .GT. NSPL) WRITE(LU06,*) 
     $     ' Error 193: Parameter conflict (proof.f)'
      RETURN
      END ! PROOF

