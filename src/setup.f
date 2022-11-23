
      SUBROUTINE SETUP
C***********************************************************************
C...  Things that do not change throughout the code, e.g. constants, filenames
C
C     Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      INCLUDE 'PROFOIL.INC'
C---- logical unit numbers
      LU5 = 5
      LU6 = 6
      LU05   = 5
      LU06   = 6
C...  input
      LU10   = 10
      FILE10 = 'profoil.in'
c     11 through 19 ... used in bldat.f
c     12
c     13
c     14
c     15
c     16
c     17
c     18
c     19
      LU20   = 20
C...  dump
c     21
      FILE21 = 'profoil.dmp'
      LU30   = 30
c     31
      FILE31 = 'profoil_mirror.dmp'
      LU40   = 40
      LU50   = 50 
c...  v-dist
      LU60   = 60
      FILE60 = 'profoil.vel'
      LU70   = 70
      LU80   = 80
      LU85   = 85
      LU90   = 90
c...  coordinates
c...  96
      FILE96 = 'profoil.xy'
c...  coordinates - gensym
c     97
      FILE97 = 'gensym.xy'
      LU100  = 100
      LU101  = 101
c---- Optional files
c     Eppler style output
      FILE85 = 'profoil_tra.txt'
C
C---- some constants
      PI = 2*DASIN(1.D0)
      DTOR = PI/180.
      RTOD = 1./DTOR
      RETURN
      END ! SETUP

