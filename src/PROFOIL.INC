
C***********************************************************************
C     PROFOIL global include file
C
C     Copyright (c) 1990-2022 Michael Selig
C***********************************************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      LOGICAL TT, FF
      PARAMETER (TT = .TRUE., FF = .FALSE.)
      PARAMETER (
     $     NSEG    = 60, 
     $     NALF = 10, 
     $     NARGP   = 1001,
     $     NARGP2 = (NARGP-1)/2,
     $     NGENSYM = NARGP,
     $     NEQU1   = 30, 
     $     NEQU2   = 40,
     $     NEQU    = NEQU1 + NEQU2,
     $     NSPL    = 100,
     $     NBL = 300,
     $     NCLAMP1 = NEQU1,
     $     NPRTBL  = 12)
      PARAMETER (NFREQ = 100, NBLPTS = 300)
C-----Note: NEQU must be less than lda used in subroutine dgesl()
C-----Note: NSEG must be less than 100 due to conflict in SETNEW ITP1(.) = 1
C-----Note: NADJSBS must be less than NSPL (see FITSEGV.FOR)
C-----Note: In subroutine SPLINE, NMAX must be set to NSPL (if SPLINE() is used)
      PARAMETER (NSPSEG = 10, NSBSEG  = 20)
      PARAMETER (NADJS  = 5, NADJSBS = 15)
      
C-----Convention: [N]SEG/* --- Maxmimum possible segments         /other
C                 [I]SEG/* --- Actual number of segments used     /other
C                 [J]SEG/* --- Current segment in the computation /other
C-----require: NSEG >= ISEG >= JSEG
      CHARACTER*90 LINE, TLINE 
      LOGICAL      
     $             LITON, LSTAG, LARGHP, LSLENS, LHQ, LHP, LECHO,
     $             LCOORD, LNORM, LTHICK, LTHICKE, LMOMENT, LFTE, LENN, 
     $             LSENSI, LDELV(NSEG), LLDELV, LDELVSC(NSEG), LWGHT, 
     $             LDRLN, LMANUAL, LCONSEC, LSTEP, LNEWT1, LNEWT2, 
     $             LOFFCTP, LWHERE, LEXTRA,
     $             LLBOS(NEQU1), LLBE(NADJS), LBOSAN, LFIRST, 
     $             LADJSS, LCONV, LBE, LBOS, LATT, LCLAMP1,
     $             LCLAMP2,
     $             LPRT10,LPRT20,
     $             LPRT30,LPRT40,LPRT50,LPRT60,LPRT70,LPRT80,
     $             LPRT85, LPRT90, LPRT100, LPRT101,
     $             LSYM, LEDES, LSNEWT2, LFXTR, LBOSTR,
     $             LLFXTR1(NEQU1), LLFXTR2(NADJS), LLBOSTR(NEQU1),
     $             LLBETR(NADJS), LALFASP, LSKIP, LADDALF, LNEGN, 
     $             LJACOBI, LSCALEV, LTEZERO, LVELF0, LWEDGE, LFLIP,
     $             LGENSYM, 
     $             LSKIPQ, LGENSYM_REPORT, LGENSYM_XF_YF,
     $             lteflap
C-----add logical to common below
      COMMON/ LOG/
     $             LITON, LSTAG, LARGHP, LSLENS, LHQ, LHP, LECHO,
     $             LCOORD, LNORM, LTHICK, LTHICKE, LMOMENT, LFTE, LENN, 
     $             LSENSI, LDELV, LLDELV, LDELVSC, LWGHT, LDRLN,
     $             LMANUAL, LCONSEC, LSTEP, LNEWT1, LNEWT2, 
     $             LOFFCTP, LWHERE, LEXTRA,
     $             LLBOS, LLBE, LBOSAN, LFIRST, 
     $             LADJSS, LCONV, LBE, LBOS, LATT, LCLAMP1,
     $             LCLAMP2,
     $             LPRT10,LPRT20,
     $             LPRT30,LPRT40,LPRT50,LPRT60,LPRT70,LPRT80,
     $             LPRT85, LPRT90, LPRT100, LPRT101,
     $             LSYM, LEDES, LSNEWT2, LFXTR, LBOSTR,
     $             LLFXTR1, LLFXTR2, LLBOSTR,
     $             LLBETR, LALFASP, LSKIP, LADDALF, LNEGN, 
     $             LJACOBI, LSCALEV, LTEZERO, LVELF0, LWEDGE, LFLIP,
     $             LGENSYM, 
     $             LSKIPQ, LGENSYM_REPORT, LGENSYM_XF_YF,
     $             lteflap

      COMMON/ SMALL/ TINY, ONE, ZERO
      COMMON/ RW/
     $     LU5,
     $     LU6,
     $     LU05,
     $     LU06,
     $     LU10,
     $     LU20,
     $     LU30,
     $     LU40,
     $     LU50,
     $     LU60,
     $     LU70,
     $     LU80,
     $     LU85,
     $     LU90,
     $     LU100,
     $     LU101
      COMMON/ TRN/
     $     SBLTR,
     $     VBLTR,
     $     JSEGTR1(NEQU1),
     $     JSEGTR2(NADJS)
      COMMON/ ANA/ 
     $     IAN, 
     $     JSEGAN, 
     $     ALFAAN, 
     $     RINFAN, 
     $     IPRTBL,
     $     MPRTBL(NPRTBL),
     $     JSEGTR
      COMMON/ CHR/ LINE, TLINE 
      COMMON/ PTT/ IPRT(10)
      COMMON/ BAS/ PI, DTOR, RTOD
      COMMON/ SYS/ AIJ(4,4), BI(4) 
      COMMON/ SPL/ XDM(NSPL), YDM(NSPL), Y2(NSPL)
      COMMON/ FTE/ EPP, PHIEPP(2)
      COMMON/ SOL/ 
     $ UMU(2),
     $ HKH(2),
     $ SKS,
     $ OMEGA(2),
     $ OMEGP(2),
     $ OMEGT(2)
      COMMON/ DES/ 
     $ AKA(2), 
     $ AM0,
     $ AM(NSEG),
     $ AW(2),
     $ AS(2),
     $ ALFAS(NSEG),
     $ VS(NSEG), 
     $ ILE, 
     $ IVEL,
     $ ISEG, 
     $ JFOIL
      COMMON/ MSC/
     $ RADLE,
     $ XCRDLE(3),
     $ YCRDLE(3)
      COMMON/ BQA/
     $ AMLOG(NSEG),
     $ AFCOS(NSEG),
     $ AFSIN(NSEG),
     $ TLOG,
     $ VLOG1,
     $ VLOG2,
     $ ORFA(NSEG), OCK(NSEG),
     $ AMDEL(NSEG)
      COMMON/ VEL/ ALFA(NALF), IALF
      COMMON/ DEF/ 
     $ DELAN,
     $ RELAX,
     $ SIGNPER,
     $ SCLF
      COMMON/ TOLER/
     $ TOL,
     $ TOLSPEC,
     $ SQSUM
      COMMON/ NWT/ 
     $ FNT1_P(NEQU1),
     $ FNT1_0(NEQU1),
     $ FNT1_1(NEQU1), 
     $ FNT1_X(NEQU1),
     $ COND1(NEQU1),
     $ COND2(NEQU1), 
     $ FNEWT1(NEQU1), 
     $ CIJ(NEQU,NEQU),
     $ DI(NEQU),
     $ CIJ_SV(NEQU,NEQU),
     $ DELTAP(NEQU1),
     $ DELTAS(NEQU1),
     $ svalue1(nequ1), value,
     $ JSEGIX1(NEQU1), 
     $ ITP1(NEQU1),
     $ ITP2(NEQU1),
     $ IFTP1(NEQU1), 
     $ IEQU1,
     $ IVAR1,
     $ IEQU,
     $ IVAR,
     $ ITER,
     $ ITERMAX,
     $ ISTEPTO,
     $ INDX(NEQU)
      COMMON/ LINPACKD1/
     $ rcond,
     $ z( NEQU ), 
     $ b( NEQU ),
     $ job, 
     $ ipvt( NEQU )
      COMMON/ STG/ ISTAGE
      COMMON/TTMP/
     $ WGHTPHI(NSPSEG,NSBSEG), 
     $ SSS(NADJS,NADJSBS),
     $ SSF(NADJS,NADJSBS),
     $ SSFNN(NSBSEG),
     $ SJSBSEG(NSBSEG),
     $ FJSBSEG(NSBSEG),
     $ FNT2_P(NEQU2),
     $ FNT2_0(NEQU2),
     $ FNT2_1(NEQU2),
     $ FNT2_X(NEQU2),
     $ DELTAVP(NEQU2),
     $ DELTAVS(NEQU2),
     $ SBOS,
     $ R1(NADJS),
     $ R2(NADJS),
     $ KADJSBS(NADJS),
     $ JVARIDX(NEQU2,2),
     $ JSEGIX2(NADJS),
     $ IADJS,
     $ IEQU2,
     $ IVAR2,
     $ IFTP2(NADJS)
      COMMON/FLAPIT_A/
     $     XF, YF, YHALFTC
      COMMON/FLAPP/ xhinge, yhinge, deltaflap, xte, yte, cflap,
     $     yhtop, yhbot, iflapmode
C-----setup for call to BL subroutine
      COMMON/STBL/
     $ SPACK(NSBSEG+1),
     $ VPACK(NSBSEG+1),
     $ STEMP(NSBSEG+1),
     $ AMVEL0,
     $ PHIEND,
     $ VSSEND,
     $ SSTAG,
     $ SBL(NBL),
     $ VBL(NBL),
     $ BLNEWT,
     $ RINF,
     $ ALFABL,
     $ H32BL(NBL),
     $ H12BL(NBL), 
     $ D2BL(NBL),
     $ D3BL(NBL),
     $ RD2BL(NBL),
     $ CFBL(NBL),
     $ CDBL(NBL),
     $ DRLNBL(NBL),
     $ IBL,
     $ INDXBL(NSBSEG+1),
     $ IPACK
      COMMON/ VDU/
     $ IDVTP(NSEG),
     $ VTILDE(NSEG),
     $ VAA(NSEG),
     $ VBB(NSEG),
     $ VCC(NSEG),
     $ VAASS(NSPSEG,NSBSEG),
     $ VBBSS(NSPSEG,NSBSEG),
     $ VCCSS(NSPSEG,NSBSEG),
     $ VDDSS(NSPSEG,NSBSEG),
     $ ULOG(NSEG),
     $ URGO(NSEG),
     $ UCOS(NSEG),
     $ USIN(NSEG)
      COMMON/ FSN/
     $ FAASS(NADJS,NADJSBS),
     $ FBBSS(NADJS,NADJSBS),
     $ FCCSS(NADJS,NADJSBS),
     $ FDDSS(NADJS,NADJSBS)
      COMMON/ EMX/
     $ EAA(2),
     $ EBB(2),
     $ ECC(2),
     $ EDD(2)
      COMMON/ HPQ/
     $ DEL_PHI,
     $ ARGP(NARGP),
     $ ARGPB(NARGP),
     $ ARGPV(NARGP), 
     $ APHI(NARGP),
     $ AMILE,
     $ ARGPILE,
     $ HPILE,
     $ HP(NARGP),
     $ HPSMO(3*NARGP+1),
     $ HQ(NARGP),
     $ IARGP,
     $ IARGP2,
     $ COTAN(NARGP2),
     $ IARGPU,
     $ ILECTP 
      COMMON/ CRD/
     $ XMAP(NARGP),
     $ YMAP(NARGP),
     $ EIJ(4,4),
     $ FI(4),
     $ INDX2(4),
     $ X_PHI(NARGP),
     $ Y_PHI(NARGP),
     $ SX,
     $ SY
      COMMON/ NRM/
     $ XLE(4),
     $ YLE(4),
     $ XCRD(NARGP),
     $ YCRD(NARGP), 
     $ XTMP(NARGP),
     $ YTMP(NARGP),
     $ ALFA0,
     $ CHORD,
     $ THKMAX,
     $ THKMAXE,
     $ XCMAXE,
     $ thkmaxg,
     $ XCMAX,
     $ CMBMAX, 
     $ IMAX
      common/ gens/ xgensym(ngensym), ygensym(ngensym), fgensym, igensym
      COMMON/ SBS/
     $ SSDELV(NSPSEG,NSBSEG),
     $ SSPHI(NSPSEG,NSBSEG),
     $ JSPSEG,
     $ ISPSEG,
     $ KSPSEG(NSEG), 
     $ JSBSEG,
     $ ISBSEG,
     $ KSBSEG(NSEG)
      COMMON/ ARC/ SLENS(NARGP)
      COMMON/ THT/ THETAP(NARGP)
      COMMON/ CMT/
     $ FSB0,
     $ FSA1,
     $ FSB1,
     $ FSA2,
     $ FSB2,
     $ CM0
      COMMON/ ETC/ EM, ER
      COMMON/ CLP/
     $ CLAMP1(NCLAMP1),
     $ CLAMPVS
C...  Stuff for scaling velocity distribution used in PROBL
      COMMON/ SCL/ PROPO
      COMMON/ GIH/ DELVSC(NSEG)
      COMMON/ SOT/ CHORDO, AREA, VOLUME_B, LENGTH_B
c...  chordo   chord length
c...  area     area of the airfoil
c...  volume_b volume of a bulb
c...  length_b length of the bulb
      COMMON/ TED/
     $ TEX,
     $ TEY,
     $ DELSX,
     $ DELSY,
     $ XMAPOFF,
     $ YMAPOFF, 
     $ XTEMIDPT,
     $ YTEMIDPT
      COMMON/ WED/ WEDGET
C============================================================================
C Common statements below added to enable hardwiring filenames for
C units 10, 21, 60 and 96

C...  File Names
      CHARACTER*20 
     $     FILE10,
     $     FILE20,
     $     FILE21,
     $     FILE30,
     $     FILE31,
     $     FILE40,
     $     FILE50,
     $     FILE60,
     $     FILE70,
     $     FILE80,
     $     FILE85,
     $     FILE90,
     $     FILE96,
     $     FILE97,
     $     FILE100,
     $     FILE101
      COMMON/ FILES/
     $     FILE10,
     $     FILE20,
     $     FILE21,
     $     FILE30,
     $     FILE31,
     $     FILE40,
     $     FILE50,
     $     FILE60,
     $     FILE70,
     $     FILE80,
     $     FILE85,
     $     FILE90,
     $     FILE96,
     $     FILE97,
     $     FILE100,
     $     FILE101


C============================================================================
    

C-----Variables:
C     WEDGET     = trailing edge thickness to be added to y-coordinates
C     THKMAXE    = thickness as computed by Eppler's routine
C     XCMAXE     = location of max thickness as computed by Eppler's routine
C
C     LVELF0     = FF do not write out the extra stag vel point when on coord point (veldist.f)
C                = TT do write point (in this case the stag point will be written twice)
C     XTEMIDPT   midpoint of the TE in x
C     YTEMIDPT   midpoint of the TE in y
C     XMAPOFF  initial x values for getting the coordinates when integrating
C     YMAPOFF  initial y values for getting the coordinates when integrating
C     LTEZERO     = TT  close trailing edge, 
C                 = FF  when TE thickness is prescribed
C     CHORDO      physical chord length used to get area of the airfoil
C     AREA        area of the airfoil based on the chord length CHORDO, which
C                 is default to one.
C     DELVSC(.)   used before DELV/FOIL lines to scale the DELV distributions
C     PROP        used with probl.f for scaling BL velocity distribution
C     RADLE  radius of the leading edge
C     LLFXTR1(.)  .TRUE. if transition is fixed for the current NEWT1 line
C     LLBOSTR(.)  .TRUE. if transition is at BOS
C     JSEGTR1(.)  segment for which transition is fixed 
C                 for the current NEWT1 line
C     LLFXTR2(.)  .TRUE. if transition is fixed for the current NEWT2 line
C     LLBETR(.)   .TRUE. if transition is at BOS 
C     JSEGTR2(.)  segment for which transition is fixed 
C                 for the current NEWT2 line
C     SBLTR & VBLTR local arc length and velocity at fixed transtion pt
C     JSEGTR      segment for which transition is fixed at either
C                 the beginning or the end
C     LBOSTR      .TRUE. transition happens at beginning
C                        of segment JSEGTR
C     LFXTR       .TRUE. if transition is fixed for current
C                        boundary layer call
C     LMANUAL     .TRUE. iteration in manual mode.
C     DRLNBL(.)   n development according to Drela's method.
C     LENN        .TRUE. if necessary to calculate n_ENN development
C     LDRLN       .TRUE. if necessary to calculate n_Drela development
C     LSENSI      .TRUE. to save the sensitivity before call to SYSNEWT
C     IFREQ       number of frequencies being tracked
C     ENNMAX      envelope value at a given location JBL
C     FREQMAX     envelope value at a given location JBL---not used in PROFOIL
C     AMPF(..)    amplification factor in the e^n method
C     IAN         number of select boundary layer analyses
C     JSEGAN      segments for BL analyses
C     LBOSAN      .TRUE. if BOS to EOS analyses
C     ALFAAN      angles of attack for analyses
C     RINFAN      Reynolds numbers for analyses
C     IPRTBL      Number of files to print for each analysis
C     MPRTBL(.)  file to print for each analysis
C
C     XDM(.), YDM(.), Y2(.)  variables used to spline data
C     LU5, LU6    read and write logical unit numbers
C     ISTAGE      number of iteration stages (default=1), 
C                 incremented by CONV line
C     SCLF        scale factor for AM(.)
C     CLAMP1(.)   clamp values for NEWT1 equations
C     CLAMPVS     clamp value  for NEWT2 equations
C     EM, ER      Eppler transition criterion constants: m, r
C     PHIEND      arc limit (converted to s) to pack at the end of the SBL(.)
C     VSSEND      design velocity (converted to off-design angle of attack)
C                 to pack at the end of the VBL(.)
C    
C     JSEGIX1(.)  segment index for NEWT1 specification
C     JSEGIX2(.)  segment index for NEWT2 specification
C     RINF        current Reynolds number
C     ALFABL      angle of attack for for BL analysis in Newton iteration
C     BLNEWT      single variable Newton iteration from BL analysis
C     SBL(.)      boundary layer arc length
C     VBL(.)      boundary layer velocity distribution
C     IBL         length of boundary layer arrays
C     INDXBL(.)   Newton nodes 
C     STEMP(.)    temp array for SJSBSEG(.)
C     SPACK(.)    arc length array to be packed into SBL(.)
C     VPACK(.)    velocity   array to be packed into VBL(.)
C     IPACK       dimension of SPACK(.) for adjs special segment
C     SBOS        arc length at the beginning of the segment
C     LSNEWT2     spline the prescribed Newton distributions
C     LCLAMP1     .TRUE. if step size is check for clamping on NEWT1 input
C     LCLAMP2     .TRUE. if step size is check for clamping on NEWT2 input
C     LLBE(.)     .TRUE. if BL coordinates run BOS to EOS
C     LBE         .TRUE. if BL coordinates run BOS to EOS
C     LLBOS(.)    .TRUE. if BOS Rx 
C     LBOS        .TRUE. if BOS Rx 
C     COND1(.)    Single-variable Newton iteration with first condition
C     COND2(.)    Single-variable Newton iteration with second condition
C     R1(.)       Segment prescription with one condition
C
C     FSB0        fourier coefficient based on Q(phi) ---> b0 = 0. theory
C     FSA1        fourier coefficient based on Q(phi) ---> a1 = 1. theory
C     FSB1        fourier coefficient based on Q(phi) ---> b1 = 0. theory
C     CMA2        moment integral of P cos(2*phi)
C     CMB2        moment integral of P sin(2*phi)
C     CM0         zero lift moment coefficient
C     NSEG        maximum number of segments (<100) due to SETPER ITP* codes
C     ISEG        number of segments
C-----special segment---------------------------------------------------
C     NSPSEG      maximum number of special segments
C     KSPSEG(.)   special segment index JSPSEG for a given segment JSEG
C     ISPSEG      actual  "
C     JSPSEG      current special segment
C-----subsegments-------------------------------------------------------
C     NSBSEG      maximum number of subsegments per special segment
C     KSBSEG(.)   number of subsegments for a given segment JSEG
C     ISBSEG      current number of subsegments for the segment JSEG
C     JSBSEG      current subsegment
C
C     AM(.)       segment  arc limits (deg)
C     AW(.)       recovery arc limits (deg)
C     AS(.)       closure  arc limits (deg)
C     AM0         arc limit = 0.      (deg)
C     AKA(.)      k  upper and lower main recovery coefficients
C     ALFAS(.)    alphas* for segments      (deg)
C     VS(.)       v*      for segments
C     SSPHI(..)   subsegment phi 
C     WGHTPHI(..) weight factor (0-1) for the distribution of SSPHI(..)
C     ------------for the adjustable special segments
C     SSDELV(..)  [s]ub[s]egment [del]ta [v]*
C     ILE         index of the leading edge arc limit
C     IVEL        index of the segment with the specified velocity level
C     DELAN       nominal delta arc limit for integration (deg)
C
C     VLOG1       term  in the continuity equation
C     TLOG        term  in the continuity equation
C     VLOG2       term  in the integral constraints
C     AMLOG(.)    terms in the integral constraints
C     AFCOS(.)    terms in the integral constraints
C     AFSIN(.)    terms in the integral constraints
C     ORFA(.)     terms in the integral constraints
C     OCK(.)      terms in the integral constraints
C     AMDEL(.)    terms in the integral constraints
C     ULOG(.)     term in the integral constraint equations
C     URGO(.)     term in the integral constraint equations
C     UCOS(.)     term in the integral constraint equations
C     USIN(.)     term in the integral constraint equations
C
C     AIJ(..)     system to solve for mu u/l and kh u/l
C     BI(.)       rhs of the system of equations--->solution
C
C     UMU(.)      mu upper and lower main recovery exponents
C     HKH(.)      kh upper and lower closure exponents
C     SKS         ks  used in the Newton equations
C     OMEGA(.)    omega for the upper and lower recovery-EPPLER
C     OMEGP(.)    omega' upper and lower in Newton equations-EPPLER
C     OMEGT(.)    omega(tot) upper and lower in Newton equations-SELIG
C     SLENS(.)    arc length s
C
C     DEL_PHI     incremental step in phi for computation of P(phi)
C     NARGP       dimension of ARGP(.) and APHI(.)
C     IARGP       number of control points
C     IARGP2      number of control points/2
C     APHI(.)     control points - angles at which P and arg(P) are computed
C     ARGP(.)     argument of P
C     HP(.)       P
C     HQ(.)       Q
C     ILECTP      index of the control point just before the LE arc limit
C     AMILE       AM(ILE) leading edge arc limit
C     ARGPILE     argument of P if AM(ILE) does not fall on control point
C     HPILE       P if AM(ILE) does not fall on control point
C
C     IMAX        index of RRMAX
C     XMAP(.)     mapped x airfoil coordinate
C     YMAP(.)     mapped y airfoil coordinate
C     XCRD(.)     normalized x airfoil coordinate
C     YCRD(.)     normalized y airfoil coordinate
C
C     LSYM        .TRUE.  if airfoil is symmetric
C     LARGHP      .TRUE.  if P(phi) is needed for Newton functions
C     LSLENS      .TRUE.  if s(phi)             "
C     LHP         .TRUE.  if P(phi)             "
C     LHQ         .TRUE.  if Q(phi)             "
C     LSTAG       .FALSE. if current phi is less than the stagnation phi
C     LITON       .FALSE. if no Newton iteration is done
C     LOFFCTP     .FALSE. if LE arc limit AM(ILE) does fall on a control point
C     LDELV(.)    .FALSE. if no delta v* prescription is given the segment
C     LLDELV      .FALSE. if no segment has prescribed delta v*
C     LWHERE      .TRUE.  if sub-segment is known for the given special seg
C     LEXTRA      .TRUE.  if out-of-range of subsegment extrapolate delta v*
C     LCONV       .TRUE.  if convergence is reached 
C     LEDES       .TRUE.  if design is finished
C
C     SSS(..)     subsegment prescription independent variable, like arc length
C     SSF(..)     subsegment prescription dependent variable, like H_12
C     SSA(..)     subsegment prescription dependent variable: alpha
C     SJSBSEG(.)  arc length s for the subsegment JSBSEG at which Newton
C                 equation is evaluated.
C     SSFNN(.)    prescribed value at the Newton nodes SJSBSEG(.)
C     FJSBSEG(.)  actual value at the Newton nodes SJSBSEG(.)
C
C     IEQU        sum of IEQU1 and IEQU2 for solution of linear system
C     IEQU1       number of Newton equations from line> NEWT*1
C     IVAR1       number of Newton variables 
C     IFTP1(.)    Newton equation types
C     ITP1(.)     index of the class of the variable to iterate
C     ITP2(.)     index of the the particular iterate of the class given
C     DELTAP(.)   use to compute the [p]artial derivatives
C     DELTAS(.)   solution of the Newton system for the [s]tep size
C     RELAX       relaxation factor in the Newton system step, rarely used
C     FNT_P(.)    value of the Newton function at (p)
C     FNT_0(.)    value of the Newton function at (0)
C     FNT_1(.)    value of the Newton function at (1)
C
C     IEQU2       number of Newton equations for the adjustable segments
C                 from line> NEWT2*
C     IVAR2       number of Newton variables for the adjustable segments
C     JVARIDX(..) indices [JSEG,JSBSEG] of v*(phi)_jvar2 for Newton iteration
C     IDVTP(.)    type of delta(v*) velocity prescription
C     DELTAVP(.)  perturbations in SSDELV(.) for Newton iteration
C     DELTAVS(.)  steps in SSDELV(.) determined by solution of Newt equation
C     FNT2_P(.)   values of the IEQU2 Newton equation at location 'P'
C     FNT2_0(.)   values of the IEQU2 Newton equation at location '0'
C     FNT2_1(.)   values of the IEQU2 Newton equation at location '1'
C     FNT2_X(.)   dummy values of the IEQU2 Newton equation 
C
C     CIJ(..)     lhs of Newton system
C     DI(.)       rhs of Newton system--->solution
C
C     ALFA(.)     angles of attack to compute the velocity distribution
C     NALF        dimension of ALFA(.) 
C
C     PI          3.1415926...   
C     DTOR        degrees to radians
C     RTOD        radians to degrees
C
C...Local variables sometimes passed to other subroutines
C     ANGLE
C     ALPHA
C     JSEG


C-----start: e^n method commons
C     4/10/2022 this e^n not supported
      COMMON/ EEN/ AMPF(NFREQ, NBL), FREQMAX, ENNMAX
      common/etothen/
     $ en(nfreq),
     $ env(nblpts),
     $ endr(nblpts),
     $ freq(nfreq),
     $ ent
      common/nflags/
     $ ndrela,
     $ ncomp,
     $ icrit,
     $ ncrit(nfreq),
     $ ifreq
      common/romb/
     $ senn(nblpts,nfreq),
     $ yenn(nblpts,nfreq),
     $ zenn(nblpts,nfreq),
     $ ydr(nblpts),
     $ sdr(nblpts),
     $ zdr(nblpts),
     $ nspli(nfreq),
     $ ntemp(nfreq),
     $ ndrspl,
     $ ndrtemp,
     $ ndrcrit
      common/amp/
     $ amp(nblpts,nfreq),
     $ xam(nblpts,nfreq),
     $ ds0(nfreq),
     $ nnn,
     $ nmin
C-----end e^n commons
