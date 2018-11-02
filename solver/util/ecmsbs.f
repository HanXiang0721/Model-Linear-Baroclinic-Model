      PROGRAM ECMSBS
*
*     writtedn on 2005/08/31
*     This routine makes a basic state from ECMWF reanalysis 
*     (ERA40) climatology for 1961-1990
*
      include 'dim.f'
      include 'hdim.f'
*
*     [dimension]
      INTEGER    NLEV0, NVAR1, NLEVMX
      PARAMETER  ( NLEV0 = 23, NVAR1 = 6, NLEVMX = 20 )
*
*     [work]
      REAL*4    DAT0( IMAX, JMAX )
      REAL*4    ZP( IMAX*JMAX, NLEV0)
      REAL*4    UP( IMAX*JMAX, NLEV0)
      REAL*4    VP( IMAX*JMAX, NLEV0)
      REAL*4    TP( IMAX*JMAX, NLEV0)
      REAL*4    PP( IMAX*JMAX)
      REAL*4    QP( IMAX*JMAX, NLEV0)
      REAL*4    SLP( IMAX*JMAX)
      REAL*4    GDZS( IMAX*JMAX)
      
      REAL*8    GDI( IMAX*JMAX, NLEV0 )
      REAL*8    U( IMAX*JMAX, KMAX)
      REAL*8    V( IMAX*JMAX, KMAX)
      REAL*8    T( IMAX*JMAX, KMAX)
      REAL*8    P( IMAX*JMAX)
      REAL*8    Q( IMAX*JMAX, KMAX)

      REAL*8    WT( NMDIM, KMAX)
      REAL*8    WS( NMDIM )
      REAL*8    ZT( IMAX+1, JMAX, KMAX)
      REAL*8    ZS( IMAX+1, JMAX)

      REAL*4    GT( IMAX*JMAX, KMAX)
      REAL*4    GS( IMAX*JMAX      )

      REAL*4    TS( IMAX*JMAX      )
      REAL*4    QS( IMAX*JMAX      )

      REAL*8    SIG( KMAX )

      REAL*8    TLAPSX, TVS, TV0, TVX, VTMP, EPSVT

      INTEGER   ILEV, IREC, IVAR, ISSN, IAVG, I, J, K, N, IJ
*
*     [parameter]
*
      INTEGER   KLEV1( NVAR1 )  !! ERA40 available levels
      REAL*8    SIG5( NLEVMX )  !! 5 sigma level
      REAL*8    PLEV0( NLEV0 )  !! ERA40 pressure level
      REAL*8    SIG8( NLEVMX )  !! 8 sigma level
      REAL*8    SIG11( NLEVMX ) !! 11 sigma level
      REAL*8    SIG20( NLEVMX ) !! 20 sigma level
      REAL*8    RAIR, GRAV, EPSV
      CHARACTER HEAD( 64 )*16
      CHARACTER CHEAD( 2 )*16
      CHARACTER CHEAD5( 2 )*16
      CHARACTER CHEAD8( 2 )*16
      CHARACTER CHEAD11( 2 )*16
      CHARACTER CHEAD20( 2 )*16

      REAL*4     UNDEF1, XMISS
      INTEGER    NMO
      INTEGER    NOUT
*
*     [intrinsic]
*
      CHARACTER*90 CECM          !! ERA40 pressure level data (grads)
      CHARACTER*90 CECM2         !! ERA40 surface pressure    (grads)
      CHARACTER*90 CALT          !! topography               (gtool3)
      CHARACTER*90 CBS0          !! basic state              (gtool)
      CHARACTER*90 CBS           !! basic state              (grads)
      INTEGER      KMO           !! first month 
      INTEGER      NAVG          !! no. of month averaged
      LOGICAL      OZM           !! zonal mean
      LOGICAL      OSW           !! zonal asymmetry
      LOGICAL      ONH           !! symmetric NH state
      LOGICAL      OSH           !! symmetric SH state
*
      NAMELIST    /NMECM/   CECM, CALT, 
     &                      KMO, NAVG, OZM, OSW, ONH, OSH
      NAMELIST    /NMBS/    CBS0, CBS
*
      DATA       UNDEF1  / -9.999E20  /
      DATA       XMISS   / -999.      /
      DATA       NMO     / 12         /
      DATA       NOUT    / 6          /

      DATA KLEV1 / NLEV0, NLEV0, NLEV0, NLEV0, NLEV0, 1 /
      DATA      PLEV0/ 1000.,925.,850.,775.,700.,600.,500.,
     &                 400.,300.,250.,200.,150.,100.,70.,
     &                 50.,30.,20.,10,7.,5.,3.,2.,1./
      DATA      SIG5 / 0.8987,0.6983,0.4439,0.2220,0.06224,0.,0.,
     &                 0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0./
      DATA CHEAD5  /'CSIG5           ','               5'/
      DATA      SIG8  / 0.99500,0.94474,0.82930,0.65299,0.46314,
     &                  0.30278,0.16186,0.041495,0.,0.,
     &                  0.,0.,0.,0.,0.,0.,0.,0.,0.,0. /
      DATA CHEAD8  /'CSIG8           ','               8'/
      DATA      SIG11 / 0.99500,0.97999,0.94995,0.89988,0.81456,
     &                  0.67888,0.51332,0.34780,0.20250,0.0923428,
     &                  0.0207475,0.,0.,0.,0.,0.,0.,0.,0.,0. /
      DATA CHEAD11 /'CSIG11          ','              11'/
      DATA      SIG20 / 0.99500,0.97999,0.94995,0.89988,0.82977,
     &                  0.74468,0.64954,0.54946,0.45447,0.36948,
     &                  0.29450,0.22953,0.17457,0.12440,0.0846830,
     &                  0.0598005,0.0449337,0.0349146,0.0248800,
     &                  0.00829901/
      DATA CHEAD20 /'CSIG20          ','              20'/

      DATA KMO       / 12      /
      DATA NAVG      / 3       /
      DATA OZM       / .FALSE. /
      DATA OSW       / .FALSE. /
      DATA ONH       / .FALSE. /
      DATA OSH       / .FALSE. /

      DATA RAIR      / 287.04  /
      DATA GRAV      / 9.8D0   /
      DATA EPSV      / 0.622D0 /
*
*     open the NAMELIST file
*
      REWIND( 1 )
      OPEN ( 0, FILE='IEEE_ERROR')
      OPEN ( 1, FILE = 'SETPAR', STATUS = 'OLD')
*
      READ( 1, NMECM) 
      REWIND( 1 )
      READ( 1, NMBS) 
*
*
      WRITE( NOUT, * )
      WRITE( NOUT, * ) '### start making basic state ###'
      WRITE( NOUT, * )
*
*     selected parameters
*
      WRITE( NOUT, *) '... selected month from:', KMO,' ...'
      WRITE( NOUT, *) '... number of month averaged:', NAVG,' ...'
      IF( KMAX . EQ. 5 ) THEN
         CHEAD( 1 ) = CHEAD5( 1 )
         CHEAD( 2 ) = CHEAD5( 2 )
         DO 100 K = 1, KMAX
            SIG( K ) = SIG5( K )
 100     CONTINUE
      ELSEIF( KMAX . EQ. 8 ) THEN
         CHEAD( 1 ) = CHEAD8( 1 )
         CHEAD( 2 ) = CHEAD8( 2 )
         DO 110 K = 1, KMAX
            SIG( K ) = SIG8( K )
 110     CONTINUE
      ELSEIF( KMAX . EQ. 11 ) THEN
         CHEAD( 1 ) = CHEAD11( 1 )
         CHEAD( 2 ) = CHEAD11( 2 )
         DO 120 K = 1, KMAX
            SIG( K ) = SIG11( K )
 120     CONTINUE
      ELSEIF( KMAX . EQ. 20 ) THEN
         CHEAD( 1 ) = CHEAD20( 1 )
         CHEAD( 2 ) = CHEAD20( 2 )
         DO 130 K = 1, KMAX
            SIG( K ) = SIG20( K )
 130     CONTINUE
      ELSE
         WRITE( NOUT, *) '   ### level not assigned ### '
         CALL XABORT( 1 )
      ENDIF
*
      OPEN (30, FILE=CALT,  FORM='UNFORMATTED', STATUS='OLD' )
      OPEN (70, FILE=CBS0,  FORM='UNFORMATTED', STATUS='UNKNOWN' )
      OPEN (80, FILE=CBS,   FORM='UNFORMATTED', STATUS='UNKNOWN' )
      WRITE( NOUT, *) 
      WRITE( NOUT, *) ' @@ Input :',CECM
      WRITE( NOUT, *) ' @@ Input :',CALT
      WRITE( NOUT, *) ' @@ Output:',CBS0
      WRITE( NOUT, *) ' @@ Output:',CBS
      WRITE( NOUT, *) 
*
*     read input
*
      ISSN = 0
      IAVG = 0
      WRITE( NOUT, *) '... data read start ...'
      ISSN = KMO
 222  OPEN (10, FILE=CECM, FORM='UNFORMATTED', STATUS='OLD' )
      WRITE( NOUT, *) '   ... month = ', ISSN
      IREC = 0
      DO 200 N = 1, NVAR1
         IREC = IREC + (ISSN-1)*KLEV1(N)
 200  CONTINUE
      DO 210 N = 1, IREC
         READ( 10, END=111 ) DAT0
 210  CONTINUE

      ILEV = 0
      IVAR = 1
 300  ILEV = ILEV + 1
      READ( 10, END=111) DAT0

      IF( ONH ) THEN
         DO 800 J = 1, JMAX/2
            DO 800 I = 1, IMAX
               DAT0( I,J ) = DAT0( I,JMAX+1-J )
 800     CONTINUE
      ELSEIF( OSH ) THEN
         DO 810 J = JMAX/2+1, JMAX
            DO 810 I = 1, IMAX
               DAT0( I,J ) = DAT0( I,JMAX+1-J )
 810     CONTINUE
      ENDIF

      IF( IVAR .EQ. 1 ) THEN
         IJ = 0
         DO 20 J = 1, JMAX
            DO 20 I = 1, IMAX
            IJ = IJ + 1
            UP( IJ, ILEV) = 
     &           UP( IJ, ILEV) + DAT0( I, JMAX+1-J ) / FLOAT(NAVG)
            IF( DAT0( I, JMAX+1-J ) .EQ. UNDEF1 ) UP( IJ, ILEV) = XMISS
   20    CONTINUE
      ENDIF
      IF( IVAR .EQ. 2 ) THEN
         IJ = 0
         DO 30 J = 1, JMAX
            DO 30 I = 1, IMAX
            IJ = IJ + 1
            VP( IJ, ILEV) = 
     &           VP( IJ, ILEV) + DAT0( I, JMAX+1-J ) / FLOAT(NAVG)
            IF( DAT0( I, JMAX+1-J ) .EQ. UNDEF1 ) VP( IJ, ILEV) = XMISS
   30    CONTINUE
      ENDIF
      IF( IVAR .EQ. 3 ) THEN
         IJ = 0
         DO 40 J = 1, JMAX
            DO 40 I = 1, IMAX
            IJ = IJ + 1
            TP( IJ, ILEV) = 
     &           TP( IJ, ILEV) + DAT0( I, JMAX+1-J ) / FLOAT(NAVG)
            IF( DAT0( I, JMAX+1-J ) .EQ. UNDEF1 ) TP( IJ, ILEV) = XMISS
   40    CONTINUE
      ENDIF
      IF( IVAR .EQ. 4 ) THEN
         IJ = 0
         DO 50 J = 1, JMAX
            DO 50 I = 1, IMAX
            IJ = IJ + 1
            ZP( IJ, ILEV) = 
     &           ZP( IJ, ILEV) + DAT0( I, JMAX+1-J ) / FLOAT(NAVG)
            IF( DAT0( I, JMAX+1-J ) .EQ. UNDEF1 ) ZP( IJ, ILEV) = XMISS
   50    CONTINUE
      ENDIF
      IF( IVAR .EQ. 5 ) THEN
         IJ = 0
         DO 60 J = 1, JMAX
            DO 60 I = 1, IMAX
            IJ = IJ + 1
            QP( IJ, ILEV) = 
     &           QP( IJ, ILEV) + DAT0( I, JMAX+1-J ) / FLOAT(NAVG) 
            IF( DAT0( I, JMAX+1-J ) .EQ. UNDEF1 ) QP( IJ, ILEV) = XMISS
   60    CONTINUE
      ENDIF
      IF( IVAR .EQ. 6 ) THEN
         IJ = 0
         DO 70 J = 1, JMAX
            DO 70 I = 1, IMAX
            IJ = IJ + 1
            SLP( IJ ) = 
     &           SLP( IJ ) + DAT0( I, JMAX+1-J ) / FLOAT(NAVG) 
            IF( DAT0( I, JMAX+1-J ) .EQ. UNDEF1 ) SLP( IJ ) = XMISS
   70    CONTINUE
      ENDIF

      IF( ILEV .EQ. KLEV1( IVAR ) ) THEN
         ILEV = 0
         IVAR = IVAR + 1
         IF( IVAR .GT. NVAR1 ) GOTO 111
      ENDIF

      GOTO 300
  111 CLOSE( 10 )

      ISSN = ISSN + 1
      IAVG = IAVG + 1
      IF( ISSN .GT. NMO  ) ISSN = ISSN - NMO
      IF( IAVG .EQ. NAVG ) GOTO 333
      GOTO 222
*
  333 WRITE( NOUT, *) '... data read end ...'
*
*     topography
*
      READ( 30 ) HEAD
      READ( 30 ) DAT0
      CLOSE( 30 )
      IF( ONH ) THEN
         DO 820 J = JMAX/2+1, JMAX
            DO 820 I = 1, IMAX
               DAT0( I,J ) = DAT0( I,JMAX+1-J )
 820     CONTINUE
      ELSEIF( OSH ) THEN
         DO 830 J = 1, JMAX/2
            DO 830 I = 1, IMAX
               DAT0( I,J ) = DAT0( I,JMAX+1-J )
 830     CONTINUE
      ENDIF
      IJ = 0
      DO 840 J = 1, JMAX
         DO 840 I = 1, IMAX
            IJ = IJ + 1
            GDZS( IJ ) = DAT0( I,J )
 840  CONTINUE
      WRITE( NOUT, *) '... topography data read end ...'
*
*     Z --> Ps
*
      WRITE( NOUT, *) '... Z used for Ps (temporary) ...'
      CALL Z2PS
     O       ( PP  ,
     I         ZP  , GDZS  , PLEV0,  IMAX*JMAX , NLEV0  )
*
      CALL SPECSM
     I     ( IMAX, JMAX, 1, 'Ps', 
     M       PP,
     W       WS, ZS  )
*
*     p --> sigma transform
*
      CALL COPY4T8( P, PP, IMAX*JMAX )
      WRITE( NOUT, *)
     &     '... P( 23 ) --> S(',KMAX,' ) transform for T and Q ...' 
      CALL COPY4T8( GDI, TP, IMAX*JMAX*NLEV0 )
      CALL P2S
     O     ( T    ,
     I       GDI  , P, PLEV0, SIG, 
     I       IMAX*JMAX, NLEV0, KMAX, XMISS )
      CALL COPY8T4( TS, T, IMAX*JMAX )
      CALL SPECSM
     I     ( IMAX, JMAX,   1, 'T ', 
     M       TS,
     W       WT, ZT  )
*
      CALL COPY4T8( GDI, QP, IMAX*JMAX*NLEV0 )
      CALL P2S
     O     ( Q    ,
     I       GDI  , P, PLEV0, SIG, 
     I       IMAX*JMAX, NLEV0, KMAX, XMISS )
      CALL COPY8T4( QS, Q, IMAX*JMAX )
      CALL SPECSM
     I     ( IMAX, JMAX,   1, 'Q ', 
     M       QS,
     W       WT, ZT  )
*
*     SLP -> Ps
*
      WRITE( NOUT, *) '... SLP --> Ps ...'
      DO 850 IJ = 1, IMAX*JMAX
         EPSVT  = 1.D0 / EPSV - 1.D0
         TLAPSX = 6.5D-3
         VTMP   = DBLE( TS( IJ ) ) 
     &              * ( 1.D0 + EPSVT * DBLE( QS( IJ ) ) ) !! sfc vertual temp.
         TVS    = VTMP 
     &            * ( 1.D0 + TLAPSX * RAIR / GRAV * (1.D0/SIG(1)-1.D0) )
         TV0    = TVS + DBLE( GDZS( IJ ) ) * TLAPSX

         IF ( TV0 .GT. 290.5D0 .AND. TVS .LE. 290.5D0 ) THEN
            TLAPSX = ( 290.5D0 - TVS ) / DBLE( GDZS( IJ ) )
         ELSEIF ( TV0 .GT. 290.5D0 .AND. TVS .GT. 290.5D0 ) THEN
            TLAPSX = 0.D0
            TVS    = 0.5D0 * ( 290.5D0 + TVS )
         ELSEIF ( TVS .LT. 255.D0 ) THEN
            TVS    = 0.5D0 * ( 255.D0 + TVS )
         ENDIF
         TVX    = TLAPSX * DBLE( GDZS( IJ ) ) / TVS
         TVX    = 1.D0 - TVX / 2.D0 + TVX*TVX / 3.D0
         TVX    = EXP(      GRAV * DBLE( GDZS( IJ ) ) 
     &                  / ( RAIR * TVS ) * TVX          ) 
         PP( IJ ) = SLP( IJ ) / SNGL( TVX )
 850  CONTINUE
      CALL SPECSM
     I     ( IMAX, JMAX, 1, 'Ps', 
     M       PP,
     W       WS, ZS  )
*
*     p --> sigma transform
*
      CALL COPY4T8( P, PP, IMAX*JMAX )
      WRITE( NOUT, *) 
     &     '... P( 23 ) --> S(',KMAX,' ) transform ...'
      CALL COPY4T8( GDI, UP, IMAX*JMAX*NLEV0 )
      CALL P2S
     O     ( U    ,
     I       GDI  , P, PLEV0, SIG, 
     I       IMAX*JMAX, NLEV0, KMAX, XMISS )
      CALL COPY4T8( GDI, VP, IMAX*JMAX*NLEV0 )
      CALL P2S
     O     ( V    ,
     I       GDI  , P, PLEV0, SIG, 
     I       IMAX*JMAX, NLEV0, KMAX, XMISS )
      CALL COPY4T8( GDI, TP, IMAX*JMAX*NLEV0 )
      CALL P2S
     O     ( T    ,
     I       GDI  , P, PLEV0, SIG, 
     I       IMAX*JMAX, NLEV0, KMAX, XMISS )
      CALL COPY4T8( GDI, QP, IMAX*JMAX*NLEV0 )
      CALL P2S
     O     ( Q    ,
     I       GDI  , P, PLEV0, SIG, 
     I       IMAX*JMAX, NLEV0, KMAX, XMISS )
*
*     read basic/add/write
*
      WRITE( NOUT, *) '... write basic state ...'
      DO 600 N = 1, 64
         HEAD( N ) = CHEADU( N )
 600  CONTINUE
      HEAD( 35 ) = CHEAD( 1 )
      HEAD( 37 ) = CHEAD( 2 )
      CALL COPY8T4( GT, U, IMAX*JMAX*KMAX )
      CALL SPECSM
     I     ( IMAX, JMAX, KMAX, 'U ', 
     M       GT,
     W       WT, ZT  )
      CALL ZMSW
     M     ( GT  , 
     I       IMAX, JMAX, KMAX, OZM, OSW )
      WRITE( 70 ) HEAD
      WRITE( 70 ) GT
      DO 610 K = 1, KMAX
         WRITE( 80 ) (GT(IJ,K),IJ=1,IMAX*JMAX)
 610  CONTINUE
      WRITE( NOUT, *) '    ... U ... '

      DO 620 N = 1, 64
         HEAD( N ) = CHEADV( N )
 620  CONTINUE
      HEAD( 35 ) = CHEAD( 1 )
      HEAD( 37 ) = CHEAD( 2 )
      CALL COPY8T4( GT, V, IMAX*JMAX*KMAX )
      CALL SPECSM
     I     ( IMAX, JMAX, KMAX, 'V ', 
     M       GT,
     W       WT, ZT  )
      CALL ZMSW
     M     ( GT  , 
     I       IMAX, JMAX, KMAX, OZM, OSW )
      WRITE( 70 ) HEAD
      WRITE( 70 ) GT
      DO 630 K = 1, KMAX
         WRITE( 80 ) (GT(IJ,K),IJ=1,IMAX*JMAX)
 630  CONTINUE
      WRITE( NOUT, *) '    ... V ... '

      DO 640 N = 1, 64
         HEAD( N ) = CHEADT( N )
 640  CONTINUE
      HEAD( 35 ) = CHEAD( 1 )
      HEAD( 37 ) = CHEAD( 2 )
      CALL COPY8T4( GT, T, IMAX*JMAX*KMAX )
      CALL SPECSM
     I     ( IMAX, JMAX, KMAX, 'T ', 
     M       GT,
     W       WT, ZT  )
      CALL ZMSW
     M     ( GT  , 
     I       IMAX, JMAX, KMAX, OZM, OSW )
      WRITE( 70 ) HEAD
      WRITE( 70 ) GT
      DO 650 K = 1, KMAX
         WRITE( 80 ) (GT(IJ,K),IJ=1,IMAX*JMAX)
 650  CONTINUE
      WRITE( NOUT, *) '    ... T ... '

      DO 660 N = 1, 64
         HEAD( N ) = CHEADP( N )
 660  CONTINUE
      CALL COPY8T4( GS, P, IMAX*JMAX )
      CALL ZMSW
     M     ( GS  , 
     I       IMAX, JMAX, 1, OZM, OSW )
      WRITE( 70 ) HEAD
      WRITE( 70 ) GS
      WRITE( 80 ) GS
      WRITE( NOUT, *) '    ... Ps ... '

      DO 670 N = 1, 64
         HEAD( N ) = CHEADQ( N )
 670  CONTINUE
      HEAD( 35 ) = CHEAD( 1 )
      HEAD( 37 ) = CHEAD( 2 )
      CALL COPY8T4( GT, Q, IMAX*JMAX*KMAX )
      CALL SPECSM
     I     ( IMAX, JMAX, KMAX, 'Q ', 
     M       GT,
     W       WT, ZT  )
      CALL ZMSW
     M     ( GT  , 
     I       IMAX, JMAX, KMAX, OZM, OSW )
      WRITE( 70 ) HEAD
      WRITE( 70 ) GT
      DO 680 K = 1, KMAX
         WRITE( 80 ) (GT(IJ,K),IJ=1,IMAX*JMAX)
 680  CONTINUE
      WRITE( NOUT, *) '    ... Q ... '
*
*     Ql (dummy)
*
C      DO 690 N = 1, 64
C         HEAD( N ) = CHEADQ( N )
C 690  CONTINUE
C      HEAD( 35 ) = CHEAD( 1 )
C      HEAD( 37 ) = CHEAD( 2 )
C      DO 700 K = 1, KMAX
C         DO 700 IJ = 1, IMAX*JMAX
C            GT( IJ,K ) = 0.
C 700  CONTINUE
C      WRITE( 70 ) HEAD
C      WRITE( 70 ) GT
*
      CLOSE( 70 )
      CLOSE( 80 )
      WRITE( NOUT, *) '... end execution ...'
*
*
  999 STOP
      END
************************************
      SUBROUTINE COPY4T8( 
     O     GD8, 
     I     GD4, IDIM )

      INTEGER   IDIM, I
      REAL*8    GD8( IDIM )
      REAL*4    GD4( IDIM )

      DO 10 I = 1, IDIM
         GD8( I ) = DBLE( GD4( I ) )
   10 CONTINUE

      RETURN
*===================================
      ENTRY COPY8T4( 
     O     GD4, 
     I     GD8, IDIM )

      DO 20 I = 1, IDIM
         GD4( I ) = SNGL( GD8( I ) )
   20 CONTINUE

      RETURN
      END
************************************
      SUBROUTINE ACOPY4T8( 
     O     GD8, 
     I     GD4, IDIM, JDIM )

      INTEGER   IDIM, JDIM, I, J
      REAL*8    GD8( IDIM+1, JDIM)
      REAL*4    GD4( IDIM, JDIM)

      DO 10 J = 1, JDIM
         DO 20 I = 1, IDIM
         GD8( I, J) = DBLE( GD4( I, J) )
   20 CONTINUE
      GD8( IDIM+1, J) = GD8( 1, J)
   10 CONTINUE

      RETURN
*===================================
      ENTRY ACOPY8T4( 
     O     GD4, 
     I     GD8, IDIM, JDIM )

      DO 30 J = 1, JDIM
         DO 40 I = 1, IDIM
            GD4( I, J) = SNGL( GD8( I, J) )
   40    CONTINUE
   30 CONTINUE

      RETURN
      END
************************************
      SUBROUTINE SPECSM
     I     ( JX, JY, JZ, CV, 
     M       G4,
     W       W8, G8  )
*
      INTEGER NMAX, NMDIM
      PARAMETER ( NMAX = 21, NMDIM = 506 )
*
      INTEGER JX, JY, JZ
      CHARACTER CV*2
      REAL*4  G4( JX, JY, JZ)
      REAL*8  G8( JX+1, JY, JZ)      
      REAL*8  W8( NMDIM, JZ)

      REAL*8    ER, PI
      INTEGER   NMO   ( 2, 0:NMAX, 0:NMAX ) !! order of spect. suffix
      REAL*8    EDEL  ( NMDIM  ) !! vor.,D -> U,V
      LOGICAL OFIRST
      DATA    OFIRST / .TRUE. /
*
*     spectral smoothing
*
      IF( OFIRST ) THEN
         ER = 6370.D+3
         PI = ATAN( 1.D0 ) * 4.D0
         CALL SETCONS
         CALL SPSTUP            !! spherical harmonic functions
         CALL SETNMO2
     O        ( NMO   ,
     D        NMAX  , NMAX  , NMAX  , 1    )
         CALL DSETED2
     O        ( EDEL, 
     I        NMDIM, NMAX, NMAX, NMAX, 1, NMO, ER )
         OFIRST = .FALSE.
      ENDIF
*
      WRITE( 6, *) '... spectral smoothing for ',CV,' ...'
      CALL ACOPY4T8( G8, G4, JX, JY )
      CALL G2W
     M     ( W8  ,
     I       G8  , '    ', 'POS ', JZ )
      CALL W2G
     O     ( G8  ,
     I       W8  , '    ', 'POS ', JZ )
      CALL ACOPY8T4( G4, G8, JX, JY )

      RETURN
      END
***************************************************************
      SUBROUTINE Z2PS
     O         ( GDPS  , 
     I           GDZ   , GDZS  , PLEV0 , IDIM , NLEV  )
*
      INTEGER    IDIM, NLEV
*
*   [OUTPUT] 
      REAL*4     GDPS ( IDIM )
*   [INPUT] 
      REAL*4     GDZ  ( IDIM, NLEV )
      REAL*4     GDZS ( IDIM )
      REAL*8     PLEV0 ( NLEV )
*
      REAL*4     AL, BL, CL, DL
      INTEGER    KA, IJ, K
*
      KA = NLEV
      DO 10 IJ = 1, IDIM
         DO 20 K = 1, NLEV
            IF ( GDZ(IJ,K) .GT. GDZS(IJ) ) THEN
               KA = K
               GOTO 30
            ENDIF
   20    CONTINUE
   30    CONTINUE
         KA = MIN( MAX( KA,2 ), NLEV )
         AL = LOG( SNGL(PLEV0(KA-1)) )
         BL = ( LOG( SNGL(PLEV0(KA)) ) - LOG( SNGL(PLEV0(KA-1)) ) )
         CL = ( GDZS(IJ)    - GDZ(IJ,KA-1) )
         DL = ( GDZ (IJ,KA) - GDZ(IJ,KA-1) )
         GDPS(IJ) = EXP( AL + BL * CL / DL )
   10 CONTINUE
*
      RETURN
      END
***************************************************************
      SUBROUTINE ZMSW
     M         ( GDX  , 
     I           IDIM, JDIM, KDIM, OZM, OSW )

      INTEGER  IDIM, JDIM, KDIM
      REAL*4   GDX( IDIM,JDIM,KDIM )
      LOGICAL  OZM, OSW

      REAL*4   ZM
      INTEGER  I, J, K

      DO K = 1, KDIM
         DO J = 1, JDIM
            ZM = 0.
            DO I = 1, IDIM
               ZM = ZM + GDX( I,J,K ) / FLOAT( IDIM )
            ENDDO
            IF( OZM ) THEN
               DO I = 1, IDIM
                  GDX( I,J,K ) = ZM
               ENDDO
            ELSEIF( OSW ) THEN
               DO I = 1, IDIM
                  GDX( I,J,K ) = GDX( I,J,K ) - ZM
               ENDDO
            ENDIF
         ENDDO
      ENDDO

      RETURN
      END
***************************************************************
      SUBROUTINE P2S
     O         ( GDO  ,
     I           GDI, GDPS, PLI, SLI, IJDIM, KMAXP, KMAXS, XMISS )
*
*   [PARAM] 
      INTEGER    IJDIM
      INTEGER    KMAXP, KMAXS
*   [OUTPUT] 
      REAL*8       GDO ( IJDIM, KMAXS ) !! sigma level
*   [INPUT] 
      REAL*8       GDPS( IJDIM )
      REAL*8       GDI ( IJDIM, KMAXP ) !! p level
      REAL*8       PLI ( KMAXP )
      REAL*8       SLI ( KMAXS )
      LOGICAL    OMISB
      LOGICAL    OMIST
      REAL*8       XMISS
      DATA       OMISB / .FALSE. /
      DATA       OMIST / .FALSE. /
*
*   [INTERNAL WORK] 
      INTEGER    KMAXD
      PARAMETER  ( KMAXD = 100 )
      REAL*8       PILN ( KMAXD )
      REAL*8       POLN ( KMAXD )
      REAL*8       GDIZ ( KMAXD )
      REAL*8       GDOZ ( KMAXD )
      INTEGER    IJ, KI, KO
*
      DO 1000 KI = 1, KMAXP
         PILN( KI ) = LOG( PLI( KI ) )
 1000 CONTINUE 

      DO 2000 IJ = 1, IJDIM
         DO 2010 KI = 1, KMAXP
            GDIZ(KI) = GDI(IJ,KI)
 2010    CONTINUE 
         DO 2020 KO = 1, KMAXS
            POLN(KO) = LOG( GDPS(IJ)*SLI(KO) )
 2020    CONTINUE
         CALL SPLINE
     O        ( GDOZ,
     I                 POLN,  KMAXS,
     I          GDIZ,  PILN,  KMAXP,
     I          OMISB, OMIST, XMISS  )
         DO 2100 KO = 1, KMAXS
            GDO(IJ,KO) = GDOZ(KO)
 2100    CONTINUE 
 2000 CONTINUE
*
      RETURN
      END
*********************************************************************
      SUBROUTINE SPLINE
     O         ( ZI   ,
     I                  XI   , LMAX ,
     I           Z    , X    , KMAX , 
     I           OMISB, OMIST, XMISS  )
*
      INTEGER    LMAX, KMAX
      REAL*8       ZI  ( LMAX )
      REAL*8       XI  ( LMAX )
      REAL*8       Z   ( KMAX )
      REAL*8       X   ( KMAX )
      LOGICAL    OMISB
      LOGICAL    OMIST
      REAL*8       XMISS

*   [INTERNAL WORK] 
      INTEGER    KMAXD
      PARAMETER  ( KMAXD = 1024 )
      REAL*8       Y2  ( KMAXD )
      REAL*8       U   ( KMAXD )
      REAL*8       SIG, P, QN, UN, H, A, B 
      INTEGER    K, L, KU
*
      IF ( KMAX .GT. KMAXD ) THEN
         WRITE (6,*) ' ### KMAXD IS TOO SMALL ', KMAXD, KMAX
         STOP
      ENDIF
*
      Y2(1)=0.
      U (1)=0.
      DO 120 K=2, KMAX-1
         SIG   = (X(K)-X(K-1))/(X(K+1)-X(K-1))
         P     = SIG*Y2(K-1)+2.
         Y2(K) = (SIG-1.)/P
         U (K) = (6.*( (Z(K+1)-Z(K))/(X(K+1)-X(K))
     &                -(Z(K)-Z(K-1))/(X(K)-X(K-1)))
     &              /(X(K+1)-X(K-1))
     &             - SIG*U(K-1)                     )/P
  120 CONTINUE
      QN = 0.
      UN = 0.
      Y2(KMAX) = (UN-QN*U(KMAX-1))/(QN*Y2(KMAX-1)+1.)
      DO 130 K= KMAX-1, 1, -1
         Y2(K) = Y2(K)*Y2(K+1)+U(K)
  130 CONTINUE
*
      DO 500 L = 1, LMAX
         KU = 1
         DO 300 K = 1, KMAX
            IF( X(K) .LT. XI(L) ) THEN
               KU = K
               GOTO 310
            ENDIF
  300    CONTINUE
         KU = KMAX+1
  310    CONTINUE
*
         IF      ( KU .EQ. 1 ) THEN
            IF ( OMISB ) THEN
               ZI(L) = XMISS
            ELSE
               ZI(L) = Z(1)
            ENDIF
         ELSE IF ( KU .EQ. KMAX+1 ) THEN
            IF ( OMIST ) THEN
               ZI(L) = XMISS
            ELSE
               ZI(L) = Z(KMAX)
            ENDIF            
         ELSE
            KU   = MAX(KU,2)
            H    = X(KU)-X(KU-1)
            A    = (X(KU)-XI(L))/H
            B    = (XI(L)-X(KU-1))/H
            ZI(L)= A*Z(KU-1)+B*Z(KU)
     &           + (A*(A*A-1)*Y2(KU-1)+B*(B*B-1)*Y2(KU))*(H*H)/6.
         ENDIF
  500 CONTINUE
*
      RETURN
      END
*********************************************************************
      SUBROUTINE SETNMO2    !! order of matrix
     O         ( NMO   ,
     D           MMAX  , LMAX  , NMAX  , MINT    )
*
*   [PARAM] 
      INTEGER    MMAX
      INTEGER    LMAX
      INTEGER    NMAX
      INTEGER    MINT
*
*   [OUTPUT]
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
*
*   [INTERNAL WORK] 
      INTEGER    L, M, MEND, NMH
*
      NMH  = 0
      DO 2200 L = 0, LMAX
         MEND = MIN( MMAX, NMAX-L )
         DO 2100 M = 0, MEND, MINT
            NMH = NMH + 1
            IF ( MMAX .EQ. 0 ) THEN
               NMO ( 1, M, L ) = NMH
               NMO ( 2, M, L ) = NMH
            ELSE
               NMO ( 1, M, L ) = 2* NMH - 1
               NMO ( 2, M, L ) = 2* NMH
            ENDIF
 2100    CONTINUE
 2200 CONTINUE
*
      RETURN
      END
*********************************************************************
      SUBROUTINE    DSETED2          !! factors for spectral calculation
     O     ( EDEL,
     I     NMDIM, NMAX, LMAX, MMAX, MINT, NMO, ER )
*
      INTEGER NMDIM, NMAX, LMAX, MMAX, MINT
      INTEGER NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      REAL*8  EDEL  ( NMDIM  )  !! vor.,D -> U,V
      REAL*8  ER
*
      INTEGER L, M, N, LEND
*
      IF ( LMAX .NE. 0 ) THEN      
         DO 3110 M = 0 , MMAX, MINT
            LEND = MIN( LMAX, NMAX-M )
            DO 3100 L = 0 , LEND
               N = L + M
               IF ( N .GT. 0 ) THEN
                  EDEL  ( NMO(1,M,L) )= - ER / DBLE( N*(N+1) )
                  EDEL  ( NMO(2,M,L) )= - ER / DBLE( N*(N+1) )
               ENDIF
 3100       CONTINUE
 3110    CONTINUE
         EDEL  ( NMO(1,0,0) ) =  0.  
         EDEL  ( NMO(2,0,0) ) =  0.  
      ELSE
         DO 3200 M = 0 , MMAX, MINT
            IF ( M .GT. 0 ) THEN
               EDEL  ( NMO(1,M,0) )= - ER / DBLE( M**2 )
               EDEL  ( NMO(2,M,0) )= - ER / DBLE( M**2 )
            ENDIF
 3200    CONTINUE
         EDEL  ( NMO(1,0,0) ) =  ER
         EDEL  ( NMO(2,0,0) ) =  0.  
      ENDIF
*
      RETURN
      END
