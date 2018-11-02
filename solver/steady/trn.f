      PROGRAM TRN
*
*     derived from trnpw.f on 2001/09/20
*     This program transforms a matrix in wave space to that in a
*     physical space.
*
*     should be run on DEC alpha
*
      include 'dim.f'
*
*
      INTEGER    MAXN
      REAL*8     ER                          !! radius of the earth
      REAL*8     XMISS

      PARAMETER( MAXN  = 2*NMAX*NTR*(NVAR*KMAX+1) )
CC      PARAMETER ( MAXN=10935) !! T21L11m6 moist LBM
CC      PARAMETER ( MAXN=15795) !! T21L11m10 moist LBM
      PARAMETER( ER    = 6370.E+3 )
      PARAMETER( XMISS = -999.0 )

      REAL*8      DAT( MAXN )
      REAL*8      W( NMDIM, KMAX, NVAR+1 )
      REAL*8      G( IDIM*JMAX, KMAX )
      REAL*8      G2( IDIM*JMAX, KMAX )
      REAL*8      GTV( IDIM*JMAX, KMAX )
      REAL*8      GQ( IDIM*JMAX, KMAX )
      REAL*8      GZ( IDIM*JMAX, KMAX )
      REAL*8      GP( IDIM*JMAX      )
      REAL*8      GPS( IDIM*JMAX      )
      REAL*8      DUM( IDIM*JMAX      )
      REAL*4      GO( IMAX, JMAX )
      INTEGER     IFS, IFG, IFB
      INTEGER     I, J, K, L, M1, M2, M3, N, IJ, NM, ICHK
      INTEGER     LDA, MSIZT, MVAR, MTR, MTR2, LEND
*
*     [work]
      REAL*8      EDEL  ( NMDIM  )   !! vor.,D -> U,V
      REAL*8      SIG( KMAX )        !! sigma level
      REAL*8      SIGM( KMAX+1 )     !! sigma level increment
      REAL*8      DSIG( KMAX )       !! unused
      REAL*8      DSIGM( KMAX+1 )    !! unused
      REAL*8      U( IDIM*JMAX, KMAX ) !! U for O2UV=.true.
      REAL*8      V( IDIM*JMAX, KMAX ) !! V for O2UV=.true.
      REAL*8      SALPHA( KMAX  )    !! fact. of hydrostatic eq.
      REAL*8      SBETA ( KMAX  )    !! fact. of hydrostatic eq.
      CHARACTER   HSIG *(16)         !! unused
      CHARACTER   HSIGM *(16)        !! unused
      INTEGER     NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      INTEGER     NOUT
      INTEGER     KLEV( NVAR+1 )

      CHARACTER   CVAR( NVAR+1 )

      SAVE        GPS, GQ
      SAVE        NMO, EDEL, SALPHA, SBETA, SIG, SIGM
*     ..
*     .. External Subroutines ..
      EXTERNAL    W2G
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC   DBLE, SNGL
*
*     namelist variables
*
      CHARACTER*90 CFM          !! input file name (matrix A)
      CHARACTER*90 CFR          !! input file name (rhs)
      CHARACTER*90 CFS          !! output file name (temporary)
      CHARACTER*90 CFG          !! output file name (grads)
      CHARACTER*90 CBS          !! basic state
      LOGICAL      O2UV         !! write UV instead of PSI/XI
      LOGICAL      OPL          !! sigma --> pressure
      LOGICAL      OWALL        !! write all the wavenumber
      LOGICAL      OCLASSIC     !! classic dry model

      NAMELIST    /NMFIN/   CFM, CFR, CFS, CFG
      NAMELIST    /NMBS/    CBS
      NAMELIST    /NMUV/    O2UV, OPL
      NAMELIST    /NMALL/   OWALL
      NAMELIST    /NMCLS/   OCLASSIC


      DATA         NOUT / 6 /
      DATA         CVAR   /'v','d','t','p','q'/
      DATA         KLEV   / KMAX, KMAX, KMAX,   1, KMAX /
      DATA         O2UV     / .FALSE. / !! psi,chi
      DATA         OPL      / .TRUE.  / !! sigma->pressure
      DATA         OWALL    / .FALSE. / !! PWM
      DATA         OCLASSIC / .TRUE.  / !! dry model
*
*     open the NAMELIST file
*
      REWIND( 1 )
      OPEN ( 0, FILE='IEEE_ERROR')
      OPEN ( 1, FILE = 'SETPAR', STATUS = 'OLD')
*
      READ( 1, NMFIN) 
      READ( 1, NMBS) 
      READ( 1, NMUV) 
      READ( 1, NMALL) 
      READ( 1, NMCLS) 
*
      WRITE( NOUT, *) 
      WRITE( NOUT, *) '### transform wave --> grid ###'
      WRITE( NOUT, *) 
      WRITE( NOUT, *) ' @@@ output:', CFG
      WRITE( NOUT, *) 
*
*     input file
*
      IFS = 88
      open( IFS, FILE = CFS, FORM = 'UNFORMATTED',
     $     STATUS = 'UNKNOWN' )
*
*     output file
*
      IFG = 44
      OPEN( IFG, FILE = CFG, FORM = 'UNFORMATTED', 
     $     STATUS = 'UNKNOWN' )
*
*     read basic state Ps and Q for calc. Ps and Z
*
      IFB = 55
      OPEN( IFB, FILE = CBS, FORM = 'UNFORMATTED', 
     $     STATUS = 'UNKNOWN' )
      DO 100 I = 1, KMAX*3
         READ( IFB ) GO
 100  CONTINUE
      READ( IFB ) GO
      DO 110 J = 1, JMAX
         DO 120 I = 1, IMAX
            IJ = IJ + 1
            GP( IJ  ) = DBLE( LOG( GO( I, J) ) )
            GPS( IJ ) = DBLE( GO( I, J) )
 120     CONTINUE
         IJ = IJ + 1
         GP( IJ  ) = DBLE( LOG( GO( 1, J) ) )
         GPS( IJ ) = DBLE( GO( 1, J) )
 110  CONTINUE
*
      DO 130 K = 1, KMAX
         READ( IFB ) GO
         IJ = 0
         DO 140 J = 1, JMAX
            DO 150 I = 1, IMAX
               IJ = IJ + 1
               GQ( IJ, K) = DBLE( GO( I, J) )
 150        CONTINUE
            IJ = IJ + 1
            GQ( IJ, K) = DBLE( GO( 1, J) )
 140     CONTINUE
 130  CONTINUE
      CLOSE( IFB )
*
      CALL SETCONS
      CALL SPSTUP               !! spherical harmonic functions
      CALL SETNMO2
     O     ( NMO   ,
     D     MMAX  , LMAX  , NMAX  , MINT    )
      CALL DSETED2
     O     ( EDEL, 
     I     NMDIM, NMAX, LMAX, MMAX, MINT, NMO, ER )
      CALL SETSIG
     O     ( SIG   , DSIG , HSIG  , 
     O       SIGM  , DSIGM, HSIGM  )
      CALL VTFACT
     O     ( SALPHA, SBETA, 
     I     KMAX  , SIG  , SIGM )
      CALL RESET( DUM, IDIM*JMAX )
      CALL GETLAT
*
*     read matrix X
*
      LEND = 0
      DO 200 M1 = 0, NTR
         L = NMAX - M1 + 1
         IF( M1 .EQ. 0 ) THEN
            L = L - 1
         ELSE
            L = 2 * L
         ENDIF
         LEND = LEND + L
 200  CONTINUE
*
      MTR = NTR
      MVAR = NVAR
      IF( OWALL ) THEN
         MTR = 0
      ENDIF
      IF( OCLASSIC ) THEN
         MVAR = NVAR -1
      ENDIF
      DO 300 M1 = 0, MIN( MTR, NTR )  !! zonal wave number

         CALL RESET( W,   NMDIM*KMAX*(NVAR+1) )
         CALL RESET( G,   IDIM*JMAX*KMAX      )
         CALL RESET( G2,  IDIM*JMAX*KMAX      )

         L = NMAX - M1 + 1
         IF( M1 .EQ. 0 ) THEN
            L = L - 1
         ELSE
            L = 2 * L
         ENDIF
         IF( OWALL ) THEN
            MSIZT = LEND * ( MVAR * KMAX + 1 )
         ELSE
            MSIZT = L * ( MVAR * KMAX + 1 )
         ENDIF
         LDA = MSIZT
         LEND = MIN( LMAX, NMAX-M1)

         ICHK = 0               !! check count for total #
         READ( IFS ) (DAT(I),I=1,LDA)
*
         NM = 0
         MTR2 = 0
         IF( OWALL ) THEN
            MTR2 = NTR
         ENDIF

         DO 310 M2 = 0, MTR2    !! zonal wave number
            IF( OWALL ) THEN
               LEND = MIN( LMAX, NMAX-M2)
               M3 = M2
            ELSE
               M3 = M1
            ENDIF

            DO 320 N = 1, MVAR + 1
     
               DO 330 K = 1, KLEV( N )
                  DO 340 L = 0, LEND !! meridional wave number
                     IF( M3 .EQ. 0 .AND. L .EQ. 0 ) GOTO 340
                     NM = NM + 1
                     I = NMO( 1, M3, L)
                     W( I, K, N ) = DAT( NM )
                     IF( M3 .EQ. 0 ) GOTO 340
                     NM = NM + 1
                     I = NMO( 2, M3, L)
                     W( I, K, N ) = DAT( NM )
*
 340              CONTINUE
 330           CONTINUE
 320        CONTINUE
            ICHK = NM
 310     CONTINUE
*
*     vor, div --> psi, chi
*
         DO 350 N = 1, 2
            DO 360 K = 1, KLEV( N )
               DO 370 I = 1, NMDIM
                  W( I, K, N ) = W( I, K, N ) * EDEL( I ) * ER 
 370           CONTINUE
 360        CONTINUE
 350     CONTINUE
*
*     wave to grid
* 
         DO 400 N = 1, MVAR + 1
            CALL W2G
     O           ( G       ,
     I             W(1,1,N), '    ', 'POSO', KLEV(N) )
*
*     sigma to pressure
* 
            IF( N .NE. 4 ) THEN
               IF( OPL ) THEN
                  CALL S2P
     O                 ( G2,
     I                   G , GPS, PLEV, SIG, IDIM*JMAX, KMAX, XMISS )
               ELSE
                  CALL MCOPY
     O                 ( G2, 
     I                   G , IDIM*JMAX*KMAX )
               ENDIF
            ENDIF
*
*     write down
*
            DO 410 L = 1, KLEV( N )
               IJ = 0
               DO 420 J = 1, JMAX
                  DO 430 I = 1, IDIM
                     IJ = IJ + 1
                     IF( I .LT. IDIM ) THEN
                        GO( I, J) = SNGL( G2( IJ, L ) )
                        IF( N .EQ. 4 ) THEN
                           GO( I, J) = SNGL( DEXP( 
     &                          G(IJ,L)+GP(IJ) ) - DEXP( GP(IJ) ) )
                        ENDIF
                     ENDIF
 430              CONTINUE
 420           CONTINUE
               WRITE( IFG ) GO 
 410        CONTINUE
*     
*     lead geopotential height
*            
            IF( N .EQ. 3 ) THEN               
               CALL VIRTMD
     O              ( GTV  ,
     I                G    , GQ     )
               CALL CALCZ
     O              ( GZ  ,
     I                GTV , DUM , SALPHA , SBETA ,
     I                IDIM, JDIM, KMAX  )
               IF( OPL ) THEN
                  CALL S2P
     O                 ( G2,
     I                   GZ, GPS, PLEV, SIG, IDIM*JMAX, KMAX, XMISS )
               ELSE
                  CALL MCOPY
     O                 ( G2, 
     I                   GZ, IDIM*JMAX*KMAX )
               ENDIF
*            
               DO 500 L = 1, KLEV( N )
                  IJ = 0
                  DO 510 J = 1, JMAX
                     DO 520 I = 1, IDIM
                        IJ = IJ + 1
                        IF( I .LT. IDIM ) THEN
                           GO( I, J) = SNGL( G2( IJ, L ) )
                        ENDIF
 520                 CONTINUE
 510              CONTINUE
                  WRITE( IFG ) GO 
 500           CONTINUE
               CALL RESET( GZ, IDIM*JMAX*KMAX )
            ENDIF
*
*     re-write U, V if required
* 
            IF( N .EQ. 2 ) THEN
               IF( .NOT. O2UV ) GOTO 1000
*     
*     psi, chi --> u, v
* 
               CALL W2G         !! convert to U nondiv.
     O              ( U       ,
     I                W(1,1,1), 'YGRA', 'NEG ', KLEV(1) )
               CALL W2G         !! convert to V nondiv.
     O              ( V       ,
     I                W(1,1,1), 'XGRA', 'POS ', KLEV(1) )
               CALL W2G         !! convert to U irrot.
     O              ( U       ,
     I                W(1,1,2), 'XGRA', 'ADD ', KLEV(2) )
               CALL W2G         !! convert to U irrot.
     O              ( V       ,
     I                W(1,1,2), 'YGRA', 'ADD ', KLEV(2) )
               CALL MFACT
     M              ( U , 
     I                ER, .TRUE., .TRUE. )
               CALL MFACT
     M              ( V , 
     I                ER, .TRUE., .TRUE. )
               
               IF( OPL ) THEN
                  CALL S2P
     O                 ( G2,
     I                   U , GPS, PLEV, SIG, IDIM*JMAX, KMAX, XMISS )
                  CALL S2P
     O                 ( U ,
     I                   V , GPS, PLEV, SIG, IDIM*JMAX, KMAX, XMISS )
                  CALL MCOPY
     O                 ( V , 
     I                   U , IDIM*JMAX*KMAX )
                  CALL MCOPY
     O                 ( U , 
     I                   G2, IDIM*JMAX*KMAX )
               ENDIF
            
               CALL FREWND( IFG )
               DO 600 L = 1, M1 * ( ( MVAR+1 ) * KMAX + 1 ) !! read dummy
                  READ( IFG ) GO
 600           CONTINUE
               
               DO 610 L = 1, KLEV( N )
                  IJ = 0
                  DO 620 J = 1, JMAX
                     DO 630 I = 1, IDIM-1
                        IJ = IJ + 1
                        GO( I, J) = SNGL( U( IJ, L) ) 
 630                 CONTINUE
                     IJ = IJ + 1
 620              CONTINUE
                  WRITE( IFG ) GO 
 610           CONTINUE
               
               DO 640 L = 1, KLEV( N )
                  IJ = 0
                  DO 650 J = 1, JMAX
                     DO 660 I = 1, IDIM-1
                        IJ = IJ + 1
                        GO( I, J) = SNGL( V( IJ, L) ) 
 660                 CONTINUE
                     IJ = IJ + 1
 650              CONTINUE
                  WRITE( IFG ) GO 
 640           CONTINUE
            ENDIF
 1000       CONTINUE               

 400     CONTINUE
*
*     log
*
         IF( .NOT. OWALL ) 
     $        WRITE( NOUT, *) '# zonal wave:',M1,' check count:',ICHK
*
 300  CONTINUE
      CLOSE( IFS )
      CLOSE( IFG )

      WRITE( NOUT, FMT = * )
      WRITE( NOUT, * ) '### END OF EXECUTION ###'
      WRITE( NOUT, FMT = * )

      STOP
      END
*##############################################
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
*##########################
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
*##########################
      SUBROUTINE VTFACT 
     O     ( SALPHA, SBETA, 
     I       KMAX  , SIG  , SIGM )
*
*   [INPUT] 
      INTEGER    KMAX
      REAL*8     SIG ( KMAX   )
      REAL*8     SIGM( KMAX+1 )
*
*   [OUTPUT] 
      REAL*8     SALPHA( KMAX )              !! fact. of hydrostatic eq.
      REAL*8     SBETA ( KMAX )              !! fact. of hydrostatic eq.
*
*   [INTERNAL WORK]
      REAL*8     RAIR, CP
      DATA       RAIR  / 287.04D0 /
      DATA       CP    / 1004.6D0 /
      REAL*8     AKAPPA
      INTEGER    K
*
      AKAPPA = RAIR / CP
*
      DO 1100 K = 1, KMAX
         SALPHA( K ) =        ( SIGM( K   )/SIG ( K ) )**AKAPPA - 1.D0
         SBETA ( K ) = 1.D0 - ( SIGM( K+1 )/SIG ( K ) )**AKAPPA
 1100 CONTINUE
*
      RETURN
      END
*##########################
      SUBROUTINE CALCZ          !! hydrostatic relation (grid)
     O         ( GDZ   ,
     I           GDT   , GDZS , SALPHA , SBETA ,
     I           IDIM  , JDIM , KMAX  )
*
*     [input]
      INTEGER   IDIM, JDIM, KMAX
      REAL*8    GDT( IDIM*JDIM, KMAX )  !! virtual temp.
      REAL*8    GDZS( IDIM*JDIM      ) !! topography
      REAL*8    SALPHA( KMAX )
      REAL*8    SBETA( KMAX )
*
*     [output]
      REAL*8    GDZ( IDIM*JDIM, KMAX )  !! geopotential height
*
*     [work]
      INTEGER   IJ, K
      REAL*8    CP, GRAV
      DATA      CP   / 1004.6D0 /
      DATA      GRAV / 9.8D0 /
*
      DO 2100 IJ = 1, IDIM*JDIM
         GDZ ( IJ,1 ) = GDZS( IJ )
     &                + CP * SALPHA( 1 ) * GDT ( IJ,1 )/GRAV
 2100 CONTINUE
*
      DO 2200 K = 2, KMAX
         DO 2200 IJ = 1, IDIM*JDIM
            GDZ ( IJ,K ) = GDZ( IJ,K-1 )
     &                     +(  CP * SALPHA( K   ) * GDT ( IJ,K   ) 
     &                       + CP * SBETA ( K-1 ) * GDT ( IJ,K-1 ) )
     &                     / GRAV
 2200 CONTINUE
*
      RETURN
      END
*##############################################
      SUBROUTINE S2P
     O         ( GDO  ,
     I           GDI, GDPS, PLI, SLI, IJDIM, KMAX, XMISS )
*
*   [PARAM] 
      INTEGER    IJDIM
      INTEGER    KMAX
*   [OUTPUT] 
      REAL*8       GDO ( IJDIM, KMAX )
*   [INPUT] 
      REAL*8       GDPS( IJDIM )
      REAL*8       GDI ( IJDIM, KMAX )
      REAL*8       PLI ( KMAX )
      REAL*8       SLI ( KMAX )
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
      REAL*8       TMP
*
      DO 2000 IJ = 1, IJDIM
         DO 2010 KI = 1, KMAX
            GDIZ(KI) = GDI(IJ,KI)
            PILN(KI) = LOG( GDPS(IJ)*SLI(KI) )
 2010    CONTINUE 
         IF(PILN(1).LT.PILN(2))THEN
            DO 2020 KI = 1, KMAX/2
               TMP              = GDIZ(KMAX+1-KI)
               GDIZ(KMAX+1-KI) = GDIZ(KI)
               GDIZ(KI)         = TMP
               TMP              = PILN(KMAX+1-KI)
               PILN(KMAX+1-KI) = PILN(KI)
               PILN(KI)         = TMP
 2020       CONTINUE 
         ENDIF
         DO 2030 KO = 1, KMAX
            POLN(KO) = LOG( PLI(KO) )
 2030    CONTINUE
         CALL SPLINE
     O        ( GDOZ,
     I                 POLN,  KMAX,
     I          GDIZ,  PILN,  KMAX,
     I          OMISB, OMIST, XMISS  )
         DO 2100 KO = 1, KMAX
            GDO(IJ,KO) = GDOZ(KO)
            IF( GDPS(IJ) .LT. PLI(KO) ) GDO(IJ,KO) = XMISS
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
*##########################
      SUBROUTINE MFACT          !! multiply factor
     M     ( A, 
     I       FACT, OINV, OWGT )
*
      include      'dim.f'
*
*   [INPUT] 
      REAL*8     A ( IDIM, JMAX, KMAX )
      REAL*8     FACT
      REAL*8     WGT
      LOGICAL    OINV           !! factor inverse?
      LOGICAL    OWGT           !! multiply latitudinal weight?
*
*   [INTERNAL WORK]
      REAL*8     RAIR, CP
      DATA       RAIR  / 287.04D0 /
      DATA       CP    / 1004.6D0 /
      REAL*8     FACTX
      INTEGER    I, J, K
*
*   [ENTRY SAVE] 
      REAL * 8   ALAT(JMAX)
      SAVE       ALAT
*
*   [ENTRY INTERNAL WORK] 
      REAL * 8   QPN( 0:JMAX+1 )            !" Pn (double precision)
      REAL * 8   DELTP
      REAL * 8   PI, X0, DELTX, R, RP
      REAL * 8   EPS, EPSM, EPSP, QDPN
      INTEGER    N, ITER
      INTEGER    NNLAT
*
*   [ENTRY INTERNAL PARM] 
      INTEGER    ITRMAX
      DATA       ITRMAX / 50 /               !" max. of iteration
*
      FACTX = FACT
      IF( OINV ) FACTX = 1.D0 / FACT
*
      DO 1000 K = 1, KMAX
         DO 1100 J = 1, JMAX
            WGT = 1.D0
            IF( OWGT ) WGT = 1.D0 / COS( ALAT( J ) )
            DO 1200 I = 1, IDIM
               A( I, J, K ) = A( I, J, K ) * FACTX * WGT
 1200       CONTINUE
 1100    CONTINUE
 1000 CONTINUE
*
      RETURN
*##########################
      ENTRY GETLAT
*
*"         < 0. check precision >
*
      DELTP = 1.
      DO 3100 I = 1, 30
         DELTP = DELTP / 10.
         R   = 1. 
         RP  = R + DELTP
*         WRITE ( 6,* ) ' *** precision check .. ', RP
         IF ( RP .LE. R ) GOTO 3200
 3100 CONTINUE 
 3200 CONTINUE 
      DELTP = DELTP *100.
      PI = ATAN( 1.d0 ) * 4.d0
      NNLAT = JMAX / 2
      DO 5100 J = 1 , NNLAT
         X0 = COS (  ( DBLE ( J ) - 0.5d0  )/ DBLE ( JMAX ) * PI   )
         DO 4100 ITER = 1, ITRMAX
            QPN   ( 0 ) = 1.d0
            QPN   ( 1 ) = SQRT( 3.d0  ) * X0
            DO 2100 N = 2, JMAX+1
               EPS      = SQRT (   ( DBLE ( N   )**2                )
     &                           / ( 4.d0 * DBLE ( N   )**2 - 1.d0  ) )
               EPSM     = SQRT (   ( DBLE ( N-1 )**2                )
     &                           / ( 4.d0 * DBLE ( N-1 )**2 - 1.d0  ) )
               QPN ( N ) = (   QPN  ( N-1 ) * X0
     &                        - QPN  ( N-2 ) * EPSM    ) / EPS
 2100       CONTINUE
            EPS      = SQRT (   ( DBLE ( JMAX   )**2                  )
     &                        / ( 4.d0 * DBLE ( JMAX   )**2 - 1.d0 )  )
            EPSP     = SQRT (   ( DBLE ( JMAX+1 )**2                  )
     &                        / ( 4.d0 * DBLE ( JMAX+1 )**2 - 1.d0 )  )
            QDPN      =   QPN  ( JMAX+1 ) * DBLE ( JMAX   ) * EPSP
     &                  - QPN  ( JMAX-1 ) * DBLE ( JMAX+1 ) * EPS
            DELTX = QPN ( JMAX ) / QDPN * ( 1.d0 - X0 **2 )
            X0    = X0 + DELTX
            IF ( ABS( DELTX ) .LT. DELTP ) GOTO 4200
 4100    CONTINUE
         WRITE ( 6,* ) ' error in GAUSS(4) ', DELTX
 4200    CONTINUE
         ALAT(J) = ASIN(-X0)
         ALAT(JMAX-J+1) = ASIN(X0)
 5100 CONTINUE
*
*
      RETURN
      END
*##########################
      SUBROUTINE MCOPY          !! copy matrix
     O     ( XO, 
     I       XI, IXDIM )
*
*   [INPUT] 
      INTEGER    IXDIM
      REAL*8     XO ( IXDIM   )
      REAL*8     XI ( IXDIM   )
*
*   [INTERNAL WORK]
      INTEGER    IJ
*
      DO 1000 IJ = 1, IXDIM
         XO( IJ ) = XI( IJ )
 1000 CONTINUE
*
      RETURN
      END
