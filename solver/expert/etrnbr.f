      PROGRAM ETRNBR
*
*     derived from strnbr.f on 2001/10/03
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

      PARAMETER( MAXN=MSIZ2 )
      PARAMETER( ER   = 6370.E+3 )
      PARAMETER( XMISS = -999.0 )

      REAL*8      DAT( MAXN )
      REAL*8      W( NMDIM )
      REAL*8      G( IDIM*JMAX )
      REAL*4      GO( IMAX, JMAX )
      INTEGER     IFS, IFG
      INTEGER     I, J, L, M, M2, M3, IJ, NM, ICHK
      INTEGER     LDA, LEND, MTR
*
*     [work]
      INTEGER     NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      INTEGER     NOUT
      REAL*8      EDEL  ( NMDIM  )        !! vor.,D -> U,V
      SAVE        NMO, EDEL
*
*     .. External Subroutines ..
      EXTERNAL    W2G
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC   DBLE, SNGL
*
*     namelist variables
*
      CHARACTER*90 CFM          !! input file name (matrix A)
      CHARACTER*90 CFE          !! eigenvalue (unused)
      CHARACTER*90 CFS          !! eigenvector file name (temporary)
      CHARACTER*90 CFG          !! eigenvector file name (grads)
      INTEGER      NWAVE        !! selected zonal wavenumber
      LOGICAL      OWALL        !! write all the wavenumber

      NAMELIST    /NMEFIN/   CFM, CFE, CFS, CFG
      NAMELIST    /NMWAVE/   NWAVE 
      NAMELIST    /NMALL/    OWALL

      DATA         NOUT / 6 /
      DATA         NWAVE / 0       /
      DATA         OWALL / .FALSE. / !! PWM
*
*     open the NAMELIST file
*
      REWIND( 1 )
      OPEN ( 0, FILE='IEEE_ERROR')
      OPEN ( 1, FILE = 'SETPAR', STATUS = 'OLD')
*
      READ( 1, NMEFIN )
      READ( 1, NMWAVE )
      READ( 1, NMALL) 
*
      WRITE( NOUT, *) 
      WRITE( NOUT, *) '### transform wave --> grid ###'
      WRITE( NOUT, *) 
      IF( OWALL ) THEN
         NWAVE = -1
         LDA = MSIZ2 
      ELSE
         LDA = NMAX - NWAVE + 1
         IF( NWAVE .EQ. 0 ) THEN
            LDA = LDA - 1
         ELSE
            LDA = 2 * LDA
         ENDIF
      ENDIF
*
*     input file
*
      IFS = 88
      open( IFS, FILE = CFS, FORM = 'UNFORMATTED', STATUS = 'UNKNOWN' )
*
*     output file
*
      IFG = 44
      OPEN( IFG, FILE = CFG, FORM = 'UNFORMATTED', STATUS = 'UNKNOWN' )
*
      CALL SPSTUP               !! spherical harmonic functions
      CALL SETNMO2
     O     ( NMO   ,
     D     MMAX  , LMAX  , NMAX  , MINT    )
      CALL DSETED2
     O     ( EDEL, 
     I     NMDIM, NMAX, LMAX, MMAX, MINT, NMO, ER )
      CALL GETLAT
*
*     read matrix X
*
      DO 100 M = 1, LDA         !! # mode
         ICHK = 0               !! check count for total #
         READ( IFS ) ( DAT(I), I=1,LDA )
*
         DO 200 I = 1, NMDIM
            W( I ) = 0.D0
  200    CONTINUE
         NM = 0
         MTR = 0
         IF( OWALL ) THEN
            MTR = MMAX
         ENDIF
         DO 300 M2 = 0, MTR
            IF( OWALL ) THEN
               LEND = MIN( LMAX, NMAX-M2)
               M3 = M2
            ELSE
               LEND = MIN( LMAX, NMAX-NWAVE)
               M3 = NWAVE
            ENDIF
            DO 400 L = 0, LEND  !! meridional wave number
               IF( M3 .EQ. 0 .AND. L .EQ. 0 ) GOTO 400
               NM = NM + 1
               I = NMO( 1, M3, L)
               W( I ) = DAT( NM )
               IF( M3 .EQ. 0 ) GOTO 400
               NM = NM + 1
               I = NMO( 2, M3, L)
               W( I ) = DAT( NM )
  400       CONTINUE
  300    CONTINUE
         ICHK = NM
*
*     vorticity ---> stream function
*
         DO 500 I = 1, NMDIM
            W( I ) = W( I ) * EDEL( I ) * ER 
  500    CONTINUE
*
*     wave to grid
* 
         CALL W2G
     O        ( G   ,
     I          W   , '    ', 'POSO', 1   )
*
*     sigma to pressure
* 
         IJ = 0
         DO 600 J = 1, JMAX
            DO 700 I = 1, IDIM
               IJ = IJ + 1
               IF( I .LT. IDIM ) THEN
                  GO( I,J ) = SNGL( G( IJ ) )
               ENDIF
  700       CONTINUE
  600    CONTINUE
         WRITE( IFG ) 
     &        ((GO(I,J),I=1,IMAX),J=1,JMAX)
*
         CALL RESET( W, NMDIM     )
         CALL RESET( G, IDIM*JMAX )
*
*     log
*
         WRITE( NOUT, *) '# mode :',M,' check count:',ICHK
*
  100 CONTINUE
*
      CLOSE( IFS )
      CLOSE( IFG )
*
      WRITE( NOUT, FMT = * )
      WRITE( NOUT, * ) '### END OF EXECUTION ###'
      WRITE( NOUT, FMT = * )
*
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
      RETURN
      END
