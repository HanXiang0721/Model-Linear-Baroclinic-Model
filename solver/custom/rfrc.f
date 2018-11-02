      PROGRAM RFRC
*
*     derived from util/fvec.f on 1999/03/11
*     This routine makes forcing vectors for the
*     3-D linear response from a GrADS file.
*     Each forcing vector contains spatially random forcing.
*
*     variables: vor, div, T, LnPs, q
*
      include 'dim.f'
*
      INTEGER    NEXP
      INTEGER    MAXN
      PARAMETER ( NEXP = 1000 )
      PARAMETER ( MAXN = IMAX*JMAX*(KMAX*NVAR+1) )
*
      REAL*4       DAT ( MAXN )

      REAL*8       GVOR( IJDIM, KMAX)      !! vor. forcing
      REAL*8       GDIV( IJDIM, KMAX)      !! vor. forcing
      REAL*8       GTMP( IJDIM, KMAX)      !! vor. forcing
      REAL*8       GPS ( IJDIM      )      !! vor. forcing
      REAL*8       GSPH( IJDIM, KMAX)      !! vor. forcing

      REAL*8       WVOR( NMDIM, KMAX)  
      REAL*8       WDIV( NMDIM, KMAX)  
      REAL*8       WTMP( NMDIM, KMAX)  
      REAL*8       WPS ( NMDIM      )  
      REAL*8       WSPH( NMDIM, KMAX)  
*
*     [work]
      INTEGER      IFR, IFG
      INTEGER      I, J, K, M, IJ, IR
      INTEGER      NOUT
*
*     [intrinsic]
      INTRINSIC    DBLE
*
*     [external]
      EXTERNAL     G2W
*
      CHARACTER*90 CFM          !! file name (dumy)
      CHARACTER*90 CFR          !! output file name (matrix)
      CHARACTER*90 CFS          !! file name (dumy)
      CHARACTER*90 CFG          !! file name (random seaquence)
      INTEGER      NFTYPE       !! no. of forcing (max=1000)
      LOGICAL      OVOR         !! make vor. forcing?
      LOGICAL      ODIV         !! make div. forcing?
      LOGICAL      OTMP         !! make tmp. forcing?
      LOGICAL      OPS          !! make ps   forcing?
      LOGICAL      OSPH         !! make sph  forcing?
      LOGICAL      OWALL        !! write all the wavenumber
      LOGICAL      OCLASSIC     !! classic dry model
      REAL*8       FACT( NVAR+1 ) !! factor 
      REAL*8       XLONW        !! western longitude
      REAL*8       XLONE        !! eastern longitude
      REAL*8       YLATS        !! southern latitude
      REAL*8       YLATN        !! northern latitude
*
      NAMELIST    /NMFIN/   CFM, CFR, CFS, CFG
      NAMELIST    /NMVAR/   OVOR, ODIV, OTMP, OPS, OSPH
      NAMELIST    /NMFRC/   FACT, XLONW, XLONE, YLATS, YLATN
      NAMELIST    /NMFNO/   NFTYPE
      NAMELIST    /NMALL/   OWALL
      NAMELIST    /NMCLS/   OCLASSIC
*
      DATA         NOUT / 6 /
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
      READ( 1, NMVAR) 
      READ( 1, NMFRC) 
      READ( 1, NMFNO) 
      READ( 1, NMALL) 
      READ( 1, NMCLS) 
*
*
      WRITE( NOUT, * ) '### MAKE FORCING MATRIX ###'
      WRITE( NOUT, * )
*
*     open input/output file
*
      IFG = 88
      OPEN( IFG, FILE = CFG, FORM = 'UNFORMATTED', STATUS = 'OLD' )
      IFR = 20
      OPEN ( IFR, FILE = CFR, FORM = 'UNFORMATTED', STATUS = 'UNKNOWN' )

      WRITE( NOUT, * ) 'Random sequence (input) :', CFG
      WRITE( NOUT, * ) 'Forcing file     (output):', CFR
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 'Total # forcing:', MIN( NFTYPE, NEXP )
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 

      CALL SPSTUP               !! spherical harmonic functions
*
*     read random number
*
      CALL SETZ( GVOR, IJDIM*KMAX )
      CALL SETZ( GDIV, IJDIM*KMAX )
      CALL SETZ( GTMP, IJDIM*KMAX )
      CALL SETZ( GPS , IJDIM      )
      CALL SETZ( GSPH, IJDIM*KMAX )
      CALL SETZ( WVOR, NMDIM*KMAX )
      CALL SETZ( WDIV, NMDIM*KMAX )
      CALL SETZ( WTMP, NMDIM*KMAX )
      CALL SETZ( WPS , NMDIM*KMAX )
      CALL SETZ( WSPH, NMDIM*KMAX )

      WRITE( NOUT, * ) 'Make random forcing file'
      WRITE( NOUT, * ) '................'
      DO 100 M = 1, MIN( NFTYPE, NEXP )        !! # case
         READ( IFG ) DAT  
*
         IR = 0
         DO 110 K = 1, KMAX
            IJ = 0
            DO 120 J = 1, JMAX
               DO 130 I = 1, IMAX
                  IR = IR + 1
                  IJ = IJ + 1
                  IF( OVOR )
     &                 GVOR( IJ,K ) = DBLE( DAT( IR ) * FACT( 1 ) )
 130           CONTINUE
               IJ = IJ + 1
               GVOR( IJ,K ) = GVOR( IJ-IMAX,K )
 120        CONTINUE
 110     CONTINUE
         DO 210 K = 1, KMAX
            IJ = 0
            DO 220 J = 1, JMAX
               DO 230 I = 1, IMAX
                  IR = IR + 1
                  IJ = IJ + 1
                  IF( ODIV )
     &                 GDIV( IJ,K ) = DBLE( DAT( IR ) * FACT( 2 ) )
 230           CONTINUE
               IJ = IJ + 1
               GDIV( IJ,K ) = GDIV( IJ-IMAX,K )
 220        CONTINUE
 210     CONTINUE
         DO 310 K = 1, KMAX
            IJ = 0
            DO 320 J = 1, JMAX
               DO 330 I = 1, IMAX
                  IR = IR + 1
                  IJ = IJ + 1
                  IF( OTMP )
     &                 GTMP( IJ,K ) = DBLE( DAT( IR ) * FACT( 3 ) )
 330           CONTINUE
               IJ = IJ + 1
               GTMP( IJ,K ) = GTMP( IJ-IMAX,K )
 320        CONTINUE
 310     CONTINUE
         IJ = 0
         DO 420 J = 1, JMAX
            DO 430 I = 1, IMAX
               IR = IR + 1
               IJ = IJ + 1
               IF( OPS )
     &              GPS( IJ ) = DBLE( DAT( IR ) * FACT( 4 ) )
 430        CONTINUE
            IJ = IJ + 1
            GPS( IJ ) = GPS( IJ-IMAX )
 420     CONTINUE
         DO 510 K = 1, KMAX
            IJ = 0
            DO 520 J = 1, JMAX
               DO 530 I = 1, IMAX
                  IR = IR + 1
                  IJ = IJ + 1
                  IF( OSPH )
     &                 GSPH( IJ,K ) = DBLE( DAT( IR ) * FACT( 5 ) )
 530           CONTINUE
               IJ = IJ + 1
               GSPH( IJ,K ) = GSPH( IJ-IMAX,K )
 520        CONTINUE
 510     CONTINUE
*
         IF( OVOR ) THEN
            CALL G2W
     O           ( WVOR   ,
     I             GVOR   , '    ', 'POSO', KMAX )
         ENDIF
         IF( ODIV ) THEN
            CALL G2W
     O           ( WDIV   ,
     I             GDIV   , '    ', 'POSO', KMAX )
         ENDIF
         IF( OTMP ) THEN
            CALL G2W
     O           ( WTMP   ,
     I             GTMP   , '    ', 'POSO', KMAX )
         ENDIF
         IF( OPS  ) THEN
            CALL G2W
     O           ( WPS    ,
     I             GPS    , '    ', 'POSO', 1    )
         ENDIF
         IF( OSPH ) THEN
            CALL G2W
     O           ( WSPH   ,
     I             GSPH   , '    ', 'POSO', KMAX )
         ENDIF
         CALL WRTWAVE
     I        ( WVOR , WDIV , WTMP , WPS , WSPH , 
     I          IFR  , OWALL, OCLASSIC  )
*
*     log
*
         WRITE( NOUT, *) '   # case:',M

 100  CONTINUE
*
      CLOSE( IFR )
      IF( OWALL ) THEN
         WRITE( NOUT, * ) 'Written to matrix file (all)'
      ELSE
         WRITE( NOUT, * ) 'Written to matrix file (pwm)'
      ENDIF
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 
*
      WRITE( NOUT, * )
      WRITE( NOUT, * ) '### END OF EXECUTION ###'
      WRITE( NOUT, * )
*
*
  999 STOP
      END
*######################
      SUBROUTINE SETZ( A, IA )
*
*
      INTEGER IA
      REAL*8  A( IA )

      REAL*8  ZERO
      DATA    ZERO / 0.D0 /
      INTEGER I
*
      DO 10 I = 1, IA
         A( I ) = ZERO
 10   CONTINUE

      RETURN
      END
*######################
      SUBROUTINE SETZ4( A, IA )
*
*
      INTEGER IA
      REAL*4  A( IA )

      REAL*4  ZERO
      DATA    ZERO / 0.0 /
      INTEGER I
*
      DO 10 I = 1, IA
         A( I ) = ZERO
 10   CONTINUE

      RETURN
      END
*######################
      SUBROUTINE COPY4( A, A2, IA )
*
*
      INTEGER IA, I
      REAL*4  A( IA )
      REAL*4  A2( IA )
*
      DO 10 I = 1, IA
         A2( I ) = A( I )
 10   CONTINUE

      RETURN
      END
*######################
      SUBROUTINE WRTWAVE
     I     ( WV , WD , WT , WP , WQ , 
     I       IFM, OWALL   , OCLASSIC  )
*
*
      include 'dim.f'
*
      INTEGER      MAXN
      PARAMETER (  MAXN = 2*NMAX*(NVAR*KMAX+1) )
*
*
*     [ input ]
      REAL*8       WV( NMDIM, KMAX)  
      REAL*8       WD( NMDIM, KMAX)  
      REAL*8       WT( NMDIM, KMAX)  
      REAL*8       WP( NMDIM )
      REAL*8       WQ( NMDIM, KMAX)  

      INTEGER      IFM          !! output file unit number
      LOGICAL      OWALL        !! write all the wavenumber
      LOGICAL      OCLASSIC     !! classic dry model
*
*     [ work ]
      REAL*8       WXV( MAXN, KMAX, 0:NTR)  
      REAL*8       WXD( MAXN, KMAX, 0:NTR)  
      REAL*8       WXT( MAXN, KMAX, 0:NTR)  
      REAL*8       WXP( MAXN,       0:NTR)
      REAL*8       WXQ( MAXN, KMAX, 0:NTR)  

      INTEGER      NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      INTEGER      I, J, K, L, M
      INTEGER      IW, JW(0:NTR), LEND

      SAVE         NMO

      LOGICAL      OFIRST
      DATA         OFIRST / .TRUE. /

*
      IF( OFIRST ) THEN
         CALL SPSTUP      
         CALL SETNMO2
     O        ( NMO   ,
     D          MMAX  , LMAX  , NMAX  , MINT    )
         OFIRST = .FALSE.
      ENDIF

      IW = 0
      DO 1000 M = 0, NTR
         LEND = MIN( LMAX, NMAX-M)
         DO 1100 K = 1, KMAX
            IW = 0
            DO 1200 L = 0, LEND
               IF( M .EQ. 0 .AND. L .EQ. 0 ) GOTO 1200
               I = NMO( 1, M, L)
               J = NMO( 2, M, L)
               IW = IW + 1
               WXV(IW,K,M) = WV(I,K)
               WXD(IW,K,M) = WD(I,K)
               WXT(IW,K,M) = WT(I,K)
               WXP(IW,M  ) = WP(I  )
               WXQ(IW,K,M) = WQ(I,K)
               IF( M .EQ. 0 ) GOTO 1200
               IW = IW + 1
               WXV(IW,K,M) = WV(J,K)
               WXD(IW,K,M) = WD(J,K)
               WXT(IW,K,M) = WT(J,K)
               WXP(IW,M  ) = WP(J  )
               WXQ(IW,K,M) = WQ(J,K)
 1200       CONTINUE
 1100    CONTINUE
         IF( .NOT. OWALL ) THEN
            IF( OCLASSIC ) THEN
               WRITE( IFM ) 
     $              ((WXV(I,K,M),I=1,IW),K=1,KMAX),
     $              ((WXD(I,K,M),I=1,IW),K=1,KMAX),
     $              ((WXT(I,K,M),I=1,IW),K=1,KMAX),
     $              ( WXP(I,M)  ,I=1,IW          )
            ELSE
               WRITE( IFM ) 
     $              ((WXV(I,K,M),I=1,IW),K=1,KMAX),
     $              ((WXD(I,K,M),I=1,IW),K=1,KMAX),
     $              ((WXT(I,K,M),I=1,IW),K=1,KMAX),
     $              ( WXP(I,M)  ,I=1,IW          ),
     $              ((WXQ(I,K,M),I=1,IW),K=1,KMAX)
            ENDIF
         ELSE
            JW( M ) = IW
         ENDIF
 1000 CONTINUE
      IF( OWALL ) THEN
         IF( OCLASSIC ) THEN
            WRITE( IFM ) 
     $           (((WXV(I,K,M),I=1,JW(M)),K=1,KMAX),
     $            ((WXD(I,K,M),I=1,JW(M)),K=1,KMAX),
     $            ((WXT(I,K,M),I=1,JW(M)),K=1,KMAX),
     $            ( WXP(I,M)  ,I=1,JW(M)          ),M=0,NTR)
         ELSE
            WRITE( IFM ) 
     $           (((WXV(I,K,M),I=1,JW(M)),K=1,KMAX),
     $            ((WXD(I,K,M),I=1,JW(M)),K=1,KMAX),
     $            ((WXT(I,K,M),I=1,JW(M)),K=1,KMAX),
     $            ( WXP(I,M)  ,I=1,JW(M)          ),
     $            ((WXQ(I,K,M),I=1,JW(M)),K=1,KMAX),M=0,NTR)
         ENDIF
      ENDIF


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
