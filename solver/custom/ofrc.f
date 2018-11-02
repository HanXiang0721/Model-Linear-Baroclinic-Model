      PROGRAM OFRC
*
*     derived from util/fvec.f on 1999/03/11
*     This routine makes forcing vectors for the
*     3-D linear response from a GrADS file.
*     Each forcing vector contains only the 
*     values at one point.
*
*     variables: vor, div, T, LnPs, q
*
      include 'dim.f'
*
      REAL*8       GFRC( IDIM*JMAX, KMAX)  
      REAL*8       WFRC( NMDIM, KMAX)  
      REAL*8       WDUM( NMDIM, KMAX)  
      REAL*8       WDUMS( NMDIM )

      REAL*4       GRID( IDIM, JMAX, KMAX) !! flag for forcing
      REAL*4       GRIDV( IDIM, JMAX, KMAX) !! flag vor. forcing
      REAL*4       GRIDD( IDIM, JMAX, KMAX) !! flag div. forcing
      REAL*4       GRIDT( IDIM, JMAX, KMAX) !! flag tmp. forcing
      REAL*4       GRIDP( IDIM, JMAX      ) !! flag ps   forcing
      REAL*4       GRIDQ( IDIM, JMAX, KMAX) !! flag sph. forcing
      INTEGER      NFTYPE       !! # of forcing (output)
*
*     [work]
      REAL*8       BLON( IDIM, JMAX )
      REAL*8       BLAT( IDIM, JMAX )
      REAL*8       DLON( IDIM, JMAX ) !! unused
      REAL*8       PI
      INTEGER      IFR
      INTEGER      NF0, NFV, NFD, NFT, NFP, NFQ
      INTEGER      I, J, K, IJ, NM
      CHARACTER    HALON *(16)        !! unused
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
      CHARACTER*90 CFG          !! file name (dumy)
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
      READ( 1, NMALL) 
      READ( 1, NMCLS) 
*
*
      WRITE( NOUT, * ) '### MAKE FORCING MATRIX ###'
      WRITE( NOUT, * )
*
      CALL SPSTUP               !! spherical harmonic functions
*
      PI = ATAN( 1.D0 ) * 4.D0
      CALL SETLON
     O         ( BLON  , DLON , HALON  )
      CALL SETLAT
     O         ( BLAT  , DLON , HALON  )
      DO 100 J = 1, JMAX
         DO 100 I = 1, IDIM
            BLON( I, J) = BLON( I, J) * 360.D0 / ( 2.D0 * PI )
            BLAT( I, J) = BLAT( I, J) * 360.D0 / ( 2.D0 * PI )
            IF( I .EQ. IDIM ) BLAT( I, J) = -999.D0
 100  CONTINUE
*
*     seek # of forcing
*
      NF0 = 0
      CALL SETZ4( GRID, IDIM*JMAX*KMAX )
      DO 110 J = 1, JMAX
         DO 120 I = 1, IDIM
            IF( BLAT( I, J) .GE. YLATS .AND. 
     $           BLAT( I, J) .LE. YLATN ) THEN 
               IF( XLONW .LT. XLONE ) THEN
                  IF( BLON( I, J) .GE. XLONW .AND. 
     $                 BLON( I, J) .LE. XLONE ) THEN 
                     GRID( I, J, 1) = 1.0
                     NF0 = NF0 + 1
                  ENDIF
               ELSE             !! straddling 0 deg.
                  IF( BLON( I, J) .GE. XLONW .OR. 
     $                 BLON( I, J) .LE. XLONE ) THEN 
                     GRID( I, J, 1) = 1.0
                     NF0 = NF0 + 1
                  ENDIF
               ENDIF
            ENDIF
            DO 130 K = 1, KMAX
               GRID( I, J, K) = GRID( I, J, 1)
 130        CONTINUE
 120     CONTINUE
 110  CONTINUE
*
      NFTYPE = 0
      DO 140 K = 1, KMAX
         NFTYPE = NFTYPE + NF0            
 140  CONTINUE
      NFV = 0
      IF( OVOR ) NFV = NFTYPE
      NFD = 0
      IF( ODIV ) NFD = NFTYPE
      NFT = 0
      IF( OTMP ) NFT = NFTYPE
      NFP = 0
      IF( OPS  ) NFP = NF0
      NFQ = 0
      IF( OCLASSIC ) OSPH = .FALSE.
      IF( OSPH ) NFQ = NFTYPE
      NFTYPE = NFV + NFD + NFT + NFP + NFQ
      WRITE( NOUT, * ) 
      WRITE( NOUT, * ) 'Total # forcing:', NFTYPE
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 
*
*     open output file
*
      IFR = 20
      OPEN ( IFR, FILE = CFR, FORM = 'UNFORMATTED', STATUS = 'UNKNOWN' )
      WRITE( NOUT, * ) 'Forcing file (output):', CFR
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 
*
*     read GrADS data
*
      WRITE( NOUT, * ) 'Make one-point forcing file'
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 
*
      CALL SETZ4( GRIDV, IDIM*JMAX*KMAX )
      CALL SETZ4( GRIDD, IDIM*JMAX*KMAX )
      CALL SETZ4( GRIDT, IDIM*JMAX*KMAX )
      CALL SETZ4( GRIDP, IDIM*JMAX      )
      CALL SETZ4( GRIDQ, IDIM*JMAX*KMAX )
      CALL SETZ ( WDUM , NMDIM*KMAX     )
      CALL SETZ ( WDUMS, NMDIM          )
*
      IF( OVOR ) CALL COPY4( GRID, GRIDV, IDIM*JMAX*KMAX )
      IF( ODIV ) CALL COPY4( GRID, GRIDD, IDIM*JMAX*KMAX )
      IF( OTMP ) CALL COPY4( GRID, GRIDT, IDIM*JMAX*KMAX )
      IF( OPS  ) CALL COPY4( GRID, GRIDP, IDIM*JMAX      )
      IF( OSPH ) CALL COPY4( GRID, GRIDQ, IDIM*JMAX*KMAX )
*
      NF0 = 0
 200  NF0 = NF0 + 1
      NM = 0
      CALL SETZ( GFRC, IDIM*JMAX*KMAX )
      CALL SETZ( WFRC, NMDIM*KMAX )
      IF( OVOR ) THEN
         DO 210 K = 1, KMAX      !! vorticity
            IJ = 0
            DO 220 J = 1, JMAX
               DO 230 I = 1, IDIM
                  IJ = IJ + 1
                  IF( GRIDV( I, J, K) .EQ. 1.0 ) THEN
                     GFRC( IJ, K) = FACT(1) 
                     GRIDV( I, J, K) = 0.0
                     GOTO 250
                  ENDIF
 230           CONTINUE
 220        CONTINUE            
 210     CONTINUE
*     
*     grid to wave (forcing matrix)
*
 250     CALL G2W
     O        ( WFRC   ,
     I          GFRC   , '    ', 'POSO', KMAX )
         CALL WRTWAVE
     I        ( WFRC , WDUM , WDUM , WDUMS, WDUM , 
     I          IFR  , OWALL   , OCLASSIC  )
      ENDIF
*
      IF( NF0 .LT. NFV ) GOTO 200
      WRITE( NOUT, * ) 'grid --> wave (vorticity)'
      WRITE( NOUT, * ) '     # forcing:',NFV
      WRITE( NOUT, * ) '................'
*
      NF0 = 0
 300  NF0 = NF0 + 1
      NM = 0
      CALL SETZ( GFRC, IDIM*JMAX*KMAX )
      CALL SETZ( WFRC, NMDIM*KMAX )
      IF( ODIV ) THEN
         DO 310 K = 1, KMAX      !! divergence
            IJ = 0
            DO 320 J = 1, JMAX
               DO 330 I = 1, IDIM
                  IJ = IJ + 1
                  IF( GRIDD( I, J, K) .EQ. 1.0 ) THEN
                     GFRC( IJ, K) = FACT(2) 
                     GRIDD( I, J, K) = 0.0
                     GOTO 350
                  ENDIF
 330           CONTINUE
 320        CONTINUE            
 310     CONTINUE
*     
*     grid to wave (forcing matrix)
*
 350     CALL G2W
     O        ( WFRC   ,
     I          GFRC   , '    ', 'POSO', KMAX )
         CALL WRTWAVE
     I        ( WDUM , WFRC , WDUM , WDUMS, WDUM , 
     I          IFR  , OWALL   , OCLASSIC  )
      ENDIF
*
      IF( NF0 .LT. NFD ) GOTO 300
      WRITE( NOUT, * ) 'grid --> wave (divergence)'
      WRITE( NOUT, * ) '     # forcing:',NFD
      WRITE( NOUT, * ) '................'
*
      NF0 = 0
 400  NF0 = NF0 + 1
      NM = 0
      CALL SETZ( GFRC, IDIM*JMAX*KMAX )
      CALL SETZ( WFRC, NMDIM*KMAX )
      IF( OTMP ) THEN
         DO 410 K = 1, KMAX      !! temperature
            IJ = 0
            DO 420 J = 1, JMAX
               DO 430 I = 1, IDIM
                  IJ = IJ + 1
                  IF( GRIDT( I, J, K) .EQ. 1.0 ) THEN
                     GFRC( IJ, K) = FACT(3) 
                     GRIDT( I, J, K) = 0.0
                     GOTO 450
                  ENDIF
 430           CONTINUE
 420        CONTINUE            
 410     CONTINUE
*     
*     grid to wave (forcing matrix)
*
 450     CALL G2W
     O        ( WFRC   ,
     I          GFRC   , '    ', 'POSO', KMAX )
         CALL WRTWAVE
     I        ( WDUM , WDUM , WFRC , WDUMS, WDUM , 
     I          IFR  , OWALL   , OCLASSIC  )
      ENDIF
*
      IF( NF0 .LT. NFT ) GOTO 400
      WRITE( NOUT, * ) 'grid --> wave (temperature)'
      WRITE( NOUT, * ) '     # forcing:',NFT
      WRITE( NOUT, * ) '................'
*
      NF0 = 0
 500  NF0 = NF0 + 1
      NM = 0
      CALL SETZ( GFRC, IDIM*JMAX*KMAX )
      CALL SETZ( WFRC, NMDIM*KMAX     )
      IF( OPS ) THEN
         IJ = 0
         DO 520 J = 1, JMAX     !! LnPs
            DO 530 I = 1, IDIM
               IJ = IJ + 1
               IF( GRIDP( I, J) .EQ. 1.0 ) THEN
                  GFRC( IJ, 1) = FACT(4) 
                  GRIDP( I, J) = 0.0
                  GOTO 550
               ENDIF
 530        CONTINUE
 520     CONTINUE            
*     
*     grid to wave (forcing matrix)
*
 550     CALL G2W
     O        ( WFRC   ,
     I          GFRC   , '    ', 'POSO', 1     )
         CALL WRTWAVE
     I        ( WDUM , WDUM , WDUM , WFRC, WDUM , 
     I          IFR  , OWALL   , OCLASSIC  )
      ENDIF
*
      IF( NF0 .LT. NFP ) GOTO 500
      WRITE( NOUT, * ) 'grid --> wave (sfc. pressure)'
      WRITE( NOUT, * ) '     # forcing:',NFP
      WRITE( NOUT, * ) '................'
*
      NF0 = 0
 600  NF0 = NF0 + 1
      NM = 0
      CALL SETZ( GFRC, IDIM*JMAX*KMAX )
      CALL SETZ( WFRC, NMDIM*KMAX )
      IF( OSPH ) THEN
         DO 610 K = 1, KMAX      !! specific humidity
            IJ = 0
            DO 620 J = 1, JMAX
               DO 630 I = 1, IDIM
                  IJ = IJ + 1
                  IF( GRIDQ( I, J, K) .EQ. 1.0 ) THEN
                     GFRC( IJ, K) = FACT(5) 
                     GRIDQ( I, J, K) = 0.0
                     GOTO 650
                  ENDIF
 630           CONTINUE
 620        CONTINUE            
 610     CONTINUE
*     
*     grid to wave (forcing matrix)
*
 650     CALL G2W
     O        ( WFRC   ,
     I          GFRC   , '    ', 'POSO', KMAX )
         CALL WRTWAVE
     I        ( WDUM , WDUM , WDUM , WDUMS, WFRC , 
     I          IFR  , OWALL   , OCLASSIC  )
      ENDIF
*
      IF( NF0 .LT. NFQ ) GOTO 600
      WRITE( NOUT, * ) 'grid --> wave (humidity)'
      WRITE( NOUT, * ) '     # forcing:',NFQ
      WRITE( NOUT, * ) '................'
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
      INTEGER      IW, JW( 0:NTR ), LEND

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
