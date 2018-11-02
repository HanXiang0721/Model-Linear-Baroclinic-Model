      PROGRAM FVEC
*
*     derived from fvecpw.f on 2001/09/17
*     This routine makes forcing vector for the
*     3-D linear response from a GrADS file.
*
*     variables: V, D, T, LnPs
*
      include 'dim.f'
*
*
      INTEGER      MAXN
      PARAMETER (  MAXN = 2*NMAX*(NVAR*KMAX+1) )
*
      REAL*4       DAT( IMAX, JMAX)
      REAL*8       GFRC( IDIM*JMAX, KMAX)  
      REAL*8       WVOR( NMDIM, KMAX)  
      REAL*8       WDIV( NMDIM, KMAX)  
      REAL*8       WTMP( NMDIM, KMAX)  
      REAL*8       WPS ( NMDIM )
      REAL*8       WSPH( NMDIM, KMAX)  

      REAL*8       WXVOR( MAXN, KMAX, 0:NTR)  
      REAL*8       WXDIV( MAXN, KMAX, 0:NTR)  
      REAL*8       WXTMP( MAXN, KMAX, 0:NTR)  
      REAL*8       WXPS ( MAXN,       0:NTR)
      REAL*8       WXSPH( MAXN, KMAX, 0:NTR)  
*
*     [work]
      INTEGER      NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      INTEGER      IFM, IFG
      INTEGER      I, J, K, L, M, IJ, IW, JW( 0:NTR ), LEND
      INTEGER      NOUT
      REAL*8       ONE
*
*     [intrinsic]
      INTRINSIC    DBLE
*
      CHARACTER*90 CFM          !! output file name (matrix)
      CHARACTER*90 CFG          !! input file name (grads)
      LOGICAL      OWALL        !! write all the wavenumber
      LOGICAL      OCLASSIC     !! classic dry model
      REAL*8       FACT( 5 )    !! factor

      NAMELIST    /NMFIN/   CFM, CFG, FACT
      NAMELIST    /NMALL/   OWALL
      NAMELIST    /NMCLS/   OCLASSIC
*
      DATA         NOUT / 6 /
      DATA         ONE / 1.D0 /
      DATA         FACT / 1.D0,1.D0,1.D0,1.D0,1.D0 /
*
*     open the NAMELIST file
*
      REWIND( 1 )
      OPEN ( 0, FILE='IEEE_ERROR')
      OPEN ( 1, FILE = 'SETPAR', STATUS = 'OLD')
*
      READ( 1, NMFIN) 
      REWIND( 1 )
      READ( 1, NMALL) 
      REWIND( 1 )
      READ( 1, NMCLS) 
*
*
      WRITE( NOUT, * ) '### MAKE FORCING MATRIX ###'
      WRITE( NOUT, * )
*
*     open files
*
      IFG = 10
      IFM = 20
      OPEN ( IFG, FILE = CFG, FORM = 'UNFORMATTED', STATUS = 'UNKNOWN' )
      OPEN ( IFM, FILE = CFM, FORM = 'UNFORMATTED', STATUS = 'UNKNOWN' )
      WRITE( NOUT, * ) 'Forcing data (input):', CFG
      WRITE( NOUT, * ) 'Matrix file (output):', CFM
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 
*
      CALL SETCONS
      CALL SPSTUP               !! spherical harmonic functions
      CALL SETNMO2
     O     ( NMO   ,
     D     MMAX  , LMAX  , NMAX  , MINT    )
*
*     read GrADS data
*
      WRITE( NOUT, * ) 'Read forcing file'
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 
*
      CALL SETZ( GFRC, IDIM*JMAX*KMAX )
      CALL SETZ( WVOR, NMDIM*KMAX )
      DO 10 K = 1, KMAX         !! vorticity
         READ( IFG ) DAT

         IJ = 0
         DO 20 J = 1, JMAX
            DO 30 I = 1, IMAX
               IJ = IJ + 1
               GFRC( IJ, K) = FACT(1) * DBLE( DAT( I, J) )
 30         CONTINUE
            IJ = IJ + 1
            GFRC( IJ, K) = FACT(1) * DBLE( DAT( 1, J) )
 20      CONTINUE

 10   CONTINUE
*
*     grid to wave (forcing matrix)
*
      CALL G2W
     O     ( WVOR   ,
     I       GFRC   , '    ', 'POSO', KMAX )
      WRITE( NOUT, * ) 'grid --> wave (vorticity)'
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 
*
      CALL SETZ( GFRC, IDIM*JMAX*KMAX )
      CALL SETZ( WDIV, NMDIM*KMAX )
      DO 40 K = 1, KMAX         !! divergence
         READ( IFG ) DAT

         IJ = 0
         DO 50 J = 1, JMAX
            DO 60 I = 1, IMAX
               IJ = IJ + 1
               GFRC( IJ, K) = FACT(2) * DBLE( DAT( I, J) )
 60         CONTINUE
            IJ = IJ + 1
            GFRC( IJ, K) = FACT(2) * DBLE( DAT( 1, J) )
 50      CONTINUE

 40   CONTINUE
*
*     grid to wave (forcing matrix)
*
      CALL G2W
     O     ( WDIV   ,
     I       GFRC   , '    ', 'POSO', KMAX )
      WRITE( NOUT, * ) 'grid --> wave (divergence)'
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 
*
      CALL SETZ( GFRC, IDIM*JMAX*KMAX )
      CALL SETZ( WTMP, NMDIM*KMAX )
      DO 70 K = 1, KMAX         !! temperature
         READ( IFG ) DAT

         IJ = 0
         DO 80 J = 1, JMAX
            DO 90 I = 1, IMAX
               IJ = IJ + 1
               GFRC( IJ, K) = FACT(3) * DBLE( DAT( I, J) )
 90         CONTINUE
            IJ = IJ + 1
            GFRC( IJ, K) = FACT(3) * DBLE( DAT( 1, J) )
 80      CONTINUE

 70   CONTINUE
*
*     grid to wave (forcing matrix)
*
      CALL G2W
     O     ( WTMP   ,
     I       GFRC   , '    ', 'POSO', KMAX )
      WRITE( NOUT, * ) 'grid --> wave (temperature)'
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 
*
      CALL SETZ( GFRC, IDIM*JMAX*KMAX )
      CALL SETZ( WPS , NMDIM          )
      READ( IFG ) DAT           !! LnPs

      IJ = 0
      DO 100 J = 1, JMAX
         DO 110 I = 1, IMAX
            IJ = IJ + 1
            GFRC( IJ, 1) = FACT(4) * DBLE( DAT( I, J) )
 110     CONTINUE
         IJ = IJ + 1
         GFRC( IJ, 1) = FACT(4) * DBLE( DAT( 1, J) )
 100  CONTINUE
*
*     grid to wave (forcing matrix)
*
      CALL G2W
     O     ( WPS    ,
     I       GFRC   , '    ', 'POSO', 1    )
      WRITE( NOUT, * ) 'grid --> wave (Ln pressure)'
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 
*
      CALL SETZ( GFRC, IDIM*JMAX*KMAX )
      CALL SETZ( WSPH, NMDIM*KMAX )
      IF( .NOT. OCLASSIC ) THEN
         DO 120 K = 1, KMAX     !! humidity
            READ( IFG ) DAT

            IJ = 0
            DO 130 J = 1, JMAX
               DO 140 I = 1, IMAX
                  IJ = IJ + 1
                  GFRC( IJ, K) = FACT(5) * DBLE( DAT( I, J) )
 140           CONTINUE
               IJ = IJ + 1
               GFRC( IJ, K) = FACT(5) * DBLE( DAT( 1, J) )
 130        CONTINUE

 120     CONTINUE
*
*     grid to wave (forcing matrix)
*
         CALL G2W
     O        ( WSPH   ,
     I          GFRC   , '    ', 'POSO', KMAX )
         WRITE( NOUT, * ) 'grid --> wave (humidity)'
         WRITE( NOUT, * ) '................'
         WRITE( NOUT, * ) 
      ENDIF
*
*     write down
*
      IW = 0
      DO 200 M = 0, NTR
         LEND = MIN( LMAX, NMAX-M)
         DO 220 K = 1, KMAX
            IW = 0
            DO 210 L = 0, LEND
               IF( M .EQ. 0 .AND. L .EQ. 0 ) GOTO 210
               I = NMO( 1, M, L)
               J = NMO( 2, M, L)
               IW = IW + 1
               WXVOR(IW,K,M) = WVOR(I,K)
               WXDIV(IW,K,M) = WDIV(I,K)
               WXTMP(IW,K,M) = WTMP(I,K)
               WXPS (IW,M  ) = WPS (I  )
               WXSPH(IW,K,M) = WSPH(I,K)
               IF( M .EQ. 0 ) GOTO 210
               IW = IW + 1
               WXVOR(IW,K,M) = WVOR(J,K)
               WXDIV(IW,K,M) = WDIV(J,K)
               WXTMP(IW,K,M) = WTMP(J,K)
               WXPS (IW,M  ) = WPS (J  )
               WXSPH(IW,K,M) = WSPH(J,K)
  210       CONTINUE
  220    CONTINUE
         IF( .NOT. OWALL ) THEN
            IF( OCLASSIC ) THEN
               WRITE( IFM ) 
     $              ((WXVOR(I,K,M),I=1,IW),K=1,KMAX),
     $              ((WXDIV(I,K,M),I=1,IW),K=1,KMAX),
     $              ((WXTMP(I,K,M),I=1,IW),K=1,KMAX),
     $              ( WXPS (I,M)  ,I=1,IW          )
            ELSE
               WRITE( IFM ) 
     $              ((WXVOR(I,K,M),I=1,IW),K=1,KMAX),
     $              ((WXDIV(I,K,M),I=1,IW),K=1,KMAX),
     $              ((WXTMP(I,K,M),I=1,IW),K=1,KMAX),
     $              ( WXPS (I,M)  ,I=1,IW          ),
     $              ((WXSPH(I,K,M),I=1,IW),K=1,KMAX)
            ENDIF
         ELSE
            JW( M ) = IW
         ENDIF
  200 CONTINUE
      IF( OWALL ) THEN
         IF( OCLASSIC ) THEN
            WRITE( IFM ) 
     $           (((WXVOR(I,K,M),I=1,JW(M)),K=1,KMAX),
     $            ((WXDIV(I,K,M),I=1,JW(M)),K=1,KMAX),
     $            ((WXTMP(I,K,M),I=1,JW(M)),K=1,KMAX),
     $            ( WXPS (I,M)  ,I=1,JW(M)          ),M=0,NTR)
         ELSE
            WRITE( IFM ) 
     $           (((WXVOR(I,K,M),I=1,JW(M)),K=1,KMAX),
     $            ((WXDIV(I,K,M),I=1,JW(M)),K=1,KMAX),
     $            ((WXTMP(I,K,M),I=1,JW(M)),K=1,KMAX),
     $            ( WXPS (I,M)  ,I=1,JW(M)          ),
     $            ((WXSPH(I,K,M),I=1,JW(M)),K=1,KMAX),M=0,NTR)
         ENDIF
      ENDIF
      CLOSE( IFM )
      IF( OWALL ) THEN
         WRITE( NOUT, * ) 'Written to matrix file (all)'
      ELSE
         WRITE( NOUT, * ) 'Written to matrix file (pwm)'
      ENDIF
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 
*
  999 CLOSE( IFG )
      CLOSE( IFM )
*
      WRITE( NOUT, * )
      WRITE( NOUT, * ) '### END OF EXECUTION ###'
      WRITE( NOUT, * )
*
*
      STOP
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
