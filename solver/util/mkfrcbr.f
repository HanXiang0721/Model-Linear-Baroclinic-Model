      PROGRAM MKFRCBR
*
*     derived from mkfrcng.f on 2001/06/19
*     This routine makes simplified vorticity forcing function
*     for the barotropic response.
*
*     Alternative selection of the forcing shape is:
*      Horizontal: Elliptic function/zonally uniform function 
*
      include 'dim.f'
*
*
*
      REAL*8       GFRCT( IDIM*JMAX, KMAX)  
      REAL*8       WFRCT( NMDIM, KMAX)  
      REAL*8       WFRCF( NMDIM, KMAX)  
*
      REAL*8       ALON( IDIM, JMAX )
      REAL*8       ALAT( IDIM, JMAX )
      REAL*8       DLON( IDIM, JMAX ) !! unused
      CHARACTER    HALON *(16)        !! unused
*
*     [work]
      INTEGER      NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      REAL*4       DAT( IMAX, JMAX)
      REAL*8       AXY( IDIM, JMAX)
      REAL*8       DUM( IMAX, JMAX)
      REAL*8       PI
      REAL*8       V1, V2, V
      REAL*8       S2D
      REAL*8       ONE
      INTEGER      IFM, IFG
      INTEGER      I, J, K, L, M, IJ, KV, LEND, IW
      INTEGER      NOUT
*
*     [intrinsic]
      INTRINSIC    DSIN, DSQRT, DBLE, SNGL
*
      CHARACTER    CHPR( MAXH )*15
      CHARACTER*90 CFM          !! output file name (matrix)
      CHARACTER*90 CFG          !! output file name (grads)
      INTEGER      KHPR         !! type of horizontal profile
      REAL*8       HAMP         !! horizontal amplitude
      REAL*8       XDIL         !! x-dilation for Elliptic-shape
      REAL*8       YDIL         !! y-dilation for Elliptic-shape
      REAL*8       XCNT         !! x-center for Elliptic-shape
      REAL*8       YCNT         !! y-center for Elliptic-shape
      REAL*8       FACT( 5 )    !! factor (unused)
      LOGICAL      OWALL        !! write all the wavenumber
      LOGICAL      OSYM         !! equatorial symmetry
*
      NAMELIST    /NMFIN/   CFM, CFG, FACT
      NAMELIST    /NMHPR/   KHPR, HAMP, XDIL, YDIL, XCNT, YCNT
      NAMELIST    /NMALL/   OWALL
      NAMELIST    /NMSYM/   OSYM
*
      DATA         NOUT / 6 /
      DATA         S2D / 86400.D0 /
      DATA         ONE / 1.D0 /
      DATA         CHPR
     $           /'Elliptic       ','Zonally uniform'/
*                  123456789012345   123456789012345   
      DATA         OWALL / .FALSE. / !! PWM
      DATA         OSYM  / .FALSE. / !! not symmetric
*
*     open the NAMELIST file
*
      REWIND( 1 )
      OPEN ( 0, FILE='IEEE_ERROR')
      OPEN ( 1, FILE = 'SETPAR', STATUS = 'OLD')
*
      READ( 1, NMFIN) 
      REWIND( 1 )
      READ( 1, NMHPR) 
      REWIND( 1 )
      READ( 1, NMALL) 
      REWIND( 1 )
      READ( 1, NMSYM) 
*
*
      WRITE( NOUT, * ) '### MAKE FORCING MATRIX ###'
      WRITE( NOUT, * )
      WRITE( NOUT, * ) '................'
      IF( KHPR .GT. 0 ) THEN
         WRITE( NOUT, * ) 'Selected shape:'
         WRITE( NOUT, * ) '  Horizontal:',CHPR( KHPR )
      ELSE
         WRITE( NOUT, * ) 'Only grid->wave conversion'
      ENDIF
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * )
*
*     open files
*
      IFM = 10
      IFG = 20
      OPEN ( IFM, FILE = CFM, FORM = 'UNFORMATTED', STATUS = 'UNKNOWN' ) 
      OPEN ( IFG, FILE = CFG, FORM = 'UNFORMATTED', STATUS = 'UNKNOWN' )
      WRITE( NOUT, * ) 'Matrix file         :', CFM
      WRITE( NOUT, * ) 'Matrix file (GrADS) :', CFG
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 
*
      CALL SPSTUP               !! spherical harmonic functions
      CALL SETNMO2
     O     ( NMO   ,
     D     MMAX  , LMAX  , NMAX  , MINT    )
*
      PI = ATAN( 1.0D0 ) * 4.0D0
      CALL SETLON
     O         ( ALON  , DLON , HALON  )
      CALL SETLAT
     O         ( ALAT  , DLON , HALON  )
*
      DO 1 J = 1, JMAX
         DO 1 I = 1, IDIM
            ALON( I, J) = ALON( I, J) * 360.D0 / ( 2.D0 * PI )
            ALAT( I, J) = ALAT( I, J) * 360.D0 / ( 2.D0 * PI )
 1    CONTINUE
*
      CALL SETZ( GFRCT, IDIM*JMAX*KMAX )
      CALL SETZ( DUM, IMAX*JMAX )
      CALL SETZ( WFRCT, NMDIM*KMAX )
      CALL SETZ( WFRCF, NMDIM*KMAX )
*
      IF( KHPR .LE. 0 ) GOTO 1010
*
*     horizontal profile
*
      IJ = 0
      DO 20 J = 1, JMAX
         DO 30 I = 1, IDIM
            IJ = IJ + 1
*
            IF( KHPR .EQ. 1 ) THEN !! Elliptic
               IF( XDIL .LE. 0.D0 .OR. YDIL .LE. 0.D0 ) THEN
                  WRITE( NOUT, *) 
     $                 '### Parameter XDIL/YDIL not correct ###' 
                  STOP
               ENDIF
*
               V1 = ( ALON( I, J ) - XCNT )**2 / XDIL**2
               IF( ALON(I,J).LT.180. .AND. XCNT+XDIL.GE.360. )
     $              V1 = ( 360. + ALON( I, J ) - XCNT )**2 / XDIL**2
               IF( ALON(I,J).GT.180. .AND. XCNT-XDIL.LE.0. )
     $              V1 = ( ALON( I, J ) - 360. - XCNT )**2 / XDIL**2
               V2 = ( ALAT( I, J ) - YCNT )**2 / YDIL**2
               V = V1 + V2
               IF( V .GT. 1. ) THEN
                  AXY( I, J) = 0.D0
               ELSE
                  V = DSQRT( V ) 
                  AXY( I, J) = HAMP * ( 1.D0 - V )
               ENDIF
            ENDIF
            IF( KHPR .EQ. 2 ) THEN !! zonal uniform
*
               IF( ALAT( I, J) .GE. YCNT - YDIL .AND.
     $             ALAT( I, J) .LE. YCNT + YDIL ) THEN
                  V = HAMP
               ELSE
                  V = 0.D0
               ENDIF

               AXY( I, J) = V
            ENDIF

            KV = KMAX
            DO 40 K = 1, KV
               GFRCT( IJ, K) = AXY( I, J) / S2D
 40         CONTINUE
*
 30      CONTINUE
 20   CONTINUE
      WRITE( NOUT, * ) 'Set horizontal shape'
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 
*
*     write GrADS file
*
      DO 100 K = 1, KMAX
         DO 110 J = 1, JMAX
            DO 110 I = 1, IMAX
               IJ = (J-1)*IDIM + I
               DUM( I, J) = GFRCT( IJ, K)
 110     CONTINUE
         WRITE( IFG ) ((SNGL(DUM(I,J)),I=1,IMAX),J=1,JMAX)
 100  CONTINUE
*
      CLOSE( IFG )
      WRITE( NOUT, * ) 'Written to GrADS file'
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 
      GOTO 1020
*
*     read GrADS data
*
 1010 WRITE( NOUT, * ) 
      WRITE( NOUT, * ) 'Read forcing file'
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 
*
      DO 50 K = 1, KMAX         !! vorticity
         READ( IFG ) DAT
         IJ = 0
         DO 60 J = 1, JMAX
            DO 70 I = 1, IMAX
               IJ = IJ + 1
               GFRCT( IJ, K) = FACT(1) * DBLE( DAT( I, J) )
  70        CONTINUE
            IJ = IJ + 1
            GFRCT( IJ, K) = FACT(1) * DBLE( DAT( 1, J) )
  60     CONTINUE
  50  CONTINUE
      CLOSE( IFG )
*
*     grid to wave (forcing matrix)
*
 1020 CALL G2W
     O     ( WFRCT   ,
     I       GFRCT   , '    ', 'POSO', KMAX )
*
*     write matrix file
*
      IW = 0
      DO 200 M = 0, NTR
         LEND = MIN( LMAX, NMAX-M)
         DO 220 K = 1, KMAX
            IF( .NOT. OWALL ) IW = 0
            DO 210 L = 0, LEND
               IF( M .EQ. 0 .AND. L .EQ. 0 ) GOTO 210
               IF( OSYM .AND. MOD( L,2 ) .EQ. 1 ) GOTO 210
               I = NMO( 1, M, L)
               J = NMO( 2, M, L)
               IW = IW + 1
               WFRCF(IW,K) = WFRCT(I,K)
               IF( M .EQ. 0 ) GOTO 210
               IW = IW + 1
               WFRCF(IW,K) = WFRCT(J,K)
  210       CONTINUE
  220    CONTINUE
         IF( .NOT. OWALL ) WRITE( IFM ) ((WFRCF(I,K),I=1,IW),K=1,KMAX)
  200 CONTINUE
      IF( OWALL ) WRITE( IFM ) ((WFRCF(I,K),I=1,IW),K=1,KMAX)
*
      CLOSE( IFM )
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
