      PROGRAM RANDOM
*
*     This program makes random forcing in a
*     physical space.
*     random sequence is now internally generated [05/08/29]
*
      include 'dim.f'
*
      INTEGER    MAXN
      PARAMETER ( MAXN = IMAX*JMAX*(KMAX*NVAR+1) )
*
      REAL*4      DAT ( MAXN )
      REAL*4      GVOR( IMAX*JMAX, KMAX )
      REAL*4      GDIV( IMAX*JMAX, KMAX )
      REAL*4      GTMP( IMAX*JMAX, KMAX )
      REAL*4      GPS ( IDIM*JMAX      )
      REAL*4      GSPH( IMAX*JMAX, KMAX )
      INTEGER     IFG
      INTEGER     I, J, IJ, K, M, IJKN
*
*     [work]
      REAL*8       ALAT0( IDIM, JMAX )
      REAL*8       DLAT( IDIM, JMAX ) !! unused
      REAL*8       PI
      REAL*4       ALAT( IMAX*JMAX )
      CHARACTER    HALAT *(16)        !! unused
      INTEGER      NOUT

      REAL*4       GASDEV, RAN
*
*     [intrinsic]
*
      CHARACTER*90 CRDMG            !! random initial perturbation
      REAL*4       FACT( NVAR+ 1 )  !! amplitudes
      REAL*4       SLAT1            !! sponge-applied latitude limit
      REAL*4       SLAT2            !! restoring latitude limit
      REAL*4       SFACT            !! factor for sponge
      INTEGER      NEXP             !! no. of samples 
      LOGICAL      OSPONG           !! apply sponge 
      LOGICAL      OCLASSIC         !! classic dry model

      NAMELIST    /NMRDM/  CRDMG, NEXP, 
     &                     FACT, OSPONG, SLAT1, SLAT2, SFACT
      NAMELIST    /NMCLS/   OCLASSIC

      DATA NOUT      / 6  /
      DATA FACT      / 6.7E-6 , 6.7E-7 , 6.7E0, 0.E0 , 6.7E-5  /
      DATA OSPONG    / .FALSE. /
      DATA OCLASSIC  / .TRUE.  /
      DATA NEXP      / 1000    /
      DATA SLAT1     / 20.0    /
      DATA SLAT2     / 25.0    /
      DATA SFACT     / 0.1     /
*
*     open the NAMELIST file
*
      OPEN ( 0, FILE='IEEE_ERROR')
      OPEN ( 1, FILE = 'SETPAR', STATUS = 'OLD')
*
      REWIND( 1 )
      READ( 1, NMRDM ) 
      REWIND( 1 )
      READ( 1, NMCLS ) 
*
*
      WRITE( NOUT, * )
      WRITE( NOUT, * ) '### start making random forcing ###'
      WRITE( NOUT, * )
*
*     open files
*
      IFG = 44
      OPEN( IFG, FILE = CRDMG, FORM = 'UNFORMATTED',
     &                         STATUS = 'UNKNOWN' )

      WRITE( NOUT, *) 
      WRITE( NOUT, *) ' @@ Output:',CRDMG
      WRITE( NOUT, *) 

      CALL SETZ4( GVOR, IMAX*JMAX*KMAX )
      CALL SETZ4( GDIV, IMAX*JMAX*KMAX )
      CALL SETZ4( GTMP, IMAX*JMAX*KMAX )
      CALL SETZ4( GPS , IMAX*JMAX      )
      CALL SETZ4( GSPH, IMAX*JMAX*KMAX )
*
*     set up
*
      CALL SETCONS
      CALL SPSTUP
      PI = ATAN( 1.0D0 ) * 4.0D0
      CALL SETLAT
     O         ( ALAT0 , DLAT , HALAT  )
      IJ = 0
      DO 10 J = 1, JMAX
         DO 20 I = 1, IMAX
            IJ = IJ + 1
            ALAT( IJ ) = SNGL( ALAT0( I,J ) * 180.D0 / PI )
 20      CONTINUE
 10   CONTINUE
*
*     read random number
*
      IJKN = -102893
      DO 100 M = 1, NEXP        !! # case
*
         DAT( 1 ) =  GASDEV( IJKN )
         DO 101 IJKN = 2, MAXN
            DAT( IJKN ) =  GASDEV( IJKN )
 101     CONTINUE
         CALL XSTD( DAT, MAXN )
         IJKN = -1 * AINT( ABS( DAT( MAXN ) )*1.E3 )
*
         I = 0
         DO 110 K = 1, KMAX
            DO 120 IJ = 1, IMAX*JMAX
               I = I + 1
               GVOR( IJ,K ) = DAT( I ) * FACT( 1 )
 120        CONTINUE
 110     CONTINUE
         DO 130 K = 1, KMAX
            DO 140 IJ = 1, IMAX*JMAX
               I = I + 1
               GDIV( IJ,K ) = DAT( I ) * FACT( 2 )
 140        CONTINUE
 130     CONTINUE
         DO 150 K = 1, KMAX
            DO 160 IJ = 1, IMAX*JMAX
               I = I + 1
               GTMP( IJ,K ) = DAT( I ) * FACT( 3 )
 160        CONTINUE
 150     CONTINUE
         IF( .NOT. OCLASSIC ) THEN
            DO 170 K = 1, KMAX
               DO 180 IJ = 1, IMAX*JMAX
                  I = I + 1
                  GSPH( IJ,K ) = DAT( I ) * FACT( 5 )
 180           CONTINUE
 170        CONTINUE
         ENDIF
*                  
         IF( OSPONG ) THEN
            DO 190 K = 1, KMAX
               DO 190 IJ = 1, IMAX*JMAX
                  IF( ABS( ALAT( IJ ) ) .LT. SLAT1 ) THEN
                     GVOR( IJ,K ) = GVOR( IJ,K ) * SFACT
                     GDIV( IJ,K ) = GDIV( IJ,K ) * SFACT
                     GTMP( IJ,K ) = GTMP( IJ,K ) * SFACT
                     GSPH( IJ,K ) = GSPH( IJ,K ) * SFACT
                  ELSEIF( ABS( ALAT( IJ ) ) .LT. SLAT2 ) THEN
                     GVOR( IJ,K ) = GVOR( IJ,K ) * ( SFACT + 1. ) / 2.
                     GDIV( IJ,K ) = GDIV( IJ,K ) * ( SFACT + 1. ) / 2.
                     GTMP( IJ,K ) = GTMP( IJ,K ) * ( SFACT + 1. ) / 2.
                     GSPH( IJ,K ) = GSPH( IJ,K ) * ( SFACT + 1. ) / 2.
                  ENDIF
 190        CONTINUE
         ENDIF
*
         DO 200 K = 1, KMAX
            WRITE( IFG ) ( GVOR( IJ,K ), IJ = 1, IMAX*JMAX )
 200     CONTINUE
         DO 210 K = 1, KMAX
            WRITE( IFG ) ( GDIV( IJ,K ), IJ = 1, IMAX*JMAX )
 210     CONTINUE
         DO 220 K = 1, KMAX
            WRITE( IFG ) ( GTMP( IJ,K ), IJ = 1, IMAX*JMAX )
 220     CONTINUE
         WRITE( IFG ) GPS
         IF( .NOT. OCLASSIC ) THEN
            DO 230 K = 1, KMAX
               WRITE( IFG ) ( GSPH( IJ,K ), IJ = 1, IMAX*JMAX )
 230        CONTINUE
         ENDIF
*
*     log
*
         WRITE( NOUT, *) '# case:',M
*
 100  CONTINUE
      CLOSE( IFG )
      WRITE( NOUT, * )
      WRITE( NOUT, * ) '### end of execution ###'
      WRITE( NOUT, * )

      STOP
      END
*##############################################
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
*##############################################
      SUBROUTINE XSTD( A, IA )
*
*
      INTEGER IA
      REAL*4  A( IA )

      REAL*4  AM, SM
      INTEGER I
*
      AM = 0.
      DO 10 I = 1, IA
         AM = AM + A( I ) / FLOAT( IA )
 10   CONTINUE
*
      SM = 0.
      DO 20 I = 1, IA
         SM = SM + ( A( I ) - AM )**2 / FLOAT( IA )
 20   CONTINUE
      SM = SQRT( SM )
*
      DO 30 I = 1, IA
         A( I ) = ( A( I ) - AM ) / SM
 30   CONTINUE
*
      RETURN
      END
*##############################################
      FUNCTION GASDEV( IDUM )
*
*     random number generation with zero-mean, unit-variance, 
*     normally distributed random sequence
*     taken from numerical recipes
*
      INTEGER IDUM
      REAL*4  GASDEV
      INTEGER ISET

      REAL*4  FAC, GSET, RSQ, V1, V2, RAN1, RAN2

      SAVE    ISET, GSET

      DATA    ISET / 0 /
*
      IF( ISET .EQ. 0 ) THEN
C 1       V1 = 2. * RAN1( IDUM ) - 1.
C         V2 = 2. * RAN1( IDUM ) - 1.
 1       V1 = 2. * RAN2( IDUM ) - 1.
         V2 = 2. * RAN2( IDUM ) - 1.
         RSQ = V1**2 + V2**2
         IF( RSQ .GE. 1. .OR. RSQ .EQ. 0. ) GOTO 1
         FAC = SQRT( -2. * LOG( RSQ ) / RSQ )
         GSET = V1 * FAC
         GASDEV = V2 * FAC
         ISET = 1
      ELSE
         GASDEV = GSET
         ISET = 0
      ENDIF
*
      RETURN
      END
*##############################################
      FUNCTION RAN1( IDUM )
*
*     "Minimal" random number generator
*     a uniform distribution between 0. and 1.
*     taken from numerical recipes
*
      INTEGER IDUM
      REAL*4  RAN1

      INTEGER IA, IM, IQ, IR, NTAB, NDIV
      REAL*4  AM, EPS, RNMX
      PARAMETER ( IA = 16807, IM = 2147483647, AM = 1./IM )
      PARAMETER ( IQ = 127773, IR = 2836, NTAB = 32 )
      PARAMETER ( NDIV = 1+(IM-1)/NTAB )
      PARAMETER ( EPS = 1.2E-7, RNMX = 1.-EPS )

      INTEGER J, K, IV( NTAB ), IY

      SAVE    IV, IY

      DATA    IV / NTAB*0 /
      DATA    IY / 0      /
*
      IF( IDUM .LE. 0 .OR. IY .EQ. 0 ) THEN
         IDUM = MAX( -IDUM,1 )
         DO 1000 J = NTAB+8, 1, -1
            K = IDUM / IQ
            IDUM = IA * ( IDUM - K * IQ ) - IR * K
            IF( J .LE. NTAB ) IV( J ) = IDUM
 1000    CONTINUE
         IY = IV( 1 )
      ENDIF
*
      K = IDUM / IQ
      IDUM = IA * ( IDUM - K * IQ ) - IR * K
      IF( IDUM .LT. 0 ) IDUM = IDUM + IM
      J = 1 + IY / NDIV
      IY = IV( J )
      IV( J ) = IDUM
      RAN1 = MIN( AM * IY, RNMX )
*
      RETURN
      END
*##############################################
      FUNCTION RAN2( IDUM )
*
*     L'Ecuyer with Bays-Durham random number generator
*     a uniform distribution between 0. and 1.
*     taken from numerical recipes
*
      INTEGER IDUM
      REAL*4  RAN2

      INTEGER IM1, IM2, IMM1, IA1, IA2, IQ1, IQ2, IR1, IR2, NTAB, NDIV
      REAL*4  AM, EPS, RNMX
      PARAMETER ( IM1 = 2147483563, IM2 = 2147483399, AM = 1./IM1 )
      PARAMETER ( IMM1 = IM1-1, IA1 = 40014, IA2 = 40692 )
      PARAMETER ( IQ1 = 53668, IQ2 = 52774, IR1 = 12211, IR2 = 3791 )
      PARAMETER ( NTAB = 32, NDIV = 1+IMM1/NTAB )
      PARAMETER ( EPS = 1.2E-7, RNMX = 1.-EPS )

      INTEGER IDUM2, J, K, IV( NTAB ), IY

      SAVE    IV, IY, IDUM2

      DATA    IDUM2 / 123456789 /
      DATA    IV    / NTAB*0    /
      DATA    IY    / 0         /
*
      IF( IDUM .LE. 0 ) THEN
         IDUM = MAX( -IDUM,1 )
         IDUM2 = IDUM
         DO 1000 J = NTAB+8, 1, -1
            K = IDUM / IQ1
            IDUM = IA1 * ( IDUM - K * IQ1 ) - IR1 * K
            IF( J .LE. NTAB ) IV( J ) = IDUM
 1000    CONTINUE
         IY = IV( 1 )
      ENDIF
*
      K = IDUM / IQ1
      IDUM = IA1 * ( IDUM - K * IQ1 ) - IR1 * K
      IF( IDUM .LT. 0 ) IDUM = IDUM + IM1
      K = IDUM2 / IQ2
      IDUM2 = IA2 * ( IDUM2 - K * IQ2 ) - IR2 * K
      IF( IDUM2 .LT. 0 ) IDUM2 = IDUM2 + IM2

      J = 1 + IY / NDIV
      IY = IV( J ) - IDUM2
      IV( J ) = IDUM
      IF( IY .LT. 1 ) IY = IY + IMM1
      RAN2 = MIN( AM * IY, RNMX )
*
      RETURN
      END
