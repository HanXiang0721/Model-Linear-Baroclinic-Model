      PROGRAM INVERSE
*
*     derived from solver/lin.f on 1999/02/24
*     This program computes a inverse matrix L-1
*     by calling the LAPACK routine DGESV. 
*     The input right-hand-side matrices are NxN unit matrix
*     which diagonal is 1 and 0 elsewhere.
*
*     .. Parameters ..
*
      include 'dim.f'
*
      INTEGER            MAXN
CC      PARAMETER          ( MAXN = 2*NMAX*NTR*(KMAX*NVAR+1) )
CC      PARAMETER          ( MAXN=15795) !! T21L11m10 moist LBM
CC      PARAMETER          ( MAXN=10935) !! T21L11m6 moist LBM
      PARAMETER          ( MAXN = 2*NMAX*(KMAX*NVAR+1) ) !! PWM
*     ..
*     .. Local Scalars ..
      INTEGER            INFO, NOUT
      INTEGER            IFM, IFS
      INTEGER            LMATS, LDA, LEND, MTR, MVAR, MSIZT
      INTEGER            JSTR, JEND
      INTEGER            I, J, L, M, IJ
*     ..
*     .. Local Arrays ..
      REAL*8             A( MAXN*MAXN )
      REAL*8             B( MAXN*MAXN )

CC      REAL*8,  ALLOCATABLE :: A(:)
CC      REAL*8,  ALLOCATABLE :: B(:)

      INTEGER            IPIV( MAXN )
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGESV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          SNGL, DBLE, MAX, IDINT
*
*     namelist variables
*
      CHARACTER*90 CFM          !! input file name (matrix L)
      CHARACTER*90 CFR          !! input file name (rhs, dummy) 
      CHARACTER*90 CFS          !! output file name (inverse of L)
      CHARACTER*90 CFG          !! output file name (grads, dummy)
      LOGICAL      OWALL        !! write all the wavenumber
      LOGICAL      OCLASSIC     !! classic dry model
*
      NAMELIST    /NMFIN/   CFM, CFR, CFS, CFG
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
      READ( 1, NMALL) 
      READ( 1, NMCLS) 
*
CC      ALLOCATE( A(MAXN*MAXN) )
*
*     output file
*
      IFS = 88
      open( IFS, FILE = CFS, FORM = 'UNFORMATTED',
     $     STATUS = 'UNKNOWN' )
*
*
      WRITE( NOUT, FMT = * )
      WRITE( NOUT, * ) '### COMPUTE INVERSE MATRIX ###'
      WRITE( NOUT, FMT = * )
*
*     Read from file and distribute matrices L 
*
      IFM = 77
      OPEN( IFM, FILE=CFM, FORM='UNFORMATTED', STATUS='OLD', ERR=1999 )
      WRITE( NOUT, FMT = * )
     $     '***********************************************'
      WRITE( NOUT, FMT = * )
      WRITE( NOUT, FMT = * )
     $     'Read data for the matrix L'
      WRITE( NOUT, FMT = * )
*
      LEND = 0
      DO 200 M = 0, NTR
         L = NMAX - M + 1
         IF( M .EQ. 0 ) THEN
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
         MVAR = NVAR - 1
      ENDIF
      DO 300 M = 0, MIN( MTR, NTR ) 
         L = NMAX - M + 1
         IF( M .EQ. 0 ) THEN
            L = L - 1
         ELSE
            L = 2 * L
         ENDIF
         IF( OWALL ) THEN
            MSIZT = LEND * ( MVAR * KMAX + 1 )
         ELSE
            MSIZT = L * ( MVAR * KMAX + 1 )
            WRITE( NOUT, FMT = * )
     $           'Read matrix L for zonal WN = ',M
            WRITE( NOUT, FMT = * )
         ENDIF

         LDA = MSIZT
         LMATS = LDA
         WRITE( NOUT, *) '  Matrix Size (left)=', LMATS,' x', LMATS
         WRITE( NOUT, *)
*
         DO 100 I = 1, LDA 
            JSTR = (I-1)*LDA + 1
            JEND = JSTR + LDA - 1
            READ( IFM, ERR=2999 ) (A(J),J=JSTR,JEND)
  100    CONTINUE
*
*     unit matrices B
*
CC         ALLOCATE( B(MAXN*MAXN) )
         IJ = 0
         DO 110 I = 1, LDA
            DO 120 J = 1, LDA
               IJ = IJ + 1
               B( IJ ) = 0.D0
               IF( J .EQ. I ) B( IJ ) = 1.D0
 120        CONTINUE
 110     CONTINUE
*
**********************************************************************
*     Call LAPACK DGESV routine
**********************************************************************
*
         WRITE( NOUT, FMT = * )
     $        '***********************************************'
         WRITE( NOUT, FMT = * )
     $        '      LAPACK routine call: (DGESV)'
         WRITE( NOUT, FMT = * )
     $        '***********************************************'
         WRITE( NOUT, FMT = * )
         WRITE( NOUT, FMT = * ) 'call DGESV'

         CALL DGESV( LDA, LDA, A, LDA, IPIV, B, LDA, INFO )
*
         WRITE( NOUT, FMT = * )
         WRITE( NOUT, FMT = * ) 'INFO code returned by DGESV = ', INFO
         WRITE( NOUT, FMT = * )
*
*     write down the matrix L_inverse stored in B
*
         CALL TRANS( A, B, LDA ) !! transpose
*
         DO 130 I = 1, LDA
            JSTR = (I-1)*LDA + 1
            JEND = JSTR + LDA - 1
            WRITE( IFS ) (A(J),J=JSTR,JEND)
 130     CONTINUE

 300  CONTINUE
      CLOSE( IFS )
*
      WRITE( NOUT, FMT = * )
     $     '***********************************************'
*
      WRITE( NOUT, FMT = * )
      WRITE( NOUT, * ) '### END OF EXECUTION ###'
      WRITE( NOUT, FMT = * )
      GOTO 3999

 1999 WRITE( 6, *) ' #### FILE OPEN ERROR #### '
      CALL XABORT( 1 )
 2999 WRITE( 6, *) ' #### FILE READ ERROR #### '
      CALL XABORT( 1 )

 3999 CLOSE( IFM )
      CLOSE( IFS )
*
*
      STOP
      END
*********************************************************************
      SUBROUTINE   TRANS
     O     ( Y ,
     I       X , LDA )
*
*     transpose matrix
*
      INTEGER LDA
      REAL*8  X( LDA, LDA )
      REAL*8  Y( LDA, LDA )
*
      INTEGER I, J
*
      DO 1000 J = 1, LDA
         DO 2000 I = 1, LDA
            Y( J,I ) = X( I,J )
 2000    CONTINUE
 1000 CONTINUE

      RETURN
      END
