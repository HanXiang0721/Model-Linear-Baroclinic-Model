      PROGRAM SVDBR
*
*     derived from svdpw.f on 2001/07/02
*     This program performs a singular value decomposition
*           A = U T V^t
*     by calling the LAPACK routine DGESVD. 
*     The input left-hand-side matrix A is
*     read from a file. The solution for singular value, 
*     left- and right-singular vectors (U and V) are 
*     also written to a file.
*
*     .. Parameters ..
*
      include 'dim.f'
*
      INTEGER            MAXN
      INTEGER            NWORK
      PARAMETER          ( MAXN=MSIZ2 )
      PARAMETER          ( NWORK=100*MAXN )
*     ..
*     .. Local Scalars ..
      INTEGER            LDA, MTR
      INTEGER            M, I, J, L
      INTEGER            INFO, NOUT
      INTEGER            IFM, IFE, IFL, IFR
      INTEGER            LMATS
      INTEGER            JSTR, JEND
      CHARACTER          JOBU  * 1
      CHARACTER          JOBVT * 1
*     ..
*     .. Local Arrays ..
      REAL*8             X( MAXN )      !! input matrix
      REAL*8             A( MAXN*MAXN ) !! input matrix
      REAL*8             S( MAXN )      !! singular value (real) 
      REAL*8             DUM( 1 )       !! dumy for left vector
      REAL*8             VT( MAXN*MAXN) !! right singular vector
      REAL*8             V( MAXN, MAXN) !! right singular vector
      REAL*8             WORK( NWORK )  !! work
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGESVD
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          SNGL, DBLE, MAX, IDINT
*
*     namelist variables
*
      CHARACTER*90 CFM          !! input file name (matrix A)
      CHARACTER*90 CFE          !! singular value file (grads)
      CHARACTER*90 CFL          !! left vector file name (temporary)
      CHARACTER*90 CFR          !! right vector file name (temporary)
      CHARACTER*90 CFLG         !! left vector file name (grads)
      CHARACTER*90 CFRG         !! right vector file name (grads)
      INTEGER      NWAVE        !! selected zonal wavenumber
      LOGICAL      OWALL        !! write all the wavenumber

      NAMELIST    /NMSFIN/   CFM, CFE, CFL, CFR, CFLG, CFRG
      NAMELIST    /NMWAVE/   NWAVE 
      NAMELIST    /NMALL/    OWALL

      DATA         NOUT  / 6 /
      DATA         JOBU  /'O'/
      DATA         JOBVT /'S'/
      DATA         NWAVE / 0       /
      DATA         OWALL / .FALSE. / !! PWM
*
*     open the NAMELIST file
*
      REWIND( 1 )
      OPEN ( 0, FILE='IEEE_ERROR')
      OPEN ( 1, FILE = 'SETPAR', STATUS = 'OLD')
*
      READ( 1, NMSFIN) 
      READ( 1, NMWAVE) 
      READ( 1, NMALL) 
      IF( OWALL ) NWAVE = -1
*
*     output file
*
      IFE = 55
      OPEN( IFE, FILE = CFE, FORM = 'UNFORMATTED', STATUS = 'UNKNOWN' )
*
      IFL = 66
      OPEN( IFL, FILE = CFL, FORM = 'UNFORMATTED', STATUS = 'UNKNOWN' )
*
      IFR = 77
      OPEN( IFR, FILE = CFR, FORM = 'UNFORMATTED', STATUS = 'UNKNOWN' )
*
*
      WRITE( NOUT, FMT = * )
      WRITE( NOUT, * ) '### SOLVE SINGULAR VALUE PROBLEM ###'
      WRITE( NOUT, FMT = * )
*
*     Read from file and distribute matrices L
*
      IFM = 88
      OPEN( IFM, FILE=CFM, FORM='UNFORMATTED', STATUS='OLD' )
      WRITE( NOUT, FMT = * )
     $     '***********************************************'
*
      MTR = NTR
      IF( OWALL ) THEN
         MTR = 0
      ENDIF
      DO 100 M = 0, MIN( MTR, NTR )
         L = NMAX - M + 1
         IF( M .EQ. 0 ) THEN
            L = L - 1
         ELSE
            L = 2 * L
         ENDIF
         IF( OWALL ) THEN
            LDA = MSIZ2
            LMATS = LDA
            WRITE( NOUT, FMT = * )
            WRITE( NOUT, *) '  Matrix Size =', LMATS,' x', LMATS
            WRITE( NOUT, *)
         ELSE
            LDA = L
            IF( NWAVE .GE. 0 .AND. M .EQ. NWAVE ) THEN
               WRITE( NOUT, FMT = * )
     $              'Read matrix A for zonal WN = ',M
               LMATS = LDA
               WRITE( NOUT, FMT = * )
               WRITE( NOUT, *)
     $              '  Matrix Size =', LMATS,' x', LMATS
               WRITE( NOUT, *)
            ENDIF
         ENDIF
*
         DO 200 I = 1, LDA 
            READ( IFM, END = 99 ) (X(J),J=1,LDA)
            JSTR = (I-1)*LDA + 1
            JEND = JSTR + LDA - 1
            DO 300 J = JSTR, JEND
               A( J ) = X( J-JSTR+1 )
  300       CONTINUE
  200    CONTINUE
         IF( NWAVE .GE. 0 .AND. M .EQ. NWAVE ) GOTO 99
  100 CONTINUE
   99 CLOSE( IFM )
*
*
**********************************************************************
*     Call LAPACK DGESVD routine
**********************************************************************
*
      WRITE( NOUT, FMT = * )
     $     '***********************************************'
      WRITE( NOUT, FMT = * )
     $     '      LAPACK routine call: (DGESVD)'
      WRITE( NOUT, FMT = * )
     $     '***********************************************'
      WRITE( NOUT, FMT = * )
      WRITE( NOUT, FMT = * ) 'call DGESVD'

      CALL DGESVD( JOBU, JOBVT, LDA, LDA, A, LDA, S, DUM, 1,
     $     VT, LDA, WORK, NWORK, INFO )
*
      WRITE( NOUT, FMT = * )
      WRITE( NOUT, FMT = * ) 'INFO code returned by DGESVD = ', INFO
      WRITE( NOUT, FMT = * )
*
*     transpose V vector
*
      DO 400 I = 1, LDA
         JSTR = (I-1)*LDA + 1
         JEND = JSTR + LDA - 1
         DO 410 J = JSTR, JEND
            V( I, J-JSTR+1) = VT( J )
  410    CONTINUE
 400  CONTINUE
*
*     write down singular values/ left and right vectors
*
      DO 500 J = 1, LDA
         JSTR = (J-1)*LDA + 1
         JEND = JSTR + LDA - 1
         WRITE( IFE ) SNGL(S(J))
         WRITE( IFL ) (A(I),I=JSTR,JEND)  !! i: grid, j: mode
         WRITE( IFR ) (V(I,J),I=1,LDA)    !! i: grid, j: mode
 500  CONTINUE
      CLOSE( IFE )
      CLOSE( IFL )
      CLOSE( IFR )
      WRITE( NOUT, FMT = * )
     $     '***********************************************'
      WRITE( NOUT, FMT = * )
*
      WRITE( NOUT, * ) '### END OF EXECUTION ###'
      WRITE( NOUT, FMT = * )
*
*
      STOP
      END
