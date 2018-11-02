      PROGRAM EOF
*
*     derived from expert/eof.f on 2000/04/24
*     This program solves a non-symmetric eigenproblem
*           A = Z T Z^t
*     by calling the LAPACK routine DGEEV. 
*     The input left-hand-side matrix A is
*     read from a file. The solution is also written to a file.
*
*     .. Parameters ..
*
      include 'dim.f'
*
      INTEGER            MAXN
      INTEGER            NWORK
*
*     standard
      PARAMETER          ( MAXN=2*NMAX*NTR*(NVAR*KMAX+1) )
CC      PARAMETER ( MAXN=10935) !! T21L11m6 moist LBM
CC      PARAMETER ( MAXN=8262) !! T21L11m6 dry LBM
*
*     reduced memory
CC      PARAMETER          ( MAXN=2*NMAX*(NVAR*KMAX+1) ) 
*
      PARAMETER          ( NWORK=1000*MAXN )
*     ..
*     .. Local Scalars ..
      REAL*8             X0, X1, X2
      INTEGER            I, J, L, M
      INTEGER            MUE1, MUE2, MRE
      INTEGER            INFO, NOUT
      INTEGER            IFM, IFE, IFS
      INTEGER            LDA, MTR, MVAR, LEND, LMATS, MSIZT, JSTR, JEND 
      CHARACTER          JOBVL * 1
      CHARACTER          JOBVR * 1
*     ..
*     .. Local Arrays ..
      REAL*8             X( MAXN )      !! input matrix
      REAL*8             A( MAXN*MAXN ) !! input matrix
      REAL*8             WR( MAXN )     !! eigenvalue (real) 
      REAL*8             WI( MAXN )     !! eigenvalue (imaginary)
      REAL*8             DUM( 1 )       !! dumy for left vector
      REAL*8             E( MAXN*MAXN ) !! right eigenvector
      REAL*8             WORK( NWORK )  !! work
*     ..
*     .. External Subroutines ..
      EXTERNAL           DGEEV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          SNGL, DBLE, MAX, IDINT
*
*     namelist variables
*
      CHARACTER*90 CFM          !! input file name (matrix A)
      CHARACTER*90 CFE          !! eigenvalue  file name (grads)
      CHARACTER*90 CFS          !! eigenvector file name (temporary)
      CHARACTER*90 CFG          !! eigenvector file name (grads)
      INTEGER      NWAVE
      LOGICAL      OWALL        !! write all the wavenumber
      LOGICAL      OCLASSIC     !! classic dry model

      NAMELIST    /NMEFIN/   CFM, CFE, CFS, CFG
      NAMELIST    /NMWAVE/   NWAVE !! selected zonal wavenumber
      NAMELIST    /NMALL/    OWALL
      NAMELIST    /NMCLS/    OCLASSIC

      DATA         NOUT / 6 /
      DATA         JOBVL /'N'/
      DATA         JOBVR /'V'/
      DATA         NWAVE / 0       /
      DATA         OWALL    / .FALSE. / !! PWM
      DATA         OCLASSIC / .TRUE.  / !! dry model
*
*     open the NAMELIST file
*
      REWIND( 1 )
      OPEN ( 0, FILE='IEEE_ERROR')
      OPEN ( 1, FILE = 'SETPAR', STATUS = 'OLD')
*
      READ( 1, NMEFIN) 
      REWIND( 1 )
      READ( 1, NMWAVE) 
      REWIND( 1 )
      READ( 1, NMALL) 
      REWIND( 1 )
      READ( 1, NMCLS) 
      IF( OWALL ) NWAVE = -1
*
*     output file
*
      IFE = 66
      open( IFE, FILE = CFE, FORM = 'UNFORMATTED', STATUS = 'UNKNOWN' )
*
      IFS = 77
      open( IFS, FILE = CFS, FORM = 'UNFORMATTED', STATUS = 'UNKNOWN' )
*
*
      WRITE( NOUT, FMT = * )
      WRITE( NOUT, * ) '### SOLVE EIGENPROBLEM ###'
      WRITE( NOUT, FMT = * )
*
*     Read from file and distribute matrices L
*
      IFM = 88
      OPEN( IFM, FILE=CFM, FORM='UNFORMATTED', STATUS='OLD' )
*
*     Read matrix L
*
      WRITE( NOUT, FMT = * )
     $     '***********************************************'
      WRITE( NOUT, FMT = * )
*
      LEND = 0
      DO 100 M = 0, NTR
         L = NMAX - M + 1
         IF( M .EQ. 0 ) THEN
            L = L - 1
         ELSE
            L = 2 * L
         ENDIF
         LEND = LEND + L
 100  CONTINUE
*
      MTR = NTR
      MVAR = NVAR
      IF( OWALL ) THEN
         MTR = 0
      ENDIF
      IF( OCLASSIC ) THEN
         MVAR = NVAR - 1
      ENDIF
      DO 200 M = 0, MIN( MTR, NTR )
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
            IF( NWAVE .GE. 0 .AND. M .EQ. NWAVE ) THEN
               WRITE( NOUT, FMT = * )
     $              'Read matrix L for zonal WN = ',M
               WRITE( NOUT, FMT = * )
            ENDIF
         ENDIF

         LDA = MSIZT
         LMATS = LDA
         WRITE( NOUT, *) '  Matrix Size =', LMATS,' x', LMATS
         WRITE( NOUT, *)

         DO 300 I = 1, LDA 
            READ( IFM, END = 299 ) (X(J),J=1,LDA)
            JSTR = (I-1)*LDA + 1
            JEND = JSTR + LDA - 1
            DO 310 J = JSTR, JEND
               A( J ) = X( J-JSTR+1 )
  310       CONTINUE
  300    CONTINUE
         IF( NWAVE .GE. 0 .AND. M .EQ. NWAVE ) GOTO 299
  200 CONTINUE
  299 CLOSE( IFM )
*
**********************************************************************
*     Call LAPACK DGEEV routine
**********************************************************************
*
      WRITE( NOUT, FMT = * )
     $     '***********************************************'
      WRITE( NOUT, FMT = * )
     $     '      LAPACK routine call: (DGEEV)'
      WRITE( NOUT, FMT = * )
     $     '***********************************************'
      WRITE( NOUT, FMT = * )
      WRITE( NOUT, FMT = * ) 'call DGEEV'

      CALL DGEEV( JOBVL, JOBVR, LDA, A, LDA, WR, WI, DUM, 1,
     $     E, LDA, WORK, NWORK, INFO )
*
      WRITE( NOUT, FMT = * )
      WRITE( NOUT, FMT = * ) 'INFO code returned by DGEEV = ', INFO
      WRITE( NOUT, FMT = * )
*
*     write down eigenvalues/eigenvectors
*
      DO 400 J = 1, LDA
         JSTR = (J-1)*LDA + 1
         JEND = JSTR + LDA - 1
         WRITE( IFE ) SNGL(-WR(J)), SNGL(WI(J))
         WRITE( IFS ) (E(I),I=JSTR,JEND) !! i: grid, j: mode
 400  CONTINUE
      CLOSE( IFE )
      CLOSE( IFS )
      WRITE( NOUT, FMT = * )
     $     '***********************************************'
      WRITE( NOUT, FMT = * )
*
*     seek # of leading unstable modes
*
      MUE1 = 1
      MUE2 = 1
      MRE  = 1
      X1 = WR( 1 )
      X2 = WR( 1 )
      X0 = 999.D0
      DO 500 J = 1, LDA
         IF( WI( J ) .GE. 0.D0 .AND. WR( J ) .GT. X1 ) THEN
            X2 = X1
            MUE2 = MUE1
            X1 = WR( J )
            MUE1 = J
         ENDIF
         IF( WI( J ) .GT. 0.D0 ) THEN
            IF( WR( J ) .GT. 0.D0 .AND. WR( J ) .LT. X0 ) THEN
               MRE = J
               X0 = WR( J )
            ENDIF
         ENDIF
 500  CONTINUE
*
CX      WRITE( NOUT, FMT = * )
CX     $     '***********************************************'
CX      WRITE( NOUT, FMT = * )
CX      WRITE( NOUT, FMT = * ) '# Most unstable mode'
CX      WRITE( NOUT, FMT = * )
CX      WRITE( NOUT, FMT = * ) 'First :',MUE1
CX      WRITE( NOUT, FMT = * ) 'Second:',MUE2
CX      WRITE( NOUT, FMT = * )
CX      WRITE( NOUT, FMT = * ) '# First resonant mode'
CX      WRITE( NOUT, FMT = * )
CX      WRITE( NOUT, FMT = * ) '      :',MRE
CX      WRITE( NOUT, FMT = * )
CX     $     '***********************************************'
*
  999 WRITE( NOUT, FMT = * )
      WRITE( NOUT, * ) '### END OF EXECUTION ###'
      WRITE( NOUT, FMT = * )
      CLOSE( IFM )
*
*
      STOP
      END
