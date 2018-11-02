      PROGRAM EOFBR
*
*     derived from expert/eofpw.f on 2001/10/3
*     This program solves a non-symmetric eigenproblem
*           L = E S E^t
*     by calling the LAPACK routine DGEEV. 
*     The input left-hand-side matrix L is
*     read from a file. The solution is also written to a file.
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
      REAL*8             X0, X1, X2
      INTEGER            LDA, MTR
      INTEGER            M, I, J, L
      INTEGER            MUE1, MUE2, MRE
      INTEGER            INFO, NOUT
      INTEGER            IFM, IFE, IFS
      INTEGER            LMATS
      INTEGER            JSTR, JEND
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
      EXTERNAL           DGESVD
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
      INTEGER      NWAVE        !! selected zonal wavenumber
      LOGICAL      OWALL        !! write all the wavenumber

      NAMELIST    /NMEFIN/   CFM, CFE, CFS, CFG
      NAMELIST    /NMWAVE/   NWAVE 
      NAMELIST    /NMALL/    OWALL

      DATA         NOUT / 6 /
      DATA         JOBVL /'N'/
      DATA         JOBVR /'V'/
      DATA         NWAVE / 0       /
      DATA         OWALL / .FALSE. / !! PWM
*
*     open the NAMELIST file
*
      REWIND( 1 )
      OPEN ( 0, FILE='IEEE_ERROR')
      OPEN ( 1, FILE = 'SETPAR', STATUS = 'OLD')
*
      READ( 1, NMEFIN) 
      READ( 1, NMWAVE) 
      READ( 1, NMALL) 
      IF( OWALL ) NWAVE = -1
*
*     output file
*
      IFE = 55
      OPEN( IFE, FILE = CFE, FORM = 'UNFORMATTED', STATUS = 'UNKNOWN' )
*
      IFS = 66
      OPEN( IFS, FILE = CFS, FORM = 'UNFORMATTED', STATUS = 'UNKNOWN' )
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
     $              '  Matrix Size (left)=', LMATS,' x', LMATS
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
      DO 500 J = 1, LDA
         JSTR = (J-1)*LDA + 1
         JEND = JSTR + LDA - 1
         WRITE( IFE ) SNGL(-WR(J)), SNGL(WI(J))
         WRITE( IFS ) (E(I),I=JSTR,JEND)  !! i: grid, j: mode
 500  CONTINUE
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
      X1 = -WR( 1 )
      X2 = -WR( 1 )
      X0 = 999.D0
      DO 400 J = 1, LDA
         IF( WI( J ) .GE. 0.D0 .AND. -WR( J ) .GT. X1 ) THEN
            X2 = X1
            MUE2 = MUE1
            X1 = -WR( J )
            MUE1 = J
         ENDIF
         IF( WI( J ) .GE. 0.D0 ) THEN
            IF( -WR( J ) .LE. 0.D0 .AND. -WR( J ) .LT. X0 ) THEN
               MRE = J
               X0 = -WR( J )
            ENDIF
         ENDIF
 400  CONTINUE
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
      WRITE( NOUT, * ) '### END OF EXECUTION ###'
      WRITE( NOUT, FMT = * )
*
*
      STOP
      END
