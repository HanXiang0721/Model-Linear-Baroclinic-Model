      PROGRAM LINBR
*
*     derived from linpw.f on 2001/06/19
*     This program solves a linear system for barotropic 
*     vorticity equation. 
*     The input left- and right-hand-side matrices are
*     read from a file. The solution is also written to a file.
*
*     .. Parameters ..
*
      include 'dim.f'
*
      INTEGER            MAXN
      PARAMETER          ( MAXN=MSIZ )
      REAL*8             AMIND  !! minimum diagonal component
      PARAMETER          ( AMIND = 0.D+0 )
*     ..
*     .. Local Scalars ..
      INTEGER            LDA, MTR
      INTEGER            I, J, M, L, JSTR, JEND
      INTEGER            INFO, NOUT
      INTEGER            IFM, IFR, IFS
      INTEGER            LMATS, NRSYM
*     ..
*     .. Local Arrays ..
      REAL*8             A( MAXN*MAXN)
      REAL*8             B( MAXN*MSIZR)
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
      CHARACTER*90 CFM          !! input file name (matrix A)
      CHARACTER*90 CFR          !! input file name (rhs)
      CHARACTER*90 CFS          !! output file name (temporary)
      CHARACTER*90 CFG          !! output file name (grads)
      LOGICAL      OWALL        !! write all the wavenumber
      LOGICAL      OSYM         !! equatorial symmetry
*
      NAMELIST    /NMFIN/   CFM, CFR, CFS, CFG
      NAMELIST    /NMALL/   OWALL
      NAMELIST    /NMSYM/   OSYM
*
      DATA         NOUT / 6 /
      DATA         OWALL / .FALSE. / !! PWM
      DATA         OSYM  / .FALSE. / !! not symmetric
*
*     open the NAMELIST file
*
      REWIND( 1 )
      OPEN ( 0, FILE='IEEE_ERROR')
      OPEN ( 1, FILE = 'SETPAR', STATUS = 'OLD')
*
      READ( 1, NMFIN ) 
      REWIND( 1 )
      READ( 1, NMALL ) 
      REWIND( 1 )
      READ( 1, NMSYM) 
*
*     output file
*
      IFS = 88
      open( IFS, FILE = CFS, FORM = 'UNFORMATTED',
     $     STATUS = 'UNKNOWN' )
*
*
      WRITE( NOUT, FMT = * )
      WRITE( NOUT, * ) '### SOLVE LINEAR SYSTEM ###'
      WRITE( NOUT, FMT = * )
*
*     Open files
*
      IFM = 77
      OPEN( IFM, FILE=CFM, FORM='UNFORMATTED', STATUS='OLD' )
      IFR = 66 
      OPEN( IFR, FILE=CFR, FORM='UNFORMATTED', STATUS='OLD' )
*
*     Read matrix A
*
      WRITE( NOUT, FMT = * )
     $     '***********************************************'
      WRITE( NOUT, FMT = * )
      WRITE( NOUT, FMT = * )
     $     'Read matrix A'
      WRITE( NOUT, FMT = * )
*
      NRSYM = 0
      IF( OSYM ) THEN
         DO 10 M = 0, NTR
            L = NMAX - M + 1
            NRSYM = NRSYM + L / 2
 10      CONTINUE
         NRSYM = NRSYM * 2 - ( NMAX + 1 ) / 2
      ENDIF
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
            LDA = MSIZ2 - NRSYM
         ELSE
            LDA = L
            WRITE( NOUT, FMT = * )
     $           'Read matrix A for zonal WN = ',M
         ENDIF
         WRITE( NOUT, FMT = * )
         LMATS = LDA
         WRITE( NOUT, *) '  Matrix Size (left)=', LMATS,' x', LMATS
         WRITE( NOUT, *) 
*
         DO 200 I = 1, LDA 
            JSTR = (I-1)*LDA + 1
            JEND = JSTR + LDA - 1
            READ( IFM ) (A(J),J=JSTR,JEND)
  200    CONTINUE
*     
*     Read matrix B
*
         READ( IFR ) ( B( I ), I = 1, LDA )
*     
*     solve matrix
*     
         WRITE( NOUT, FMT = * )
     $        '  LAPACK routine call: (DGESV)'
         WRITE( NOUT, FMT = * )
         
         CALL DGESV( LDA, MSIZR, A, LDA, IPIV, B, LDA, INFO )
* 
         WRITE( NOUT, FMT = * ) 
     $        '  INFO code returned by DGESV = ', INFO
         WRITE( NOUT, FMT = * )
*     
*     write down the matrix X stored in B
*
         IF( INFO .NE. 0 ) THEN
            DO 300 J = 1, LDA
               B( J ) = 0.D0
  300       CONTINUE
         ENDIF
         WRITE( IFS ) (B(J),J=1,LDA)
  100 CONTINUE
      WRITE( NOUT, FMT = * )
     $     '***********************************************'
*
      WRITE( NOUT, FMT = * )
      WRITE( NOUT, * ) '### END OF EXECUTION ###'
      WRITE( NOUT, FMT = * )
      
      CLOSE( IFM )
      CLOSE( IFR )
      CLOSE( IFS )
*
      STOP
      END
