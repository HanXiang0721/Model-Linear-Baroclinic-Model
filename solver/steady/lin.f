      PROGRAM LIN
*
*     derived from linpw.f on 2001/09/20
*     This program solves a simple linear system of
*             L X = F
*     by calling the LAPACK routine DGESV. 
*     The input left- and right-hand-side matrices are
*     read from a file. The solution is also written to a file.
*
*     .. Parameters ..
*
      include 'dim.f'
*
      INTEGER            MAXN
*
*     standard
      PARAMETER          ( MAXN=2*NMAX*NTR*(NVAR*KMAX+1) )
CC      PARAMETER ( MAXN=10935) !! T21L11m6 moist LBM
CC      PARAMETER ( MAXN=15795) !! T21L11m10 moist LBM
*
*     reduced memory
CC      PARAMETER          ( MAXN=2*NMAX*(NVAR*KMAX+1) )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J, L, M, MTR, MVAR, MSIZT, JSTR, JEND, LEND
      INTEGER            INFO, NOUT
      INTEGER            IFM, IFR, IFS
      INTEGER            LMATS
      INTEGER            LDA
*     ..
*     .. Local Arrays ..
      REAL*8             A( MAXN*MAXN )
      REAL*8             B( MAXN )
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
      LOGICAL      OCLASSIC     !! classic dry model

      NAMELIST    /NMFIN/   CFM, CFR, CFS, CFG 
      NAMELIST    /NMALL/   OWALL
      NAMELIST    /NMCLS/   OCLASSIC

      DATA         NOUT     / 6 /
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
      OPEN( IFM, FILE=CFM, FORM='UNFORMATTED', STATUS='OLD', ERR=1999 )
      IFR = 66 
      OPEN( IFR, FILE=CFR, FORM='UNFORMATTED', STATUS='OLD' )
*
*     Read matrix L
*
      WRITE( NOUT, FMT = * )
     $     '***********************************************'
      WRITE( NOUT, FMT = * )
      WRITE( NOUT, FMT = * )
     $     'Read matrix L'
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
*     Read matrix B
*
         WRITE( NOUT, FMT = * )
     $        'Read vector F'
         WRITE( NOUT, FMT = * )
         READ( IFR ) ( B( I ), I = 1, LDA )
*     
*     solve matrix
*     
         WRITE( NOUT, FMT = * )
     $        'Solve for X = L-1 F'
         WRITE( NOUT, FMT = * )
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
            DO 310 J = 1, LDA
               B( J ) = 0.D0
  310       CONTINUE
         ENDIF
         WRITE( IFS ) (B(J),J=1,LDA)
*
  300 CONTINUE
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
      CLOSE( IFR )
      CLOSE( IFS )
*
*
      STOP
      END
