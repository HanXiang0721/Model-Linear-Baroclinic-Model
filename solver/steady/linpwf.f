      PROGRAM LINPWF
*
*     derived from linpw.f on 2000/06/14
*     This program solves a simple linear system of
*           A X = B
*     by calling the LAPACK routine DGESV. 
*     The input left- and right-hand-side matrices are
*     read from a file. The solution is also written to a file.
*
*     .. Parameters ..
*
      include 'dim.f'
*
      INTEGER            MAXN
      INTEGER            LDA
      PARAMETER          ( MAXN=2*LMAX*(NTR+1)*(NVAR*KMAX+1) )
      REAL*8             AMIND  !! minimum diagonal component
      PARAMETER          ( AMIND = 0.0D+0 )
      REAL*8     ER             !! radius of the earth
      PARAMETER( ER   = 6370.E+3 )
*     ..
*     .. Local Scalars ..
      INTEGER            J, J1, J2, L, LL, M, MM, MSIZT, JSTR, JEND
      INTEGER            INFO, NOUT
      INTEGER            IFM, IFR, IFS
      INTEGER            LMATS
      DATA               NOUT / 6 /
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
      NAMELIST    /NMFIN/   CFM, CFR, CFS, CFG
      NAMELIST    /NMOMT/   OMITZM
      CHARACTER*70 CFM          !! input file name (matrix A)
      CHARACTER*70 CFR          !! input file name (rhs)
      CHARACTER*70 CFS          !! output file name (temporary)
      CHARACTER*70 CFG          !! output file name (grads)
      LOGICAL      OMITZM       !! omit zonal mean?
      DATA         OMITZM  / .FALSE. /
*
*     open the NAMELIST file
*
      OPEN ( 1, FILE = 'SETPAR', STATUS = 'OLD')
*
      READ( 1, NMFIN) 
      READ( 1, NMOMT) 
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
     $     'Read full matrix A '
      IF( OMITZM ) THEN
      WRITE( NOUT, FMT = * )
     $     ' @@@ Zonal mean m=0 omitted '
      ENDIF
      WRITE( NOUT, FMT = * )
      MSIZT = 0
      LDA = 0
      JEND = 0
      DO 300 M = 0, NTR
         L = NMAX - M + 1
         IF( M .EQ. 0 ) THEN
            L = L - 1
         ELSE
            L = 2 * L
         ENDIF
         MSIZT = L * ( NVAR * KMAX + 1 )
         LDA = LDA + MSIZT 
*
         DO 310 L = 1, MSIZT
            DO 320 MM = 0, NTR
               LL = NMAX - MM + 1
               IF( MM .EQ. 0 ) THEN
                  LL = LL - 1
               ELSE
                  LL = 2 * LL
               ENDIF
               LL = LL * ( NVAR * KMAX + 1 )
               JSTR = JEND + 1
               JEND = JSTR + LL - 1
               READ( IFM ) (A(J),J=JSTR,JEND)
  320       CONTINUE
  310    CONTINUE
*
*     Read matrix B
*
         J1 = LDA - MSIZT + 1
         J2 = J1 + MSIZT - 1
         READ( IFR ) (B(J),J=J1,J2)
  300 CONTINUE
*
      LMATS = LDA
      WRITE( NOUT, *) '  Matrix Size (left)=', LMATS,' x', LMATS
      WRITE( NOUT, *)
      WRITE( NOUT, *) '  Matrix Size (right)=', MSIZR,' x', LMATS
      WRITE( NOUT, *)
*     
*     solve matrix
*     
      WRITE( NOUT, FMT = * )
     $     '  LAPACK routine call: (DGESV)'
      WRITE( NOUT, FMT = * )
         
      CALL DGESV( LDA, MSIZR, A, LDA, IPIV, B, LDA, INFO )
* 
      WRITE( NOUT, FMT = * ) 
     $     '  INFO code returned by DGESV = ', INFO
      WRITE( NOUT, FMT = * )
*     
*     write down the matrix X stored in B
*
      IF( INFO .NE. 0 ) THEN
         DO 400 J = 1, LDA
            B( J ) = 0.D0
  400    CONTINUE
      ENDIF

      LDA = 0
      DO 410 M = 0, NTR
         L = NMAX - M + 1
         IF( M .EQ. 0 ) THEN
            L = L - 1
         ELSE
            L = 2 * L
         ENDIF
         MSIZT = L * ( NVAR * KMAX + 1 )
         LDA = LDA + MSIZT 
         J1 = LDA - MSIZT + 1
         J2 = J1 + MSIZT - 1
         WRITE( IFS ) (B(J),J=J1,J2)
  410 CONTINUE
*
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
*
      STOP
      END
