      PROGRAM CHKVL
*
*     derived from outstr.f on 1999/05/06
*     This routine checks values from a storm track model
*     and omits values which amp. larger than 3SD.
*
*     variables: PSI, VOR, U, V, T, Z, Ps
*
      include 'dim.f'
*
      REAL*4       XMISS, THRE
      PARAMETER (  XMISS = -999., THRE = 3. )
      INTEGER      MVAR, MAXDY
      PARAMETER (  MVAR = 6*KMAX + 1 ) 
      PARAMETER (  MAXDY = 4000 )
*
      REAL*4       DAT1( IMAX*JMAX )
      REAL*4       DAT2( IMAX*JMAX, MAXDY )
*
*     [work]
      REAL*4       D0( MAXDY )      
      REAL*4       VAR
      REAL*4       SD( IMAX*JMAX, MVAR )
      INTEGER      IFO, IFOR
      INTEGER      IJ, K, IREC, NREC, IDAY, NDAY, NNDAY, IVAR,
     $     KVAR 
      INTEGER      NOUT
*
*     namelist
*
      INTEGER      NFCS         !! no. of integration
      INTEGER      NFDAY        !! first day for use
      INTEGER      NLDAY        !! length of integration (day)
      REAL*4       TINT         !! output interval
      CHARACTER*10 TUNIT        !! time unit for output
      CHARACTER*90 CFO          !! input file name (grads)
      CHARACTER*90 CFOC         !! output file name (grads)

      NAMELIST    /NMREC/   NFCS, NFDAY, NLDAY, TINT, TUNIT
      NAMELIST    /NMFIL/   CFO, CFOC

      DATA         NOUT / 6 /
*
*     open the NAMELIST file
*
      REWIND( 1 )
      OPEN ( 0, FILE='IEEE_ERROR')
      OPEN ( 1, FILE = 'SETPAR', STATUS = 'OLD')
*
      REWIND( 1 )
      READ( 1, NMREC) 
      REWIND( 1 )
      READ( 1, NMFIL) 
      CLOSE( 1 )
*
      WRITE( NOUT, * ) 'CHKVL: check value within 3SD'
*
*     read GrADS data
*
      IFO  = 80
      IFOR = 81
      OPEN ( IFO, FILE = CFO, FORM = 'UNFORMATTED', 
     $     STATUS = 'OLD' )
      OPEN ( IFOR, FILE = CFOC, FORM = 'UNFORMATTED', 
     $     STATUS = 'UNKNOWN' )
*
      IF( TUNIT(1:3) .EQ. 'DAY' ) THEN
         NNDAY = 1
      ELSEIF( TUNIT(1:4) .EQ. 'HOUR' ) THEN
         NNDAY = 24 / IFIX( TINT )
      ELSE
         WRITE( 6, *) ' 24 / TINT not integer'
         CALL XABORT( 1 )
      ENDIF
*
      NREC = NNDAY * ( NLDAY-NFDAY+1 ) * NFCS * MVAR
*
      DO 500 KVAR = 1, MVAR
         WRITE( NOUT, * ) '   variable ',KVAR,' (<=',MVAR,')'
*
         IREC = 0
         IVAR = 0
         IDAY = 1
   10    IREC = IREC + 1
         IF( IREC .GT. NREC ) GOTO 99
         READ( IFO ) DAT1
*     
         IVAR = IVAR + 1
         IF( IVAR .EQ. KVAR ) THEN
            DO 20 IJ = 1, IMAX*JMAX
               DAT2( IJ, IDAY ) = DAT1( IJ )
   20       CONTINUE
         ENDIF

         IF( IVAR .EQ. MVAR ) THEN
            IVAR = 0
            IDAY = IDAY + 1
         ENDIF

         GOTO 10
*     
   99    NDAY = IDAY - 1
*
         DO 40 IJ = 1, IMAX*JMAX
            DO 50 K = 1, NDAY
               D0( K ) = DAT2( IJ, K )
   50       CONTINUE
            CALL VARIANCE( D0, NDAY, XMISS, VAR)
            SD( IJ, KVAR ) = THRE * SQRT( VAR )
   40    CONTINUE
*
  500 CONTINUE

      REWIND( IFO )
      IREC = 0
      IDAY = 1
      IVAR = 0
   60 IREC = IREC + 1
      IF( IREC .GT. NREC ) GOTO 88
      READ( IFO ) DAT1
*     
      IVAR = IVAR + 1
      DO 70 IJ = 1, IMAX*JMAX
         IF( ABS( DAT1( IJ ) ) .GT. SD( IJ, IVAR ) )
     $        DAT1( IJ ) = XMISS
   70 CONTINUE

      WRITE( IFOR ) DAT1

      IF( IVAR .EQ. MVAR ) IVAR = 0

      GOTO 60
*     
   88 CLOSE( IFO )
      CLOSE( IFOR )
*
*      
      STOP
      END
*##############################################
      SUBROUTINE VARIANCE
     I     ( X, INT, XMISS,
     O     VAR)
*
      INTEGER INT
      REAL*4 X(INT)
      REAL*4 VAR
*
*     [work]
      INTEGER I, N
      REAL*4 XMISS
      REAL*4 XM
      REAL*4 SX
*
      N = 0
      XM = 0.
      SX = 0.
      VAR = 0.
      DO 100 I=1,INT
         IF(X(I).NE.XMISS) THEN   
            N = N+1                                
            XM = XM + X(I)
         ENDIF 
  100 CONTINUE        
*
      XM=XM/FLOAT(N)
*
      DO 200 I=1,INT
         IF(X(I).NE.XMISS) THEN
            SX = SX + (X(I)-XM)**2
         ENDIF
  200 CONTINUE
*
      VAR = SX/FLOAT(N)
*
      RETURN
      END 
