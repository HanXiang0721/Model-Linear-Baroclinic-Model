      PROGRAM BSGRD
*
*     transform Gtool BS file to GrADS file
*
*     should be run on DEC alpha
*
      include 'dim.f'
*
      REAL*4      DAT0( IMAX, JMAX, KMAX )
      REAL*4      DAT1( IMAX, JMAX )
      REAL*4      DAT( IMAX, JMAX )
      INTEGER     I, J, K
      CHARACTER   HEAD( 64 )* 16
      CHARACTER*90 CBS0         !! basic state (Gtool)
      CHARACTER*90 CBS          !! basic state (GrADS)
*
      NAMELIST    /NMBS/    CBS0, CBS
*
*     open the NAMELIST file
*
      REWIND( 1 )
      OPEN ( 0, FILE='IEEE_ERROR')
      OPEN ( 1, FILE = 'SETPAR', STATUS = 'OLD')
*
      READ( 1, NMBS) 
*
*     open output file
*
      OPEN( 10, FILE = CBS, FORM = 'UNFORMATTED', STATUS='UNKNOWN' )
*
*     read GT3 data
*
      OPEN( 30, FILE = CBS0, FORM = 'UNFORMATTED', STATUS='OLD' )
      WRITE(6,*) 'INPUT:',CBS0
               
      READ( 30 ) HEAD
      READ( 30 ) DAT0           !! u
      DO 10 K = 1, KMAX
         DO 20 J = 1, JMAX
            DO 30 I = 1, IMAX
               DAT( I, J) = DAT0( I, J, K)
 30         CONTINUE
 20      CONTINUE
         WRITE( 10 ) DAT
 10   CONTINUE
      WRITE( 6, *) '--- U ---'
*
      READ( 30 ) HEAD
      READ( 30 ) DAT0           !! v
      DO 40 K = 1, KMAX
         DO 50 J = 1, JMAX
            DO 60 I = 1, IMAX
               DAT( I, J) = DAT0( I, J, K)
 60         CONTINUE
 50      CONTINUE
         WRITE( 10 ) DAT
 40   CONTINUE
      WRITE( 6, *) '--- V ---'
*
      READ( 30 ) HEAD
      READ( 30 ) DAT0           !! t
      DO 70 K = 1, KMAX
         DO 80 J = 1, JMAX
            DO 90 I = 1, IMAX
               DAT( I, J) = DAT0( I, J, K)
 90         CONTINUE
 80      CONTINUE
         WRITE( 10 ) DAT
 70   CONTINUE
      WRITE( 6, *) '--- T ---'
*
      READ( 30 ) HEAD
      READ( 30 ) DAT1           !! Ps
      DO 100 J = 1, JMAX
         DO 110 I = 1, IMAX
            DAT( I, J) = DAT1( I, J)
 110     CONTINUE
 100  CONTINUE
      WRITE( 10 ) DAT
      WRITE( 6, *) '--- Ps --'
*
      READ( 30 ) HEAD
      READ( 30 ) DAT0           !! q
      DO 120 K = 1, KMAX
         DO 130 J = 1, JMAX
            DO 140 I = 1, IMAX
               DAT( I, J) = DAT0( I, J, K)
 140        CONTINUE
 130     CONTINUE
         WRITE( 10 ) DAT
 120  CONTINUE
      WRITE( 6, *) '--- Q ---'
*
      CLOSE( 30 )
      CLOSE( 10 )

      STOP
      END
