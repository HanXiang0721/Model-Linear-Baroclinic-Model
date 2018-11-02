      PROGRAM OFRCSST
*
*     derived from ofrc.f on 2002/04/18
*     This routine makes forcing vectors for the
*     3-D linear response from a GrADS file.
*     Each forcing vector contains only the 
*     values at one point.
*
*     variables: vor, div, T, LnPs, q
*
      include 'dim.f'
*
      REAL*4       GFRC( IMAX, JMAX )
      REAL*4       GIDX( IMAX, JMAX ) !! surface grid index
      REAL*4       GRID( IMAX, JMAX ) !! flag for forcing
*
*     [work]
      REAL*8       BLON( IDIM, JMAX )
      REAL*8       BLAT( IDIM, JMAX )
      REAL*8       DLON( IDIM, JMAX ) !! unused
      REAL*8       PI
      INTEGER      IFR, IFS
      INTEGER      NF0
      INTEGER      I, J
      CHARACTER    HEAD( 64 )* 16
      CHARACTER    HALON *(16)        !! unused
      INTEGER      NOUT
*
*     [intrinsic]
      INTRINSIC    SNGL
*
      CHARACTER*90 CFM          !! file name (dumy)
      CHARACTER*90 CFR          !! output file name (grid)
      CHARACTER*90 CFS          !! input file name (grid index, gtool)
      CHARACTER*90 CFG          !! file name (dumy)
      INTEGER      NFTYPE        !! no. of forcing
      LOGICAL      OWALL        !! write all the wavenumber
      REAL*8       FACT( NVAR+1 ) !! factor 
      REAL*8       XLONW        !! western longitude
      REAL*8       XLONE        !! eastern longitude
      REAL*8       YLATS        !! southern latitude
      REAL*8       YLATN        !! northern latitude
*
      NAMELIST    /NMFIN/   CFM, CFR, CFS, CFG
      NAMELIST    /NMFRC/   FACT, XLONW, XLONE, YLATS, YLATN
      NAMELIST    /NMFNO/  NFTYPE
      NAMELIST    /NMALL/   OWALL
*
      DATA         NOUT / 6 /
      DATA         NFTYPE   / 1            /
      DATA         OWALL    / .TRUE. / !! PWM
*
*     open the NAMELIST file
*
      REWIND( 1 )
      OPEN ( 0, FILE='IEEE_ERROR')
      OPEN ( 1, FILE = 'SETPAR', STATUS = 'OLD')
*
      READ( 1, NMFIN) 
      REWIND( 1 )
      READ( 1, NMFRC) 
      REWIND( 1 )
      READ( 1, NMFNO) 
      REWIND( 1 )
      READ( 1, NMALL) 
*
*
      WRITE( NOUT, * ) '### MAKE SST FORCING on GRID ###'
      WRITE( NOUT, * )
*
*     grid index
*
      IFS = 10
      OPEN ( IFS, FILE = CFS, FORM = 'UNFORMATTED', STATUS = 'UNKNOWN' )
      READ( IFS ) HEAD
      READ( IFS ) GIDX
      CLOSE( IFS )
*
      PI = ATAN( 1.D0 ) * 4.D0
      CALL SETLON
     O         ( BLON  , DLON , HALON  )
      CALL SETLAT
     O         ( BLAT  , DLON , HALON  )
      DO 100 J = 1, JMAX
         DO 100 I = 1, IDIM
            BLON( I, J) = BLON( I, J) * 360.D0 / ( 2.D0 * PI )
            BLAT( I, J) = BLAT( I, J) * 360.D0 / ( 2.D0 * PI )
            IF( I .EQ. IDIM ) BLAT( I, J) = -999.D0
 100  CONTINUE
*
*     seek # of forcing
*
      NF0 = 0
      CALL SETZ4( GRID, IMAX*JMAX )
      DO 110 J = 1, JMAX
         DO 120 I = 1, IMAX
            IF( GIDX( I,J ) .NE. 0. ) GOTO 120 !! only over oceans
            IF( BLAT( I, J) .GE. YLATS .AND. 
     $           BLAT( I, J) .LE. YLATN ) THEN 
               IF( XLONW .LT. XLONE ) THEN
                  IF( BLON( I, J) .GE. XLONW .AND. 
     $                 BLON( I, J) .LE. XLONE ) THEN 
                     GRID( I,J ) = 1.0
                     NF0 = NF0 + 1
                  ENDIF
               ELSE             !! straddling 0 deg.
                  IF( BLON( I, J) .GE. XLONW .OR. 
     $                 BLON( I, J) .LE. XLONE ) THEN 
                     GRID( I,J ) = 1.0
                     NF0 = NF0 + 1
                  ENDIF
               ENDIF
            ENDIF
 120     CONTINUE
 110  CONTINUE
*
      WRITE( NOUT, * ) 
      WRITE( NOUT, * ) 'Total # forcing:', NF0
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 
      IF( NFTYPE .GT. NF0 ) THEN
         WRITE( NOUT, * ) '### Specified # invalid ###'
         STOP
      ELSE
         WRITE( NOUT, * ) 'Assigned # forcing:', NFTYPE
         WRITE( NOUT, * ) '................'
      ENDIF
*
*     open output file
*
      IFR = 20
      OPEN ( IFR, FILE = CFR, FORM = 'UNFORMATTED', STATUS = 'UNKNOWN' )
      WRITE( NOUT, * ) 'Forcing file (output):', CFR
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 
*
*     read GrADS data
*
      WRITE( NOUT, * ) 'Make one-point forcing file'
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 
*
      NF0 = 0
      CALL SETZ4( GFRC, IMAX*JMAX )
      DO 220 J = 1, JMAX
         DO 230 I = 1, IMAX
            IF( GRID( I,J ) .EQ. 1.0 ) THEN
               NF0 = NF0 + 1
               IF( NF0 .EQ. NFTYPE ) THEN
                  GFRC( I,J ) = SNGL( FACT(1)  )
                  GOTO 250
               ENDIF
            ENDIF
 230     CONTINUE
 220  CONTINUE            

 250  WRITE( IFR )  GFRC
      CALL SETZ4( GFRC, IMAX*JMAX )
      WRITE( IFR )  GFRC        !! dummy for Wg
      CLOSE( IFR )
      IF( OWALL ) THEN
         WRITE( NOUT, * ) 'Written to matrix file (all)'
      ELSE
         WRITE( NOUT, * ) 'Written to matrix file (pwm)'
      ENDIF
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 
*
      WRITE( NOUT, * )
      WRITE( NOUT, * ) '### END OF EXECUTION ###'
      WRITE( NOUT, * )
*
*
  999 STOP
      END
*######################
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
