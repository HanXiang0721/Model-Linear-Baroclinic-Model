      PROGRAM MKFRCSST
*
*     derived from mkfrcng.f on 2001/10/16
*     This routine makes simplified SST forcing function
*     for the moist LBM.
*
*     Alternative selection of the forcing shape is:
*      Horizontal: Elliptic function/zonally uniform function 
*
      include 'dim.f'
*
*
      REAL*8       GFRCT( IDIM*JMAX )
*
      REAL*8       ALON( IDIM, JMAX )
      REAL*8       ALAT( IDIM, JMAX )
      REAL*8       DLON( IDIM, JMAX ) !! unused
      CHARACTER    HALON *(16)        !! unused
*
*     [work]
      REAL*4       GRID( IMAX, JMAX)
      REAL*8       AXY( IDIM, JMAX)
      REAL*8       DUM( IMAX, JMAX)
      REAL*8       PI
      REAL*8       V1, V2, V
      INTEGER      IFM, IFG
      INTEGER      I, J, IJ
      INTEGER      NOUT
      CHARACTER    HEAD( 64 )*16
*
*     [intrinsic]
      INTRINSIC    DSIN, DSQRT, DBLE, SNGL
*
      CHARACTER    CHPR( MAXH )*15
      CHARACTER*90 CFM          !! grid index file  (Gtool)
      CHARACTER*90 CFG          !! output file name (grads)
      INTEGER      KHPR         !! type of horizontal profile
      REAL*8       HAMP         !! horizontal amplitude
      REAL*8       XDIL         !! x-dilation for Elliptic-shape
      REAL*8       YDIL         !! y-dilation for Elliptic-shape
      REAL*8       XCNT         !! x-center for Elliptic-shape
      REAL*8       YCNT         !! y-center for Elliptic-shape
      REAL*8       FACT( 5 )    !! factor (unused)

      NAMELIST    /NMFIN/   CFM, CFG, FACT
      NAMELIST    /NMHPR/   KHPR, HAMP, XDIL, YDIL, XCNT, YCNT
*
      DATA         NOUT / 6 /
      DATA         CHPR
     $           /'Elliptic       ','Zonally uniform'/
*                  123456789012345   123456789012345   
      DATA    FACT
     &       / 1.,1.,1.,1.,1. /
*
*     open the NAMELIST file
*
      REWIND( 1 )
      OPEN ( 0, FILE='IEEE_ERROR')
      OPEN ( 1, FILE = 'SETPAR', STATUS = 'OLD')
*
      READ( 1, NMFIN) 
      REWIND( 1 )
      READ( 1, NMHPR) 
*
*
      WRITE( NOUT, * )
      WRITE( NOUT, * ) '### MAKE SST FORCING ###'
*
      WRITE( NOUT, * )
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 'Selected shape:'
      WRITE( NOUT, * ) '  Horizontal:',CHPR( KHPR )
      WRITE( NOUT, * ) '     amplitude:',HAMP,' [K]'
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * )
*
*     open files
*
      IFM = 10
      IFG = 20
      OPEN ( IFM, FILE = CFM, FORM = 'UNFORMATTED', STATUS = 'UNKNOWN' ) 
      OPEN ( IFG, FILE = CFG, FORM = 'UNFORMATTED', STATUS = 'UNKNOWN' )
      WRITE( NOUT, * ) 'Forcing file (GrADS) :', CFG
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 
*
*     grid index
*
      READ( IFM ) HEAD
      READ( IFM ) GRID
      CLOSE( IFM )
*
      PI = ATAN( 1.0D0 ) * 4.0D0
      CALL SETLON
     O         ( ALON  , DLON , HALON  )
      CALL SETLAT
     O         ( ALAT  , DLON , HALON  )
*
      DO 1 J = 1, JMAX
         DO 1 I = 1, IDIM
            ALON( I, J) = ALON( I, J) * 360.D0 / ( 2.D0 * PI )
            ALAT( I, J) = ALAT( I, J) * 360.D0 / ( 2.D0 * PI )
 1    CONTINUE
*
      CALL SETZ( GFRCT, IDIM*JMAX )
      CALL SETZ( DUM, IMAX*JMAX )
*
*     horizontal profile
*
      IJ = 0
      DO 20 J = 1, JMAX
         DO 30 I = 1, IDIM
            IJ = IJ + 1
*
            IF( KHPR .EQ. 1 ) THEN !! Elliptic
               IF( XDIL .LE. 0.D0 .OR. YDIL .LE. 0.D0 ) THEN
                  WRITE( NOUT, *) 
     $                 '### Parameter XDIL/YDIL not correct ###' 
                  STOP
               ENDIF
*
               V1 = ( ALON( I, J ) - XCNT )**2 / XDIL**2
               IF( ALON(I,J).LT.180. .AND. XCNT+XDIL.GE.360. )
     $              V1 = ( 360. + ALON( I, J ) - XCNT )**2 / XDIL**2
               IF( ALON(I,J).GT.180. .AND. XCNT-XDIL.LE.0. )
     $              V1 = ( ALON( I, J ) - 360. - XCNT )**2 / XDIL**2
               V2 = ( ALAT( I, J ) - YCNT )**2 / YDIL**2
               V = V1 + V2
               IF( V .GT. 1. ) THEN
                  AXY( I, J) = 0.D0
               ELSE
                  V = DSQRT( V ) 
                  AXY( I, J) = HAMP * ( 1.D0 - V )
               ENDIF
            ENDIF
               
            IF( KHPR .EQ. 2 ) THEN !! zonal uniform
*
               IF( ALAT( I, J) .GE. YCNT - YDIL .AND.
     $             ALAT( I, J) .LE. YCNT + YDIL ) THEN
                  V = HAMP
               ELSE
                  V = 0.D0
               ENDIF

               AXY( I, J) = V
            ENDIF

            GFRCT( IJ ) = AXY( I, J) 
*
 30      CONTINUE
 20   CONTINUE
      WRITE( NOUT, * ) 
      WRITE( NOUT, * ) 'Set horizontal shape'
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 
*
*     write GrADS file (SST)
*
      DO 110 J = 1, JMAX
         DO 110 I = 1, IMAX
            IJ = (J-1)*IDIM + I
            DUM( I, J) = GFRCT( IJ )
            IF( GRID( I, J) .GT. 0. ) THEN
               DUM( I, J) = 0.D0
            ENDIF
 110  CONTINUE
      WRITE( IFG ) ((SNGL(DUM(I,J)),I=1,IMAX),J=1,JMAX)
*
*     write GrADS file (Wg) !! dummy at present 
*
      CALL SETZ( DUM, IMAX*JMAX )
      WRITE( IFG ) ((SNGL(DUM(I,J)),I=1,IMAX),J=1,JMAX)
*
      CLOSE( IFG )
      WRITE( NOUT, * ) 'Written to GrADS file'
      WRITE( NOUT, * ) '................'
*
      WRITE( NOUT, * )
      WRITE( NOUT, * ) '### END OF EXECUTION ###'
      WRITE( NOUT, * )
*
*
  999 STOP
      END
*######################
      SUBROUTINE SETZ( A, IA )
*
*
      INTEGER IA
      REAL*8  A( IA )

      REAL*8  ZERO
      DATA    ZERO / 0.D0 /
      INTEGER I
*
      DO 10 I = 1, IA
         A( I ) = ZERO
 10   CONTINUE

      RETURN
      END
