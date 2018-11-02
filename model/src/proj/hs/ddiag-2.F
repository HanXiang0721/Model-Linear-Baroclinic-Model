* PACKAGE DDIAG  !! dynamics: diagnosis
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         95/05/28(numaguti) restructure
*         01/07/05(hiro)     omega
*         05/05/24(hiro)     for initializing BS
***********************************************************************
      SUBROUTINE DDIAG     !!  diagnostic output
     O          (  GDZ   ,
     I             GDT   , GDQ   , GDZS   )
*
#include        "zcdim.F"                /* # of grid point & wave  */
#include        "zpdim.F"                /* # of physics grid point */
#include        "zccom.F"                /* stand. physical const.  */
#include        "zcord.F"                /* coordinate              */
*
*   [OUTPUT]
      REAL*8     GDZ   ( IDIM*JDIM, KMAX   ) !! geopotential height
*
*   [INPUT]
      REAL*8     GDT   ( IJDIM, KMAX )      !! temperature T
      REAL*8     GDQ   ( IJDIM, KMAX )      !! humidity q
      REAL*8     GDZS  ( IJDIM  )           !! surface topography
*
*   [WORK] 
      REAL*8     GDTV  ( IJDIM, KMAX )      !! virtual temperature
*
*   [INTERNAL WORK] 
      INTEGER    IFPAR, JFPAR
*
*   [INTERNAL PARM] 
      REAL*8     TLAPS                       !! lapse rate: underground
      LOGICAL    OFIRST
      NAMELIST  /NMSLP/ TLAPS
      DATA       TLAPS  /  0.005 /
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DDIAG: DYNAMICS DIAGNOSTICS 96/05/28'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMSLP, END=190 )
  190    WRITE  ( JFPAR, NMSLP )
      ENDIF
*
      CALL VIRTMD
     O         ( GDTV  ,
     I           GDT   , GDQ     )
      CALL GHYDRO               !! hydrostatic eq.
     O         ( GDZ   ,
     I           GDTV  , GDZS  )
*
      RETURN
      END
***********************************************************************
      SUBROUTINE RMSE     !!  root-mean-square error
     I          (  GDX   , GDY   ,  KDIMD  ,
     O             OCONV                     )
*
#include        "zcdim.F"                /* # of grid point & wave  */
#include        "zpdim.F"                /* # of physics grid point */
#include        "zccom.F"                /* stand. physical const.  */
#include        "zcord.F"                /* coordinate              */
*
*   [OUTPUT]
      LOGICAL    OCONV
*
*   [INPUT]
      INTEGER    KDIMD
      REAL*8     GDX   ( IJDIM, KMAX )      !! 
      REAL*8     GDY   ( IJDIM, KMAX )      !! 
*
*   [WORK] 
      REAL*8     GDTV  ( IJDIM, KMAX )      !! virtual temperature
*
*   [INTERNAL WORK] 
      INTEGER    IFPAR, JFPAR
      INTEGER    IJ, K
      REAL*8     RMS            !! global average RMS error
*
*   [INTERNAL PARM] 
      REAL*8     EPS            !! convergence threshold of error
      LOGICAL    OFIRST
      NAMELIST  /NMEPS/ EPS
      DATA       EPS  /  1.d3  /
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ RMSE: ESTIMATE ERROR 05/05/25 '
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMEPS, END=190 )
  190    WRITE  ( JFPAR, NMEPS )
      ENDIF
*
      DO 1000 K = 1, KDIMD
         RMS = 0.D0
         DO 1100 IJ = 1, IJDIM
            RMS = RMS + ( GDX( IJ,K ) - GDY( IJ,K ) )**2
 1100    CONTINUE
 1000 CONTINUE
      RMS = RMS / DBLE( IJDIM*KMAX )
      write( 6,* ) ' $$ RMS=',RMS
      IF( RMS .LT. EPS ) OCONV  = .TRUE.
*
      RETURN
      END
**********************************************************************
      SUBROUTINE WMODBS_OLD          !! write modified state
     I         ( GDU   , GDV   , GDT   , GDPS  , GDQ    )
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc.           */
#include        "zhdim.F"                /* # of char. in string   */
*
*   [INPUT]
      REAL*8     GDU ( IJDIM, KMAX )        !! zonal wind
      REAL*8     GDV ( IJDIM, KMAX )        !! meridional wind
      REAL*8     GDT ( IJDIM, KMAX )        !! temperature
      REAL*8     GDPS( IJDIM       )        !! surface pressure
      REAL*8     GDQ ( IJDIM, KMAX )        !! specific humidity
*
*   [WORK]
      REAL*4     X    ( IMAX, JMAX  )        !! temporary for forcing data
      INTEGER    I, J, K, IJ
      INTEGER    IFPAR, JFPAR
*
*   [INTERNAL PARM] 
      CHARACTER  FMODBS*90
      LOGICAL    OFIRST
      NAMELIST  /NMMODBS/ FMODBS
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ WMODBS: WRITE MODIFIED STATE 05/05/24'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMMODBS, END=190 )
  190    WRITE  ( JFPAR, NMMODBS )
      ENDIF
*
*
*
      IF( FMODBS(1:1) .NE. ' ' ) THEN
         OPEN( 87, FILE = FMODBS, FORM='UNFORMATTED',
     &        STATUS='UNKNOWN' ) 
      ELSE
         WRITE( 6, *) '    ### MODIFIED STATE NOT DAMPED ### '
         WRITE( 6, *) 
         RETURN
      ENDIF
*
      DO 300 K = 1, KMAX
         IJ = 0
         DO 310 J = 1, JMAX
            DO 320 I = 1, IMAX
               IJ = IJ + 1
               X( I, J) = SNGL( GDU( IJ,K ) )
 320        CONTINUE
            IJ = IJ + 1
 310     CONTINUE
         WRITE( 87 ) X           !! temperature
 300  CONTINUE
*     
      DO 330 K = 1, KMAX
         IJ = 0
         DO 340 J = 1, JMAX
            DO 350 I = 1, IMAX
               IJ = IJ + 1
               X( I, J) = SNGL( GDV( IJ,K ) )
 350        CONTINUE
            IJ = IJ + 1
 340     CONTINUE
         WRITE( 87 ) X           !! meridional wind
 330  CONTINUE
*     
      DO 360 K = 1, KMAX
         IJ = 0
         DO 370 J = 1, JMAX
            DO 380 I = 1, IMAX
               IJ = IJ + 1
               X( I, J) = SNGL( GDT( IJ,K ) )
 380        CONTINUE
            IJ = IJ + 1
 370     CONTINUE
         WRITE( 87 ) X           !! temperature
 360  CONTINUE
*     
      IJ = 0
      DO 390 J = 1, JMAX
         DO 400 I = 1, IMAX
            IJ = IJ + 1
            X( I, J) = SNGL( GDPS( IJ ) )
 400     CONTINUE
         IJ = IJ + 1
 390  CONTINUE
      WRITE( 87 ) X             !! surface pressure
*
      DO 410 K = 1, KMAX
         IJ = 0
         DO 420 J = 1, JMAX
            DO 430 I = 1, IMAX
               IJ = IJ + 1
               X( I, J) = SNGL( GDQ( IJ,K ) )
 430        CONTINUE
            IJ = IJ + 1
 420     CONTINUE
         WRITE( 87 ) X           !! humidity
 410  CONTINUE
*     
      RETURN
      END
