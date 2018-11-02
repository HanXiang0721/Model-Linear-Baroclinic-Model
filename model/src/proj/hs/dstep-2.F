* PACKAGE DSTEP !! dynamics: integration of eqs.
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/05/28(numaguti) restructure
*         05/05/24(hiro)     for initializing BS
**********************************************************************
      SUBROUTINE DYNSTP    !! time step of dynamics
     O         ( OCONV ,
     M           GAT   , GAPS  ,
     I           WTDIV , 
     I           FLAPLA, WPHIS , GG       )
*     
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
#include        "zpdim.F"                /* physics etc. */
#include        "zccom.F"                /* physics etc. */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT] 
      LOGICAL    OCONV          !! convergence flag
*
*   [MODIFY] 
      REAL*8     GAT   ( IJDIM, KMAX      )  !! temperature T
*
*   [INPUT] 
      REAL*8     GAPS  ( IJDIM            )  !! surface pressure
      REAL*8     WTDIV ( NMDIM, KMAX      )  !! tendency of divergence D

      REAL*8     FLAPLA( NMDIM )             !! laplacian factor
      REAL*8     WPHIS ( NMDIM       )       !! topography
      REAL*8     GG    ( KMAX )              !! for semi-implicit
*
*   [WORK] 
      REAL*8     WDT   ( NMDIM, KMAX      )  !! temperature
      REAL*8     WDTS  ( NMDIM, KMAX      )  !! temp(2grid noise removed)
      REAL*8     WDPI  ( NMDIM            )  !! log Ps=pi
      REAL*8     DWPI  ( NMDIM            )  !! correction for pi
      REAL*8     GDX   ( IJDIM            )  !! log Ps=pi
*
      INTEGER    IFPAR, JFPAR, IJ, K, NM, KK
*
*   [INTERNAL SAVE] 
      REAL*8     GBPS  ( IJDIM            )  !! surface pressure
      LOGICAL    OFIRST
      SAVE       GBPS
      DATA       OFIRST          / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DSTEP: DYNAMICAL STEP 05/05/24'
         OFIRST = .FALSE.
         CALL COPY( GBPS , GAPS , IJDIM )
      ENDIF
*
*  << G2W : grid->spectral >>
*
      CALL G2W
     O         ( WDT   ,
     I           GAT   , '    ', 'POSO', KMAX )
      DO 1100 IJ = 1, IJDIM
         GDX( IJ ) = LOG( GAPS( IJ ) )
 1100 CONTINUE
      CALL G2W
     O         ( WDPI  ,
     I           GDX   , '    ', 'POSO', 1    )
*
*     modify temperature with Ps at step n
*
      CALL TINTGR                      !! spectral time integration
     M         ( WDT   , 
     I           WTDIV , WDPI  , WPHIS ,
     C           FLAPLA, GG      )
*
*     remove vertical 2-grid noize
*
      CALL VSMOOTHWS
     O         ( WDTS  ,
     I           WDT      )
*
*     correction to lnPs with T at step n+1
*
      CALL PSCORCT        !! lnPs correction
     O         ( DWPI  , 
     M           WDT   , WDTS   ,
     C           GG                )
*
*     wave to grid
*
      CALL W2G
     O         ( GAT   ,
     I           WDTS  , '    ', 'POSO', KMAX )
      CALL W2G
     O         ( GDX   ,
     I           DWPI  , '    ', 'ADDO', 1    )
      DO 2000 IJ = 1, IJDIM
         GAPS( IJ ) = EXP( GDX( IJ ) )
 2000 CONTINUE
*
*     estimate toward convergence
*
      CALL RMSE ( GAPS , GBPS , 1    , OCONV )
      CALL COPY ( GBPS , GAPS , IJDIM )
*
      RETURN
      END
**********************************************************************
      SUBROUTINE WRFRC          !! write forcing 
     I         ( RSFRC  , CFTYPE ,
     I           GRFV   , GRFD   , GRFT   , GRFP   )
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc.           */
#include        "zhdim.F"                /* # of char. in string   */
*
*   [INPUT]
      REAL*8     GRFV ( IJDIM, KMAX )        !! R-forcing for vor.
      REAL*8     GRFD ( IJDIM, KMAX )        !! R-forcing for div.
      REAL*8     GRFT ( IJDIM, KMAX )        !! R-forcing for temp.
      REAL*8     GRFP ( IJDIM       )        !! R-forcing for LnPs

      CHARACTER  RSFRC*90
      CHARACTER  CFTYPE*8
*
*   [WORK]
      REAL*4     X    ( IMAX, JMAX  )        !! temporary for forcing data
      REAL*8     FCT 
      INTEGER    I, J, K, IJ
      DATA       FCT / -1.D0 /
*
*
      WRITE ( 6,* ) ' @@@ WRRFRC: WRITE FORCING 01/10/19'

      IF( RSFRC(1:1) .NE. ' ' ) THEN
         OPEN( 87, FILE = RSFRC, FORM='UNFORMATTED',
     &        STATUS='UNKNOWN' ) 
         WRITE( 6, *) '    $$$ ',CFTYPE,' FORCING FILE:', RSFRC,' $$$'
      ELSE
         WRITE( 6, *) '    ### ',CFTYPE,' FORCING NOT SAVED ### '
         WRITE( 6, *) 
         RETURN
      ENDIF
*
      DO 300 K = 1, KMAX
         IJ = 0
         DO 310 J = 1, JMAX
            DO 320 I = 1, IMAX
               IJ = IJ + 1
               X( I, J) = SNGL( FCT * GRFV( IJ,K ) )
 320        CONTINUE
            IJ = IJ + 1
 310     CONTINUE
         WRITE( 87 ) X           !! vorticity
 300  CONTINUE
      write(6,*) 'vor.'
*     
      DO 330 K = 1, KMAX
         IJ = 0
         DO 340 J = 1, JMAX
            DO 350 I = 1, IMAX
               IJ = IJ + 1
               X( I, J) = SNGL( FCT * GRFD( IJ,K ) )
 350        CONTINUE
            IJ = IJ + 1
 340     CONTINUE
         WRITE( 87 ) X           !! divergence
 330  CONTINUE
      write(6,*) 'div.'
*     
      DO 360 K = 1, KMAX
         IJ = 0
         DO 370 J = 1, JMAX
            DO 380 I = 1, IMAX
               IJ = IJ + 1
               X( I, J) = SNGL( FCT * GRFT( IJ,K ) )
 380        CONTINUE
            IJ = IJ + 1
 370     CONTINUE
         WRITE( 87 ) X           !! temperature
 360  CONTINUE
      write(6,*) 'tmp.'
*     
      IJ = 0
      DO 390 J = 1, JMAX
         DO 400 I = 1, IMAX
            IJ = IJ + 1
            X( I, J) = SNGL( FCT * GRFP( IJ ) )
 400     CONTINUE
         IJ = IJ + 1
 390  CONTINUE
      WRITE( 87 ) X             !! LnPs
      write(6,*) 'lnps'
*
C      DO 410 K = 1, KMAX
C         IJ = 0
C         DO 420 J = 1, JMAX
C            DO 430 I = 1, IMAX
C               IJ = IJ + 1
C               X( I, J) = 0.D0
C 430        CONTINUE
C            IJ = IJ + 1
C 420     CONTINUE
C         WRITE( 87 ) X           !! humidity (dummy)
C 410  CONTINUE
*     
      RETURN
      END
