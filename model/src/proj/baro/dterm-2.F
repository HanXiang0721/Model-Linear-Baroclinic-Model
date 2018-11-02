* PACKAGE DCIRC !! dynamics: terms of eqs.
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/05/28(numaguti) restructure
*         00/04/10(hiro)     planetary wave model
*         01/06/18(hiro)     barotropic model
*         GF* : basic state
*         GA* : perturbation
**********************************************************************
      SUBROUTINE DYNTRM    !! dynamics terms
     O         ( WTVOR , WAVOR ,
     M           GAU   , GAV   , 
     I           GFU   , GFV   , GDZS  , WTUNTT,
     C           CORIOL                  )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* # of wave              */
#include        "zpdim.F"                /* physics etc.           */
#include        "zccom.F"                /* physical constants     */
#include        "zcord.F"                /* coordinate             */
#include        "zhdim.F"                /* # of char. in string */
*
*   [OUTPUT] 
      REAL*8     WTVOR ( NMDIM, KMAX     ) !! tendency of vorticity
      REAL*8     WAVOR ( NMDIM, KMAX     ) !! vorticity
*
*   [MODIFY] 
      REAL*8     GAU ( IJDIM, KMAX )        !! zonal wind
      REAL*8     GAV ( IJDIM, KMAX )        !! meridional wind
*
*   [INPUT] 
      REAL*8     GFU ( IJDIM, KMAX )        !! BS zonal wind
      REAL*8     GFV ( IJDIM, KMAX )        !! BS meridional wind
*
      REAL*8     GDZS  ( IJDIM )            !! topography
      REAL*8     CORIOL( IJDIM )            !! Corioris factor f
*
      REAL*8     WTUNTT( NMDIM, KMAX   )  !! unit matrix 
*
*   [WORK] 
      REAL*8     GAVOR ( IJDIM, KMAX     ) !! vorticity
      REAL*8     GTUX  ( IJDIM, KMAX   )  !! tend. zonal  adv.
      REAL*8     GTVX  ( IJDIM, KMAX   )  !! tend. merid. adv.
      REAL*8     GTX   ( IJDIM, KMAX   )  !! work
      REAL*8     GTY   ( IJDIM, KMAX   )  !! work
      REAL*8     WTX   ( NMDIM, KMAX   )  !! work
      REAL*8     WTY   ( NMDIM, KMAX   )  !! work
      INTEGER    IFPAR, JFPAR
      INTEGER    IJ, K
*
#ifdef OPT_WVFRC
      REAL*4     DATG( IMAX, JDIM)
      REAL*8     GAPSI ( IJDIM, KMAX   )  !! stream function
      REAL*8     WAPSI ( NMDIM, KMAX   )  !! tendency of vorticity
      INTEGER    I, J 
#endif
*
*   [INTERNAL SAVE] !! variables for basic state
      REAL*8     GFVOR( IJDIM, KMAX )    !! BS for VOR
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      REAL*8     UVFACT( IJDIM  )        !! factor of u -> U
*
      SAVE       GFVOR
      SAVE       NMO, UVFACT 
*
*   [INTERNAL PARAM.] 
#ifdef OPT_WVFRC
      REAL*8     FACT           !! factor for perturbations
      INTEGER    KWAVE, LWAVE, IC
      CHARACTER  WVFRC *(NFILN) !! file name for steady forcing
      NAMELIST  /NMWFRC/ WVFRC, FACT, KWAVE, LWAVE, IC, OEP
      DATA       WVFRC  / '     ' /
      DATA       FACT   / 1.D0    /
      DATA       KWAVE  / -1      /
      DATA       LWAVE  / -1      /
#endif
      REAL*8     H0
      LOGICAL    OEP
      LOGICAL    OFIRST

      DATA       H0 / 1.D4 /
      DATA       OEP    / .FALSE. /
      DATA       OFIRST / .TRUE.  /

      IF ( OFIRST ) THEN
         CALL   REWNML ( IFPAR , JFPAR )
         WRITE ( JFPAR,* ) ' @@@ DTERM: DYNAMICS EQ. TERMS 98/10/17'
         CALL DSETNM
     O         ( NMO    )
         CALL   DSETUV
     O        ( UVFACT )
*
*     calculation for basic state
*
         CALL UV2VDG
     O        ( GFVOR , GTX   ,
     I          GFU   , GFV   , .TRUE., .FALSE.,
     W          GTUX  , GTVX  , WTVOR , WTX   )
*     
         CALL RESET( GTX , IJDIM*KMAX )
         IF( KMAX .EQ. 1 ) THEN
            CALL VD2UVG
     O           ( GFU   , GFV   ,
     I             GFVOR , GTX   , .TRUE., .FALSE.,
     W             WTVOR , WTX    )
         ENDIF
*
 1099       OFIRST = .FALSE.
      ENDIF
*
#ifdef OPT_MKMAT
*
*     make perturbation matrices
*
      CALL RESET( GAU  , IJDIM*KMAX )
      CALL RESET( GAV  , IJDIM*KMAX )
      CALL RESET( GAVOR, IJDIM*KMAX )
      CALL RESET( WTVOR, NMDIM*KMAX )
*
*     wave to grid
*
      CALL VDW2UV               !! (vor.,D) -> (U,V)
     O        ( GAU    , GAV   ,
     I          WTUNTT , WTVOR , .TRUE., .FALSE.,
     W          WTX    , WTY                     )

      CALL W2G
     O        ( GAVOR ,
     I          WTUNTT,
     I          '    ', 'POS ', KMAX )
      CALL COPY( WAVOR , WTUNTT , NMDIM*KMAX )
#endif
*
#ifdef OPT_WVFRC
*
*     make forcing vector
*
      CALL RESET( GAU  , IJDIM*KMAX )
      CALL RESET( GAV  , IJDIM*KMAX )
      CALL RESET( GAVOR, IJDIM*KMAX )
*
      CALL   REWNML ( IFPAR , JFPAR )
      READ   ( IFPAR, NMWFRC, END=490 )
  490 WRITE  ( JFPAR, NMWFRC)
      IF( KWAVE .LT. 0 .AND. LWAVE .LT. 0 ) THEN
         OPEN( 79, FILE = WVFRC, FORM='UNFORMATTED',
     $        STATUS = 'UNKNOWN' )
*
         DO 100 K = 1, KMAX
            READ( 79 ) DATG
            IJ = 0
            DO 110 J = 1, JDIM
               DO 120 I = 1, IMAX
                  IJ = IJ + 1
                  GAPSI( IJ, K) = FACT * DBLE( DATG( I, J) )
  120          CONTINUE
               IJ = IJ + 1
               GAPSI( IJ, K) = GAPSI( IJ-IMAX, K)
  110       CONTINUE
  100    CONTINUE
         CLOSE( 79 )
*
*     psi perturbation --> wave
*
         CALL G2W
     O        ( WAPSI,
     I          GAPSI,
     I         '    ', 'POSO', KMAX )
*
*     psi perturbation --> U V
*
         CALL W2G
     O        ( GAU   ,
     I          WAPSI ,
     I          'YGRA', 'NEG ', KMAX )
         CALL W2G
     M        ( GAV   ,
     I          WAPSI ,
     I          'XGRA', 'POS ', KMAX )
         DO 200 K = 1, KMAX
            DO 200 IJ = 1, IJDIM
               GAU ( IJ,K ) = GAU ( IJ,K ) / UVFACT( IJ ) / ER
               GAV ( IJ,K ) = GAV ( IJ,K ) / UVFACT( IJ ) / ER
  200    CONTINUE
*
         CALL UV2VDG            !! UV-->VOR.DIV.
     O        ( GAVOR , GTX   ,
     I          GAU   , GAV   , .TRUE., .FALSE.,
     W          GTUX  , GTVX  , WTX   , WTY     )
         CALL G2W
     O        ( WAVOR,
     I          GAVOR,
     I          '    ', 'POSO', KMAX )
      ELSE
         WAVOR( NMO(IC,KWAVE,LWAVE), 1) = FACT
         CALL VDW2UV            !! (vor.,D) -> (U,V)
     O        ( GAU    , GAV   ,
     I          WAVOR  , WTVOR , .TRUE., .FALSE.,
     W          WTX    , WTY                     )
         CALL W2G
     O        ( GAVOR ,
     I          WAVOR ,
     I          '    ', 'POS ', KMAX )
      ENDIF
#endif
*
*    << vorticity eq. >>
*
      CALL GRUADV                      !!  momentum advection
     O         ( GTUX  , GTVX  ,
     I           GAU   , GAV   , GAVOR , 
     I           GFU   , GFV   , GFVOR , 
     I           GDZS  , OEP   ,
     C           CORIOL, H0              )
*
      IF( OEP ) THEN
         DO 300 K = 1, KMAX
            DO 300 IJ = 1, IJDIM
               GTVX ( IJ,K ) = GTVX ( IJ,K ) * UVFACT( IJ ) 
  300    CONTINUE
         CALL G2W
     O        ( WTX ,
     I          GTVX,
     I        'YGRA', 'POSO', KMAX )
         CALL W2G
     O        ( GTVX,
     I          WTX ,
     I        '    ', 'POS0', KMAX )
         DO 400 K = 1, KMAX
            DO 400 IJ = 1, IJDIM
               GTVX ( IJ,K ) = GTVX( IJ,K ) + 
     &              + GAV( IJ,K ) * CORIOL( IJ ) * GDZS( IJ ) / H0 
  400    CONTINUE
         CALL G2W
     O        ( WTVOR,
     I           GTVX,
     I         'YGRA', 'POS ', KMAX )
      ELSE
         CALL    UV2VDW      !! (U,V) -> (vor.,D)spectral
     O         ( WTVOR , WTX   ,
     I           GTUX  , GTVX  , .TRUE., .FALSE.,
     W           GTX   , GTY                      )
      ENDIF
*
      RETURN
      END
*******************************************************************
      SUBROUTINE GRUADV    !!  momentum advection
     O         ( GTUA  , GTVA  , 
     I           GAU   , GAV   , GAVOR , 
     I           GFU   , GFV   , GFVOR , 
     I           GDZS  , OEP   ,
     C           CORIOL, H0               )
*
*   [PARAMETER]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT]
      REAL*8     GTUA  ( IJDIM, KMAX )   !! u advection UA
      REAL*8     GTVA  ( IJDIM, KMAX )   !! merid. adv. of momentum
*
*   [INPUT]
      REAL*8     GAU   ( IJDIM, KMAX )   !! westerly U(t)
      REAL*8     GAV   ( IJDIM, KMAX )   !! southern wind V(t)
      REAL*8     GAVOR ( IJDIM, KMAX )   !! vorticity zeta(t)
*
      REAL*8     GFU   ( IJDIM, KMAX )   !! BS westerly
      REAL*8     GFV   ( IJDIM, KMAX )   !! BS southern wind
      REAL*8     GFVOR ( IJDIM, KMAX )   !! BS vorticity 
      REAL*8     GDZS  ( IJDIM )         !! topography
      REAL*8     CORIOL( IJDIM )         !! Corioris factor f
      REAL*8     H0
      LOGICAL    OEP
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K
*
* << UVADV : UA,VA-terms >>
*
      DO 1100 K = 1, KMAX
         DO 1100 IJ = 1, IJDIM
            GTUA( IJ,K ) = 
     &                     + GAV ( IJ,K )
     &                       * ( GFVOR( IJ,K ) + CORIOL ( IJ )  )
     &                     + GFV ( IJ,K )
     &                       * ( GAVOR( IJ,K )                  )
*
            GTVA( IJ,K ) = 
     &                     - GAU ( IJ,K )
     &                       * ( GFVOR( IJ,K ) + CORIOL ( IJ )  )
     &                     - GFU ( IJ,K )
     &                       * ( GAVOR( IJ,K )                  )
#ifdef OPT_WVFRC
cx            GTUA( IJ,K ) = GTUA( IJ,K ) - GAV( IJ,K ) * CORIOL( IJ )
cx            GTVA( IJ,K ) = GTVA( IJ,K ) + GAU( IJ,K ) * CORIOL( IJ )
cx            GTUA( IJ,K ) = 0.D0
            GTUA( IJ,K ) = 
     &                  + GFV ( IJ,K ) * GAVOR( IJ,K )
            GTVA( IJ,K ) = 
     &                  - GAU ( IJ,K ) * GFVOR( IJ,K ) 
     &                  - GAU ( IJ,K ) * CORIOL( IJ ) * GDZS( IJ ) / H0
#endif
*
            IF( OEP ) THEN
               GTUA( IJ,K ) = 0.D0
               GTVA( IJ,K ) = 
     &                     + GAU ( IJ,K )*GFV( IJ,K )
     &                     + GFU ( IJ,K )*GAV( IJ,K )
cx     &                         ( GAU ( IJ,K ) + GFU( IJ,K ) )
cx     &                       * ( GAV ( IJ,K ) + GFV( IJ,K ) )
cx     &                     -     GFU ( IJ,K )*GFV ( IJ,K )
            ENDIF
*
 1100 CONTINUE
*
      RETURN
      END
