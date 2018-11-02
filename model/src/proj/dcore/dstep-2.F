* PACKAGE DSTEP !! dynamics: integration of eqs.
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/05/28(numaguti) restructure
**********************************************************************
      SUBROUTINE DYNSTP    !! time step of dynamics
     M         ( GAU   , GAV   , GAT   , GAPS  , GLAPS ,
     M           GBU   , GBV   , GBT   , GBPS  , GLBPS ,
     M           GAQ   , 
     I           WTVOR , WTDIV , WTT   , WTPI  ,
     I           DELT  , OINIT ,
     I           DIFV  , DIFD  , DIFT  , DIFQ  , FLAPLA, 
     I           WPHIS , GG    , TMTX  , WHMTX , GCMTX , 
     W           GDX   , GDY   , WDX   , WDY            )
*     
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
#include        "zpdim.F"                /* physics etc. */
#include        "zccom.F"                /* physics etc. */
#include        "zcord.F"                /* coordinate             */
*
*   [MODIFY] 
      REAL*8     GAU   ( IJDIM, KMAX      )  !! westerly u
      REAL*8     GAV   ( IJDIM, KMAX      )  !! southern wind v
      REAL*8     GAT   ( IJDIM, KMAX      )  !! temperature T
      REAL*8     GAPS  ( IJDIM            )  !! surface pressure
      REAL*8     GLAPS ( IJDIM            )  !! log surface pressure
*        : grid value(t) <GENGD>  grid value(t+dt)
*
      REAL*8     GBU   ( IJDIM, KMAX      )  !! westerly u
      REAL*8     GBV   ( IJDIM, KMAX      )  !! southern wind v
      REAL*8     GBT   ( IJDIM, KMAX      )  !! temperature T
      REAL*8     GBPS  ( IJDIM            )  !! surface pressure
      REAL*8     GLBPS ( IJDIM            )  !! log surface pressure
*        : grid value(t-dt) <DADVNC>  grid value(t)
*
      REAL*8     GAQ   ( IJDIM, KMAX, NTR )  !! moisture etc.
*        : apply diffusion correction only
*
*   [INPUT] 
      REAL*8     WTVOR ( NMDIM, KMAX      )  !! tendency of vorticity
      REAL*8     WTDIV ( NMDIM, KMAX      )  !! tendency of divergence D
      REAL*8     WTT   ( NMDIM, KMAX      )  !! tendency of temperature
      REAL*8     WTPI  ( NMDIM            )  !! logPs pi tendency
*
      REAL*8     DIFV  ( NMDIM, KMAX )       !! h-diff. coef (vor)
      REAL*8     DIFD  ( NMDIM, KMAX )       !! h-diff. coef (div)
      REAL*8     DIFT  ( NMDIM, KMAX )       !! h-diff. coef. of heat
      REAL*8     DIFQ  ( NMDIM, KMAX )       !! h-diff. coef. of tracer
      REAL*8     FLAPLA( NMDIM )             !! laplacian factor
*
      REAL*8     WPHIS ( NMDIM       )       !! topography
      REAL*8     GG    ( KMAX )              !! for semi-implicit
      REAL*8     TMTX  ( KMAX, KMAX )        !! for semi-implicit
      REAL*8     WHMTX ( KMAX, KMAX )        !! for semi-implicit
      REAL*8     GCMTX ( KMAX, KMAX )        !! for semi-implicit
*
      REAL*8     DELT                        !! time step
      LOGICAL    OINIT                       !! initialized condition ?
*
*   [WORK] 
      REAL*8     GDX   ( IJDIM, KMAX )
      REAL*8     GDY   ( IJDIM, KMAX )
      REAL*8     WDX   ( NMDIM, KMAX )
      REAL*8     WDY   ( NMDIM, KMAX )
*
*   [INTERNAL WORK] 
      REAL*8     WDVOR ( NMDIM, KMAX )       !! vorticity
      REAL*8     WDDIV ( NMDIM, KMAX )       !! divergence D
      REAL*8     WDT   ( NMDIM, KMAX )       !! temperature T
      REAL*8     WDPI  ( NMDIM       )       !! log Ps=pi
*
      INTEGER    IFPAR, JFPAR, IJ
*
*   [INTERNAL SAVE] 
      CHARACTER  RSFRC*90
      LOGICAL    OTPDIF                  !! p-diffusion for T
      LOGICAL    OQPDIF                  !! p-diffusion for q
      LOGICAL    OFRICH                  !! frictional heat
      LOGICAL    OFIRST
      NAMELIST  /NMDCOR/ OTPDIF, OQPDIF, OFRICH
      NAMELIST  /NMRFRC/ RSFRC 
C      DATA       OTPDIF, OQPDIF, OFRICH  / .TRUE., .TRUE., .FALSE. /
C 05/08/30
      DATA       OTPDIF, OQPDIF, OFRICH  / .TRUE., .FALSE., .FALSE. /
      DATA       RSFRC           / '              ' /
      DATA       OFIRST          / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DSTEP: DYNAMICAL STEP 94/11/30'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMDCOR, END=190 )
  190    WRITE  ( JFPAR, NMDCOR )
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMRFRC, END=200 )
  200    WRITE  ( JFPAR, NMRFRC )
      ENDIF
*
*  << G2W : grid->spectral >>
*
      CALL UV2VDW          !! (U,V) -> (vor.,D)
     O         ( WDVOR , WDDIV ,
     I           GBU   , GBV   , .TRUE., .TRUE.,
     W           GDX   , GDY                     )
      CALL G2W
     O         ( WDT   ,
     I           GBT   , '    ', 'POSO', KMAX )
      DO 1100 IJ = 1, IJDIM
         GDX( IJ,1 ) = LOG( GBPS( IJ ) )
         GLBPS( IJ ) = GDX( IJ, 1)
 1100 CONTINUE
      CALL G2W
     O         ( WDPI  ,
     I           GDX   , '    ', 'POSO', 1    )
*
*  << INTGR : time integration >>
*
      CALL TINTGR                      !! spectral time integration
     M         ( WDVOR , WDDIV , WDT   , WDPI  ,
     M           WTVOR , WTDIV , WTT   , WTPI  ,
     I           DELT  , WPHIS , 
     C           DIFV  , DIFD  , DIFT  , FLAPLA,
     C           GG    , TMTX  , WHMTX , GCMTX ,
     W           WDX   , WDY                    )
*
      IF ( .NOT. OINIT ) THEN
         CALL DADVNC                   !! time advancing + time filter
     M         ( GBU   , GBV   , GBT   , GBPS  ,
     I           GAU   , GAV   , GAT   , GAPS   )
      ENDIF
*
#ifdef OPT_RWRIT
      CALL W2G
     O         ( GBU   ,
     I           WTVOR , '    ', 'POSO', KMAX )
      CALL W2G
     O         ( GBV   ,
     I           WTDIV , '    ', 'POSO', KMAX )
      CALL W2G
     O         ( GBT   ,
     I           WTT   , '    ', 'POSO', KMAX )
      CALL W2G
     O         ( GBPS  ,
     I           WTPI  , '    ', 'POSO', 1    )
      CALL WRFRC          !! write forcing 
     I         ( RSFRC  , 'RESIDUAL'       ,
     I           GBU    , GBV    , GBT     , GBPS    )
#endif
*
*   << GENGD : new grid >>
*
      CALL VDW2UV      !! (vor.,D) -> (U,V)
     O         ( GAU   , GAV   ,
     I           WDVOR , WDDIV , .TRUE., .TRUE.,
     W           WDX   , WDY                     )
      CALL W2G
     O         ( GAT   ,
     I           WDT   , '    ', 'POSO', KMAX )
      CALL W2G
     O         ( GAPS  ,
     I           WDPI  , '    ', 'POSO', 1    )
      DO 2100 IJ = 1, IJDIM
         GLAPS( IJ ) = GAPS( IJ )
         GAPS ( IJ ) = EXP( GAPS( IJ ) )
 2100 CONTINUE
*
*  << CORRCT : correction >>
*
      IF ( OTPDIF ) THEN
         CALL CORDIF            !! p-level diff. correc.
     M         ( GAT   ,
     I           WDPI  , DELT  , 
     C           DIFT  , 1     ,
     W           GDX   , WDX    )
      ENDIF
      IF ( OQPDIF ) THEN
         CALL CORDIF            !! p-level diff. correc.
     M         ( GAQ   ,
     I           WDPI  , DELT  , 
     C           DIFQ  , NTR   ,
     W           GDX   , WDX             )
      ENDIF
*
      IF ( OFRICH ) THEN
*        note : destroy WDVOR, WDDIV
         CALL CORFRC            !! friction heat of horiz. diff.
     M         ( GAT   ,
     M           WDVOR , WDDIV , 
     I           GAU   , GAV   , DELT  ,
     C           DIFV  , DIFD  , 
     W           GDX   , GDY   , WDX   , WDY   )
      ENDIF
*
      RETURN
      END
***********************************************************************
      SUBROUTINE SPCTLD    !!  diagnosis in spectral
     I         ( WDATA , HTTL  , KDIMD )
*
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
*
*   [INPUT]
      INTEGER    KDIMD
      REAL*8     WDATA ( NMDIM, KDIMD )
      CHARACTER  HTTL  *(*)
*
*   [INTERNAL WORK]
      REAL*8     WAMP  ( 0:NMAX, KMAX+1 )
      LOGICAL    ODIAG
      INTEGER    JFLDIA, NCOLS, ISTR, IEND, IINT
      INTEGER    K,L,M,N
*
*   [INTERNAL SAVE] 
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      SAVE       NMO
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         OFIRST = .FALSE.
         CALL DSETNM
     O         ( NMO    )
      ENDIF
*
      CALL QDIAGP
     O     ( ODIAG , JFLDIA, NCOLS,
     O       ISTR  , IEND  , IINT ,
     I       KDIMD                  )
*
      IF ( .NOT. ODIAG )       RETURN
*
      CALL RESET ( WAMP, (NMAX+1)*KDIMD )
*
      DO 2100 K = 1, KDIMD
         DO 2110 N = 0 , NMAX
            DO 2120 M = 0 , MMAX, MINT
               L = N - M
               IF ( ( L .GE. 0 ).AND.( L .LE. LMAX ) ) THEN
                  WAMP ( N,K ) = WAMP ( N,K )
     &                         + WDATA( NMO(1,M,L),K ) ** 2
     &                         + WDATA( NMO(2,M,L),K ) ** 2
               ENDIF
 2120      CONTINUE
 2110   CONTINUE
 2100 CONTINUE
*
      WRITE ( JFLDIA, * ) '@@@@@@@@@@@@< SPECTRAL AMPLITUDE ', HTTL,
     &                    '>@@@@@@@@@@@@@'
*
      DO 3100 K = 1, KDIMD
         CALL PRINTD
     I       ( JFLDIA, '(1PE12.4)', WAMP  ,
     I         1     , NMAX+1 , 1     , K    ,
     I         NMAX+1, KDIMD  , NCOLS         )
 3100 CONTINUE
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
      INTEGER    IFPAR, JFPAR
      INTEGER    I, J, K, IJ
*
*   [PARAMETER]
      LOGICAL    ORVOR, ORDIV, ORT, ORPI
      NAMELIST /NMRWRIT/  ORVOR, ORDIV, ORT, ORPI
      DATA       ORVOR / .FALSE. /
      DATA       ORDIV / .FALSE. /
      DATA       ORT   / .TRUE. /
      DATA       ORPI  / .FALSE. /
      DATA       FCT   / -1.D0 /
*
*
      CALL   REWNML ( IFPAR , JFPAR )
      WRITE ( JFPAR,* ) ' @@@ WRFRC: WRITE FORCING 05/08/29'

      READ   ( IFPAR, NMRWRIT, END=190 )
  190 WRITE  ( JFPAR, NMRWRIT )

      IF( RSFRC(1:1) .NE. ' ' ) THEN
         OPEN( 87, FILE = RSFRC, FORM='UNFORMATTED',
     &        STATUS='UNKNOWN' ) 
         WRITE( JFPAR, *) 
     &        '    $$$ ',CFTYPE,' FORCING FILE:', RSFRC,' $$$'
      ELSE
         WRITE( JFPAR, *)
     &        '    ### ',CFTYPE,' FORCING NOT SAVED ### '
         WRITE( JFPAR, *) 
         RETURN
      ENDIF
*
      IF( .NOT. ORVOR ) CALL RESET( GRFV , IJDIM*KMAX )
      IF( .NOT. ORDIV ) CALL RESET( GRFD , IJDIM*KMAX )
      IF( .NOT. ORT   ) CALL RESET( GRFT , IJDIM*KMAX )
      IF( .NOT. ORPI  ) CALL RESET( GRFP , IJDIM      )
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
      write(JFPAR,*) 'vor.'
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
      write(JFPAR,*) 'div.'
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
      write(JFPAR,*) 'tmp.'
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
      write(JFPAR,*) 'lnps'
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
