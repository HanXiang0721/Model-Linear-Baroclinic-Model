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
      INTEGER    IFPAR, JFPAR, IJ , k,i,j
      real*8  coriol( ijdim ), gavor( ijdim, kmax )
      real*8  gau2( ijdim,kmax )
      real*8  gav2( ijdim,kmax )
      real*4  out( imax,jdim)
*
*   [INTERNAL SAVE] 
      CHARACTER  RSFRC*90
      LOGICAL    OTPDIF                  !! p-diffusion for T
      LOGICAL    OQPDIF                  !! p-diffusion for q
      LOGICAL    OFRICH                  !! frictional heat
      LOGICAL    OFIRST
      NAMELIST  /NMDCOR/ OTPDIF, OQPDIF, OFRICH
      NAMELIST  /NMRFRC/ RSFRC 
      DATA       OTPDIF, OQPDIF, OFRICH  / .TRUE., .TRUE., .FALSE. /
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


c
      CALL DSETCO  
     O     ( CORIOL )
      CALL UV2VDG
     O         ( GAVOR   , GDY   ,
     I           GAU   , GAU   , .TRUE., .TRUE., .FALSE. ,
     W           GBU  , GBV  , WDX , WDY           )
      do k = 1, kmax
         do ij = 1, ijdim
            gdy( ij,k ) = gavor( ij,k ) + coriol( ij )
         enddo
      enddo
      CALL VDW2UV      !! (vor.,D) -> (U,V)
     O         ( GAU   , GAV   ,
     I           WDVOR , WDDIV , .FALSE., .TRUE.,
     W           WDX   , WDY                     )
      do k = 1, kmax
         ij = 0
         do j = 1, jdim
            do i = 1, imax
               ij = ij + 1
               out( i,j ) = sngl( gau( ij,k ) )
            enddo
            ij = ij + 1
         enddo
         write( 96 ) out
      enddo
      write( 6,* ) '  @@@ divergence wind-x '
      do k = 1, kmax
         ij = 0
         do j = 1, jdim
            do i = 1, imax
               ij = ij + 1
               out( i,j ) = sngl( gav( ij,k ) )
            enddo
            ij = ij + 1
         enddo
         write( 96 ) out
      enddo
      write( 6,* ) '  @@@ divergence wind-y '
      do k = 1, kmax
         ij = 0
         do j = 1, jdim
            do i = 1, imax
               ij = ij + 1
               out( i,j ) = sngl( gdy( ij,k ) )
            enddo
            ij = ij + 1
         enddo
         write( 96 ) out
      enddo
      write( 6,* ) '  @@@ absolute vorticity '
      do k = 1, kmax
         do ij = 1, ijdim
            gau2( ij,k ) = gau( ij,k ) * gavor( ij,k )
            gav2( ij,k ) = gav( ij,k ) * gavor( ij,k )
            gau( ij,k ) = gau( ij,k ) * gdy( ij,k )
            gav( ij,k ) = gav( ij,k ) * gdy( ij,k )
         enddo
      enddo
      CALL UV2VDG
     O         ( GDX   , GDY   ,
     I           GAU   , GAU   , .TRUE., .TRUE., .FALSE. ,
     W           GBU  , GBV  , WDX , WDY           )
      do k = 1, kmax
         ij = 0
         do j = 1, jdim
            do i = 1, imax
               ij = ij + 1
               out( i,j ) = sngl( gdy( ij,k ) )
            enddo
            ij = ij + 1
         enddo
         write( 96 ) out
      enddo
      write( 6,* ) '  @@@ Rossby source  '
      CALL UV2VDG
     O         ( GDX   , GDY   ,
     I           GAU2   , GAU2   , .TRUE., .TRUE., .FALSE. ,
     W           GBU  , GBV  , WDX , WDY           )
      do k = 1, kmax
         ij = 0
         do j = 1, jdim
            do i = 1, imax
               ij = ij + 1
               out( i,j ) = sngl( gdy( ij,k ) )
            enddo
            ij = ij + 1
         enddo
         write( 96 ) out
      enddo
      write( 6,* ) '  @@@ Rossby source without f '
      close( 96 )
      write(6,* ) ' %%%%%%% FILE WRITTEN %%%%%%'
      call xabort( 1 )
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
