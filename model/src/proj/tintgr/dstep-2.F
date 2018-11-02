* PACKAGE DSTEP !! dynamics: integration of eqs.
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/05/28(numaguti) restructure
**********************************************************************
      SUBROUTINE DYNSTP    !! time step of dynamics
     M         ( GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     M           GBU   , GBV   , GBT   , GBPS  , GBQ   ,
     M           WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
     I           GFPS  , 
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
      REAL*8     GAQ   ( IJDIM, KMAX      )  !! humidity
*        : grid value(t) <GENGD>  grid value(t+dt)
*
      REAL*8     GBU   ( IJDIM, KMAX      )  !! westerly u
      REAL*8     GBV   ( IJDIM, KMAX      )  !! southern wind v
      REAL*8     GBT   ( IJDIM, KMAX      )  !! temperature T
      REAL*8     GBPS  ( IJDIM            )  !! surface pressure
      REAL*8     GBQ   ( IJDIM, KMAX      )  !! humidity
*        : grid value(t-dt) <DADVNC>  grid value(t)
*
*   [INPUT] 
      REAL*8     GFPS  ( IJDIM            )  !! BS surface pressure
*
      REAL*8     WTVOR ( NMDIM, KMAX      )  !! tendency of vorticity
      REAL*8     WTDIV ( NMDIM, KMAX      )  !! tendency of divergence D
      REAL*8     WTT   ( NMDIM, KMAX      )  !! tendency of temperature
      REAL*8     WTPI  ( NMDIM            )  !! logPs pi tendency
      REAL*8     WTQ   ( NMDIM, KMAX      )  !! tendency of humidity
*
      REAL*8     DELT                        !! time step
      LOGICAL    OINIT                       !! initialized condition ?
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
*   [WORK] 
      REAL*8     GDX   ( IJDIM, KMAX )
      REAL*8     GDY   ( IJDIM, KMAX )
      REAL*8     WDX   ( NMDIM, KMAX )
      REAL*8     WDY   ( NMDIM, KMAX )
      REAL*8     GLFPS ( IJDIM       )
*
*   [INTERNAL WORK] 
      REAL*8     WDVOR ( NMDIM, KMAX )       !! vorticity
      REAL*8     WDDIV ( NMDIM, KMAX )       !! divergence D
      REAL*8     WDT   ( NMDIM, KMAX )       !! temperature T
      REAL*8     WDPI  ( NMDIM       )       !! log Ps=pi
      REAL*8     WDQ   ( NMDIM, KMAX )       !! humidity
*
      INTEGER    IFPAR, JFPAR, IJ
*
*   [INTERNAL SAVE] 
      LOGICAL    OTPDIF                  !! p-diffusion for T
      LOGICAL    OQPDIF                  !! p-diffusion for q
      LOGICAL    OFRICH                  !! frictional heat
      LOGICAL    OFIRST
      NAMELIST  /NMDCOR/ OTPDIF, OQPDIF, OFRICH
      DATA       OTPDIF, OQPDIF, OFRICH  / .FALSE., .FALSE., .FALSE. /
      DATA       OFIRST          / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DSTEP: DYNAMICAL STEP 98/11/12'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMDCOR, END=190 )
  190    WRITE  ( JFPAR, NMDCOR )
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
         GDX( IJ,1 ) = LOG( GBPS(IJ)+GFPS(IJ) ) - LOG( GFPS(IJ) )
         GLFPS( IJ ) = LOG( GFPS( IJ ) )
 1100 CONTINUE
      CALL G2W
     O         ( WDPI  ,
     I           GDX   , '    ', 'POSO', 1    )
      CALL G2W
     O         ( WDQ   ,
     I           GBQ   , '    ', 'POS' , KMAX )
*
*  << INTGR : time integration >>
*
      CALL TINTGR                      !! spectral time integration
     M         ( WDVOR , WDDIV , WDT   , WDPI  , WDQ   ,
     M           WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
     I           DELT  , WPHIS , 
     C           DIFV  , DIFD  , DIFT  , DIFQ  , FLAPLA,
     C           GG    , TMTX  , WHMTX , GCMTX ,
     W           WDX   , WDY                    )
*
      IF ( .NOT. OINIT ) THEN
         CALL DADVNC                   !! time advancing + time filter
     M         ( GBU   , GBV   , GBT   , GBPS  ,
     I           GAU   , GAV   , GAT   , GAPS   )
         CALL QADVNC                   !! time advancing + time filter
     M         ( GBQ   ,
     I           GAQ     )
      ENDIF
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
         GAPS ( IJ ) = EXP( GAPS(IJ)+GLFPS(IJ) ) - EXP( GLFPS(IJ) ) 
 2100 CONTINUE
      CALL W2G
     O         ( GAQ   ,
     I           WDQ   , '    ', 'POSO', KMAX )
*
*     damping sfc. flux
*
#ifndef OPT_CLASSIC
      CALL IMPTQ
     M     ( GAT  , GAQ  , 
     I       DELT          )
#endif
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
