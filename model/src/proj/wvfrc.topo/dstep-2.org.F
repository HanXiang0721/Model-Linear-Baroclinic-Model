* PACKAGE DSTEP !! dynamics: integration of eqs.
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/05/28(numaguti) restructure
*         00/04/10(hiro)     planetary wave model
*         01/06/15(hiro)     PWM incl. moisture
**********************************************************************
      SUBROUTINE DYNSTP    !! time step of dynamics
     M         ( GBU   , GBV   , GBT   , GBPS  , GBQ   ,
     M           WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
     I           DELT  , 
     I           DIFV  , DIFD  , DIFT  , DIFQ  , FLAPLA, 
     I           OMATV , OMATD , OMATT , OMATP , OMATQ ,
     I           TDMPC ,
     I           WPHIS , GG    , TMTX  , WHMTX , GCMTX , NOMWV ,
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
      REAL*8     GBU   ( IJDIM, KMAX      )  !! westerly u
      REAL*8     GBV   ( IJDIM, KMAX      )  !! southern wind v
      REAL*8     GBT   ( IJDIM, KMAX      )  !! temperature T
      REAL*8     GBPS  ( IJDIM            )  !! surface pressure
      REAL*8     GBQ   ( IJDIM, KMAX      )  !! humidity 
*
*   [INPUT] 
      REAL*8     WTVOR ( NMDIM, KMAX      )  !! tendency of vorticity
      REAL*8     WTDIV ( NMDIM, KMAX      )  !! tendency of divergence D
      REAL*8     WTT   ( NMDIM, KMAX      )  !! tendency of temperature
      REAL*8     WTPI  ( NMDIM            )  !! logPs pi tendency
      REAL*8     WTQ   ( NMDIM, KMAX      )  !! tendency of humidity
*
      REAL*8     DELT                        !! time step
      LOGICAL    OMATV, OMATD, OMATT, OMATP, OMATQ
      REAL*8     TDMPC          !! damping coefficient for temperature
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

      INTEGER    NOMWV ( NMDIM )
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
      REAL*8     WDQ   ( NMDIM, KMAX )       !! humidity Q
*
      INTEGER    IJ
*
*   [INTERNAL SAVE] 
      LOGICAL    OFIRST
      DATA       OFIRST          / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DSTEP: DYNAMICAL STEP 00/04/12'
         OFIRST = .FALSE.
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
         GDX( IJ,1 ) = GBPS(IJ)
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
     I           DELT  , WPHIS , NOMWV ,
     I           OMATV , OMATD , OMATT , OMATP , OMATQ ,
     I           TDMPC ,
     C           DIFV  , DIFD  , DIFT  , DIFQ  , FLAPLA,
     C           GG    , TMTX  , WHMTX , GCMTX ,
     W           WDX   , WDY                            )
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
