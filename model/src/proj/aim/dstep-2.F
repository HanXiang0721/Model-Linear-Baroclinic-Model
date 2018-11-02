* PACKAGE DSTEP !! dynamics: integration of eqs.
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/05/28(numaguti) restructure
*         00/04/10(hiro)     planetary wave model
*         01/06/15(hiro)     PWM incl. moisture
**********************************************************************
      SUBROUTINE DYNSTP    !! time step of dynamics
     M         ( GAU   , GAV   , GAT   , GAPI  , GAQ   ,
     M           GBU   , GBV   , GBT   , GBPI  , GBQ   ,
     M           WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
     I           DELT  , 
     I           DIFV  , DIFD  , DIFT  , DIFQ  , 
     I           FLAPLA, TDMPC ,
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
      REAL*8     GAPI  ( IJDIM            )  !! surface pressure
      REAL*8     GAQ   ( IJDIM, KMAX      )  !! humidity 
*
      REAL*8     GBU   ( IJDIM, KMAX      )  !! westerly u
      REAL*8     GBV   ( IJDIM, KMAX      )  !! southern wind v
      REAL*8     GBT   ( IJDIM, KMAX      )  !! temperature T
      REAL*8     GBPI  ( IJDIM            )  !! surface pressure
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
      CALL G2W
     O         ( WDPI  ,
     I           GBPI  , '    ', 'POSO', 1    )
      CALL G2W
     O         ( WDQ   ,
     I           GBQ   , '    ', 'POS' , KMAX )
*
*  << INTGR : time integration >>
*
      CALL TINTGR                      !! spectral time integration
     M         ( WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
     M           WDVOR , WDDIV , WDT   , WDPI  , WDQ   ,
     I           DELT  , WPHIS , 
     I           TDMPC ,
     C           DIFV  , DIFD  , DIFT  , DIFQ  , 
     C           FLAPLA, GG    , TMTX  , WHMTX , GCMTX ,
     W           WDX   , WDY                            )
*
      CALL DADVNC               !! time advancing + time filter
     M         ( GBU   , GBV   , GBT   , GBPI  ,
     I           GAU   , GAV   , GAT   , GAPI   )
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
     O         ( GAPI  ,
     I           WDPI  , '    ', 'POSO', 1    )
*
      RETURN
      END
**********************************************************************
      SUBROUTINE DYNSTP_WAVE    !! time step of dynamics
     M         ( WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
     M           WAVOR , WADIV , WAT   , WAPI  , WAQ   ,
     M           WBVOR , WBDIV , WBT   , WBPI  , WBQ   ,
     I           DELT  , 
     I           DIFV  , DIFD  , DIFT  , DIFQ  , 
     I           FLAPLA, TDMPC ,
     I           WPHIS , GG    , TMTX  , WHMTX , GCMTX  )
*     
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
#include        "zpdim.F"                /* physics etc. */
#include        "zccom.F"                /* physics etc. */
#include        "zcord.F"                /* coordinate             */
*
*   [MODIFY] 
      REAL*8     WTVOR ( NMDIM, KMAX      )  !! tendency of vorticity
      REAL*8     WTDIV ( NMDIM, KMAX      )  !! tendency of divergence D
      REAL*8     WTT   ( NMDIM, KMAX      )  !! tendency of temperature
      REAL*8     WTPI  ( NMDIM            )  !! logPs pi tendency
      REAL*8     WTQ   ( NMDIM, KMAX      )  !! tendency of humidity

      REAL*8     WAVOR ( NMDIM, KMAX )       !! vorticity
      REAL*8     WADIV ( NMDIM, KMAX )       !! divergence D
      REAL*8     WAT   ( NMDIM, KMAX )       !! temperature T
      REAL*8     WAPI  ( NMDIM       )       !! log Ps=pi
      REAL*8     WAQ   ( NMDIM, KMAX )       !! humidity Q

      REAL*8     WBVOR ( NMDIM, KMAX )       !! vorticity
      REAL*8     WBDIV ( NMDIM, KMAX )       !! divergence D
      REAL*8     WBT   ( NMDIM, KMAX )       !! temperature T
      REAL*8     WBPI  ( NMDIM       )       !! log Ps=pi
      REAL*8     WBQ   ( NMDIM, KMAX )       !! humidity Q
*
*   [INPUT] 
      REAL*8     DIFV  ( NMDIM, KMAX )       !! h-diff. coef (vor)
      REAL*8     DIFD  ( NMDIM, KMAX )       !! h-diff. coef (div)
      REAL*8     DIFT  ( NMDIM, KMAX )       !! h-diff. coef. of heat
      REAL*8     DIFQ  ( NMDIM, KMAX )       !! h-diff. coef. of tracer
      REAL*8     FLAPLA( NMDIM )             !! laplacian factor
*
      REAL*8     DELT                        !! time step
      REAL*8     TDMPC          !! damping coefficient for temperature
*
      REAL*8     WPHIS ( NMDIM       )       !! topography
      REAL*8     GG    ( KMAX )              !! for semi-implicit
      REAL*8     TMTX  ( KMAX, KMAX )        !! for semi-implicit
      REAL*8     WHMTX ( KMAX, KMAX )        !! for semi-implicit
      REAL*8     GCMTX ( KMAX, KMAX )        !! for semi-implicit
*
*   [WORK] 
      REAL*8     WWV   ( NMDIM, KMAX )
      REAL*8     WWD   ( NMDIM, KMAX )
      REAL*8     WWT   ( NMDIM, KMAX )
      REAL*8     WWP   ( NMDIM       )
      REAL*8     WWQ   ( NMDIM, KMAX )

      REAL*8     WDX   ( NMDIM, KMAX )       !! work
      REAL*8     WDY   ( NMDIM, KMAX )       !! work
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
*  << INTGR : time integration >>
*
      CALL COPY( WWV , WBVOR , NMDIM*KMAX ) !!
      CALL COPY( WWD , WBDIV , NMDIM*KMAX ) !!
      CALL COPY( WWT , WBT   , NMDIM*KMAX ) !!
      CALL COPY( WWP , WBPI  , NMDIM      ) !!
      CALL COPY( WWQ , WBQ   , NMDIM*KMAX ) !!
*
      CALL TINTGR                      !! spectral time integration
     M         ( WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
     M           WWV   , WWD   , WWT   , WWP   , WWQ   ,
     I           DELT  , WPHIS , 
     I           TDMPC ,
     C           DIFV  , DIFD  , DIFT  , DIFQ  , 
     C           FLAPLA, GG    , TMTX  , WHMTX , GCMTX ,
     W           WDX   , WDY                             )
*
      CALL DADVNCW               !! time advancing + time filter
     M         ( WBVOR , WBDIV , WBT   , WBPI  ,
     M           WAVOR , WADIV , WAT   , WAPI  ,
     I           WWV   , WWD   , WWT   , WWP    )
*
      RETURN
      END
***********************************************************************
      SUBROUTINE DADVNCW    !! WA->WB time advancing + time filter
     M         ( WBVOR , WBDIV , WBT   , WBPI  ,
     M           WAVOR , WADIV , WAT   , WAPI  ,
     I           WWV   , WWD   , WWT   , WWP    )
*
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
#include        "zpdim.F"                /* physics etc. */
*
*    [MODIFY]
      REAL*8     WAVOR ( NMDIM*KMAX )       !! vorticity
      REAL*8     WADIV ( NMDIM*KMAX )       !! divergence D
      REAL*8     WAT   ( NMDIM*KMAX )       !! temperature T
      REAL*8     WAPI  ( NMDIM      )       !! log Ps=pi
      REAL*8     WAQ   ( NMDIM*KMAX )       !! humidity Q

      REAL*8     WBVOR ( NMDIM*KMAX )       !! vorticity
      REAL*8     WBDIV ( NMDIM*KMAX )       !! divergence D
      REAL*8     WBT   ( NMDIM*KMAX )       !! temperature T
      REAL*8     WBPI  ( NMDIM      )       !! log Ps=pi
      REAL*8     WBQ   ( NMDIM*KMAX )       !! humidity Q
*
*   [INPUT] 
      REAL*8     WWV   ( NMDIM*KMAX )
      REAL*8     WWD   ( NMDIM*KMAX )
      REAL*8     WWT   ( NMDIM*KMAX )
      REAL*8     WWP   ( NMDIM      )
      REAL*8     WWQ   ( NMDIM*KMAX )
*
*    [INTERNAL WORK]
      REAL*8     TFILB, TFILA
      INTEGER    NM, NMK, LT, IFPAR, JFPAR
*
*   [INTERNAL SAVE] 
      REAL*8     TFIL                        !! factor of time filter
      LOGICAL    OFIRST
      NAMELIST  /NMTFIL/ TFIL
      DATA       TFIL   / 5.D-2 /
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DADVNW: TIME ADVANCE & FILTER 05/06/22'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMTFIL, END=90 )
   90    WRITE  ( JFPAR, NMTFIL )
      ENDIF
*
      TFILB = TFIL  /( 1.D0 - TFIL )
      TFILA = 1.D0 - TFILB
*
      DO 1100 NMK = 1, NMDIM*KMAX
         WBVOR ( NMK ) = TFILB * WBVOR ( NMK )
     &                 + TFILA * WAVOR ( NMK )
 1100 CONTINUE 
*
      DO 1200 NMK = 1, NMDIM*KMAX
         WBDIV ( NMK ) = TFILB * WBDIV ( NMK )
     &                 + TFILA * WADIV ( NMK )
 1200 CONTINUE
*
      DO 1300 NMK = 1, NMDIM*KMAX
         WBT  ( NMK ) = TFILB * WBT ( NMK )
     &                + TFILA * WAT ( NMK )
 1300 CONTINUE 
*
      DO 1400 NM = 1, NMDIM
         WBPI ( NM  ) = TFILB * WBPI( NM  )
     &                + TFILA * WAPI( NM  )
 1400 CONTINUE
*
*     time advance
*
      CALL COPY( WAVOR, WWV , NMDIM*KMAX ) !! step n+1
      CALL COPY( WADIV, WWD , NMDIM*KMAX ) !! step n+1
      CALL COPY( WAT  , WWT , NMDIM*KMAX ) !! step n+1
      CALL COPY( WAPI , WWP , NMDIM      ) !! step n+1
*
*
      RETURN
*=================================================================
      ENTRY      QADVNCW        !! WA->WB time advancing + time filter
     M         ( WBQ  ,
     M           WAQ  ,
     I           WWQ    )
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ QADVNW: TIME ADVANCE & FILTER 05/06/22'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMTFIL, END=190 )
  190    WRITE  ( JFPAR, NMTFIL )
      ENDIF
*
      TFILB = TFIL  /( 1.D0 - TFIL )
      TFILA = 1.D0 - TFILB
*
      DO 1700 NMK = 1, NMDIM*KMAX
         WBQ( NMK ) = TFILB * WBQ( NMK )
     &              + TFILA * WAQ( NMK )
 1700 CONTINUE 
      CALL COPY( WAQ  , WWQ , NMDIM*KMAX ) !! step n+1
*
      RETURN
      END
