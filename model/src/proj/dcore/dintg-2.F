* PACKAGE DINTG  !! dynamics: time integration
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         95/06/06(takata)  <2. non-IGW term> (Sugata)
*         96/05/28(numaguti) restructure
***********************************************************************
      SUBROUTINE TINTGR    !! time integration (spectral)
     M         ( WDVOR , WDDIV , WDT   , WDPI  ,
     M           WTVOR , WTDIV , WTT   , WTPI  ,
     I           DELT  , WPHIS ,
     C           DIFV  , DIFD  , DIFT  , FLAPLA,
     C           GG    , TMTX  , WHMTX , GCMTX ,
     W           WDX   , WDPHIX                  )
*
*                  Arakawa & Suarez vert.diffr in sigma
*                    semi implicit scheme
*                    implicit horizontal diffusion
*
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
#include        "zpdim.F"                /* physics etc. */
#include        "zcord.F"                /* coordinate             */
*
*   [MODIFY] 
      REAL*8     WDVOR ( NMDIM , KMAX     )  !! vorticity
      REAL*8     WDDIV ( NMDIM , KMAX     )  !! divergence D
      REAL*8     WDT   ( NMDIM , KMAX     )  !! temperature T
      REAL*8     WDPI  ( NMDIM            )  !! log Ps=pi
*
*   [INPUT] 
      REAL*8     WTVOR ( NMDIM, KMAX      )  !! tendency of vorticity
      REAL*8     WTDIV ( NMDIM, KMAX      )  !! tendency of divergence D
      REAL*8     WTT   ( NMDIM, KMAX      )  !! tendency of temperature
      REAL*8     WTPI  ( NMDIM            )  !! logPs pai tendency
      REAL*8     DELT                        !! time step delta(t)
      REAL*8     WPHIS ( NMDIM )             !! surface geopotential
*
      REAL*8     DIFV  ( NMDIM, KMAX )       !! h-diff. coef (vor)
      REAL*8     DIFD  ( NMDIM, KMAX )       !! h-diff. coef (div)
      REAL*8     DIFT  ( NMDIM, KMAX )       !! h-diff. coef. of heat
      REAL*8     FLAPLA( NMDIM )             !! Laplacian factor
      REAL*8     GG    ( KMAX )              !! for semi-implicit
      REAL*8     TMTX  ( KMAX, KMAX )        !! for semi-implicit
      REAL*8     WHMTX ( KMAX, KMAX )        !! for semi-implicit
      REAL*8     GCMTX ( KMAX, KMAX )        !! for semi-implicit
*
*   [ENTRY INPUT] 
      REAL*8     WDQ   ( NMDIM, KMAX  )      !! humidity etc.
      REAL*8     WTQ   ( NMDIM, KMAX  )      !! humidity etc.
      REAL*8     DIFQ  ( NMDIM, KMAX  )      !! h-diff. coef. of water
*
*   [WORK] 
      REAL*8     WDX   ( NMDIM, KMAX )       !! t-averaged T, D 
      REAL*8     WDPHIX( NMDIM, KMAX )       !! phi (temporary)
*
*   [INTERNAL WORK] 
      REAL*8     DELT2, WDPIX, WDPHIY
      REAL*8     WV, WD, WT, WP
      INTEGER    NM, K, KK
*
*   [INTERNAL SAVE] 
      REAL*8     DMLU  ( NMDIM, KMAX, KMAX ) !! LU decomp. of SI matrix
      INTEGER    KPIV  ( NMDIM, KMAX       ) !! pivot of SI matrix
      SAVE       DMLU, KPIV
      REAL*8     DELTSV                      !! saved value of delta(t)
      LOGICAL    OFIRST
      DATA       DELTSV / 0.0  /
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DINTG: TIME INTEGRATION 95/06/06'
         OFIRST = .FALSE.
      ENDIF
*
*          < 1. set matrix >
*
      IF ( DELT .NE. DELTSV ) THEN
         CALL DMTRIX                   !! matrix for semi-implicit
     O         ( DMLU  , KPIV  ,
     I           DELT  ,
     C           WHMTX , GCMTX ,
     C           DIFD  , DIFT  , FLAPLA )
         DELTSV = DELT
      ENDIF
*
      DELT2  = DELT*2.D0  
*
*          < 2. non-IGW term >
*
      DO 2100 K = 1 , KMAX
         DO 2100 NM = 1 , NMDIM
            WDX( NM,K ) = WDT ( NM,K )*( 1.D0 + DELT*DIFT( NM,K ) )
     &                  + WTT ( NM,K )* DELT
 2100 CONTINUE
      CALL WHYDRO
     O         ( WDPHIX,
     I           WDX     )
*
*          < 3. RHS of div-eq.  >
*
      DO 3100 K = 1 , KMAX
         DO 3110 NM = 1 , NMDIM
            WDPIX  = WDPI( NM ) + WTPI( NM )* DELT
            WDPHIY = WDPHIX( NM,K )
     &             +  ( 1.D0 + DELT2*DIFT( NM,K )     )
     &              * ( WPHIS ( NM ) +  WDPIX*GG( K ) )
            WDX( NM,K )
     &             = WDDIV( NM,K ) * ( 1.D0 + DELT *DIFD( NM,K ) )
     &                             * ( 1.D0 + DELT2*DIFT( NM,K ) )
     &             + DELT*( WTDIV ( NM,K )
     &                        * ( 1.D0 + DELT2*DIFT( NM,K ) )
     &                      - FLAPLA( NM ) * WDPHIY           )
 3110    CONTINUE
 3100 CONTINUE
*
*          < 4. solve bar(D) with LU matrix >
*
      CALL LUSOLV
     M         ( WDX   ,
     I           DMLU  , KPIV  ,
     D           NMDIM , KMAX    )
*
*          < 5. temp., press.tendency term >
*
      DO 5100 K = 1, KMAX
         DO 5100 KK = 1 , KMAX
            DO 5100 NM = 1, NMDIM
               WTT ( NM,K ) = WTT ( NM,K )
     &                      - TMTX( K,KK ) * WDX( NM,KK )
 5100 CONTINUE
      DO 5200 KK = 1 , KMAX
         DO 5200 NM = 1, NMDIM
              WTPI ( NM   ) = WTPI( NM   )
     &                      - DSIG(   KK ) * WDX( NM,KK )
 5200 CONTINUE
*
*          < 6. time integration >
*
      DO 6100  K = 1 , KMAX
         DO 6100  NM = 1 , NMDIM
            WV = ( WDVOR( NM,K ) + WTVOR( NM,K )*DELT2 )
     &          /( 1.D0 + DELT2*DIFV( NM,K )           )
            WD =  WDX( NM,K ) * 2.D0 - WDDIV ( NM,K )
            WT = ( WDT  ( NM,K ) + WTT  ( NM,K )*DELT2 )
     &         / ( 1.D0 + DELT2*DIFT( NM,K )           )
*
*     1998/11/12
*
            WTVOR( NM, K) = ( WV - WDVOR( NM, K ) ) / DELT2
            WTDIV( NM, K) = ( WD - WDDIV( NM, K ) ) / DELT2
            WTT( NM, K)   = ( WT - WDT( NM, K )   ) / DELT2
*
            WDVOR( NM, K ) = WV
            WDDIV( NM, K ) = WD
            WDT( NM, K )   = WT
 6100 CONTINUE
*
*     implicit vertical diffusion
*
CCX      CALL SOLVDF( WDVOR, WTVOR, DELT, 'VOR' )
CCX      CALL SOLVDF( WDDIV, WTDIV, DELT, 'DIV' )
CCX      CALL SOLVDF( WDT  , WTT  , DELT, 'TMP' )
*
      DO 6200 NM = 1 , NMDIM
         WP =   WDPI ( NM   ) + WTPI ( NM   )*DELT2
         WTPI ( NM   )   = ( WP - WDPI( NM    )   ) / DELT2
         WDPI ( NM   ) = WP
 6200 CONTINUE
*
      RETURN
*=======================================================================
      ENTRY      TRINTG         !! time integration (spectral)
     M         ( WDQ   , 
     I           WTQ   , 
     I           DELT  , DIFQ  )
*
      DELT2  = DELT*2.D0  
      DO 6300  K = 1 , KMAX
         DO 6300  NM = 1 , NMDIM
            WDQ ( NM,K ) 
     &                  = ( WDQ ( NM,K ) + WTQ ( NM,K )*DELT2 )
     &                  / ( 1.D0 + DELT2*DIFQ( NM,K )         )
 6300 CONTINUE
*
      RETURN
      END
