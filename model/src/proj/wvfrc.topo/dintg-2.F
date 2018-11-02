* PACKAGE DINTG  !! dynamics: time integration
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         95/06/06(takata)  <2. non-IGW term> (Sugata)
*         96/05/28(numaguti) restructure
*         98/11/05(hiro)     wave tendency
*         01/06/15(hiro)     PWM incl. moisture
***********************************************************************
      SUBROUTINE TINTGR    !! time integration (spectral)
     M         ( WDVOR , WDDIV , WDT   , WDPI  , WDQ   ,
     M           WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
     I           DELT  , WPHIS , 
     I           TDMPC ,
     C           DIFV  , DIFD  , DIFT  , DIFQ  , FLAPLA,
     C           GG    , TMTX  , WHMTX , GCMTX ,
     W           WDX   , WDPHIX                         )
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
      REAL*8     WDQ   ( NMDIM , KMAX     )  !! humidity Q
*
*   [INPUT] 
      REAL*8     WTVOR ( NMDIM, KMAX      )  !! tendency of vorticity
      REAL*8     WTDIV ( NMDIM, KMAX      )  !! tendency of divergence
      REAL*8     WTT   ( NMDIM, KMAX      )  !! tendency of temperature
      REAL*8     WTPI  ( NMDIM            )  !! logPs pai tendency
      REAL*8     WTQ   ( NMDIM , KMAX     )  !! tendency of humidity
      REAL*8     DELT                        !! time step delta(t)
      REAL*8     WPHIS ( NMDIM )             !! surface geopotential
*
      REAL*8     DIFV  ( NMDIM, KMAX )       !! h-diff. coef (vor)
      REAL*8     DIFD  ( NMDIM, KMAX )       !! h-diff. coef (div)
      REAL*8     DIFT  ( NMDIM, KMAX )       !! h-diff. coef. of heat
      REAL*8     DIFQ  ( NMDIM, KMAX  )      !! h-diff. coef. of water
      REAL*8     FLAPLA( NMDIM )             !! Laplacian factor
      REAL*8     GG    ( KMAX )              !! for semi-implicit
      REAL*8     TMTX  ( KMAX, KMAX )        !! for semi-implicit
      REAL*8     WHMTX ( KMAX, KMAX )        !! for semi-implicit
      REAL*8     GCMTX ( KMAX, KMAX )        !! for semi-implicit

      REAL*8     TDMPC          !! damping coefficient for temperature
*
*   [WORK] 
      REAL*8     WDX   ( NMDIM, KMAX )       !! t-averaged T, D 
      REAL*8     WDPHIX( NMDIM, KMAX )       !! phi (temporary)
*
*   [INTERNAL WORK] 
      REAL*8     DELT2, WDPIX, WDPHIY
      REAL*8     WV                          !! temporary for WDVOR
      REAL*8     WD                          !! temporary for WDDIV
      REAL*8     WT                          !! temporary for WDT
      REAL*8     WP                          !! temporary for WDPI
      REAL*8     WQ                          !! temporary for WDQ
      INTEGER    NM, K, KK
      INTEGER    IFPAR, JFPAR
*     
*   [INTERNAL SAVE] 
      REAL*8     DMLU  ( NMDIM, KMAX, KMAX ) !! LU decomp. of SI matrix
      INTEGER    KPIV  ( NMDIM, KMAX       ) !! pivot of SI matrix
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      SAVE       DMLU, KPIV
      REAL*8     DELTSV                      !! saved value of delta(t)
      LOGICAL    OFIRST
      DATA       DELTSV / 0.0    /
      DATA       OFIRST / .TRUE. /
      REAL*8     ZMFCTV( KMAX ) !! factor for zonal mean vor.
      REAL*8     ZMFCTD( KMAX ) !! factor for zonal mean div.
      REAL*8     ZMFCTT( KMAX ) !! factor for zonal mean tmp.
      REAL*8     ZMFCTQ( KMAX ) !! factor for zonal mean sph
      CHARACTER  TUNIT*4                     !! unit of time
      INTEGER    NDDRAGV( KMAX )             !! damping time scale (dy)
      INTEGER    NDDRAGD( KMAX )             !! damping time scale (dy)
      INTEGER    NDDRAGT( KMAX )             !! damping time scale (dy)
      INTEGER    NDDRAGQ( KMAX )             !! damping time scale (dy)
      NAMELIST  /NMDAMP/ NDDRAGV, NDDRAGD, NDDRAGT, NDDRAGQ, TUNIT
      NAMELIST  /NMZMFCT/ ZMFCTV, ZMFCTD, ZMFCTT, ZMFCTQ
      DATA       NDDRAGV / KMAX*-1 /
      DATA       NDDRAGD / KMAX*-1 /
      DATA       NDDRAGT / KMAX*-1 /
      DATA       NDDRAGQ / KMAX*-1 /
      DATA       ZMFCTV  / KMAX*1.D0 /
      DATA       ZMFCTD  / KMAX*1.D0 /
      DATA       ZMFCTT  / KMAX*1.D0 /
      DATA       ZMFCTQ  / KMAX*1.D0 /
      DATA       TUNIT   / 'DAY' /
*
*   [INTERNAL PARAM]
      REAL*8     DIST0
      REAL*8     DSEC
      REAL*8     DISTV( KMAX )               !! linear drag 
      REAL*8     DISTD( KMAX )               !! linear drag 
      REAL*8     DISTT( KMAX )               !! linear drag 
      REAL*8     DISTQ( KMAX )               !! linear drag 
      REAL*8     DISBT( KMAX )               !! boundary drag got T
      DATA       DSEC   / 0.D0 /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DINTG: TIME INTEGRATION 01/06/15'
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMDAMP, END=90 )
   90    WRITE  ( JFPAR, NMDAMP )
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMZMFCT, END=91 )
   91    WRITE  ( JFPAR, NMZMFCT )
*
         CALL DSETNM
     O         ( NMO   )
*
         CALL RESET( DISTV, KMAX )
         CALL RESET( DISTD, KMAX )
         CALL RESET( DISTT, KMAX )
         CALL RESET( DISTQ, KMAX )
         DO 1 K = 1, KMAX
            DIST0 = DBLE( NDDRAGV( K ) )
            CALL CXX2SS ( DISTV( K ), DIST0, TUNIT, DSEC  )
            DIST0 = DBLE( NDDRAGD( K ) )
            CALL CXX2SS ( DISTD( K ), DIST0, TUNIT, DSEC  )
            DIST0 = DBLE( NDDRAGT( K ) )
            CALL CXX2SS ( DISTT( K ), DIST0, TUNIT, DSEC  )
            DIST0 = DBLE( NDDRAGQ( K ) )
            CALL CXX2SS ( DISTQ( K ), DIST0, TUNIT, DSEC  )
            IF( DISTV( K ) .LE. 0 ) THEN
               DISTV( K ) = 0.D0
            ELSE
               DISTV( K ) = 1.D0 / DISTV( K )
            ENDIF
            IF( DISTD( K ) .LE. 0 ) THEN
               DISTD( K ) = 0.D0
            ELSE
               DISTD( K ) = 1.D0 / DISTD( K )
            ENDIF
            IF( DISTT( K ) .LE. 0 ) THEN
               DISTT( K ) = 0.D0
            ELSE
               DISTT( K ) = 1.D0 / DISTT( K )
            ENDIF
            IF( DISTQ( K ) .LE. 0 ) THEN
               DISTQ( K ) = 0.D0
            ELSE
               DISTQ( K ) = 1.D0 / DISTQ( K )
            ENDIF
    1    CONTINUE
*
         DO 2  K = 1 , KMAX
            DISBT( K ) = 0.D0
    2    CONTINUE
         DISBT( 1    ) = TDMPC
         DISBT( KMAX ) = TDMPC
*
         DO 1100  K = 1 , KMAX
            DO 1100  NM = 1 , NMDIM
               DIFV( NM, K) = DIFV( NM, K) + DISTV( K )
               DIFD( NM, K) = DIFD( NM, K) + DISTD( K )
               DIFT( NM, K) = DIFT( NM, K) + DISTT( K ) + DISBT( K )
               DIFQ( NM, K) = DIFQ( NM, K) + DISTQ( K )
 1100    CONTINUE 
         DO 1200 K = 1, KMAX
            DO 1300 NM = 1, LMAX
               difv(nmo(1,0,nm),k) = 
     &              difv(nmo(1,0,nm),k)-distv(k)*(1.D0-ZMFCTV(K))
               difd(nmo(1,0,nm),k) = 
     &              difd(nmo(1,0,nm),k)-distd(k)*(1.D0-ZMFCTD(K))
               dift(nmo(1,0,nm),k) = 
     &              dift(nmo(1,0,nm),k)-distt(k)*(1.D0-ZMFCTT(K))
               difq(nmo(1,0,nm),k) = 
     &              difq(nmo(1,0,nm),k)-distq(k)*(1.D0-ZMFCTQ(K))
 1300       CONTINUE
 1200    CONTINUE
*
         CALL RESET( DIFV, NMDIM*KMAX )
         CALL RESET( DIFD, NMDIM*KMAX )
         CALL RESET( DIFT, NMDIM*KMAX )
         CALL RESET( DIFQ, NMDIM*KMAX )
*
         OFIRST = .FALSE.
      ENDIF
*
      DELT2  = DELT*2.D0  
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
     &              - TMTX( K,KK ) * WDX( NM,KK )
 5100 CONTINUE
      DO 5200 KK = 1 , KMAX
         DO 5200 NM = 1, NMDIM
            WTPI ( NM   ) = WTPI( NM   )
     &           - DSIG(   KK ) * WDX( NM,KK )
 5200 CONTINUE
 5500 CONTINUE
*
*          < 6. time integration >
*
      DO 6100  K = 1 , KMAX
         DO 6100  NM = 1 , NMDIM
            WV            = ( WDVOR( NM,K ) + WTVOR( NM,K )*DELT2 )
     &                     / ( 1.D0 + DELT2*DIFV( NM,K )          )
            WD            =  WDX( NM,K ) * 2.D0 - WDDIV ( NM,K )
            WT            = ( WDT  ( NM,K ) + WTT  ( NM,K )*DELT2 )
     &                     / ( 1.D0 + DELT2*DIFT( NM,K )          )
            WQ            = ( WDQ  ( NM,K ) + WTQ  ( NM,K )*DELT2 )
     &                     / ( 1.D0 + DELT2*DIFQ( NM,K )          )
*
*     1998/11/12
*
            WTVOR( NM, K) = ( WV - WDVOR( NM, K ) ) / DELT2
            WTDIV( NM, K) = ( WD - WDDIV( NM, K ) ) / DELT2
            WTT  ( NM, K) = ( WT - WDT( NM, K )   ) / DELT2
            WDQ  ( NM, K) = ( WQ - WDQ( NM, K )   ) / DELT2
*
            WDVOR( NM, K ) = WV
            WDDIV( NM, K ) = WD
            WDT  ( NM, K ) = WT
            WDQ  ( NM, K ) = WQ
*
 6100 CONTINUE
*
*     implicit vertical diffusion
*
      CALL SOLVDF( WDVOR, WTVOR, DELT, 'VOR' )
      CALL SOLVDF( WDDIV, WTDIV, DELT, 'DIV' )
      CALL SOLVDF( WDT  , WTT  , DELT, 'TMP' )
      CALL SOLVDF( WDQ  , WTQ  , DELT, 'SPH' )
*
      DO 6200 NM = 1 , NMDIM
         WP =   WDPI ( NM   ) + WTPI ( NM   )*DELT2
         WTPI ( NM   )   = ( WP - WDPI( NM    )   ) / DELT2
         WDPI ( NM   ) = WP
 6200 CONTINUE
*
*
      RETURN
      END
