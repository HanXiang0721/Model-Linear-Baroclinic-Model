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
     C           DIFV  , DIFD  , DIFT  , DIFQ  , FLAPLA,
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
      REAL*8     WDQ   ( NMDIM , KMAX     )  !! humidity Q
*
*   [INPUT] 
      REAL*8     WTVOR ( NMDIM, KMAX      )  !! tendency of vorticity
      REAL*8     WTDIV ( NMDIM, KMAX      )  !! tendency of divergence D
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
*
*   [ENTRY INPUT]
      REAL*8     GIDX  ( IJDIM       )       !! grid index
*
*   [WORK] 
      REAL*8     WDX   ( NMDIM, KMAX )       !! t-averaged T, D 
      REAL*8     WDPHIX( NMDIM, KMAX )       !! phi (temporary)
      REAL*8     GMSK  ( IJDIM       )       !! land mask
      REAL*8     DELT2, WDPIX, WDPHIY
      REAL*8     WV                          !! temporary for WDVOR
      REAL*8     WD                          !! temporary for WDDIV
      REAL*8     WT                          !! temporary for WDT
      REAL*8     WQ                          !! temporary for WDQ
      REAL*8     DIST0
      REAL*8     DSEC
      REAL*8     DISTV( KMAX )               !! linear drag 
      REAL*8     DISTD( KMAX )               !! linear drag 
      REAL*8     DISTT( KMAX )               !! linear drag 
      REAL*8     DISTQ( KMAX )               !! linear drag 
      REAL*8     GXDMPV( IJDIM, KMAX )       !! 
      REAL*8     GXDMPD( IJDIM, KMAX )       !! 
      REAL*8     GXDMPT( IJDIM, KMAX )       !! 
      REAL*8     GXDMPQ( IJDIM, KMAX )       !! 
      REAL*8     PI
      INTEGER    NM, IJ, K, KK
      INTEGER    IFPAR, JFPAR
*
*   [INTERNAL SAVE] 
      REAL*8     DMLU  ( NMDIM, KMAX, KMAX ) !! LU decomp. of SI matrix
      REAL*8     DELTSV                      !! saved value of delta(t)
      REAL*8     GDMPLV( IJDIM, KMAX )       !! damping over land
      REAL*8     GDMPLD( IJDIM, KMAX )       !! damping over land
      REAL*8     GDMPLT( IJDIM, KMAX )       !! damping over land
      REAL*8     GDMPLQ( IJDIM, KMAX )       !! damping over land
      INTEGER    KPIV  ( NMDIM, KMAX       ) !! pivot of SI matrix

      SAVE       DMLU, KPIV
      SAVE       GDMPLV, GDMPLD, GDMPLT, GDMPLQ
*
*   [INTERNAL PARAM]
      LOGICAL    OFIRST
      CHARACTER  TUNIT*4                     !! unit of time
      CHARACTER  TUNTL*4                     !! unit of time
      REAL*8     DDRAGV ( KMAX )             !! damping time scale (dy)
      REAL*8     DDRAGD ( KMAX )             !! damping time scale (dy)
      REAL*8     DDRAGT ( KMAX )             !! damping time scale (dy)
      REAL*8     DDRAGQ ( KMAX )             !! damping time scale (dy)
      REAL*8     DRGLV  ( KMAX )             !! damping time scale (dy)
      REAL*8     DRGLD  ( KMAX )             !! damping time scale (dy)
      REAL*8     DRGLT  ( KMAX )             !! damping time scale (dy)
      REAL*8     DRGLQ  ( KMAX )             !! damping time scale (dy)
      NAMELIST  /NMDAMP/ DDRAGV, DDRAGD, DDRAGT, DDRAGQ, TUNIT
      NAMELIST  /NMLDMP/ DRGLV, DRGLD, DRGLT, DRGLQ, TUNTL

      DATA       DELTSV / 0.0        /
      DATA       OFIRST / .TRUE.     /
      DATA       DDRAGV / KMAX*-1.D0 /
      DATA       DDRAGD / KMAX*-1.D0 /
      DATA       DDRAGT / KMAX*-1.D0 /
      DATA       DDRAGQ / KMAX*-1.D0 /
      DATA       DRGLV  / KMAX*-1.D0 /
      DATA       DRGLD  / KMAX*-1.D0 /
      DATA       DRGLT  / KMAX*-1.D0 /
      DATA       DRGLQ  / KMAX*-1.D0 /
      DATA       TUNIT  / 'DAY'      /
      DATA       TUNTL  / 'DAY'      /
      DATA       DSEC   / 0.D0       /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DINTG: TIME INTEGRATION 95/06/06'
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMDAMP, END=90 )
   90    WRITE  ( JFPAR, NMDAMP )
*
         CALL RESET( DISTV, KMAX )
         CALL RESET( DISTD, KMAX )
         CALL RESET( DISTT, KMAX )
         CALL RESET( DISTQ, KMAX )
         DO 1 K = 1, KMAX
            DIST0 = DDRAGV( K )
            CALL CXX2SS ( DISTV( K ), DIST0, TUNIT, DSEC  )
            DIST0 = DDRAGD( K )
            CALL CXX2SS ( DISTD( K ), DIST0, TUNIT, DSEC  )
            DIST0 = DDRAGT( K )
            CALL CXX2SS ( DISTT( K ), DIST0, TUNIT, DSEC  )
            DIST0 = DDRAGQ( K )
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
         DO 1100  K = 1 , KMAX
            DO 1100  NM = 1 , NMDIM
               DIFV( NM, K) = DIFV( NM, K) + DISTV( K )
               DIFD( NM, K) = DIFD( NM, K) + DISTD( K )
               DIFT( NM, K) = DIFT( NM, K) + DISTT( K )
               DIFQ( NM, K) = DIFQ( NM, K) + DISTQ( K ) 
 1100    CONTINUE
*
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
     &             * ( WPHIS ( NM ) +  WDPIX*GG( K ) )
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
     &           - DSIG(   KK ) * WDX( NM,KK )
 5200 CONTINUE
*
*          < 6. time integration >
*
      DO 6100  K = 1 , KMAX
         DO 6100  NM = 1 , NMDIM
            WV            = ( WDVOR( NM,K ) + WTVOR( NM,K )*DELT2 )
     &                     / ( 1.D0 + DELT2*DIFV( NM,K )          )
*
            WD            =  WDX( NM,K ) * 2.D0 - WDDIV ( NM,K )
*
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
            WTQ  ( NM, K) = ( WQ - WDQ( NM, K )   ) / DELT2
*
            WDVOR( NM, K ) = WV
            WDDIV( NM, K ) = WD
            WDT( NM, K )   = WT
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
            WDPI ( NM   ) =   WDPI ( NM   ) + WTPI ( NM   )*DELT2
 6200 CONTINUE
*
      RETURN
*======================================================================
      ENTRY SETLDMP 
     I     ( GIDX )
*
*
      WRITE ( 6,* ) ' @@@ SETLDMP: ADD DAMPING OVER LAND 03/08/29'
*
      CALL   REWNML ( IFPAR , JFPAR )
      READ   ( IFPAR, NMLDMP, END=190 )
 190  WRITE  ( JFPAR, NMLDMP )
*
      CALL RESET( DISTV, KMAX )
      CALL RESET( DISTD, KMAX )
      CALL RESET( DISTT, KMAX )
      CALL RESET( DISTQ, KMAX )
      DO K = 1, KMAX
         DIST0 = DRGLV( K )
         CALL CXX2SS ( DISTV( K ), DIST0, TUNTL, DSEC  )
         DIST0 = DRGLD( K )
         CALL CXX2SS ( DISTD( K ), DIST0, TUNTL, DSEC  )
         DIST0 = DRGLT( K )
         CALL CXX2SS ( DISTT( K ), DIST0, TUNTL, DSEC  )
         DIST0 = DRGLQ( K )
         CALL CXX2SS ( DISTQ( K ), DIST0, TUNTL, DSEC  )
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
      ENDDO
*
      PI = ATAN( 1.D0 ) * 4.D0
*
*     area-selection for moist STM
*
      do ij = 1, ijdim
*     asia
cc         if( alon(ij)*180.d0/pi .ge. 50.d0 .and.
         if( alon(ij)*180.d0/pi .ge. 40.d0 .and.
cc     &       alon(ij)*180.d0/pi .le. 120.d0 ) then
     &       alon(ij)*180.d0/pi .le. 140.d0 ) then
            if( alat(ij)*180.d0/pi .ge. 0.d0 .and.
cc     &       alat(ij)*180.d0/pi .le. 45.d0 ) then
     &       alat(ij)*180.d0/pi .le. 40.d0 ) then
               gmsk( ij ) = 1.d0
            endif
         endif
*     north america
         if( alon(ij)*180.d0/pi .ge. 240.d0 .and.
cc     &       alon(ij)*180.d0/pi .le. 270.d0 ) then
     &       alon(ij)*180.d0/pi .le. 290.d0 ) then
cc            if( alat(ij)*180.d0/pi .ge. 10.d0 .and.
            if( alat(ij)*180.d0/pi .ge. 0.d0 .and.
cc     &       alat(ij)*180.d0/pi .le. 35.d0 ) then
     &       alat(ij)*180.d0/pi .le. 40.d0 ) then
               gmsk( ij ) = 1.d0
            endif
         endif
*     australia
         if( alon(ij)*180.d0/pi .ge. 110.d0 .and.
     &       alon(ij)*180.d0/pi .le. 160.d0 ) then
            if( alat(ij)*180.d0/pi .ge. -35.d0 .and.
     &          alat(ij)*180.d0/pi .le. -10.d0 ) then
               gmsk( ij ) = 1.d0
            endif
         endif
*     africa
         if( alon(ij)*180.d0/pi .ge. 340.d0 .or.
     &       alon(ij)*180.d0/pi .le.  60.d0 ) then
            if( alat(ij)*180.d0/pi .ge. -5.d0 .and.
     &          alat(ij)*180.d0/pi .le.  30.d0 ) then
               gmsk( ij ) = 1.d0
            endif
         endif
*     south america
         if( alon(ij)*180.d0/pi .ge. 270.d0 .or.
     &       alon(ij)*180.d0/pi .le. 300.d0 ) then
            if( alat(ij)*180.d0/pi .ge. -15.d0 .and.
     &          alat(ij)*180.d0/pi .le.   5.d0 ) then
               gmsk( ij ) = 1.d0
            endif
         endif
         if( gidx( ij ) .le. 0.d0 ) gmsk( ij ) = 0.d0
      enddo
         
cc      DO IJ = 1, IJDIM
cc         GMSK( IJ ) = GIDX( IJ )
cc         IF( GIDX( IJ ) .GT. 0.D0 ) GMSK( IJ ) = 1.D0
cc      ENDDO

      DO K = 1, KMAX
         DO IJ = 1, IJDIM
            GDMPLV( IJ,K ) = GMSK( IJ ) * DISTV( K )
            GDMPLD( IJ,K ) = GMSK( IJ ) * DISTD( K )
            GDMPLT( IJ,K ) = GMSK( IJ ) * DISTT( K )
            GDMPLQ( IJ,K ) = GMSK( IJ ) * DISTQ( K )
         ENDDO
      ENDDO
*
*
      RETURN
*======================================================================
      ENTRY GETLDMP 
     O     ( GXDMPV , GXDMPD , GXDMPT , GXDMPQ )
*
      CALL COPY( GXDMPV , GDMPLV , IJDIM*KMAX )
      CALL COPY( GXDMPD , GDMPLD , IJDIM*KMAX )
      CALL COPY( GXDMPT , GDMPLT , IJDIM*KMAX )
      CALL COPY( GXDMPQ , GDMPLQ , IJDIM*KMAX )
*
      RETURN
      END
