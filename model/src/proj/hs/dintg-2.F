* PACKAGE DINTG  !! dynamics: time integration
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         95/06/06(takata)  <2. non-IGW term> (Sugata)
*         96/05/28(numaguti) restructure
*         05/05/24(hiro)     for initializing BS
***********************************************************************
      SUBROUTINE TINTGR    !! modify temperature
     M         ( WDT   , 
     M           WTDIV , WDPI  , WPHIS ,
     C           FLAPLA, GG      )
*
*                  Arakawa & Suarez vert.diffr in sigma
*                    semi implicit scheme
*                    implicit horizontal diffusion
*
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
#include        "zpdim.F"                /* physics etc. */
#include        "zcord.F"                /* coordinate             */
#include        "zccom.F"
*
*   [MODIFY] 
      REAL*8     WDT   ( NMDIM, KMAX      )  !! temperature
*
*   [INPUT] 
      REAL*8     WTDIV ( NMDIM, KMAX      )  !! tendency of divergence D
      REAL*8     WDPI  ( NMDIM            )  !! log Ps=pi
      REAL*8     WPHIS ( NMDIM )             !! surface geopotential

      REAL*8     FLAPLA( NMDIM )             !! Laplacian factor
      REAL*8     GG    ( KMAX )              !! for semi-implicit
*
*   [WORK] 
      REAL*8     WDPHI ( NMDIM, KMAX )       !! phi
      REAL*8     WDT0  ( KMAX      )         !! global mean temperature
*
*   [INTERNAL WORK] 
      REAL*8     WDX, WDPIX
      INTEGER    NM, K
*
*   [INTERNAL SAVE] 
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      REAL*8     EDEL  ( NMDIM  )            !! vor.,D -> U,V
      LOGICAL    OFIRST
      SAVE       NMO
      SAVE       EDEL
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DINTG: TIME INTEGRATION 95/06/06'
         OFIRST = .FALSE.
         CALL   DSETNM
     O        ( NMO    )
         CALL   DSETED
     O        ( EDEL   )
      ENDIF
*
*     solve for WT (zero divergent tendency)
*
      DO 3000 K = 1 , KMAX
         DO 3100 NM = 1 , NMDIM
            WDX   =  WTDIV( NM,K ) * EDEL( NM ) * ER 
            WDPIX =  WPHIS ( NM ) + WDPI( NM ) * GG( K ) 
            WDPHI( NM,K ) = WDX - WDPIX
 3100    CONTINUE
 3000 CONTINUE
      DO 3200 K = 1, KMAX
         WDPHI( NMO(1,0,0),K ) = 0.D0
         WDT0( K ) = WDT( NMO(1,0,0),K )
 3200 CONTINUE 
*
*     T, inverse of hydrostatic operator
*
      CALL INVWHYDRO
     O         ( WDT,
     I           WDPHI    )
      DO 3300 K = 1, KMAX
         WDT( NMO(1,0,0),K ) = WDT0( K )
 3300 CONTINUE 
*
      RETURN
      END
***********************************************************************
      SUBROUTINE PSCORCT    !! lnPs correction
     O         ( WDPI  , 
     M           WDT   , WDTS   ,
     C           GG                )
*
*     lnPs correction to conserve WT+Gpi term
*     in the divergence equation before and after the
*     smoothed temperature
*
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
#include        "zpdim.F"                /* physics etc. */
#include        "zcord.F"                /* coordinate             */
#include        "zccom.F"
*
*   [OUTPUT] 
      REAL*8     WDPI  ( NMDIM            )  !! log Ps correction
*
*   [INPUT] 
      REAL*8     WDT   ( NMDIM, KMAX      )  !! temperature
      REAL*8     WDTS  ( NMDIM, KMAX      )  !! v.smoothed temperature
      REAL*8     GG    ( KMAX )              !! for semi-implicit
*
*   [WORK] 
      REAL*8     WDPHI ( NMDIM, KMAX      )  !! geopotential
      REAL*8     WDPHIS( NMDIM, KMAX      )  !! v.smoothed geopotential

      REAL*8     WDX, WDPIX
      INTEGER    NM, K
*
*   [INTERNAL SAVE] 
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      LOGICAL    OFIRST
      SAVE       NMO
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ PSCORCT: CORRECTION TO LNPS 05/05/25 '
         OFIRST = .FALSE.
         CALL   DSETNM
     O        ( NMO    )
      ENDIF
*
*     hydrostatic relationship
*
      CALL WHYDRO
     O     ( WDPHI ,
     I       WDT     )
      CALL WHYDRO
     O     ( WDPHIS,
     I       WDTS    )
*
*     correction term
*
      CALL RESET( WDPI , NMDIM )

      DO 3000 NM = 1 , NMDIM
         DO 3100 K = 1 , KMAX
            WDPI( NM ) =  WDPI( NM ) 
     &           + ( WDPHI( NM,K ) - WDPHIS( NM,K ) ) 
     &             / GG( K ) / DBLE( KMAX )
 3100    CONTINUE
 3000 CONTINUE
*
      RETURN
      END
*******************************************************************
      SUBROUTINE VSMOOTHW    !!  vertical smoothing in wave
     O         ( WDXS  ,
     I           WDX     )
*
*     smoothing based on the cubic spline
*
*   [PARAMETER]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT]
      REAL*8     WDXS  ( NMDIM, KMAX )   !! smoothed variable
*
*   [INPUT]
      REAL*8     WDX   ( NMDIM, KMAX )   !! input
*
*   [WORK]
      INTEGER    K, NM
      REAL*8     XIN( KMAX )
      REAL*8     XOUT( KMAX )
*
*   [INTERNAL SAVE] 
      REAL*8     DB, DT
      REAL*8     SLEV( KMAX )
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      LOGICAL    OFIRST
      SAVE       NMO, SLEV
      DATA       OFIRST / .TRUE. /
      DATA       DB  / -999.D0 /
      DATA       DT  / -999.D0 /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ SPLINE: VERTICAL SMOOTHING 05/05/25 '
         OFIRST = .FALSE.
         CALL   DSETNM
     O        ( NMO    )
*     
         DO 1000 K = 1, KMAX
            SLEV( K ) = SIG( KMAX+1-K )
 1000    CONTINUE
      ENDIF
*
      DO 2000 NM = 1, NMDIM
         DO 2100 K = 1, KMAX
            XIN( K ) = WDX( NM,KMAX+1-K )
 2100    CONTINUE
*
         CALL SPLINE
     I         ( XIN , SLEV , DB  , DT  )
         CALL SPLINT
     O         ( XOUT,  
     I           XIN , SLEV    )
*
         DO 2200 K = 1, KMAX
            WDXS( NM,KMAX+1-K ) = XOUT( K )
 2200    CONTINUE
 2000 CONTINUE
*
*     preserve global mean value
*
      DO 3000 K = 1, KMAX
         WDXS( NMO(1,0,0),K ) = WDX( NMO(1,0,0),K )
 3000 CONTINUE
*
      RETURN
      END
*******************************************************************
      SUBROUTINE VSMOOTHWS    !!  vertical smoothing in wave
     O         ( WDXS  ,
     I           WDX     )
*
*     smoothing based on a binomial 1-2-1 filter
*
*   [PARAMETER]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT]
      REAL*8     WDXS  ( NMDIM, KMAX )   !! smoothed variable
*
*   [INPUT]
      REAL*8     WDX   ( NMDIM, KMAX )   !! input
*
*   [WORK]
      INTEGER    K, NM
      REAL*8     VM0, VM1
*
*   [INTERNAL SAVE] 
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      LOGICAL    OFIRST
      SAVE       NMO
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ VSMOOTHWS: VERTICAL SMOOTHING 05/05/25 '
         OFIRST = .FALSE.
         CALL   DSETNM
     O        ( NMO    )
*     
      ENDIF
*
      DO 2000 NM = 1, NMDIM
         VM0 = 0.D0
         VM1 = 0.D0
         DO 2100 K = 2, KMAX-1
            WDXS( NM,K ) =   0.5D0  * WDX( NM,K   )
     &                     + 0.25D0 * WDX( NM,K-1 )
     &                     + 0.25D0 * WDX( NM,K+1 )
            VM0 = VM0 + WDX ( NM,K ) / DBLE( KMAX )
            VM1 = VM1 + WDXS( NM,K ) / DBLE( KMAX )
 2100    CONTINUE
*
         K = 1
         WDXS( NM,K ) =   0.67D0 * WDX( NM,K   )
     &                  + 0.33D0 * WDX( NM,K+1 )
         VM0 = VM0 + WDX ( NM,K ) / DBLE( KMAX )
         VM1 = VM1 + WDXS( NM,K ) / DBLE( KMAX )
         K = KMAX
         WDXS( NM,K ) =   0.67D0 * WDX( NM,K   )
     &                  + 0.33D0 * WDX( NM,K-1 )
         VM0 = VM0 + WDX ( NM,K ) / DBLE( KMAX )
         VM1 = VM1 + WDXS( NM,K ) / DBLE( KMAX )
*
*     unchange vertical average
*
         DO 2200 K = 1, KMAX
            WDXS( NM,K ) = WDXS( NM,K ) - VM1 + VM0
 2200    CONTINUE
 2000 CONTINUE
*
*     preserve global mean value
*
      DO 3000 K = 1, KMAX
         WDXS( NMO(1,0,0),K ) = WDX( NMO(1,0,0),K )
 3000 CONTINUE
*
      RETURN
      END
*******************************************************************
      SUBROUTINE VSMOOTHG    !!  vertical smoothing in grid
     O         ( GDXS  ,
     I           GDX     )
*
*   [PARAMETER]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT]
      REAL*8     GDXS  ( IJDIM, KMAX )   !! smoothed variable
*
*   [INPUT]
      REAL*8     GDX   ( IJDIM, KMAX )   !! input
*
*   [WORK]
      INTEGER    K, IJ
      REAL*8     XIN( KMAX )
      REAL*8     XOUT( KMAX )
*
*   [INTERNAL SAVE] 
      REAL*8     DB, DT
      REAL*8     SLEV( KMAX )
      SAVE       SLEV
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
      DATA       DB  / -999.D0 /
      DATA       DT  / -999.D0 /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ SPLINE: VERTICAL SMOOTHING 05/05/25 '
         OFIRST = .FALSE.
*
         DO 1000 K = 1, KMAX
            SLEV( K ) = SIG( KMAX+1-K )
 1000    CONTINUE
      ENDIF
*
      DO 2000 IJ = 1, IJDIM
         DO 2100 K = 1, KMAX
            XIN( K ) = GDX( IJ,KMAX+1-K )
 2100    CONTINUE
*
         CALL SPLINE
     I         ( XIN , SLEV , DB  , DT  )
         CALL SPLINT
     O         ( XOUT,  
     I           XIN , SLEV    )
*
         DO 2200 K = 1, KMAX
            GDXS( IJ,KMAX+1-K ) = XOUT( K )
 2200    CONTINUE
 2000 CONTINUE
*
      RETURN
      END
***********************************************************************
      SUBROUTINE SPLINE    !! 2nd derivative for cubic spline 
     I         ( XIN   , SIG   , YP1  , YPN   )
*
*     cubic spline following numerical recipes
*     !!CALLED ONCE!!
*
*     if YP1 and/or YPN given as -999. then the lower/upper
*     boundary condition is set at 0.
*
*   [PARAMETER]
#include        "zcdim.F"                /* # of grid point & wave */
*
      REAL*8    YP1, YPN
      REAL*8    SIG( KMAX )
      REAL*8    XIN( KMAX )
      REAL*8    XDER( KMAX )
      REAL*8    XOUT( KMAX )

      SAVE      XDER

      INTEGER   I, K, KK
      INTEGER   KHI, KLO
      REAL*8    P, QN, S, UN, U( KMAX )
      REAL*8    A, B, H
*
      IF( YP1 .LE. -999.D0 ) THEN
         XDER( 1 ) = 0.D0
         U( 1 ) = 0.D0
      ELSE
         XDER( 1 ) = -0.5D0
         U( 1 ) =   ( 3.D0 / ( SIG( 2 ) - SIG( 1 ) ) )
     &            * ( ( XIN( 2 ) - XIN( 1 ) ) / 
     &              ( SIG( 2 ) - SIG( 1 ) ) - YP1 )
      ENDIF
*
      DO 1000 I = 2, KMAX-1
         S = ( SIG( I ) - SIG( I-1 ) ) /
     &       ( SIG( I+1 ) - SIG( I-1 ) )
         P = S * XDER( I-1 ) + 2.D0
         XDER( I ) = ( S-1.D0 ) / P
         U( I ) = ( 6.D0 * 
     &               ( ( XIN( I+1 ) - XIN( I ) ) /
     &                 ( SIG( I+1 ) - SIG( I ) )
     &               - ( XIN( I ) - XIN( I-1 ) ) /
     &                 ( SIG( I ) - SIG( I-1 ) ) ) / 
     &              ( SIG( I+1 ) - SIG( I-1 ) )
     &              - S * U( I-1 )                   ) / P
 1000 CONTINUE
*
      IF( YPN .LE. -999.D0 ) THEN
         QN = 0.D0
         UN = 0.D0
      ELSE
         QN = 0.5D0
         UN = ( 3.D0 / ( SIG( KMAX ) - SIG( KMAX-1 ) ) )
     &            * ( YPN - ( XIN( KMAX ) - XIN( KMAX-1 ) ) / 
     &              ( SIG( KMAX ) - SIG( KMAX-1 ) ) )
      ENDIF
      XDER( KMAX ) = ( UN - QN * U( KMAX-1 ) ) / 
     &                ( QN * XDER( KMAX-1 ) + 1.D0 )
*
      DO 1100 K = KMAX-1, 1, -1
         XDER( K ) = XDER( K ) * XDER( K+1 ) + U( K )
 1100 CONTINUE
*
      RETURN
*======================================================================
      ENTRY     SPLINT          !! spline interpolation
     O     (   XOUT  ,
     I         XIN   , SIG   )
*
*     calculate interpolated value using the cubic spline 
*     polynomials
*
      DO 2000 K = 1, KMAX
*
         KLO = 1
         KHI = KMAX
 4000    IF( KHI - KLO .GT. 1 ) THEN
            KK = ( KHI + KLO ) / 2
            IF( SIG( KK ) .GT. SIG( K ) ) THEN
               KHI = KK
            ELSE
               KLO = KK
            ENDIF
            GOTO 4000
         ENDIF
*
         H = SIG( KHI ) - SIG( KLO )
         IF( H .EQ. 0.D0 ) THEN
            WRITE( 6,* ) ' ### BAD XA INPUT IN SPLINT ### '
            CALL XABORT( 1 )
            RETURN
         ENDIF
         A = ( SIG( KHI ) - SIG( K ) ) / H
         B = ( SIG( K ) - SIG( KLO ) ) / H
         XOUT( K ) =   A * XIN( KLO )
     &               + B * XIN( KHI )
     &               + ( ( A**3 - A ) * XDER( KLO )
     &                 + ( B**3 - B ) * XDER( KHI ) )
     &                 * ( H**2 ) / 6.D0 
 2000 CONTINUE
*
      RETURN
      END
