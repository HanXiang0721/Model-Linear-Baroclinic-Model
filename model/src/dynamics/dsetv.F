* PACKAGE DSETV !! dynamics: set constants (vertical difference)
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*   [HIS] 95/07/30(numaguti) TBARM
*         96/05/28(numaguti) restructure
*         00/06/08(hiro)     vert. dependence of TBAR
***********************************************************************
      SUBROUTINE DSETVD    !! vertical difference/semi implicit factor
     O        ( SALPHA, SBETA , SKAPPA,
     O          ATF   , BTF   , TBAR  , 
     O          GG    , TMTX  , WHMTX , GCMTX ,
     I          GFT    )
*
*                  Arakawa & Suarez vert.diffr in sigma
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT] 
      REAL*8     SALPHA( KMAX )              !! fact. of hydrostatic eq.
      REAL*8     SBETA ( KMAX )              !! fact. of hydrostatic eq.
      REAL*8     SKAPPA( KMAX )              !! fact. of vert. intrp. T
      REAL*8     ATF   ( KMAX )              !! fact. of vert. intrp. T
      REAL*8     BTF   ( KMAX )              !! fact. of vert. intrp. T
      REAL*8     TBAR  ( KMAX )              !! basic temp. (full lev)
*
*   [INPUT] 
      REAL*8     GFT   ( IJDIM, KMAX )       !! basic state temperature
*
      REAL*8     GG    ( KMAX )              !! for semi-implicit
      REAL*8     TMTX  ( KMAX, KMAX )        !! for semi-implicit
      REAL*8     WHMTX ( KMAX, KMAX )        !! for semi-implicit
      REAL*8     GCMTX ( KMAX, KMAX )        !! for semi-implicit
*
*   [INTERNAL WORK] 
      REAL*8     TBARM ( KMAX )              !! basic temp. (half lev)
      REAL*8     WW    ( KMAX, KMAX )        !! W
      REAL*8     SS    ( KMAX, KMAX )        !! S
      REAL*8     QQ    ( KMAX, KMAX )        !! Q
      REAL*8     RR    ( KMAX, KMAX )        !! R
      REAL*8     AKAPPA                      !! R/Cp
      INTEGER    IFPAR, JFPAR
      INTEGER    K, L, M, IJ
*
*   [INTERNAL PARM] 
      REAL*8     TBAR0                       !! basic temperature
      LOGICAL    OTBAR0
      LOGICAL    OFIRST 
      NAMELIST   /NMTBAR/ OTBAR0 , TBAR0
      DATA       TBAR0   / 300.   /
      DATA       OTBAR0  / .FALSE. /
      DATA       OFIRST  / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DSETV: SET V.COEFFICIENTS 00/06/08'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMTBAR, END=190 )
  190    WRITE  ( JFPAR, NMTBAR )
      ENDIF
*
      AKAPPA = RAIR / CP
*
*          < 1. hydrostatic eq. >
*
      CALL DSETVH
     O     ( SALPHA, SBETA  )
*
*          < 2. vertical differencing >
*
      DO 2100 K = 1, KMAX
         SKAPPA( K ) =  (    SIGM( K   ) * SALPHA( K )
     &                     + SIGM( K+1 ) * SBETA ( K )    )
     &                 /(    SIGM( K   ) - SIGM  ( K+1 )  )
         TBAR( K ) = 0.D0
         TBARM( K ) = 0.D0
 2100 CONTINUE
*
      DO 2200 K = 2, KMAX
         ATF ( K ) =   SALPHA( K )
     &              / (   1. - ( SIG( K ) / SIG( K-1 ) )**AKAPPA   )
 2200 CONTINUE
      DO 2300 K = 1, KMAX-1
         BTF ( K ) =   SBETA ( K )
     &              / (   ( SIG( K ) / SIG( K+1 ) )**AKAPPA - 1.   )
 2300 CONTINUE
      ATF ( KMAX          ) = 0.5D0
      BTF ( MAX(KMAX-1,1) ) = 0.5D0
*
      IF( OTBAR0 ) THEN
         DO 2400 K = 1, KMAX
            TBAR  ( K ) = TBAR0
            TBARM ( K ) = TBAR0
 2400    CONTINUE
      ELSE
         DO 2450 K = 1, KMAX
            DO 2450 IJ = 1, IJDIM
               TBAR( K ) = TBAR( K ) + GFT( IJ, K) / DBLE( IJDIM )
 2450    CONTINUE
         TBARM( 1 ) = TBAR( 1 )
         DO 2500 K = 2, KMAX
            TBARM ( K ) =   ATF ( K   ) * TBAR ( K   )
     &           + BTF ( K-1 ) * TBAR ( K-1 )
 2500    CONTINUE
      ENDIF
*
*          < 3. G=CpkT, W: phi=WT >
*
      DO 3100 K = 1, KMAX
         GG ( K ) = CP * SKAPPA( K ) * TBAR( K )
 3100 CONTINUE
*
      CALL RESET ( WW, KMAX*KMAX )
      DO 3200 K = 1, KMAX
         DO 3210 L = 1, K
            WW( K,L ) =             CP * SALPHA( L )
 3210   CONTINUE
         DO 3220 L = 1, K-1
            WW( K,L ) = WW( K,L ) + CP * SBETA( L )
 3220    CONTINUE
 3200 CONTINUE
*
*          < 4. S:dsig/dt=SD, Q: dsig/dt dT/dsig = QSD,  >
*
      DO 4100 K = 1, KMAX
         DO 4110 L = 1, KMAX
            SS( K,L ) =   SIGM( K ) * DSIG( L )
 4110    CONTINUE
         DO 4120 L = K, KMAX
            SS( K,L ) =   SS( K,L ) - DSIG( L )
 4120    CONTINUE
 4100 CONTINUE
*
      CALL RESET ( QQ, KMAX*KMAX )
      DO 4210 K = 1, KMAX
         QQ( K,K   ) = ( TBARM( K ) - TBAR ( K   ) ) / DSIG( K )
 4210 CONTINUE
      DO 4220 K = 1, KMAX-1
         QQ( K,K+1 ) = ( TBAR ( K ) - TBARM( K+1 ) ) / DSIG( K )
 4220 CONTINUE
*
*          < 5. R: kT(dpi/dt+dsig/dt/sig) = R D >
*
      CALL RESET ( RR, KMAX*KMAX )
      DO 5100 K = 1, KMAX
         DO 5110 L = K, KMAX
            RR( K,L ) = - SALPHA( K ) / DSIG( K ) * DSIG( L )
     &                    * TBAR( K )
 5110    CONTINUE
         DO 5120 L = K+1, KMAX
            RR( K,L ) = RR( K,L )
     &                   - SBETA ( K ) / DSIG( K ) * DSIG( L )
     &                     * TBAR( K )
 5120    CONTINUE
 5100 CONTINUE
*
*          < 6. Q S, H=QS-R, Wh, G tC >
*
      CALL RESET( TMTX, KMAX*KMAX )
      DO 6100 K = 1, KMAX
         DO 6100 L = 1, KMAX
            DO 6100 M = 1, KMAX
               TMTX( K,L ) = TMTX( K,L ) + QQ( K,M ) * SS( M,L )
 6100 CONTINUE
*
      DO 6200 K = 1, KMAX
         DO 6200 L = 1, KMAX
            TMTX( K,L ) = TMTX( K,L ) - RR( K,L )
 6200 CONTINUE
*
      CALL RESET ( WHMTX, KMAX*KMAX )
      DO 6300 K = 1, KMAX
         DO 6300 L = 1, KMAX
            DO 6300 M = 1, KMAX
               WHMTX( K,L ) = WHMTX( K,L ) + WW( K,M ) * TMTX( M,L )
 6300 CONTINUE
*
      DO 6400 K = 1, KMAX
         DO 6400 L = 1, KMAX
               GCMTX( K,L ) = GG( K ) * DSIG( L )
 6400 CONTINUE
*
      RETURN
      END
***********************************************************************
      SUBROUTINE DSETVH    !! factor of hydrostatic relation
     O         ( SALPHA, SBETA  )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT] 
      REAL*8     SALPHA( KMAX )              !! fact. of hydrostatic eq.
      REAL*8     SBETA ( KMAX )              !! fact. of hydrostatic eq.
*
*   [INTERNAL WORK] 
      REAL*8     AKAPPA
      INTEGER    K
*
      AKAPPA = RAIR / CP
*
      DO 1100 K = 1, KMAX
         SALPHA( K ) =        ( SIGM( K   )/SIG ( K ) )**AKAPPA - 1.  
         SBETA ( K ) = 1.   - ( SIGM( K+1 )/SIG ( K ) )**AKAPPA
 1100 CONTINUE
*
      RETURN
      END
