* PACKAGE DGDYN   !! dynamics: non-linear term
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         99/01/01(hiro)     LBM1.0
*         01/05/16(hiro)     include PSDOTB
*         01/05/16(hiro)     linearize GRQADV
***********************************************************************
      SUBROUTINE PSDOT     !!  surface pres. change & sigma velocity
     O        ( GSIGD , GSUMP , GAPADV, GPID  ,
     I          GAU   , GAV   , GADIV , GAPIX , GAPIY ,
     I          GFU   , GFV   , GFPIX , GFPIY  )
*
*   [PARAMETER]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT]
      REAL*8     GSIGD ( IJDIM, KMAX+1 ) !! tendency of sigma
      REAL*8     GSUMP ( IJDIM, KMAX )   !! vert. integ. of tend. of pi
      REAL*8     GAPADV( IJDIM, KMAX )   !! advection of pi
      REAL*8     GPID  ( IJDIM )         !! pi tendency
*     
*   [INPUT]
      REAL*8     GAU   ( IJDIM, KMAX )   !! westerly U(t)
      REAL*8     GAV   ( IJDIM, KMAX )   !! southern wind V(t)
      REAL*8     GADIV ( IJDIM, KMAX )   !! divergence D(t)
      REAL*8     GAPIX ( IJDIM       )   !! zonal deriv. of pi
      REAL*8     GAPIY ( IJDIM       )   !! merid. differ. of pi
*
      REAL*8     GFU   ( IJDIM, KMAX )   !! westerly U(t) for BS
      REAL*8     GFV   ( IJDIM, KMAX )   !! southern wind V(t) for BS
      REAL*8     GFPIX ( IJDIM       )   !! zonal deriv. of pi for BS
      REAL*8     GFPIY ( IJDIM       )   !! merid. differ. of pi for BS
*
*   [INTERNAL WORK]
      INTEGER    IJ, K
*
*          < PADV: Ps adv. term >
*
      DO 1100 K = 1, KMAX
         DO 1100 IJ = 1, IJDIM
            GAPADV( IJ,K ) =  
     $             GAU( IJ,K ) * GFPIX( IJ )
     $           + GFU( IJ,K ) * GAPIX( IJ )
     $           + GAV( IJ,K ) * GFPIY( IJ )
     $           + GFV( IJ,K ) * GAPIY( IJ )
 1100 CONTINUE
*
*          < PTEN: Ps tend. term >
*
      K = KMAX
      DO 2100 IJ = 1, IJDIM
            GSUMP( IJ,K ) = ( GADIV( IJ,K ) + GAPADV( IJ,K ) )
     &                       * DSIG( K )
 2100 CONTINUE
      DO 2200 K = KMAX-1, 1, -1
         DO 2200 IJ = 1, IJDIM
            GSUMP( IJ,K ) = GSUMP ( IJ,K+1 )
     &                    + ( GADIV ( IJ,K ) + GAPADV( IJ,K ) )
     &                       * DSIG  ( K )
 2200 CONTINUE
*
*          < SIGD: vert sig. vel. >
*
      DO 4100 K = 2, KMAX
         DO 4100 IJ = 1, IJDIM
            GSIGD( IJ,K ) = SIGM( K )*GSUMP( IJ,1 ) - GSUMP( IJ,K )
 4100 CONTINUE
*
      DO 4200 IJ = 1, IJDIM
         GSIGD( IJ,1 )      = 0.  
         GSIGD( IJ,KMAX+1 ) = 0.  
 4200 CONTINUE
*
      DO 4300 IJ = 1, IJDIM
         GPID( IJ ) = - GSUMP( IJ,1 )
 4300 CONTINUE
*
      RETURN
      END
**********************************************************************
      SUBROUTINE PSDOTB         !!  surface pres. change & sigma velocity
     O       (  GFSIGD, GFSUMP, GFPADV, GFPID  ,
     I          GFU   , GFV   , GFDIV , GFPIX , GFPIY  )
*
*   [PARAMETER]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT]
      REAL*8     GFSIGD ( IJDIM, KMAX+1 ) !! tendency of sigma
      REAL*8     GFSUMP ( IJDIM, KMAX )   !! vert. integ. of tend. of pi
      REAL*8     GFPADV( IJDIM, KMAX )    !! advection of pi
      REAL*8     GFPID  ( IJDIM )         !! pi tendency
*     
*   [INPUT]
      REAL*8     GFU   ( IJDIM, KMAX )   !! westerly U(t)
      REAL*8     GFV   ( IJDIM, KMAX )   !! southern wind V(t)
      REAL*8     GFDIV ( IJDIM, KMAX )   !! divergence D(t)
      REAL*8     GFPIX ( IJDIM       )   !! zonal deriv. of pi
      REAL*8     GFPIY ( IJDIM       )   !! merid. differ. of pi
*
*   [INTERNAL WORK]
      INTEGER    IJ, K
*
*          < PADV: Ps adv. term >
*
      DO 1100 K = 1, KMAX
         DO 1100 IJ = 1, IJDIM
            GFPADV( IJ,K ) =  
     $             GFU( IJ,K ) * GFPIX( IJ )
     $           + GFV( IJ,K ) * GFPIY( IJ )
 1100 CONTINUE
*
*          < PTEN: Ps tend. term >
*
      K = KMAX
      DO 2100 IJ = 1, IJDIM
            GFSUMP( IJ,K ) = ( GFDIV( IJ,K ) + GFPADV( IJ,K ) )
     &                       * DSIG( K )
 2100 CONTINUE
      DO 2200 K = KMAX-1, 1, -1
         DO 2200 IJ = 1, IJDIM
            GFSUMP( IJ,K ) = GFSUMP ( IJ,K+1 )
     &                    + ( GFDIV ( IJ,K ) + GFPADV( IJ,K ) )
     &                       * DSIG  ( K )
 2200 CONTINUE
*
*          < SIGD: vert sig. vel. >
*
      DO 4100 K = 2, KMAX
         DO 4100 IJ = 1, IJDIM
            GFSIGD( IJ,K ) = SIGM( K )*GFSUMP( IJ,1 ) - GFSUMP( IJ,K )
 4100 CONTINUE
*
      DO 4200 IJ = 1, IJDIM
         GFSIGD( IJ,1 )      = 0.  
         GFSIGD( IJ,KMAX+1 ) = 0.  
 4200 CONTINUE
*
      DO 4300 IJ = 1, IJDIM
         GFPID( IJ ) = - GFSUMP( IJ,1 )
 4300 CONTINUE
*
      RETURN
      END
*******************************************************************
      SUBROUTINE GRUADV    !!  momentum advection
     O         ( GTUA  , GTVA  , GTKE  ,
     I           GAU   , GAV   , GAVOR , 
     I           GFU   , GFV   , GFVOR , 
     I           GAPIX , GAPIY , GSIGD , GATV  , GAT   ,
     I           GFPIX , GFPIY , GFSIGD, GFTV  ,
     C           CORIOL,
     C           SALPHA, SBETA , SKAPPA, TBAR           )
*
*   [PARAMETER]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
*
*   [MODIFY]
      REAL*8     GTUA  ( IJDIM, KMAX )   !! u advection UA
      REAL*8     GTVA  ( IJDIM, KMAX )   !! merid. adv. of momentum
      REAL*8     GTKE  ( IJDIM, KMAX )   !! kinetic energy term KE
*
*   [INPUT]
      REAL*8     GAU   ( IJDIM, KMAX )   !! westerly U(t)
      REAL*8     GAV   ( IJDIM, KMAX )   !! southern wind V(t)
      REAL*8     GAVOR ( IJDIM, KMAX )   !! vorticity zeta(t)
      REAL*8     GAPIX ( IJDIM       )   !! zonal deriv. of pi
      REAL*8     GAPIY ( IJDIM       )   !! merid. differ. of pi
      REAL*8     GSIGD ( IJDIM, KMAX+1 ) !! tendency of sigma
      REAL*8     GATV  ( IJDIM, KMAX )   !! Tv
      REAL*8     GAT   ( IJDIM, KMAX )   !! T
*
      REAL*8     GFU   ( IJDIM, KMAX )   !! BS westerly
      REAL*8     GFV   ( IJDIM, KMAX )   !! BS southern wind
      REAL*8     GFVOR ( IJDIM, KMAX )   !! BS vorticity 
      REAL*8     GFPIX ( IJDIM       )   !! zonal deriv. of for BS pi
      REAL*8     GFPIY ( IJDIM       )   !! merid. differ. of for BS pi
      REAL*8     GFSIGD ( IJDIM, KMAX+1 ) !! tendency of for BS sigma
      REAL*8     GFTV  ( IJDIM, KMAX )   !! Tv for BS
*
      REAL*8     CORIOL( IJDIM )         !! Corioris factor f
      REAL*8     SALPHA( KMAX )          !! fact. of hydrostatic eq.
      REAL*8     SBETA ( KMAX )          !! fact. of hydrostatic eq.
      REAL*8     SKAPPA( KMAX )          !! fact. of vert. intrp. T
      REAL*8     TBAR  ( KMAX )          !! basic temp. (full lev)
      REAL*8     TABAR ( KMAX )          !! basic temp. 
      DATA       TABAR / KMAX*0.D0 /
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K
*
* << UVADV : UA,VA-terms >>
*
      DO 1100 K = 1, KMAX
         DO 1100 IJ = 1, IJDIM
            GTUA( IJ,K ) =   
     &                     + GAV ( IJ,K )
     &                       * ( GFVOR( IJ,K ) + CORIOL ( IJ )  )
     &                     + GFV ( IJ,K )
     &                       * ( GAVOR( IJ,K )                  )
     &                     - CP * SKAPPA(K)
     &                          * ( GATV ( IJ,K ) - TABAR( K )  )
     &                          * GFPIX( IJ )
     &                     - CP * SKAPPA(K)
     &                          * ( GFTV ( IJ,K ) - TBAR( K ) )
     &                          * GAPIX( IJ )
*
            GTVA( IJ,K ) = 
     &                     - GAU ( IJ,K )
     &                       * ( GFVOR( IJ,K ) + CORIOL ( IJ )  )
     &                     - GFU ( IJ,K )
     &                       * ( GAVOR( IJ,K )                  )
     &                     - CP * SKAPPA(K)
     &                          * ( GATV ( IJ,K ) - TABAR( K )  )
     &                          * GFPIY( IJ )
     &                     - CP * SKAPPA(K)
     &                          * ( GFTV ( IJ,K ) - TBAR( K ) )
     &                          * GAPIY( IJ )
*
 1100 CONTINUE
*
      DO 1200 K = 2, KMAX
         DO 1200 IJ = 1, IJDIM
            GTUA( IJ,K ) =  GTUA( IJ,K )
     &           -  GSIGD( IJ,K ) / ( DSIG (K) * 2.D0 )
     &           * ( GFU  ( IJ,K-1 ) - GFU  ( IJ,K   ) )
     &           -  GFSIGD( IJ,K ) / ( DSIG (K) * 2.D0 )
     &           * ( GAU  ( IJ,K-1 ) - GAU  ( IJ,K   ) )
*
            GTVA( IJ,K ) =  GTVA( IJ,K )
     &           -  GSIGD( IJ,K ) / ( DSIG (K) * 2.D0 )
     &           * ( GFV  ( IJ,K-1 ) - GFV  ( IJ,K   ) )
     &           -  GFSIGD( IJ,K ) / ( DSIG (K) * 2.D0 )
     &           * ( GAV  ( IJ,K-1 ) - GAV  ( IJ,K   ) )
*
 1200 CONTINUE
*
      DO 1300 K = 1, KMAX-1
         DO 1300 IJ = 1, IJDIM
            GTUA( IJ,K ) =  GTUA( IJ,K )
     &           -  GSIGD( IJ,K+1 ) / ( DSIG (K) * 2.D0 )
     &           * ( GFU  ( IJ,K   ) - GFU  ( IJ,K+1 ) )
     &           -  GFSIGD( IJ,K+1 ) / ( DSIG (K) * 2.D0 )
     &           * ( GAU  ( IJ,K   ) - GAU  ( IJ,K+1 ) )
*
            GTVA( IJ,K ) =  GTVA( IJ,K )
     &           -  GSIGD( IJ,K+1 ) / ( DSIG (K) * 2.D0 )
     &           * ( GFV  ( IJ,K   ) - GFV  ( IJ,K+1 ) )
     &           -  GFSIGD( IJ,K+1 ) / ( DSIG (K) * 2.D0 )
     &           * ( GAV  ( IJ,K   ) - GAV  ( IJ,K+1 ) )
*
 1300 CONTINUE
*
*  << KE : KE term + vert. temp. correc. >>
*
      DO 2100 IJ = 1, IJDIM
            GTKE( IJ,1 ) = 
     &        CP * SALPHA( 1   ) * ( GATV( IJ,1   ) - GAT( IJ,1   ) )
 2100 CONTINUE
      DO 2200 K = 2, KMAX
         DO 2200 IJ = 1, IJDIM
            GTKE( IJ,K ) = GTKE( IJ,K-1 ) 
     &                   + CP * SALPHA( K   )
     &                        *( GATV( IJ,K   ) - GAT( IJ,K   ) )
     &                   + CP * SBETA ( K-1 )
     &                        *( GATV( IJ,K-1 ) - GAT( IJ,K-1 ) )
 2200 CONTINUE
*
      DO 2300 K = 1, KMAX
         DO 2300 IJ = 1, IJDIM
            GTKE( IJ,K ) = GTKE( IJ,K )
     &           + (  GAU( IJ,K )**2 + GAV( IJ,K )**2  
     &           + 2.D0*GAU(IJ,K)*GFU(IJ,K) + 2.D0*GAV(IJ,K)*GFV(IJ,K) )
     $           /2.D0  
 2300 CONTINUE
*
      RETURN
      END
*******************************************************************
      SUBROUTINE GRTADV    !!  temperature advection
     O         ( GTUT  , GTVT  , GTT   ,
     I           GAT   , GATV  ,
     I           GFT   , GFTV  ,
     I           GAU   , GAV   , GADIV , 
     I           GFU   , GFV   , GFDIV , 
     I           GSIGD , GSUMP , GAPADV,
     I           GFSIGD, GFSUMP, GFPADV,
     C           SALPHA, SBETA , SKAPPA,
     C           ATF   , BTF   , TBAR   )
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT]
      REAL*8     GTUT  ( IJDIM, KMAX )   !! zonal adv. of temp.
      REAL*8     GTVT  ( IJDIM, KMAX )   !! merid. adv. of temp.
      REAL*8     GTT   ( IJDIM, KMAX )   !! tend. term of T
*
*   [INPUT]
      REAL*8     GAT   ( IJDIM, KMAX )   !! T:temperature
      REAL*8     GFT   ( IJDIM, KMAX )   !! T:temperature for BS
      REAL*8     GATV  ( IJDIM, KMAX )   !! Tv:virtual temperature
      REAL*8     GFTV  ( IJDIM, KMAX )   !! Tv:virtual temp. for BS
      REAL*8     GAU   ( IJDIM, KMAX )   !! westerly U(t)
      REAL*8     GAV   ( IJDIM, KMAX )   !! southern wind V(t)
      REAL*8     GADIV ( IJDIM, KMAX )   !! divergence D(t)
      REAL*8     GFU   ( IJDIM, KMAX )   !! westerly U(t) for BS
      REAL*8     GFV   ( IJDIM, KMAX )   !! southern wind V(t) for BS
      REAL*8     GFDIV ( IJDIM, KMAX )   !! divergence D(t) for BS
      REAL*8     GSIGD ( IJDIM, KMAX+1 ) !! sigma velocity
      REAL*8     GFSIGD( IJDIM, KMAX+1 ) !! BS sigma velocity
      REAL*8     GSUMP ( IJDIM, KMAX )   !! vert. integ. D tend.
      REAL*8     GFSUMP( IJDIM, KMAX )   !! vert. integ. D tend. for BS
      REAL*8     GAPADV( IJDIM, KMAX )   !! advection of pai
      REAL*8     GFPADV( IJDIM, KMAX )   !! advection of pai for BS
*
      REAL*8     SALPHA( KMAX )          !! fact. of hydrostatic eq.
      REAL*8     SBETA ( KMAX )          !! fact. of hydrostatic eq.
      REAL*8     SKAPPA( KMAX )          !! fact. of vert. intrp. T
      REAL*8     ATF   ( KMAX )          !! fact. of vert. intrp. T
      REAL*8     BTF   ( KMAX )          !! fact. of vert. intrp. T
      REAL*8     TBAR  ( KMAX )          !! basic temp. (full lev)
      REAL*8     TABAR ( KMAX )          !! basic temp. 
      DATA       TABAR / KMAX*0.D0 /
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K
*
*  << THADV : UT',VT' >>
*
      DO 1100 K = 1, KMAX
         DO 1100 IJ = 1, IJDIM
            GTUT( IJ,K ) = 
     $             GAU ( IJ,K ) * ( GFT ( IJ,K ) - TBAR( K )  )
     $           + GFU ( IJ,K ) * ( GAT ( IJ,K ) - TABAR( K ) )
            GTVT( IJ,K ) = 
     $             GAV ( IJ,K ) * ( GFT ( IJ,K ) - TBAR( K )  )
     $           + GFV ( IJ,K ) * ( GAT ( IJ,K ) - TABAR( K ) )
 1100 CONTINUE
*
*  << TVADV : H=T'D+kTw/p+sig adv. >>
*
      DO 2100 K = 1, KMAX
        DO 2100 IJ = 1, IJDIM
            GTT ( IJ,K ) = 
     $            ( GAT ( IJ,K ) - TABAR( K ) ) * GFDIV( IJ,K )
     $          + ( GFT ( IJ,K ) - TBAR( K )  ) * GADIV( IJ,K )
 2100 CONTINUE
*
      DO 2200 K = 1, KMAX
        DO 2200 IJ = 1, IJDIM
            GTT ( IJ,K ) =  GTT ( IJ,K )
     $          +  SKAPPA( K ) * GATV ( IJ,K ) * GFPADV( IJ,K )
     $          +  SKAPPA( K ) * GFTV ( IJ,K ) * GAPADV( IJ,K )
     $          -  SALPHA( K ) / DSIG ( K )
     $          * GATV ( IJ,K ) * GFSUMP ( IJ,K )
     $          -  SALPHA( K ) / DSIG ( K )
     $          * GFTV ( IJ,K ) * GSUMP ( IJ,K )
 2200 CONTINUE
*
      DO 2300 K = 2, KMAX
        DO 2300 IJ = 1, IJDIM
            GTT ( IJ,K ) =  GTT ( IJ,K )
     $          -  GFSIGD( IJ,K ) / DSIG ( K )
     $          * (ATF(K)*GAT(IJ,K)+BTF(K-1)*GAT(IJ,K-1)-GAT(IJ,K))
     $          -  GSIGD ( IJ,K ) / DSIG ( K )
     $          * (ATF(K)*GFT(IJ,K)+BTF(K-1)*GFT(IJ,K-1)-GFT(IJ,K))
 2300 CONTINUE
*
      DO 2400 K = 1, KMAX-1
        DO 2400 IJ = 1, IJDIM
            GTT ( IJ,K ) = GTT ( IJ,K )
     $          -  GFSIGD ( IJ,K+1 )  / DSIG ( K )
     $          * (GAT(IJ,K )-ATF(K+1)*GAT(IJ,K+1)-BTF(K)*GAT(IJ,K))
     $          -  SBETA ( K ) / DSIG ( K )
     $          *  GATV ( IJ,K ) * GFSUMP ( IJ,K+1 )
     $          -  GSIGD ( IJ,K+1 )  / DSIG ( K )
     $          * (GFT(IJ,K )-ATF(K+1)*GFT(IJ,K+1)-BTF(K)*GFT(IJ,K))
     $          -  SBETA ( K ) / DSIG ( K )
     $          *  GFTV ( IJ,K ) * GSUMP ( IJ,K+1 )
 2400 CONTINUE
*
      RETURN
      END
*******************************************************************
      SUBROUTINE GRQADV    !!  adv. of humidity etc.
     O         ( GTUQ  , GTVQ  , GTQ   ,
     I           GAQ   , GFQ   ,
     I           GAU   , GAV   , GADIV , GSIGD , 
     I           GFU   , GFV   , GFDIV , GFSIGD     )
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc.           */
#include        "zcord.F"                /* coordinate             */
*
*   [MODIFY]
*
*   [OUTPUT]
      REAL*8     GTUQ  ( IJDIM, KMAX  )  !! zonal advection Uq
      REAL*8     GTVQ  ( IJDIM, KMAX  )  !! meridional advection Vq
      REAL*8     GTQ   ( IJDIM, KMAX  )  !! tend. of q
*
*   [INPUT
      REAL*8     GAQ   ( IJDIM, KMAX )   !! humidity 
      REAL*8     GAU   ( IJDIM, KMAX )   !! westerly U(t)
      REAL*8     GAV   ( IJDIM, KMAX )   !! southern wind V(t)
      REAL*8     GADIV ( IJDIM, KMAX )   !! divergence D(t)
      REAL*8     GSIGD( IJDIM, KMAX+1 )  !! sigma velocity
      REAL*8     GFQ   ( IJDIM, KMAX )   !! BS humidity 
      REAL*8     GFU   ( IJDIM, KMAX )   !! BS westerly U(t) 
      REAL*8     GFV   ( IJDIM, KMAX )   !! BS southern wind V(t) 
      REAL*8     GFDIV ( IJDIM, KMAX )   !! BS divergence D(t) 
      REAL*8     GFSIGD( IJDIM, KMAX+1 ) !! BS sigma velocity
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K
*
*  << THADV : Uq, Vq >>
*
      DO 1100 K = 1, KMAX
         DO 1100 IJ = 1, IJDIM
            GTUQ( IJ,K ) =  
     &                      GAU ( IJ,K ) * GFQ ( IJ,K )
     &                    + GFU ( IJ,K ) * GAQ ( IJ,K )
            GTVQ( IJ,K ) =  
     &                      GAV ( IJ,K ) * GFQ ( IJ,K )
     &                    + GFV ( IJ,K ) * GAQ ( IJ,K )
 1100 CONTINUE
*
*  << TVADV : sigma adv. >>
*
      DO 2100 K = 1, KMAX
         DO 2100 IJ = 1, IJDIM
            GTQ ( IJ,K ) = 
     &                      GAQ ( IJ,K ) * GFDIV( IJ,K )
     &                    + GFQ ( IJ,K ) * GADIV( IJ,K )
 2100 CONTINUE
*
      DO 2200 K = 2, KMAX
         DO 2200 IJ = 1, IJDIM
            GTQ ( IJ,K ) = GTQ ( IJ,K )
     &           -  GSIGD ( IJ,K ) / ( 2.D0 * DSIG ( K ) )
     &               * ( GFQ ( IJ,K-1 ) - GFQ ( IJ,K )  )
     &           -  GFSIGD( IJ,K ) / ( 2.D0 * DSIG ( K ) )
     &               * ( GAQ ( IJ,K-1 ) - GAQ ( IJ,K )  )
 2200 CONTINUE
*
      DO 2300 K = 1, KMAX-1
         DO 2300 IJ = 1, IJDIM
            GTQ ( IJ,K ) = GTQ ( IJ,K )
     &           -  GSIGD ( IJ,K+1 )  / ( 2.D0 * DSIG ( K ) )
     &               * ( GFQ ( IJ,K ) - GFQ  ( IJ,K+1 )  )
     &           -  GFSIGD( IJ,K+1 )  / ( 2.D0 * DSIG ( K ) )
     &               * ( GAQ ( IJ,K ) - GAQ  ( IJ,K+1 )  )
 2300 CONTINUE
*
      RETURN
      END
