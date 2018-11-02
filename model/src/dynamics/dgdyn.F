* PACKAGE DGDYN   !! dynamics: non-linear term
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*
**********************************************************************
      SUBROUTINE PSDOT     !!  surface pres. change & sigma velocity
     O        ( GSIGD , GSUMP , GAPADV, GPID  ,
     I          GAU   , GAV   , GADIV , GAPIX , GAPIY  )
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
*   [INTERNAL WORK]
      INTEGER    IJ, K
*
*          < PADV: Ps adv. term >
*
      DO 1100 K = 1, KMAX
         DO 1100 IJ = 1, IJDIM
            GAPADV( IJ,K ) =  GAU( IJ,K ) * GAPIX( IJ )
     &                      + GAV( IJ,K ) * GAPIY( IJ )
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
*******************************************************************
      SUBROUTINE GRUADV    !!  momentum advection
     O         ( GTUA  , GTVA  , GTKE  ,
     I           GAU   , GAV   , GAVOR , 
     I           GAPIX , GAPIY , GSIGD , GATV  , GAT   ,
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
      REAL*8     CORIOL( IJDIM )         !! Corioris factor f
      REAL*8     SALPHA( KMAX )          !! fact. of hydrostatic eq.
      REAL*8     SBETA ( KMAX )          !! fact. of hydrostatic eq.
      REAL*8     SKAPPA( KMAX )          !! fact. of vert. intrp. T
      REAL*8     TBAR  ( KMAX )          !! basic temp. (full lev)
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K
*
* << UVADV : UA,VA-terms >>
*
      DO 1100 K = 1, KMAX
         DO 1100 IJ = 1, IJDIM
            GTUA( IJ,K ) =   GAV ( IJ,K )
     &                       * ( GAVOR( IJ,K ) + CORIOL ( IJ )  )
     &                     - CP * SKAPPA(K)
     &                          * ( GATV ( IJ,K ) - TBAR(K) )
     &                          * GAPIX( IJ )
*
            GTVA( IJ,K ) = - GAU ( IJ,K )
     &                       * ( GAVOR( IJ,K ) + CORIOL ( IJ )  )
     &                     - CP * SKAPPA(K)
     &                          * ( GATV ( IJ,K ) - TBAR(K) )
     &                          * GAPIY( IJ )
*
 1100 CONTINUE
*
      DO 1200 K = 2, KMAX
         DO 1200 IJ = 1, IJDIM
            GTUA( IJ,K ) =  GTUA( IJ,K )
     &                    -  GSIGD( IJ,K ) / ( DSIG (K) * 2.D0 )
     &                     * ( GAU  ( IJ,K-1 ) - GAU  ( IJ,K   ) )
*
            GTVA( IJ,K ) =  GTVA( IJ,K )
     &                    -  GSIGD( IJ,K ) / ( DSIG (K) * 2.D0 )
     &                     * ( GAV  ( IJ,K-1 ) - GAV  ( IJ,K   ) )
*
 1200 CONTINUE
*
      DO 1300 K = 1, KMAX-1
         DO 1300 IJ = 1, IJDIM
            GTUA( IJ,K ) =  GTUA( IJ,K )
     &                    -  GSIGD( IJ,K+1 ) / ( DSIG (K) * 2.D0 )
     &                     * ( GAU  ( IJ,K   ) - GAU  ( IJ,K+1 ) )
*
            GTVA( IJ,K ) =  GTVA( IJ,K )
     &                    -  GSIGD( IJ,K+1 ) / ( DSIG (K) * 2.D0 )
     &                     * ( GAV  ( IJ,K   ) - GAV  ( IJ,K+1 ) )
*
 1300 CONTINUE
*
*  << KE : KE term + vert. temp. correc. >>
*
      DO 2100 IJ = 1, IJDIM
            GTKE( IJ,1 ) = CP * SALPHA( 1   )
     &                        *( GATV( IJ,1   ) - GAT( IJ,1   ) )
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
     &                   + (  GAU( IJ,K )**2 + GAV( IJ,K )**2  )/2.D0  
 2300 CONTINUE
*
      RETURN
      END
*******************************************************************
      SUBROUTINE GRTADV    !!  temperature advection
     O         ( GTUT  , GTVT  , GTT   ,
     I           GAT   , GATV  ,
     I           GAU   , GAV   , GADIV , 
     I           GSIGD , GSUMP , GAPADV, 
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
*   [INPUT
      REAL*8     GAT   ( IJDIM, KMAX )   !! T:temperature
      REAL*8     GATV  ( IJDIM, KMAX )   !! Tv:virtual temperature
      REAL*8     GAU   ( IJDIM, KMAX )   !! westerly U(t)
      REAL*8     GAV   ( IJDIM, KMAX )   !! southern wind V(t)
      REAL*8     GADIV ( IJDIM, KMAX )   !! divergence D(t)
      REAL*8     GSIGD ( IJDIM, KMAX+1 ) !! sigma velocity
      REAL*8     GSUMP ( IJDIM, KMAX )   !! vert. integ. D tend.
      REAL*8     GAPADV( IJDIM, KMAX )   !! advection of pai
*
      REAL*8     SALPHA( KMAX )          !! fact. of hydrostatic eq.
      REAL*8     SBETA ( KMAX )          !! fact. of hydrostatic eq.
      REAL*8     SKAPPA( KMAX )          !! fact. of vert. intrp. T
      REAL*8     ATF   ( KMAX )          !! fact. of vert. intrp. T
      REAL*8     BTF   ( KMAX )          !! fact. of vert. intrp. T
      REAL*8     TBAR  ( KMAX )          !! basic temp. (full lev)
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K
*
*  << THADV : UT',VT' >>
*
      DO 1100 K = 1, KMAX
         DO 1100 IJ = 1, IJDIM
            GTUT( IJ,K ) =  GAU ( IJ,K ) * ( GAT ( IJ,K ) - TBAR( K ) )
            GTVT( IJ,K ) =  GAV ( IJ,K ) * ( GAT ( IJ,K ) - TBAR( K ) )
 1100 CONTINUE
*
*  << TVADV : H=T'D+kTw/p+sig adv. >>
*
      DO 2100 K = 1, KMAX
        DO 2100 IJ = 1, IJDIM
            GTT ( IJ,K )
     &        =  ( GAT ( IJ,K ) - TBAR( K ) ) * GADIV( IJ,K )
 2100 CONTINUE
*
      DO 2200 K = 1, KMAX
        DO 2200 IJ = 1, IJDIM
            GTT ( IJ,K )
     &       =  GTT ( IJ,K )
     &        +  SKAPPA( K ) * GATV ( IJ,K ) * GAPADV( IJ,K )
     &        -  SALPHA( K ) / DSIG ( K )
     &           * GATV ( IJ,K ) * GSUMP ( IJ,K )
 2200 CONTINUE
*
      DO 2300 K = 2, KMAX
        DO 2300 IJ = 1, IJDIM
            GTT ( IJ,K )
     &       =  GTT ( IJ,K )
     &        -  GSIGD ( IJ,K ) / DSIG ( K )
     &            * (   ATF( K   )* GAT( IJ,K   )
     &                + BTF( K-1 )* GAT( IJ,K-1 )
     &                - GAT( IJ,K )                 )
 2300 CONTINUE
*
      DO 2400 K = 1, KMAX-1
        DO 2400 IJ = 1, IJDIM
            GTT ( IJ,K )
     &       =  GTT ( IJ,K )
     &        -  GSIGD ( IJ,K+1 )  / DSIG ( K )
     &            * ( GAT( IJ,K )
     &                - ATF( K+1 )* GAT( IJ,K+1 )
     &                - BTF( K   )* GAT( IJ,K   ) )
     &        -  SBETA ( K ) / DSIG ( K )
     &           *  GATV ( IJ,K ) * GSUMP ( IJ,K+1 )
 2400 CONTINUE
*
      RETURN
      END
*******************************************************************
      SUBROUTINE GRQADV    !!  adv. of humidity etc.
     O         ( GTUQ  , GTVQ  , GTQ   ,
     I           GAQ   ,
     I           GAU   , GAV   , GADIV , GSIGD , LT     )
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
      REAL*8     GAQ   ( IJDIM, KMAX )   !! humidity etc.
      REAL*8     GAU   ( IJDIM, KMAX )   !! westerly U(t)
      REAL*8     GAV   ( IJDIM, KMAX )   !! southern wind V(t)
      REAL*8     GADIV ( IJDIM, KMAX )   !! divergence D(t)
      REAL*8     GSIGD ( IJDIM, KMAX+1 ) !! tendency of sigma
      INTEGER    LT                      !! tracer No.
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K
*
*  << THADV : Uq, Vq >>
*
      DO 1100 K = 1, KMAX
         DO 1100 IJ = 1, IJDIM
            GTUQ( IJ,K ) =  GAU ( IJ,K ) * GAQ ( IJ,K )
            GTVQ( IJ,K ) =  GAV ( IJ,K ) * GAQ ( IJ,K )
 1100 CONTINUE
*
*  << TVADV : sigma adv. >>
*
      DO 2100 K = 1, KMAX
         DO 2100 IJ = 1, IDIM*JDIM
            GTQ ( IJ,K )
     &           =  GAQ ( IJ,K ) * GADIV( IJ,K )
 2100 CONTINUE
*
      DO 2200 K = 2, KMAX
         DO 2200 IJ = 1, IDIM*JDIM
            GTQ ( IJ,K )
     &        =  GTQ ( IJ,K )
     &           -  GSIGD ( IJ,K ) / ( 2.D0 * DSIG ( K ) )
     &               * ( GAQ ( IJ,K-1 ) - GAQ ( IJ,K )  )
 2200 CONTINUE
*
      DO 2300 K = 1, KMAX-1
         DO 2300 IJ = 1, IDIM*JDIM
            GTQ ( IJ,K )
     &        =  GTQ ( IJ,K )
     &           -  GSIGD ( IJ,K+1 )  / ( 2.D0 * DSIG ( K ) )
     &               * ( GAQ ( IJ,K ) - GAQ  ( IJ,K+1 )  )
 2300 CONTINUE
*
      RETURN
      END
