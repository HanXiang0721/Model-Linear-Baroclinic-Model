* PACKAGE DGDYN   !! dynamics: non-linear term
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         05/05/24(hiro)     for initializing BS
**********************************************************************
      SUBROUTINE PSDOT     !!  surface pres. change & sigma velocity
     O        ( GSIGD , 
     I          GAU   , GAV   , GADIV , GAPIX , GAPIY  )
*
*   [PARAMETER]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT]
      REAL*8     GSIGD ( IJDIM, KMAX+1 ) !! tendency of sigma
*     
*   [INPUT]
      REAL*8     GAU   ( IJDIM, KMAX )   !! westerly U(t)
      REAL*8     GAV   ( IJDIM, KMAX )   !! southern wind V(t)
      REAL*8     GADIV ( IJDIM, KMAX )   !! divergence D(t)
      REAL*8     GAPIX ( IJDIM       )   !! zonal deriv. of pi
      REAL*8     GAPIY ( IJDIM       )   !! merid. differ. of pi
*
*   [INTERNAL WORK]
      REAL*8     GSUMP ( IJDIM, KMAX )   !! vert. integ. of tend. of pi
      REAL*8     GAPADV( IJDIM, KMAX )   !! advection of pi
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
