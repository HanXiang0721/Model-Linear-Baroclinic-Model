* PACKAGE PIMTX  !! physics: treatment of implicit matrix
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         95/08/05(numaguti) in case of drflxl=0 
*         96/05/13(numaguti) unit of QFLUX
***********************************************************************
      SUBROUTINE VFTND1         !! vertical diffusion
     M         ( GTU   , GTV   , 
     O           UMTX  ,         
     I           UFLUX , VFLUX , DUFLUX,
     I           GDPM  , DELTP            )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zccom.F"                /* stand. physical const. */
*
*   [MODIFY] 
      REAL*8     GTU   ( IJSDIM, KMAX )        !! 
      REAL*8     GTV   ( IJSDIM, KMAX )        !! 
*   [OUTPUT] 
      REAL*8     UMTX  ( IJSDIM, KMAX,  -1:1 ) !! impl. mat. of u
*
*   [INPUT] 
      REAL*8     UFLUX ( IJSDIM, KMAX+1 )         !! flux of U
      REAL*8     VFLUX ( IJSDIM, KMAX+1 )         !! flux of V
      REAL*8     DUFLUX( IJSDIM, KMAX+1, -1:0 )   !! impl. mat. of U
      REAL*8     GDPM  ( IJSDIM, KMAX+1 )         !! pressure (half lev)
      REAL*8     DELTP                            !! delta(t)
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K
*
*
      DO 1100 K = 1, KMAX
         DO 1100 IJ = 1, IJSDIM
            GTU ( IJ,K ) = UFLUX ( IJ,K ) - UFLUX ( IJ,K+1 )
     &                   + GTU( IJ,K )
     &                      *( GDPM ( IJ,K ) - GDPM( IJ,K+1 ) )/GRAV
            GTV ( IJ,K ) = VFLUX ( IJ,K ) - VFLUX ( IJ,K+1 )
     &                   + GTV( IJ,K )
     &                      *( GDPM ( IJ,K ) - GDPM( IJ,K+1 ) )/GRAV
 1100 CONTINUE
*     
      DO 2100 K = 1, KMAX
         DO 2100 IJ = 1, IJSDIM
            UMTX ( IJ,K, 0 ) = ( GDPM ( IJ,K )-GDPM( IJ,K+1 ) )/GRAV
     &                         - DUFLUX( IJ,K,  0  )*DELTP
     &                         + DUFLUX( IJ,K+1,-1 )*DELTP
            UMTX ( IJ,K,-1 ) = - DUFLUX( IJ,K,  -1 )*DELTP
            UMTX ( IJ,K, 1 ) =   DUFLUX( IJ,K+1, 0 )*DELTP
 2100 CONTINUE 
*
      CALL LUMAK3
     O         ( UMTX   ,
     I           IJSDIM , KMAX  )
*
      CALL LUFWD3
     M         ( GTU   ,
     I           UMTX  , IJSDIM, KMAX )
      CALL LUFWD3
     M         ( GTV   ,
     I           UMTX  , IJSDIM, KMAX )
*
      RETURN
      END
************************************************************************
      SUBROUTINE VFTND2    !! tendency by vertical diffusion
     M         ( GTU   , GTV   , 
     I           UMTX              )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
*
*   [MODIFY]
      REAL*8     GTU   ( IJSDIM, KMAX      )   !! 
      REAL*8     GTV   ( IJSDIM, KMAX      )   !! 
*
*   [INPUT] 
      REAL*8     UMTX  ( IJSDIM, KMAX,   -1:1 ) !! impl. mat. of u
*
*   [INTERNAL WORK] 
*
      CALL LUBWD3
     M         ( GTU   ,
     I           UMTX  , IJSDIM, KMAX )
      CALL LUBWD3
     M         ( GTV   ,
     I           UMTX  , IJSDIM, KMAX )
*
      RETURN
      END
********************************************************************
      SUBROUTINE FLXCOR         !! implicit correction of flux
     M     ( UFLUX , VFLUX , 
     I           GTU   , GTV   , 
     I           DUFLUX,         
     I           DELTP                     )
*
*   [INPUT] 
#include        "zcdim.F"                /* # of grid point & wave */
*
*   [MODIFY] 
      REAL*8     UFLUX ( IJSDIM, KMAX+1      ) !! flux of U
      REAL*8     VFLUX ( IJSDIM, KMAX+1      ) !! flux of V
*
*   [INPUT] 
      REAL*8     GTU   ( IJSDIM, KMAX      )   !! tend. of u adv.
      REAL*8     GTV   ( IJSDIM, KMAX      )   !! tend. merid.adv. V
      REAL*8     DUFLUX( IJSDIM, KMAX+1,   -1:0 ) !! flux deriv. of U
      REAL*8     DELTP                            !! time step delta(t)
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K
*
      DO 1100 K = 2, KMAX
         DO 1100 IJ = 1, IJSDIM
            UFLUX ( IJ,K ) = UFLUX  ( IJ,K )
     &           + DUFLUX( IJ,K, 0 ) * GTU( IJ,K   )*DELTP
     &           + DUFLUX( IJ,K,-1 ) * GTU( IJ,K-1 )*DELTP
*
            VFLUX ( IJ,K ) = VFLUX  ( IJ,K )
     &           + DUFLUX( IJ,K, 0 ) * GTV( IJ,K   )*DELTP
     &           + DUFLUX( IJ,K,-1 ) * GTV( IJ,K-1 )*DELTP
*
 1100 CONTINUE
*
      RETURN
      END
