* PACKAGE PGSFC !! physics: surface model
*
*   [HIS] 96/06/26(numaguti) AGCM5.4f
*         96/09/09(numaguti) SNRAT
*         96/11/21(numaguti) ALBEDO
*         96/12/02(numaguti) multiple GDSFCW
***********************************************************************
      SUBROUTINE SURFCE    !! surface model
     M         ( GTU   , GTV   , UFLUX , VFLUX , 
     I           UMTX  , GDU   , GDV   , 
     I           GDPM  , GDTM  , GDZ   , GDZM  , DELTP  )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc.           */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
#include        "zhdim.F"                /* # of chars             */
*
*   [MODIFY]
      REAL*8     GTU   ( IJSDIM, KMAX        )  !! convergence/tendency
      REAL*8     GTV   ( IJSDIM, KMAX        )  !! convergence/tendency
      REAL*8     UFLUX ( IJSDIM, KMAX+1      )  !! flux of U
      REAL*8     VFLUX ( IJSDIM, KMAX+1      )  !! flux of V
*
*   [INPUT] 
      REAL*8     UMTX  ( IJSDIM, KMAX,  -1:1 )  !! impl. mat. of u
      REAL*8     GDU   ( IJSDIM, KMAX      )    !! westerly u
      REAL*8     GDV   ( IJSDIM, KMAX      )    !! southern wind v
      REAL*8     GDPM  ( IJSDIM, KMAX+1 )       !! pressure (half lev)
      REAL*8     GDTM  ( IJSDIM, KMAX+1 )       !! temp. (half lev)
      REAL*8     GDZ   ( IJSDIM, KMAX   )       !! altitude
      REAL*8     GDZM  ( IJSDIM, KMAX+1 )       !! altitude (half lev)
      REAL*8     DELTP                          !! dt for implicit
*
*   [INTERNAL WORK] 
      REAL*8     GTUA  ( IJSDIM )               !! surface U tend.
      REAL*8     GTVA  ( IJSDIM )               !! surface V tend.
      REAL*8     UFLUXS( IJSDIM )               !! surface flux of U
      REAL*8     VFLUXS( IJSDIM )               !! surface flux of V
      REAL*8     UFLUXA( IJSDIM )               !! - flux of U (K=1.5)
      REAL*8     VFLUXA( IJSDIM )               !! - flux of V (K=1.5)
      REAL*8     GRZ0M ( IJSDIM )               !! surface roughness (V)
      REAL*8     DUFDU ( IJSDIM )               !! D tau / D U
*
      INTEGER    IJ
*
*   [INTERNAL PARM] 
      LOGICAL    OFIRST
      DATA       OFIRST         / .TRUE. /
*
      IF ( OFIRST ) THEN
         OFIRST = .FALSE.
         WRITE ( 6,* ) ' @@@ PSURF: SURFACE MODEL 03/09/30'
      ENDIF
*
      CALL COPY ( UFLUXA, GTU  , IJSDIM  )
      CALL COPY ( VFLUXA, GTV  , IJSDIM  )
      CALL RESET( GTU,    IJSDIM )
      CALL RESET( GTV,    IJSDIM )
*
      CALL SFCZ0                !! surface roughness
     O         ( GRZ0M   ,
     I           GDU(1,1), GDV(1,1) )
* 
      CALL SFCFLX               !! surface flux
     O         ( UFLUXS, VFLUXS, DUFDU , 
     I           GDU   , GDV   , GDTM  , GDPM  ,
     I           GDZ   , GDZM  , GRZ0M            )
*
      CALL SLVWND               !! changes in wind velocity
     M         ( UFLUXS, VFLUXS,
     O           GTUA  , GTVA  ,
     I           UFLUXA, VFLUXA,
     I           UMTX  , DUFDU ,
     I           DELTP            )
*                
      DO 4100 IJ = 1, IJSDIM
         GTU  ( IJ,1 ) = GTU  ( IJ,1 ) + GTUA  ( IJ )
         GTV  ( IJ,1 ) = GTV  ( IJ,1 ) + GTVA  ( IJ )
         UFLUX( IJ,1 ) = UFLUX( IJ,1 ) + UFLUXS( IJ )
         VFLUX( IJ,1 ) = VFLUX( IJ,1 ) + VFLUXS( IJ )
 4100 CONTINUE 
*
      RETURN
      END
********************************************************************
      SUBROUTINE SFCZ0     !! roughness of ocean for UFLUX
     O         ( GRZ0M , 
     I           USFC  , VSFC            )
*
*      roughness of sea surface (Miller et al., 1992)
*
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zccom.F"                /* stand. physical const. */
*
*   [OUTPUT] 
      REAL*8     GRZ0M  ( IJSDIM )        !! surface roughness (V)
*
*   [INPUT] 
      REAL*8     USFC   ( IJSDIM )        !! u sfc wind speed
      REAL*8     VSFC   ( IJSDIM )        !! v sfc wind speed
*
*   [INTERNAL WORK] 
      INTEGER    IJ, IFPAR, JFPAR
      REAL*8     USTAR
*
*   [INTERNAL PARM]     
      REAL*8     Z0M0                     !! base
      REAL*8     Z0MR                     !! rough factor
      REAL*8     Z0MS                     !! smooth factor
      REAL*8     VISAIR                   !! kinematic viscosity 
      REAL*8     CM0                      !! bulk coef for USTAR
      REAL*8     USTRMN                   !! min(u*)
      REAL*8     Z0MMIN                   !! minimum
      LOGICAL    OFIRST
      NAMELIST  /NMSEAZ/
     &           Z0M0, Z0MR, Z0MS,
     &           VISAIR, CM0, USTRMN, 
     &           Z0MMIN
      DATA       Z0M0, Z0MR, Z0MS / 0.,     0.018, 0.11  /
      DATA       VISAIR           / 1.5E-5  / 
      DATA       CM0              / 1.D-3   /
      DATA       USTRMN           / 1.D-3   /
      DATA       Z0MMIN           / 1.E-5   /        
      DATA       OFIRST           / .TRUE.  /
*
      IF ( OFIRST ) THEN
         CALL REWNML( IFPAR , JFPAR )
         OFIRST = .FALSE.
         WRITE ( JFPAR,* ) ' SEA SURFACE Z0 (Miller et al.) 98/06/19'
         READ   ( IFPAR, NMSEAZ, END=190 )
  190    WRITE  ( JFPAR, NMSEAZ )
      ENDIF
*
      DO 1100 IJ = 1, IJSDIM
         USTAR       = CM0 * ( USFC( IJ )**2 + VSFC( IJ )**2 )
         USTAR       = MAX( SQRT( USTAR ), USTRMN )
*
         GRZ0M( IJ ) = Z0M0 + Z0MR*USTAR**2/GRAV + Z0MS*VISAIR/USTAR
         GRZ0M( IJ ) = MAX( GRZ0M( IJ ), Z0MMIN )            
 1100 CONTINUE
*
      RETURN
      END
***********************************************************************
      SUBROUTINE SFCFLX    !!  surface momentum flux
     O         ( UFLUXS, VFLUXS, DUFDU , 
     I           GDU   , GDV   , GDTVM , GDPM  ,
     I           GDZ   , GDZM  , GRZ0M            )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc. */
#include        "zccom.F"                /* stand. physical const. */
*
*   [OUTPUT]  
      REAL*8     UFLUXS( IJSDIM )              !! flux of U
      REAL*8     VFLUXS( IJSDIM )              !! flux of V
      REAL*8     DUFDU ( IJSDIM )              !! d(tau)/du
*
*   [INPUT] 
      REAL*8     GDU   ( IJSDIM, KMAX      )   !! westerly u
      REAL*8     GDV   ( IJSDIM, KMAX      )   !! southern wind v
      REAL*8     GDTVM ( IJSDIM, KMAX      )   !! virt. temp.(half lev)
      REAL*8     GDPM  ( IJSDIM, KMAX      )   !! pressure (half lev)
      REAL*8     GDZ   ( IJSDIM, KMAX      )   !! altitude
      REAL*8     GDZM  ( IJSDIM, KMAX+1    )   !! altitude (half lev)
      REAL*8     GRZ0M ( IJSDIM )              !! surface roughness (V)
*
*   [INTERNAL WORK] 
      REAL*8     CMV   ( IJSDIM )              !! bulk trans. coef. u
      REAL*8     ZA             !! lowest layer altitude
      REAL*8     VABS           !! wind speed
      INTEGER    IJ
      INTEGER    IFPAR, JFPAR
      REAL*8     AKAPPA, RHO, CMX
*
*   [INTERNAL PARM] 
      REAL*8     CMMIN, CMMAX
      REAL*8     USMINM, USMAXM
      LOGICAL    OFIRST
      NAMELIST  /NMSFCM/ CMMIN , CMMAX ,
     &                   USMINM, USMAXM

*   [INTERNAL FUNC] 
      DATA       USMINM/ 3.0     /            !! min. wind vel. for V
      DATA       USMAXM/ 1000.   /            !! max wind vel. for V
      DATA       CMMIN  /  1.E-5 /           !! min. bulk coef. of u
      DATA       CMMAX  /   1.   /           !! max. bulk coef. of u
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ PSFCM: SURFACE FLUX 95/08/02'
         OFIRST = .FALSE.
*
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMSFCM, END=190 )
  190    WRITE  ( JFPAR, NMSFCM )
      ENDIF
*
      CALL RESET( UFLUXS, IJSDIM )
      CALL RESET( VFLUXS, IJSDIM )
      CALL RESET( CMV   , IJSDIM )
      CALL RESET( DUFDU , IJSDIM )
*
*     << BLKTC : bulk tranfer coef. >>!! neutral case
*
      AKAPPA = RAIR / CP
      DO 3100 IJ = 1, IJSDIM
         VABS      = SQRT( GDU( IJ,1 )**2 + GDV( IJ,1 )**2  )
         ZA        = GDZ( IJ,1 ) - GDZM( IJ,1 )
         CMX       = FKARM**2 / LOG( ZA/GRZ0M( IJ ) )
         CMV( IJ ) = CMX      / LOG( ZA/GRZ0M( IJ ) )
         CMV( IJ ) = MAX( MIN( CMV( IJ ), CMMAX ), CMMIN )
         CMV  ( IJ ) = CMV ( IJ )
     &        * MIN( MAX ( VABS, USMINM ), USMAXM )
 3100 CONTINUE
*
*  << FLUXS : fluxes >>
*
      DO 6100 IJ = 1, IJSDIM
         RHO = GDPM( IJ,1 ) / ( RAIR * GDTVM( IJ,1 ) )
         UFLUXS( IJ ) = - RHO * CMV( IJ ) * GDU( IJ,1 )
         VFLUXS( IJ ) = - RHO * CMV( IJ ) * GDV( IJ,1 )
         DUFDU ( IJ ) = - RHO * CMV( IJ )
 6100 CONTINUE
*
      CALL HISTIN
     I   ( CMV, 'CDVM', 'bulk transfer coef. momentum' ,'m/s', 'ASFC')
      CALL HISTIN
     I   ( GRZ0M,'Z0M' , 'roughness for momentum', 'm', 'ASFC')
*
      RETURN
      END
***********************************************************************
      SUBROUTINE SLVWND  !! changes in wind velocity
     M         ( UFLUXS, VFLUXS,
     O           GTUA  , GTVA  ,
     I           UFLUXA, VFLUXA,
     I           UMTX  , DUFDU ,
     I           DELTP           )
*      
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
*
*   [MODIFY] 
      REAL*8     UFLUXS( IJSDIM ) !! flux of U
      REAL*8     VFLUXS( IJSDIM ) !! flux of V
*
*   [OUTPUT] 
      REAL*8     GTUA  ( IJSDIM ) !! convergence/tendency
      REAL*8     GTVA  ( IJSDIM ) !! convergence/tendency
*     
*   [INPUT] 
      REAL*8     UFLUXA( IJSDIM ) !! - flux of U (K=1.5)
      REAL*8     VFLUXA( IJSDIM ) !! - flux of V (K=1.5)
      REAL*8     UMTX  ( IJSDIM, KMAX,  -1:1 ) !! impl. mat. of u
      REAL*8     DUFDU ( IJSDIM ) !! d(tau)/du
      REAL*8     DELTP          !! time step delta(t)
*
*   [WORK] 
      INTEGER    IJ
*
      DO 1100 IJ = 1, IJSDIM
         GTUA  ( IJ ) = ( UFLUXA( IJ )   + UFLUXS( IJ )       )
     &                 /( UMTX( IJ,1,0 ) - DUFDU ( IJ )*DELTP )
         GTVA  ( IJ ) = ( VFLUXA( IJ )   + VFLUXS( IJ )       )
     &                 /( UMTX( IJ,1,0 ) - DUFDU ( IJ )*DELTP )
         UFLUXS( IJ ) = UFLUXS( IJ )
     &                + DUFDU ( IJ ) * GTUA( IJ )*DELTP
         VFLUXS( IJ ) = VFLUXS( IJ )
     &                + DUFDU ( IJ ) * GTVA( IJ )*DELTP
 1100 CONTINUE 
*
      RETURN
      END
