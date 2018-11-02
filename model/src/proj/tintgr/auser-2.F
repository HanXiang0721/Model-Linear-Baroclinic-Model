* PACKAGE DOUSER  !! user defined routine
*
*   [HIS] 96/06/06(numaguti)
*         96/09/18(numaguti) GSIGD,GPID
*         99/05/19(hiro)     only vert. diffusion
***********************************************************************
      SUBROUTINE DOUSER2        !! user defined function
     M         ( GDU   , GDV   , GDT   , GDPS  , GDQ   ,
     I           GFU   , GFV   , GFT   , GFPS  ,
     I           GDSIGD, GDPID , GDZS  ,
     I           TIME  , DELTX , DELTI                   )
*     
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc.           */
#include        "zhdim.F"                /* # of char. in string   */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
*
*   [MODIFY] 
      REAL*8     GDU   ( IJDIM, KMAX      )  !! westerly
      REAL*8     GDV   ( IJDIM, KMAX      )  !! southern wind
      REAL*8     GDT   ( IJDIM, KMAX      )  !! temperature
      REAL*8     GDPS  ( IJDIM            )  !! surface pressure
      REAL*8     GDQ   ( IJDIM, KMAX, NTR )  !! humidity etc.
*
*   [INPUT] 
      REAL*8     GFU   ( IJDIM, KMAX      )  !! westerly u for BS
      REAL*8     GFV   ( IJDIM, KMAX      )  !! southern wind v for BS
      REAL*8     GFT   ( IJDIM, KMAX      )  !! temperature for BS
      REAL*8     GFPS  ( IJDIM            )  !! BS surface pressure
      REAL*8     GDSIGD( IJDIM, KMAX+1    )  !! sigma velocity
      REAL*8     GDPID ( IJDIM            )  !! pi velocity
      REAL*8     GDZS  ( IJDIM            )  !! surface topography
      REAL * 8   TIME                        !! time
      REAL*8     DELTX                       !! time step 2*delta(t)
      REAL*8     DELTI                       !! time step (int. var.)
*
*   [INTERNAL WORK] 
      REAL*8     GTU   ( IJSDIM, KMAX      ) !! tendency of zonal wind
      REAL*8     GTV   ( IJSDIM, KMAX      ) !! tend. of merid. wind
      REAL*8     GTT   ( IJSDIM, KMAX      ) !! tend. term of T
      REAL*8     GTQ   ( IJSDIM, KMAX, NTR ) !! tend. of q
*
      REAL*8     GDCFRC( IJSDIM, KMAX )         !! ratio of cloudy area
*
      REAL*8     GDTV  ( IJSDIM, KMAX   )       !! virtual temperature
      REAL*8     GDTM  ( IJSDIM, KMAX+1 )       !! temp. (half lev)
      REAL*8     GDTVM ( IJSDIM, KMAX+1 )       !! virt. temp.(half lev)
      REAL*8     GDP   ( IJSDIM, KMAX )         !! pressure P
      REAL*8     GDPM  ( IJSDIM, KMAX+1 )       !! pressure (half lev)
      REAL*8     GDZ   ( IJSDIM, KMAX )         !! altitude
      REAL*8     GDZM  ( IJSDIM, KMAX+1 )       !! altitude (half lev)
*
      REAL*8     UFLUX ( IJSDIM, KMAX+1      )  !! flux of U
      REAL*8     VFLUX ( IJSDIM, KMAX+1      )  !! flux of V
      REAL*8     TFLUX ( IJSDIM, KMAX+1      )  !! flux of T
*
      REAL*8     UMTX  ( IJSDIM, KMAX,  -1:1 )   !! impl. mat. of u
      REAL*8     TMTX  ( IJSDIM, KMAX,  -1:1 )   !! impl. mat. of T
*
      REAL*8     DELTP                     !! 2*dt for implicit
      INTEGER    IJ, K, IJS
*
*   [INTERNAL SAVE] 
      REAL*8     DUFLUX( IJSDIM, KMAX+1, -1:0 ) !! impl. mat. of U
      REAL*8     DTFLUX( IJSDIM, KMAX+1, -1:0 ) !! impl. mat. of T
      REAL*8     DFM   ( IJSDIM, KMAX )         !! trans. coef. :u
      REAL*8     DFH   ( IJSDIM, KMAX )         !! trans. coef. :T
      REAL*8     EXNER ( IJSDIM, KMAX )         !! Exner function : integer
      REAL*8     EXNERM( IJSDIM, KMAX )         !! Exner func.: half lev
      SAVE       DUFLUX, DTFLUX
      SAVE       DFM, DFH, EXNER, EXNERM
*
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         OFIRST = .FALSE. 
         WRITE ( 6,* ) ' @@@ DOUSER: DO ONLY VERT. DIFF. 99/05/20'
         CALL RESET ( GDCFRC ,  IJSDIM*KMAX         )
         CALL PSETUP            !! calc. using BS
     O        ( GDTV  , GDTM  , GDTVM ,
     O          GDP   , GDPM  , GDZ   , GDZM  ,
     I          GFT   , GDQ   , GFPS  , GDZS   )
*
         CALL VDFFLX2           !! diffusion coeff.
     O        ( DUFLUX, DTFLUX,
     O          DFM   , DFH   , EXNER , EXNERM,
     I          GDU   , GDV   , GDT   , GDQ   , 
     I          GFU   , GFV   , GFT   ,
     I          GDTV  , GDTVM , GDCFRC,
     I          GDP   , GDPM  , GDZ   , GDZM  , 
     I          DELTP , IJS                     )
*
      ENDIF
*
      DELTP = MAX( DELTX, 2.D0*DELTI )
      IJS   = 1
*
      CALL PSETUP               !! calc. using BS
     O         ( GDTV  , GDTM  , GDTVM ,
     O           GDP   , GDPM  , GDZ   , GDZM  ,
     I           GFT   , GDQ   , GFPS  , GDZS   )
*
*    << vert diff >>
*
      CALL RESET ( GTU ,  IJSDIM*KMAX         )
      CALL RESET ( GTV ,  IJSDIM*KMAX         )
      CALL RESET ( GTT ,  IJSDIM*KMAX         )
      CALL RESET ( GTQ ,  IJSDIM*KMAX*NTR     )
      CALL RESET ( UFLUX, IJSDIM*(KMAX+1)     )
      CALL RESET ( VFLUX, IJSDIM*(KMAX+1)     )
      CALL RESET ( TFLUX, IJSDIM*(KMAX+1)     )
*
*     << fluxes >>
*
      DO 5100 K = 2, KMAX
         DO 5100 IJ = 1, IJSDIM
            UFLUX( IJ,K ) = DFM( IJ,K )
     &                       * ( GDU ( IJ,K-1 ) - GDU ( IJ,K ) )
*
            VFLUX( IJ,K ) = DFM( IJ,K )
     &                       * ( GDV ( IJ,K-1 ) - GDV ( IJ,K ) )
*
            TFLUX( IJ,K ) = CP * DFH( IJ,K )  * EXNERM( IJ,K   )
     &                        * (  GDT( IJ,K-1 ) / EXNER ( IJ,K-1 )
     &                           - GDT( IJ,K   ) / EXNER ( IJ,K   ) )
 5100 CONTINUE
      DO 5200 IJ = 1, IJSDIM
         UFLUX( IJ,1 ) = 0.D0
         VFLUX( IJ,1 ) = 0.D0
         TFLUX( IJ,1 ) = 0.D0
 5200 CONTINUE
      CALL PLEVS
     O         ( GDP   , GDPM ,
     I           GDPS           )
      CALL VFTND3 
     M         ( GTU   , GTV   , GTT   ,
     O           UMTX  ,         TMTX  ,
     I           UFLUX , VFLUX , TFLUX ,
     I           DUFLUX, DTFLUX,
     I           GDPM  , DELTP , IJS             )
      CALL VFTND4
     M         ( GTU   , GTV   , GTT   ,
     I           UMTX  ,         TMTX  ,
     I           IJS                             )
*      
*     << time integration >>
*
      CALL GDINTG( GDU , GTU , DELTX, KMAX     )
      CALL GDINTG( GDV , GTV , DELTX, KMAX     )
      CALL GDINTG( GDT , GTT , DELTX, KMAX     )
*
      RETURN
      END
***********************************************************************
      SUBROUTINE VDFFLX2        !! vertical diffusion
     O         ( DUFLUX, DTFLUX,
     O           DFM   , DFH   , EXNER , EXNERM,
     I           GDU   , GDV   , GDT   , GDQ   , 
     I           GFU   , GFV   , GFT   ,
     I           GDTV  , GDTVM , GDCFRC,
     I           GDP   , GDPM  , GDZ   , GDZM  , 
     I           DELTP , IJS                     )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc. */
#include        "zccom.F"                /* stand. physical const. */
*
*   [OUTPUT] 
      REAL*8     DUFLUX( IJSDIM, KMAX+1, -1:0 ) !! impl. mat. of U
      REAL*8     DTFLUX( IJSDIM, KMAX+1, -1:0 ) !! impl. mat. of T
      REAL*8     DFM   ( IJSDIM, KMAX )         !! trans. coef. :u
      REAL*8     DFH   ( IJSDIM, KMAX )         !! trans. coef. :T
      REAL*8     EXNER ( IJSDIM, KMAX )         !! Exner function : integer
      REAL*8     EXNERM( IJSDIM, KMAX )         !! Exner func.: half lev
*
*   [INPUT] 
      REAL*8     GDU   ( IJSDIM, KMAX      )   !! westerly U
      REAL*8     GDV   ( IJSDIM, KMAX      )   !! southern wind V
      REAL*8     GDT   ( IJSDIM, KMAX      )   !! temperature T
      REAL*8     GDQ   ( IJSDIM, KMAX, NTR )   !! humidity q
      REAL*8     GFU   ( IJDIM, KMAX       )   !! westerly u for BS
      REAL*8     GFV   ( IJDIM, KMAX       )   !! southern wind v for BS
      REAL*8     GFT   ( IJDIM, KMAX       )   !! temperature for BS
      REAL*8     GDTV  ( IJSDIM, KMAX      )   !! virtual temperature
      REAL*8     GDTVM ( IJSDIM, KMAX+1    )   !! temp. (half lev)
      REAL*8     GDCFRC( IJSDIM, KMAX      )   !! ratio of cloudy area
      REAL*8     GDP   ( IJSDIM, KMAX      )   !! pressure P
      REAL*8     GDPM  ( IJSDIM, KMAX      )   !! pressure (half lev)
      REAL*8     GDZ   ( IJSDIM, KMAX      )   !! altitude
      REAL*8     GDZM  ( IJSDIM, KMAX      )   !! altitude (half lev)
      REAL*8     DELTP                         !! time step
      INTEGER    IJS                           !! IJ index
*
*   [INTERNAL WORK] 
#ifndef OPT_NOCOMMON
      COMMON 
     &           DVDZ  , RIB   ,
     &           GQS   ,
     &           WORK1 , WORK2 , WORK3 
#endif
      REAL*8     DVDZ  ( IJSDIM, KMAX )   !! d|v|/dz
      REAL*8     RIB   ( IJSDIM, KMAX )   !! bulk Ri number
      REAL*8     DFE   ( IJSDIM, KMAX )   !! trans. coef. :q
      REAL*8     GQS   ( IJSDIM, KMAX )   !! saturation humidity & work 
      REAL*8     WORK1 ( IJSDIM, KMAX )   !! work
      REAL*8     WORK2 ( IJSDIM, KMAX )
      REAL*8     WORK3 ( IJSDIM, KMAX )
*
      INTEGER    IJ, K, KK
      INTEGER    IFPAR, JFPAR
      REAL*8     AKAPPA, FRI, THVK, THVKM, FC
      REAL*8     RIBD, RIBS, RIBX
      REAL*8     RHO, RDZ
*
*   [INTERNAL PARM] 
      REAL*8     P00, THS
      REAL*8     URMIN , RIBMIN, RIBMAX
      REAL*8     FMRIB0, FMRIB1, FMRIB , RIBSMN
      LOGICAL    OFIRST
      NAMELIST  /NMVDFM/ THS   , URMIN , RIBMIN, RIBMAX, 
     &                   FMRIB0, FMRIB1, FMRIB , RIBSMN
*
*   [INTERNAL FUNC]
#include        "zqsat.F"                /* saturate vapor pressure */
      DATA       P00    / 100000. /
      DATA       THS    / 300.    /           !! basic pot. temp
      DATA       URMIN  / 0.1     /           !! min. of del-wind sq.
      DATA       RIBMIN / -100.   /           !! min. of bulk Ri number
      DATA       RIBMAX /  100.   /           !! max. of bulk Ri number
      DATA       FMRIB0 / 0.0     /           !! moist Ri factor
      DATA       FMRIB1 / 0.5     /           !! moist Ri factor
      DATA       RIBSMN /  0.     /           !! min. of wet bulk Ri number
      DATA       OFIRST / .TRUE.  /
*
*          < 1. constant >
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ PVDFM2: VERTICAL DIFFUSION 99/05/20'
         OFIRST = .FALSE.
*
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMVDFM, END=1190 )
 1190    WRITE  ( JFPAR, NMVDFM )
      ENDIF
*
      AKAPPA = RAIR / CP
      DO 1100 K = 1, KMAX
         DO 1100 IJ = 1, IJSDIM
            EXNER ( IJ,K ) = ( GDP ( IJ,K )/P00 ) ** AKAPPA
            EXNERM( IJ,K ) = ( GDPM( IJ,K )/P00 ) ** AKAPPA
            GQS   ( IJ,K ) = FQSAT( GFT( IJ,K ), GDP( IJ,K ) )
 1100 CONTINUE
*
      CALL RESET( DVDZ, IJSDIM )
      CALL RESET( RIB , IJSDIM )
*
      DO 2100 K = 2, KMAX
         DO 2100 IJ = 1, IJSDIM
            DVDZ( IJ,K ) = SQRT( MAX( URMIN,
     &                               ( GFU( IJ,K )-GFU( IJ,K-1 ) )**2
     &                              +( GFV( IJ,K )-GFV( IJ,K-1 ) )**2
     &                         )    )
     &                       / ( GDZ( IJ,K ) - GDZ( IJ,K-1 ) )
 2100 CONTINUE
*
      DO 2200 K = 2, KMAX
         DO 2200 IJ = 1, IJSDIM
            FRI   = GRAV / THS / ( GDZ( IJ,K ) - GDZ( IJ,K-1 ) )
     &                         / DVDZ ( IJ,K )**2
*
            THVK  = GDTV( IJ,K   )/EXNER( IJ,K   )
            THVKM = GDTV( IJ,K-1 )/EXNER( IJ,K-1 )
            RIBD  = FRI * ( THVK - THVKM )
*
            THVK  = THVK  + EL/CP*GQS( IJ,K   )/EXNER( IJ,K   )
            THVKM = THVKM + EL/CP*GQS( IJ,K-1 )/EXNER( IJ,K-1 )
            RIBS  = FRI * ( THVK - THVKM )
            RIBS  = MAX( RIBS, RIBSMN )
*
            FC    = FMRIB1*GDCFRC( IJ,K-1 ) + FMRIB0*GDCFRC( IJ,K )
            FC    = MIN( MAX( FC, 0.D0 ), 1.D0 )
*
            RIBX  = RIBD + FC * MIN( RIBS - RIBD, 0.D0 )
            RIB  ( IJ,K ) = MIN( MAX ( RIBX, RIBMIN ), RIBMAX )
            WORK1( IJ,K ) = RIBD
            WORK2( IJ,K ) = RIBS
 2200 CONTINUE
*
      CALL HISTIN
     I   ( WORK1, 'RIBD' , 'bulk Ri No. DRY' ,'N.D.', 'ALEV')
      CALL HISTIN
     I   ( WORK2, 'RIBS' , 'bulk Ri No. WET' ,'N.D.', 'ALEV')
      CALL HISTIN
     I   ( RIB,   'RIB'  , 'bulk Ri No.' ,'N.D.', 'ALEV')
*
      CALL VDFCOF
     O         ( DFM   , DFH   , DFE   ,
     I           RIB   , DVDZ  , GDZM  ,
     W           WORK1 , WORK2 , WORK3 , GQS   )
*
      CALL SHLCOF !! diffusive shallow cumulus
     M         ( DFM   , DFH   , DFE   ,
     I           GDT   , GDQ   , GDP   , GDZ   ,
     W           WORK1 , WORK2 , WORK3 , GQS    )
*
      CALL HISTIN
     I   ( DFM, 'DFM'  , 'diffusion coef. momentum' ,'m**2/s', 'ALEV')
      CALL HISTIN
     I   ( DFH, 'DFH'  , 'diffusion coef. heat    ' ,'m**2/s', 'ALEV')
*
*          < 4. transfer coef. >
*
      DO 4100 K = 2, KMAX
         DO 4100 IJ = 1, IJSDIM
            RHO         = GDPM( IJ,K ) / RAIR / GDTVM( IJ,K )
            RDZ         = RHO /( GDZ( IJ,K ) - GDZ( IJ,K-1 ) )
            DFM( IJ,K ) = RDZ * DFM( IJ,K )
            DFH( IJ,K ) = RDZ * DFH( IJ,K )
            DFE( IJ,K ) = RDZ * DFE( IJ,K )
 4100 CONTINUE
*
*          < 5. implicit matrix >
*
      DO 6100 K = 2, KMAX
         DO 6100 IJ = 1, IJSDIM
            DUFLUX( IJ,K, 0 ) = -      DFM( IJ,K )
            DUFLUX( IJ,K,-1 ) =        DFM( IJ,K )
*
            DTFLUX( IJ,K, 0 ) = - CP * DFH( IJ,K )*EXNERM( IJ,K   )
     &                                            /EXNER ( IJ,K   )
            DTFLUX( IJ,K,-1 ) =   CP * DFH( IJ,K )*EXNERM( IJ,K   )
     &                                            /EXNER ( IJ,K-1 )
 6100 CONTINUE
*
      DO 6300 K = 1, KMAX+1, KMAX
         DO 6300 KK = -1, 0
            CALL RESET( DUFLUX(1,K,KK), IJSDIM )
            CALL RESET( DTFLUX(1,K,KK), IJSDIM )
 6300 CONTINUE 
*
      RETURN
      END
***********************************************************************
      SUBROUTINE VFTND3         !! vertical diffusion
     M         ( GTU   , GTV   , GTT   ,
     O           UMTX  ,         TMTX  ,
     I           UFLUX , VFLUX , TFLUX ,
     I           DUFLUX, DTFLUX,
     I           GDPM  , DELTP , IJS            )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc. */
#include        "zccom.F"                /* stand. physical const. */
*
*   [MODIFY] 
      REAL*8     GTU   ( IJSDIM, KMAX )        !! 
      REAL*8     GTV   ( IJSDIM, KMAX )        !! 
      REAL*8     GTT   ( IJSDIM, KMAX      )   !! tend. -> convergence
*   [OUTPUT] 
      REAL*8     UMTX  ( IJSDIM, KMAX,  -1:1 ) !! impl. mat. of u
      REAL*8     TMTX  ( IJSDIM, KMAX,  -1:1 ) !! impl. mat. of T
*
*   [INPUT] 
      REAL*8     UFLUX ( IJSDIM, KMAX+1 )         !! flux of U
      REAL*8     VFLUX ( IJSDIM, KMAX+1 )         !! flux of V
      REAL*8     TFLUX ( IJSDIM, KMAX+1         ) !! flux of T
      REAL*8     DUFLUX( IJSDIM, KMAX+1, -1:0 )   !! impl. mat. of U
      REAL*8     DTFLUX( IJSDIM, KMAX+1, -1:0 )   !! impl. mat. of T
      REAL*8     GDPM  ( IJSDIM, KMAX+1 )         !! pressure (half lev)
      REAL*8     DELTP                            !! delta(t)
      INTEGER    IJS                              !! IJ index
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
            GTT ( IJ,K ) = TFLUX ( IJ,K ) - TFLUX ( IJ,K+1 )
     &                   + GTT( IJ,K )
     &                      * CP * ( GDPM(IJ,K)-GDPM(IJ,K+1) )/GRAV
 1100 CONTINUE
*     
      DO 2100 K = 1, KMAX
         DO 2100 IJ = 1, IJSDIM
            UMTX ( IJ,K, 0 ) = ( GDPM ( IJ,K )-GDPM( IJ,K+1 ) )/GRAV
     &                         - DUFLUX( IJ,K,  0  )*DELTP
     &                         + DUFLUX( IJ,K+1,-1 )*DELTP
            UMTX ( IJ,K,-1 ) = - DUFLUX( IJ,K,  -1 )*DELTP
            UMTX ( IJ,K, 1 ) =   DUFLUX( IJ,K+1, 0 )*DELTP
*
            TMTX ( IJ,K, 0 ) = CP * ( GDPM(IJ,K)-GDPM(IJ,K+1) )/GRAV
     &                         - DTFLUX( IJ,K,   0 )*DELTP
     &                         + DTFLUX( IJ,K+1,-1 )*DELTP
            TMTX ( IJ,K,-1 ) = - DTFLUX( IJ,K,  -1 )*DELTP
            TMTX ( IJ,K, 1 ) =   DTFLUX( IJ,K+1, 0 )*DELTP
 2100 CONTINUE 
*
      CALL LUMAK3
     O         ( UMTX   ,
     I           IJSDIM , KMAX  )
      CALL LUMAK3
     O         ( TMTX   ,
     I           IJSDIM , KMAX  )
*
      CALL LUFWD3
     M         ( GTU   ,
     I           UMTX  , IJSDIM, KMAX )
      CALL LUFWD3
     M         ( GTV   ,
     I           UMTX  , IJSDIM, KMAX )
      CALL LUFWD3
     M         ( GTT    ,
     I           TMTX   , IJSDIM , KMAX  )
*
      RETURN
      END
************************************************************************
      SUBROUTINE VFTND4         !! tendency by vertical diffusion
     M         ( GTU   , GTV   , GTT   ,
     I           UMTX  ,         TMTX  ,
     I           IJS                             )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc. */
#include        "zccom.F"                /* stand. physical const. */
*
*   [MODIFY]
      REAL*8     GTU   ( IJSDIM, KMAX      )   !! 
      REAL*8     GTV   ( IJSDIM, KMAX      )   !! 
      REAL*8     GTT   ( IJSDIM, KMAX      )   !! conv. -> tendency
*
*   [INPUT] 
      REAL*8     UMTX  ( IJSDIM, KMAX,   -1:1 ) !! impl. mat. of u
      REAL*8     TMTX  ( IJSDIM, KMAX,   -1:1 ) !! impl. mat. of T
      INTEGER    IJS                            !! IJ index
*
*   [INTERNAL WORK] 
*
      CALL LUBWD3
     M         ( GTU   ,
     I           UMTX  , IJSDIM, KMAX )
      CALL LUBWD3
     M         ( GTV   ,
     I           UMTX  , IJSDIM, KMAX )
      CALL LUBWD3
     M         ( GTT   ,
     I           TMTX  , IJSDIM, KMAX  )
*
      RETURN
      END
