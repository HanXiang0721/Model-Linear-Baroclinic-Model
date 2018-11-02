* PACKAGE PSTEP !! physics: tendency
*
*   [HIS] 96/06/11(numaguti) AGCM5.4f
*         96/09/09(numaguti) SIRAT
*         96/10/23(numaguti) dissipation heating, move out ccover
*         96/11/21(numaguti) ALBEDO
*         96/12/16(numaguti) HISTIN for CLDW moved
*         97/01/23(numaguti) SNRAT/SIRAT
***********************************************************************
      SUBROUTINE PHSTEP    !! step in physics
     M         ( GDU   , GDV   , GDT   , GDPS  , GDQ   ,
     M           GDG   , GDW   , GDRIV ,
     O           IDSFC , FMOZ  , GDICE ,
     I           GDSIGD, GDPID , GDZS  ,
     I           TIME  , DELTA , DELTI , IJS            )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc. */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
#include        "zhdim.F" 
*
*   [MODIFY] 
      REAL*8     GDU   ( IJSDIM, KMAX        ) !! westerly u
      REAL*8     GDV   ( IJSDIM, KMAX        ) !! southern wind v
      REAL*8     GDT   ( IJSDIM, KMAX        ) !! temperature T
      REAL*8     GDPS  ( IJSDIM              ) !! surface pressure Ps
      REAL*8     GDQ   ( IJSDIM, KMAX,  NTR  ) !! humidity q
      REAL*8     GDG   ( IJSDIM, KGDIM, NMOZ ) !! soil temperature
      REAL*8     GDW   ( IJSDIM, KWDIM, NMOZ ) !! soil moisture
      REAL*8     GDRIV ( IJSDIM              ) !! river
*
*   [OUTPUT] 
      INTEGER    IDSFC ( IJSDIM, NMOZ )        !! surface index
      REAL*8     FMOZ  ( IJSDIM, NMOZ )        !! surface mosaic ratio
      REAL*8     GDICE ( IJSDIM, NMOZ )        !! sea ice
*
*   [INPUT] 
      REAL*8     GDSIGD( IJSDIM, KMAX+1      ) !! sigma velocity
      REAL*8     GDPID ( IJSDIM              ) !! pi velocity
      REAL*8     GDZS  ( IJSDIM )              !! surface topography
      REAL * 8   TIME                          !! time
      REAL*8     DELTA                         !! time step (atmos.)
      REAL*8     DELTI                         !! time step (int. var.)
      INTEGER    IJS                           !! IJ index
*
*   [INTERNAL WORK] 
      REAL*8     GTU   ( IJSDIM, KMAX      ) !! tendency of zonal wind
      REAL*8     GTV   ( IJSDIM, KMAX      ) !! tend. of merid. wind
      REAL*8     GTT   ( IJSDIM, KMAX      ) !! tend. term of T
      REAL*8     GTQ   ( IJSDIM, KMAX, NTR ) !! tend. of q
*
      REAL*8     GRZSD ( IJSDIM )               !! std. dev. of topo.
      REAL*8     RFLXSU( IJSDIM, KMAX+1, NCRF ) !! upward SW rad.
      REAL*8     RFLXSD( IJSDIM, KMAX+1, NCRF ) !! down.  SW rad.
      REAL*8     RFLXLU( IJSDIM, KMAX+1, NCRF ) !! upward LW rad.
      REAL*8     RFLXLD( IJSDIM, KMAX+1, NCRF ) !! down.  LW rad.
      REAL*8     DRFLXL( IJSDIM, KMAX+1, NCRF ) !! deriv. of up. LW
      REAL*8     PAR   ( IJSDIM )               !! PAR
      REAL*8     RFLXSS( IJSDIM, NMOZ )         !! upward SW rad. sfc.
      REAL*8     RFLXLG( IJSDIM )               !! upward long wave
      REAL*8     GRALB ( IJSDIM, NRALB, NMOZ )  !! albedo for each band
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
      REAL*8     QFLUX ( IJSDIM, KMAX+1, NTR )  !! flux of q
*
      REAL*8     DUFLUX( IJSDIM, KMAX+1,  -1:0 ) !! flux deriv. of U
      REAL*8     DTFLUX( IJSDIM, KMAX+1,  -1:0 ) !! flux deriv. of T
      REAL*8     DQFLUX( IJSDIM, KMAX+1,  -1:0 ) !! flux deriv. of q
*
      REAL*8     UMTX  ( IJSDIM, KMAX,  -1:1 )   !! impl. mat. of u
      REAL*8     TMTX  ( IJSDIM, KMAX,  -1:1 )   !! impl. mat. of T
      REAL*8     QMTX  ( IJSDIM, KMAX,  -1:1 )   !! impl. mat. of q
*
      REAL*8     GDCLW ( IJSDIM, KMAX )    !! cloud water
      REAL*8     GDCFRC( IJSDIM, KMAX )    !! ratio of cloudy area
*
      REAL*8     CUMCLW( IJSDIM, KMAX )    !! cloud water in cumulus
      REAL*8     CUMFRC( IJSDIM )          !! areal rate of cumulus
*
      REAL*8     GTTRL ( IJSDIM, KMAX )    !! long wave heating rate
      REAL*8     GTTRS ( IJSDIM, KMAX )    !! shor wave heating rate
*
      REAL*8     GPRCT ( IJSDIM )          !! precipitation
      REAL*8     GSNWT ( IJSDIM )          !! snow fall
      REAL*8     GPRCC ( IJSDIM, NTR )     !! rain fall (cumulus)
      REAL*8     GPRCL ( IJSDIM, NTR )     !! rain fall (stratus)
      REAL*8     GSNWC ( IJSDIM )          !! snow fall (cumulus)
      REAL*8     GSNWL ( IJSDIM )          !! snow fall (stratus)
*
      REAL*8     SNRAT ( IJSDIM, NMOZ  )   !! snow fraction
      REAL*8     SIRAT ( IJSDIM, NMOZ  )   !! ice fraction
*
      REAL*8     DELTP                     !! 2*dt for implicit
      REAL*8     DELI
      INTEGER    IJ
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
*   [ONCE] 
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ PSTEP: PHYSICS PACKAGE 96/09/09'
         OFIRST = .FALSE.
      ENDIF
*
      DELTP = MAX( DELTA, 2.D0*DELTI )
*
*   << monitor >>
*
      DELI = 1.D0/DELTA
      CALL HISTIF
     I   ( GDU,'DUPHY','physics u-tendency','m/s**2', 'ALEV',-DELI)
      CALL HISTIF
     I   ( GDV,'DVPHY','physics v-tendency','m/s**2', 'ALEV',-DELI)
      CALL HISTIF
     I   ( GDT,'DTPHY','physics T-tendency','K/s',    'ALEV',-DELI)
      CALL HISTIF
     I   ( GDPS,'DPPHY','physics Ps-tendency','hPa/s','ASFC',-DELI)
      CALL HISTIF
     I   ( GDQ,'DQPHY','physics q-tendency','1/s',    'ALEV',-DELI)
      IF ( ITL .GE. 2 ) THEN
         CALL HISTIF
     I (GDQ(1,1,ITL),'DLPHY','physics l-tendency','1/s','ALEV',-DELI)
      ENDIF
*
*    << set up / boundary condition >>
*
      CALL SFCBND    !! surface conditions
     O         ( IDSFC , FMOZ  , SNRAT , SIRAT , 
     O           GDICE , GRZSD ,
     M           GDG   , GDW   , 
     I           TIME  , IJS                    ) 
*
      CALL PSETUP
     O         ( GDTV  , GDTM  , GDTVM ,
     O           GDP   , GDPM  , GDZ   , GDZM  ,
     I           GDT   , GDQ   , GDPS  , GDZS   )
*
*    << moist process >>
*
      CALL RESET ( GTQ, IJSDIM*KMAX*NTR ) 
      CALL CLCSTR('CUMLUS')
      CALL CUMLUS
     O         ( GTT   , GTQ   ,
     O           GPRCC , GSNWC , CUMCLW, CUMFRC,
     I           GDT   , GDQ   , GDTM  , GDSIGD,
     I           GDP   , GDPM  , GDZ   , GDZM  , 
     I           DELTA , DELTI , IJS             )
      CALL CLCEND('CUMLUS')
      CALL GDINTG( GDT , GTT , DELTA, KMAX )
      CALL GDINTG( GDQ , GTQ , DELTA, KMAX*NTR )
      CALL HISTIN
     I   ( GTT, 'DTCUM' , 'cumulus heating',    'K/s ', 'ALEV'  )
      CALL HISTIN
     I   ( GTQ, 'DQCUM' , 'cumulus moistning',  '1/s ', 'ALEV'  )
      CALL HISTIN
     I   ( GTT, 'DTCND' , 'cum+LSC heating',    'K/s ', 'ALEV'  )
      CALL HISTIN
     I   ( GTQ, 'DQCND' , 'cum+LSC moistning',  '1/s ', 'ALEV'  )
*
      CALL RESET ( GTQ, IJSDIM*KMAX*NTR ) 
      CALL CLCSTR('LSCOND')
      CALL LSCOND
     O         ( GTT   , GTQ   ,
     O           GPRCL , GSNWL , GDCLW , GDCFRC,
     I           GDT   , GDQ   , GDTM  , GDSIGD,
     I           GDP   , GDPM  , GDZ   , GDZM  ,
     I           DELTA , IJS                      )
      CALL CLCEND('LSCOND')
      CALL GDINTG( GDT , GTT , DELTA, KMAX )
      CALL GDINTG( GDQ , GTQ , DELTA, KMAX*NTR )
      CALL HISTIN
     I   ( GTT, 'DTLSC' , 'L.S.cond. heating',   'K/s', 'ALEV'  )
      CALL HISTIN
     I   ( GTQ, 'DQLSC' , 'L.S.cond. moistening','1/s', 'ALEV'  )
      CALL HISTAD( GTT  , 'DTCND',  1.D0 )
      CALL HISTAD( GTQ  , 'DQCND',  1.D0 )
*
      CALL CMQSET( GDQ , IJS   )
*
*    << radiation >>
*
      CALL CLCSTR('RADIAT')
      CALL RADIAT
     O         ( RFLXSU, RFLXSD, RFLXLU, RFLXLD,
     O           DRFLXL, PAR   , RFLXSS, GRALB ,
     I           GDTM  , GDPM  , GDZM  ,
     I           GDT   , GDP   , GDQ   ,
     M           GDCLW , GDCFRC, CUMCLW, CUMFRC,
     I           GDG   , GDW   , GDICE ,
     I           SNRAT , SIRAT , IDSFC , FMOZ  , 
     I           TIME  , IJS                      )
      CALL CLCEND('RADIAT')
*
*    << vert diff & surface >>
*
      CALL RESET ( GTU ,  IJSDIM*KMAX         )
      CALL RESET ( GTV ,  IJSDIM*KMAX         )
      CALL RESET ( GTT ,  IJSDIM*KMAX         )
      CALL RESET ( GTQ ,  IJSDIM*KMAX*NTR     )
      CALL RESET ( UFLUX, IJSDIM*(KMAX+1)     )
      CALL RESET ( VFLUX, IJSDIM*(KMAX+1)     )
      CALL RESET ( TFLUX, IJSDIM*(KMAX+1)     )
      CALL RESET ( QFLUX, IJSDIM*(KMAX+1)*NTR )
*
      CALL CLCSTR('VDF1')
      CALL VDFFLX
     O         ( UFLUX , VFLUX , TFLUX , QFLUX ,
     O           DUFLUX,         DTFLUX, DQFLUX,
     I           GDU   , GDV   , GDT   , GDQ   , 
     I           GDTV  , GDTVM , GDCFRC,
     I           GDP   , GDPM  , GDZ   , GDZM  , 
     I           DELTP , IJS                     )
      CALL VFTND1 
     M         ( GTU   , GTV   , GTT   , GTQ   , 
     O           UMTX  ,         TMTX  , QMTX  , 
     I           UFLUX , VFLUX , TFLUX , QFLUX , 
     I           RFLXSU, RFLXSD, RFLXLU, RFLXLD,
     I           DUFLUX, DTFLUX, DQFLUX, 
     I           GDPM  , DELTP , IJS             )
      CALL CLCEND('VDF1')
*
      CALL CLCSTR('SURF')
      CALL SURFCE
     M         ( GTU   , GTV   , GTT   , GTQ   , 
     M           UFLUX , VFLUX , TFLUX , QFLUX ,
     M           GDG   , GDW   , GDRIV , SNRAT ,
     O           RFLXLG, 
     I           UMTX          , TMTX  , QMTX  , 
     I           RFLXSU, RFLXSD, RFLXLU, RFLXLD,
     I           DRFLXL, PAR   , RFLXSS, GRALB ,
     I           GPRCC , GPRCL , GSNWC , GSNWL ,
     I           GDU   , GDV   , GDT   , GDQ   , 
     I           GDP   , GDPM  , GDTM  , 
     I           GDZ   , GDZM  , GRZSD ,
     I           TIME  , DELTI , DELTP , 
     I           GDICE , SIRAT , IDSFC , FMOZ  , IJS  )
      CALL CLCEND('SURF')
*
      CALL CLCSTR('VDF2')
      CALL VFTND2  
     M         ( GTU   , GTV   , GTT   , GTQ   , 
     I           UMTX  ,         TMTX  , QMTX  , 
     I           IJS                             )
      CALL RADTND
     M         ( GTT   , RFLXSU, RFLXSD, RFLXLU, RFLXLD,
     O           GTTRL , GTTRS ,
     I           DRFLXL, RFLXLG, GDPM  , DELTP , IJS    )
      CALL FLXCOR
     M         ( UFLUX , VFLUX , TFLUX , QFLUX ,
     I           GTU   , GTV   , GTT   , GTQ   ,
     I           DUFLUX,         DTFLUX, DQFLUX,
     I           DELTP , IJS                     )
      CALL CLCEND('VDF2')
*
      CALL HISTIN
     I   ( GTU,  'DUVDF', 'diffusion du/dt',   'm/s**2','ALEV' )
      CALL HISTIN
     I   ( GTV,  'DVVDF', 'diffusion dv/dt',   'm/s**2','ALEV' )
      CALL HISTIN
     I   ( GTT,  'DTVDF', 'diffusion heating  ','K/s  ','ALEV' )
      CALL HISTIN
     I   ( GTQ,  'DQVDF', 'diffusion moistning','1/s  ','ALEV' )
      CALL HISTAD ( GTTRL, 'DTVDF',  -1.D0 )
      CALL HISTAD ( GTTRS, 'DTVDF',  -1.D0 )
*
      DO 5100 IJ = 1, IJSDIM
         GPRCT( IJ ) = GPRCC( IJ,1 ) + GPRCL( IJ,1 ) 
     &               + GSNWC( IJ   ) + GSNWL( IJ   )
         GSNWT( IJ ) = GSNWC( IJ   ) + GSNWL( IJ   )
 5100 CONTINUE 
*
      CALL PDIAG
     I         ( TFLUX , QFLUX ,
     I           RFLXSU, RFLXSD, RFLXLU, RFLXLD,
     I           GTT   , GTQ   , 
     I           GDT   , GDQ   , GDPM  ,
     I           DELTI , IJS                     )
*
*    << gravity wave drag >>
*
      CALL HISTIF
     I   ( GTU,  'DUGRAV','gravity wave du/dt','m/s**2 ','ALEV',-1.D0 )
      CALL HISTIF
     I   ( GTV,  'DVGRAV','gravity wave dv/dt','m/s**2 ','ALEV',-1.D0 )
*
      CALL CLCSTR('GRAVTY')
      CALL GRAVTY 
     M         ( GTU   , GTV   , 
     I           GDU   , GDV   , GDT   , GDP   ,
     I           GDZ   , GDTM  , GDPM  , GRZSD , IJS     )
      CALL CLCEND('GRAVTY')
*
      CALL HISTAD( GTU , 'DUGRAV',  1.D0 )
      CALL HISTAD( GTV , 'DVGRAV',  1.D0 )
*      
*     << dissipation heating >>
*
      CALL DISSIP
     M         ( GTT   ,
     I           GTU   , GTV   , GDU   , GDV     )
*      
*     << time integration >>
*
      CALL GDINTG( GDU , GTU , DELTA, KMAX     )
      CALL GDINTG( GDV , GTV , DELTA, KMAX     )
      CALL GDINTG( GDT , GTT , DELTA, KMAX     )
      CALL GDINTG( GDQ , GTQ , DELTA, KMAX*NTR )
*
*    << mass source >>
*
      CALL PMSRC
     M         ( GDPS  , GDQ   ,
     I           GPRCT , QFLUX , DELTA, 
     W           GTT                     )
*
*    << dry convective adjustment >>
*
      CALL HISTIF
     I   ( GDT, 'DTDAD','dry conv. heating',  'K/s ','ALEV',-DELI )
      CALL HISTIF
     I   ( GDQ, 'DQDAD','dry conv. moistning','1/s ','ALEV',-DELI )
*
      CALL CLCSTR('DADJST')
      CALL DADJST      !! dry convective adjustment
     M         ( GDT   , GDQ   ,
     I           GDP   , GDPM  , DELTA , IJS    )
      CALL CLCEND('DADJST')
*
      CALL HISTAD( GDT, 'DTDAD' , DELI )
      CALL HISTAD( GDQ, 'DQDAD' , DELI )
*
*     << monitor >>
*
      CALL POUTS
     I         ( GPRCT , GSNWT , GPRCC , GPRCL , GSNWC , GSNWL ,
     I           UFLUX , VFLUX , TFLUX , QFLUX , GTTRL , GTTRS ,
     I           RFLXLU, RFLXLD, RFLXSU, RFLXSD, RFLXSS, PAR   , 
     I           GDT   , GDP   , GDQ   ,
     W           GTQ                                             )
*
      CALL HISTAD ( GDU    , 'DUPHY',  DELI )
      CALL HISTAD ( GDV    , 'DVPHY',  DELI )
      CALL HISTAD ( GDT    , 'DTPHY',  DELI )
      CALL HISTAD ( GDPS   , 'DPPHY',  DELI )
      CALL HISTAD ( GDQ    , 'DQPHY',  DELI )
      IF ( ITL .GE. 2 ) THEN
         CALL HISTAD ( GDQ(1,1,ITL), 'DLPHY',  DELI )
      ENDIF
*
      RETURN
      END
*************************************************************************
      SUBROUTINE POUTS    !! output physics fluxes
     I         ( GPRCT , GSNWT , GPRCC , GPRCL , GSNWC , GSNWL ,
     I           UFLUX , VFLUX , TFLUX , QFLUX , GTTRL , GTTRS ,
     I           RFLXLU, RFLXLD, RFLXSU, RFLXSD, RFLXSS, PAR   , 
     I           GDT   , GDP   , GDQ   ,
     W           GTQ                                             )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc. */
#include        "zccom.F"                /* stand. physical const. */
#include        "zhdim.F" 
*
*   [INPUT] 
      REAL*8     GPRCT ( IJSDIM )          !! precipitation
      REAL*8     GSNWT ( IJSDIM )          !! snow fall
      REAL*8     GPRCC ( IJSDIM, NTR )     !! rain fall (cumulus)
      REAL*8     GPRCL ( IJSDIM, NTR )     !! rain fall (stratus)
      REAL*8     GSNWC ( IJSDIM )          !! snow fall (cumulus)
      REAL*8     GSNWL ( IJSDIM )          !! snow fall (stratus)
*
      REAL*8     UFLUX ( IJSDIM, KMAX+1      )  !! flux of U
      REAL*8     VFLUX ( IJSDIM, KMAX+1      )  !! flux of V
      REAL*8     TFLUX ( IJSDIM, KMAX+1      )  !! flux of T
      REAL*8     QFLUX ( IJSDIM, KMAX+1, NTR )  !! flux of q
*
      REAL*8     GTTRL ( IJSDIM, KMAX )    !! long wave heating rate
      REAL*8     GTTRS ( IJSDIM, KMAX )    !! shor wave heating rate
      REAL*8     RFLXSU( IJSDIM, KMAX+1, NCRF ) !! upward shot wave
      REAL*8     RFLXSD( IJSDIM, KMAX+1, NCRF ) !! down. SW rad.
      REAL*8     RFLXLU( IJSDIM, KMAX+1, NCRF ) !! upward long wave
      REAL*8     RFLXLD( IJSDIM, KMAX+1, NCRF ) !! up. LW rad.
      REAL*8     RFLXSS( IJSDIM, NMOZ )         !! upward SW rad. sfc.
      REAL*8     PAR   ( IJSDIM )               !! PAR
*
      REAL*8     GDT   ( IJSDIM, KMAX      )  !! temperature T
      REAL*8     GDP   ( IJSDIM, KMAX )       !! pressure P
      REAL*8     GDQ   ( IJSDIM, KMAX, NTR )  !! humidity q
*
*   [WORK] 
      REAL*8     GTQ   ( IJSDIM, KMAX, NTR ) !! tend. of q
*
      INTEGER    IJ, K, IC, LT, M
      REAL*8     QSAT
      CHARACTER  HITEM *(NCC)
      CHARACTER  HTITL *(NCC*2)
*   [INTERNAL FUNC] 
#include        "zqsat.F"                /* saturate vapor pressure */
*
*
      CALL HISTIF
     I   ( GPRCT,'RAIN' ,'precipitation        ','W/m**2','ASFC', EL )
      CALL HISTIF
     I   ( GSNWT,'SNWFLX','snow precipitation  ','W/m**2','ASFC', EL )
      CALL HISTIF
     I   ( GPRCC,'RAINC','cumulus precipitation','W/m**2','ASFC', EL )
      CALL HISTAD
     I   ( GSNWC,'RAINC', EL )
      CALL HISTIF
     I   ( GPRCL,'RAINL','L.S.precipitation    ','W/m**2','ASFC', EL )
      CALL HISTAD
     I   ( GSNWL,'RAINL', EL )
*
      CALL HISTIN
     I   ( GPRCT,'PRCP' ,'precipitation        ','kg/m**2/s','ASFC')
      CALL HISTIN
     I   ( GSNWT,'SNWFAL','snow fall           ','kg/m**2/s','ASFC')
      CALL HISTIN
     I   ( GPRCC,'PRCPC','cumulus precipitation','kg/m**2/s','ASFC')
      CALL HISTAD
     I   ( GSNWC,'PRCPC', 1.D0 )
      CALL HISTIN
     I   ( GPRCL,'PRCPL','L.S.precipitation    ','kg/m**2/s','ASFC')
      CALL HISTAD
     I   ( GSNWL,'PRCPL', 1.D0 )
*
      CALL HISTIN
     I   ( UFLUX(1,1),  'TAUX','surface stress(x) ', 'N/m**2','ASFC' )
      CALL HISTIN
     I   ( VFLUX(1,1),  'TAUY','surface stress(y) ', 'N/m**2','ASFC' )
      CALL HISTIN
     I   ( TFLUX(1,1),  'SENS' ,'sensible heat flux','W/m**2','ASFC' )
      CALL HISTIF
     I   ( QFLUX(1,1,1),'EVAP','latent heat flux','W/m**2','ASFC',EL )
*
      CALL HISTIN
     I   ( RFLXLU      ,'RFLUXL','rad. flux(long)', 'W/m**2', 'AMLEV')
      CALL HISTAD
     I   ( RFLXLD      ,'RFLUXL', -1.D0 )
      CALL HISTIN
     I   ( RFLXSU      ,'RFLUXS','rad. flux(short)','W/m**2', 'AMLEV')
      CALL HISTAD
     I   ( RFLXSD      ,'RFLUXS', -1.D0 )
*
      CALL HISTIN
     I  ( RFLXLU(1,KMAX+1,1),'OLR','top longwave ','W/m**2','ASFC')
      CALL HISTIN
     I  ( RFLXLU(1,1,     1),'SLR','surf.longwave','W/m**2','ASFC')
      CALL HISTAD
     I  ( RFLXLD(1,1,     1),'SLR', -1.D0 )
      CALL HISTIN
     I  ( RFLXLU(1,1,     1),'SLRU','surf.long.up',  'W/m**2','ASFC')
      CALL HISTIN
     I  ( RFLXLD(1,1,     1),'SLRD','surf.long.down','W/m**2','ASFC')
*
      CALL HISTIN
     I  ( RFLXSU(1,KMAX+1,1),'OSR','top shortwave ','W/m**2','ASFC')
      CALL HISTAD
     I  ( RFLXSD(1,KMAX+1,1),'OSR', -1.D0 )
      CALL HISTIN
     I  ( RFLXSU(1,KMAX+1,1),'OSRU','top short.up ','W/m**2','ASFC')
      CALL HISTIN
     I  ( RFLXSD(1,KMAX+1,1),'OSRD','top short.down','W/m**2','ASFC')
      CALL HISTIN
     I  ( RFLXSU(1,1,     1),'SSR','surf.shortwave','W/m**2','ASFC')
      CALL HISTAD
     I  ( RFLXSD(1,1,     1),'SSR', -1.D0 )      
      CALL HISTIN
     I  ( RFLXSU(1,1,     1),'SSRU','surf.short.up',  'W/m**2','ASFC')
      CALL HISTIN
     I  ( RFLXSD(1,1,     1),'SSRD','surf.short.down','W/m**2','ASFC')
      CALL HISTIN
     I  ( PAR               ,'PAR ','PAR rad.flux   ','W/m**2','ASFC')
*
      IF ( NCRF .EQ. 2 ) THEN
         IC = 2
         CALL HISTIN
     I ( RFLXLU(1,1,IC),'RFLUXLC','clr.radflux(long)','W/m**2','AMLEV')
         CALL HISTAD
     I ( RFLXLD(1,1,IC),'RFLUXLC', -1.D0 )
         CALL HISTIN
     I ( RFLXSU(1,1,IC),'RFLUXSC','clr.radflux(shrt)','W/m**2','AMLEV')
         CALL HISTAD
     I ( RFLXSD(1,1,IC),'RFLUXSC', -1.D0 )
*
         CALL HISTIN
     I ( RFLXLU(1,KMAX+1,IC),'OLRC','clr.top lng.','W/m**2','ASFC')
         CALL HISTIN
     I ( RFLXLU(1,1,     IC),'SLRC','clr.sfc.lng. ','W/m**2','ASFC')
         CALL HISTAD
     I ( RFLXLD(1,1,     IC),'SLRC', -1.D0 )
         CALL HISTIN
     I ( RFLXLU(1,1,     IC),'SLRUC','clr.sfc.lng.up','W/m**2','ASFC')
         CALL HISTIN
     I ( RFLXLD(1,1,     IC),'SLRDC','clr.sfc.lng.dn','W/m**2','ASFC')
*
         CALL HISTIN
     I ( RFLXSU(1,KMAX+1,IC),'OSRC','clr.top.sht.','W/m**2','ASFC')
         CALL HISTAD
     I ( RFLXSD(1,KMAX+1,IC),'OSRC', -1.D0 )
         CALL HISTIN
     I ( RFLXSU(1,KMAX+1,IC),'OSRUC','clr.top sht.up','W/m**2','ASFC')
         CALL HISTIN
     I ( RFLXSD(1,KMAX+1,IC),'OSRDC','clr.top sht.dn','W/m**2','ASFC')
         CALL HISTIN
     I ( RFLXSU(1,1,     IC),'SSRC', 'clr.sfc.sht.  ','W/m**2','ASFC')
         CALL HISTAD
     I ( RFLXSD(1,1,     IC),'SSRC', -1.D0 )      
         CALL HISTIN
     I ( RFLXSU(1,1,     IC),'SSRUC','clr.sfc.sht.up','W/m**2','ASFC')
         CALL HISTIN
     I ( RFLXSD(1,1,     IC),'SSRDC','clr.sfc.sht.dn','W/m**2','ASFC')
      ENDIF
*
      CALL HISTIN
     I   ( GTTRL, 'DTRADL', 'radiative heating(long)','K/s',  'ALEV' )
      CALL HISTIN
     I   ( GTTRS, 'DTRADS','radiative heating(short)','K/s',  'ALEV' )
*
      DO 7100 K = 1, KMAX
         DO 7100 IJ = 1, IJSDIM
            QSAT = FQSAT( GDT( IJ,K ), GDP( IJ,K ) )
            GTQ( IJ,K,1 ) = GDQ( IJ,K,1 )/QSAT 
 7100 CONTINUE 
      CALL HISTIN
     I   ( GTQ,'RH','relative humidity', '       ',  'ALEV' )
*
      DO 7200 LT = ITL+1, NTR
         CALL CATNUM( HITEM, 'PRC',             LT-ITL, '(I2.2)' )
         CALL CATNUM( HTITL, 'tracer precip. ', LT-ITL, '(I2.2)' )
         CALL HISTIN
     I       ( GPRCL(1,LT),  HITEM, HTITL, 'kg/m**2/s', 'ASFC'   )
         CALL HISTAD
     I       ( GPRCC(1,LT), HITEM, 1.D0 )
         CALL CATNUM( HITEM, 'QFL',             LT-ITL, '(I2.2)' )
         CALL CATNUM( HTITL, 'tracer sfc flux ',LT-ITL, '(I2.2)' )
         CALL HISTIN
     I       ( QFLUX(1,1,LT),HITEM, HTITL, 'kg/m**2/s', 'ASFC'   )
 7200 CONTINUE 
*
      RETURN
      END
