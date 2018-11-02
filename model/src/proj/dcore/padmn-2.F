* PACKAGE PADMN  !! physics: main routine <<large memory & fast>>
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/09/09(numaguti) RIVERFL
***************************************************************
* PHYSCS/PHSTEP merged
*
      SUBROUTINE PHYSCS      !! main routine of physics
     M         ( GDU   , GDV   , GDT   , GDPS  , GDQ   ,
     I           GDSIGD, GDPID , GDZS  ,
     I           TIME  , DELTA , DELTI                   )
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
*
*   [INPUT] 
      REAL*8     GDSIGD( IJSDIM, KMAX+1      ) !! sigma velocity
      REAL*8     GDPID ( IJSDIM              ) !! pi velocity
      REAL*8     GDZS  ( IJSDIM )              !! surface topography
      REAL*8     TIME                          !! time
      REAL*8     DELTA                         !! time step (atmos.)
      REAL*8     DELTI                         !! time step (int. var.)
*
*   [INTERNAL WORK] 
      REAL*8     GTU   ( IJSDIM, KMAX      ) !! tendency of zonal wind
      REAL*8     GTV   ( IJSDIM, KMAX      ) !! tend. of merid. wind
      REAL*8     GTT   ( IJSDIM, KMAX      ) !! tend. term of T
      REAL*8     GTQ   ( IJSDIM, KMAX, NTR ) !! tend. of q
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
      REAL*8     DUFLUX( IJSDIM, KMAX+1,  -1:0 ) !! flux deriv. of U
      REAL*8     UMTX  ( IJSDIM, KMAX,  -1:1 )   !! impl. mat. of u
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
      REAL*8     DELTP                     !! 2*dt for implicit
      REAL*8     DELI
      INTEGER    IJ
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
*   [ONCE] 
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ PADMN: PHYSICS PMAIN 10/02/03 '
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
      CALL PSETUP
     O         ( GDTV  , GDTM  , GDTVM ,
     O           GDP   , GDPM  , GDZ   , GDZM  ,
     I           GDT   , GDQ   , GDPS  , GDZS   )
*
*    << moist process >>
*
#ifdef OPT_MOIST
      CALL RESET ( GTT, IJSDIM*KMAX     ) 
      CALL RESET ( GTQ, IJSDIM*KMAX*NTR ) 
      CALL CLCSTR('CUMLUS')
      CALL CUMLUS
     O         ( GTT   , GTQ   ,
     O           GPRCC , GSNWC , CUMCLW, CUMFRC,
     I           GDT   , GDQ   , GDTM  , GDSIGD,
     I           GDP   , GDPM  , GDZ   , GDZM  , 
     I           DELTA , DELTI                   )
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
      CALL RESET ( GTT, IJSDIM*KMAX     ) 
      CALL RESET ( GTQ, IJSDIM*KMAX*NTR ) 
      CALL CLCSTR('LSCOND')
      CALL LSCOND
     O         ( GTT   , GTQ   ,
     O           GPRCL , GSNWL , GDCLW , GDCFRC,
     I           GDT   , GDQ   , GDTM  , GDSIGD,
     I           GDP   , GDPM  , GDZ   , GDZM  ,
     I           DELTA                           )
      CALL CLCEND('LSCOND')
      CALL GDINTG( GDT , GTT , DELTA, KMAX )
      CALL GDINTG( GDQ , GTQ , DELTA, KMAX*NTR )
      CALL HISTIN
     I   ( GTT, 'DTLSC' , 'L.S.cond. heating',   'K/s', 'ALEV'  )
      CALL HISTIN
     I   ( GTQ, 'DQLSC' , 'L.S.cond. moistening','1/s', 'ALEV'  )
      CALL HISTAD( GTT  , 'DTCND',  1.D0 )
      CALL HISTAD( GTQ  , 'DQCND',  1.D0 )
#endif
*
*    << vert diff & surface >>
*
      CALL RESET ( GTU ,  IJSDIM*KMAX         )
      CALL RESET ( GTV ,  IJSDIM*KMAX         )
      CALL RESET ( GTT ,  IJSDIM*KMAX         )
      CALL RESET ( GTQ ,  IJSDIM*KMAX*NTR     )
      CALL RESET ( UFLUX, IJSDIM*(KMAX+1)     )
      CALL RESET ( VFLUX, IJSDIM*(KMAX+1)     )
*
      CALL CLCSTR('VDF1')
* modified
      CALL VDFFLX
     O         ( UFLUX , VFLUX , DUFLUX,        
     I           GDU   , GDV   , 
     I           GDT   , GDTV  , GDTVM , 
     I           GDP   , GDPM  , GDZ   , GDZM   )
* modified
      CALL VFTND1 
     M         ( GTU   , GTV   , 
     O           UMTX  ,         
     I           UFLUX , VFLUX , DUFLUX,
     I           GDPM  , DELTP            )

      CALL CLCEND('VDF1')
*
      CALL CLCSTR('SURF')
* modified
      CALL SURFCE
     M         ( GTU   , GTV   , UFLUX , VFLUX , 
     I           UMTX  , GDU   , GDV   , 
     I           GDPM  , GDTVM , GDZ   , GDZM  , DELTP  )
      CALL CLCEND('SURF')
* modified
      CALL CLCSTR('VDF2')
      CALL VFTND2  
     M         ( GTU   , GTV   , 
     I           UMTX              )
* modified
      CALL FLXCOR
     M         ( UFLUX , VFLUX , 
     I           GTU   , GTV   , 
     I           DUFLUX,         
     I           DELTP                     )
      CALL CLCEND('VDF2')
*
      CALL HISTIN
     I   ( GTU,  'DUVDF', 'diffusion du/dt',   'm/s**2','ALEV' )
      CALL HISTIN
     I   ( GTV,  'DVVDF', 'diffusion dv/dt',   'm/s**2','ALEV' )
*
#ifdef OPT_MOIST
      DO 5100 IJ = 1, IJSDIM
         GPRCC( IJ,1 ) = GPRCC( IJ,1 ) + GSNWC( IJ   )
         GPRCL( IJ,1 ) = GPRCL( IJ,1 ) + GSNWL( IJ   )
         GPRCT( IJ   ) = GPRCC( IJ,1 ) + GPRCL( IJ,1 ) 
 5100 CONTINUE 
#endif
*      
*     << time integration >>
*
#ifndef OPT_NOVDF
      CALL GDINTG( GDU , GTU , DELTA, KMAX     )
      CALL GDINTG( GDV , GTV , DELTA, KMAX     )
#endif
CX      CALL GDINTG( GDT , GTT , DELTA, KMAX     )
CX      CALL GDINTG( GDQ , GTQ , DELTA, KMAX*NTR )
*
*     << monitor >>
*
      CALL HISTIN
     I   ( GPRCT,'PRCP' ,'precipitation        ','kg/m**2/s','ASFC')
      CALL HISTIN
     I   ( GPRCC,'PRCPC','cumulus precipitation','kg/m**2/s','ASFC')
      CALL HISTIN
     I   ( GPRCL,'PRCPL','L.S.precipitation    ','kg/m**2/s','ASFC')
      CALL HISTIN
     I   ( UFLUX(1,1),'TAUX','surface stress(x) ','N/m**2','ASFC' )
      CALL HISTIN
     I   ( VFLUX(1,1),'TAUY','surface stress(y) ','N/m**2','ASFC' )
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
     I           GTTRL , GTTRS ,
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
 7200 CONTINUE 
*
      RETURN
      END
