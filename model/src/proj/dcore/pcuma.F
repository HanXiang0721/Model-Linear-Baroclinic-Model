* PACKAGE PCUMA  !!  physics: simple A-S cumulus model
*
*  [HIS] 95/03/15(numaguti) AGCM5.4.01
*        96/05/13(numaguti) speed up, tracer
*        96/09/18(numaguti) GSIGD
*        96/10/03(numaguti) more strict TEST 
*        96/11/20(numaguti) PRECR
*        96/12/02(numaguti) OPT_WTRACE
*        97/07/29(numaguti) output FPRCC
**********************************************************************
      SUBROUTINE CUMLUS    !! simplified A-S cumulus parameterization
     O         ( GTT   , GTQ   ,
     O           GPRCC , GSNWC , CUMCLW, CUMFRC, 
     I           GDT   , GDQ   , GDTM  , GDSIGD,
     I           GDP   , GDPM  , GDZ   , GDZM  , 
     I           DELTA , DELTI                     )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"
#include        "zccom.F"                /* stand. physical const. */
*
*   [OUTPUT] 
      REAL*8     GTT   ( IJSDIM, KMAX      ) !! heating rate
      REAL*8     GTQ   ( IJSDIM, KMAX, NTR ) !! change in q
      REAL*8     GPRCC ( IJSDIM, NTR       ) !! rainfall
      REAL*8     GSNWC ( IJSDIM            ) !! snowfall
      REAL*8     CUMCLW( IJSDIM, KMAX      ) !! cloud water in cumulus
      REAL*8     CUMFRC( IJSDIM            ) !! cumulus cloud fraction
*
*   [INPUT] 
      REAL*8     GDT   ( IJSDIM, KMAX      ) !! temperature T
      REAL*8     GDQ   ( IJSDIM, KMAX, NTR ) !! humidity, tracer
      REAL*8     GDTM  ( IJSDIM, KMAX+1    ) !! temperature T
      REAL*8     GDSIGD( IJSDIM, KMAX+1    ) !! sigma velocity
      REAL*8     GDP   ( IJSDIM, KMAX      ) !! pressure P
      REAL*8     GDPM  ( IJSDIM, KMAX+1    ) !! pressure (half lev)
      REAL*8     GDZ   ( IJSDIM, KMAX      ) !! altitude
      REAL*8     GDZM  ( IJSDIM, KMAX+1    ) !! altitude
      REAL*8     DELTA                       !! delta(t) (atmosphere)
      REAL*8     DELTI                       !! delta(t) (int. var.)
*
*   [INTERNAL WORK] 
#ifndef OPT_NOCOMMON
      COMMON 
     &           GDW   , DELP  , GDQS  , GAM   , GAMX  ,
     &           GDH   , GDHS  , 
     &           GCYM  , GCHB  , GCWB  , DY    , DH    ,
     &           DYM   , ELAM  , GCYT  , GCHT  , GCWT  , GPRCT ,
     &           GXH   , GXQ   , ACWF  , ACWFX , 
     &           GCLZ  , GPRCZ , CBMFX , CMMFX ,
     &           GMFLX , GPRCP , GMDD  , GMDDB
#endif
*
      REAL*8     GDW   ( IJSDIM, KMAX )
      REAL*8     DELP  ( IJSDIM, KMAX )
      REAL*8     GDQS  ( IJSDIM, KMAX )
      REAL*8     GAM   ( IJSDIM, KMAX )
      REAL*8     GAMX  ( IJSDIM, KMAX )
      REAL*8     GDH   ( IJSDIM, KMAX )
      REAL*8     GDHS  ( IJSDIM, KMAX )
*
      REAL*8     GCYM  ( IJSDIM, KMAX )
      REAL*8     GCHB  ( IJSDIM )
      REAL*8     GCWB  ( IJSDIM )
      REAL*8     DH    ( IJSDIM, KMAX )
      REAL*8     DW    ( IJSDIM, KMAX )
      REAL*8     ELAM  ( IJSDIM, KMAX )   !! entrainment ratio
      REAL*8     DY    ( IJSDIM, KMAX )
      REAL*8     DYM   ( IJSDIM, KMAX )
      REAL*8     GCYT  ( IJSDIM, KMAX )   !! norm. mass flux @top
      REAL*8     GCHT  ( IJSDIM, KMAX )   !! cloud top MSE
      REAL*8     GCWT  ( IJSDIM, KMAX )   !! adiabatic cloud top water
      REAL*8     GPRCT ( IJSDIM, KMAX )   !! precipitation/M
*
      REAL*8     GXH   ( IJSDIM, KMAX )
      REAL*8     GXQ   ( IJSDIM, KMAX )
      REAL*8     GXHB  ( IJSDIM )
      REAL*8     GXWB  ( IJSDIM )
      REAL*8     DXH   ( IJSDIM, KMAX )
      REAL*8     DXW   ( IJSDIM, KMAX )
      REAL*8     ACWF  ( IJSDIM, KMAX )   !! cloud work function
      REAL*8     ACWFX ( IJSDIM )         !! cloud work function
      INTEGER    IBUOY ( IJSDIM )         !! buoyancy flag
      REAL*8     GCLZ  ( IJSDIM, KMAX )   !! cloud water
      REAL*8     GPRCZ ( IJSDIM, KMAX )   !! precipitation
      REAL*8     CBMFX ( IJSDIM, KMAX )   !! cloud base mass flux
      REAL*8     CMMFX ( IJSDIM, KMAX )   !! middle conv. mass flux
*
      REAL*8     GMFLX ( IJSDIM, KMAX+1 ) !! mass flux
      REAL*8     GPRCP ( IJSDIM, KMAX+1 ) !! precipitation
*
      REAL*8     GEVPP ( IJSDIM, KMAX   ) !! evaporation
      REAL*8     GMDD  ( IJSDIM, KMAX+1 ) !! downdraft mass flux
      REAL*8     GMDDB ( IJSDIM )         !! downdraft mass flux
      INTEGER    KB    ( IJSDIM )
      INTEGER    KBMX
      INTEGER    KTMX
      INTEGER    KTMN
*
      INTEGER    IJ, K, KT, IFPAR, JFPAR
*
*   [INTERNAL PARM] 
      REAL*8     FTEST                       !! test flux
      INTEGER    NTRCUM                      !! involved tracers No.
      LOGICAL    OFIRST
      NAMELIST /NMASCM/ FTEST, NTRCUM
*   [INTERNAL FUNC] 
#include        "zqsat.F"                /* saturate vapor pressure */
*
      DATA       FTEST  / 0.01   /
      DATA       NTRCUM / NTR    /
      DATA       OFIRST / .TRUE. /
*
*   [ONCE] 
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ PCUMA: SIMPLE ARAKAWA-SCHUBERT 96/11/20'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMASCM, END=190 )
  190    WRITE  ( JFPAR, NMASCM )
      ENDIF
*
      CALL COPY( GDW, GDQ, IJSDIM*KMAX )
      IF ( ITL .GE. 2 ) THEN
         DO 1000 K = 1, KMAX
            DO 1000 IJ = 1, IJSDIM
               GDW( IJ,K ) = GDQ( IJ,K,1 ) + GDQ( IJ,K,ITL )
 1000    CONTINUE 
      ENDIF
      DO 1100 K = 1, KMAX
         DO 1100 IJ = 1, IJSDIM
            DELP ( IJ,K ) = GDPM( IJ,K   ) - GDPM( IJ,K+1 )
            GDQS ( IJ,K ) = FQSAT( GDT ( IJ,K ), GDP ( IJ,K ) )
            GAM  ( IJ,K ) = EL/CP*FDQSAT( GDT( IJ,K ), GDQS( IJ,K )  )
            GAMX ( IJ,K ) = GAM( IJ,K )/( 1.+GAM( IJ,K ) )/EL 
            GDH  ( IJ,K ) = CP*GDT ( IJ,K ) + GRAV*GDZ ( IJ,K ) 
     &                    + EL*GDQ ( IJ,K,1 )
            GDHS ( IJ,K ) = CP*GDT ( IJ,K ) + GRAV*GDZ ( IJ,K )
     &                    + EL*GDQS( IJ,K )
 1100 CONTINUE
      CALL RESET( GTT,   IJSDIM*KMAX     )
      CALL RESET( GTQ,   IJSDIM*KMAX*NTR )
      CALL RESET( GPRCC, IJSDIM*NTR      )
      CALL RESET( GSNWC, IJSDIM )
      CALL RESET( CUMCLW,IJSDIM*KMAX )
      CALL RESET( CUMFRC,IJSDIM )
      CALL RESET( GCYM,  IJSDIM*KMAX )
      CALL RESET( ACWF,  IJSDIM*KMAX )
      CALL RESET( CBMFX, IJSDIM*KMAX )
      CALL RESET( CMMFX, IJSDIM*KMAX )
      CALL RESET( GMFLX, IJSDIM*(KMAX+1) )
      CALL RESET( GPRCP, IJSDIM*(KMAX+1) )
      CALL RESET( GEVPP, IJSDIM*KMAX )
*
      CALL    CUMBAS
     O         ( KB    , GCYM  , KBMX  , KTMX  ,
     I           GDH   , GDW   , GDHS  , GDQS  , 
     I           GAM   , GDZM                    )
*
      CALL    CUMUP          !! Cloud structure
     O         ( GCHB  , GCWB  , DH    , DW    ,
     O           ELAM  , DY    , DYM   ,
     O           GCYT  , GCHT  , GCWT  , GPRCT , 
     I           GDH   , GDW   , GDHS  , GDQS  , GDT   ,
     I           GDZ   , GDZM  , GAM   , GAMX  ,
     I           GCYM  , KB    , KBMX  , KTMX           )
*
      KTMN = 3
      DO 2100 KT = KTMN, KTMX
         CALL CUMTFX           !! cumulus, test flux
     O         ( GXH   , GXQ   , 
     O           GXHB  , GXWB  , DXH   , DXW   ,
     M           GCYM  ,
     I           GDH   , GDQ   , 
     I           GCHB  , GCWB  , DH    , DW    ,
     I           ELAM  , DYM   , DELP  , FTEST ,
     I           GDZ   , GDZM  , KB    , KT      )
*
         CALL CUMCWF           !! Cloud work function (test)
     O         ( ACWFX ,         IBUOY , GCLZ  , GPRCZ ,
     I           ELAM  , DY    , DXH   , DXW   , GPRCT ,
     I           GXH   , GXQ   , GDHS  , GDQS  , GDT   ,
     I           GDZ   , GDZM  , GAMX  ,
     I           GXHB  , GXWB  , KB    , KT             )
*
         CALL CUMCWF           !! Cloud work function
     O         ( ACWF(1,KT)    , IBUOY , GCLZ  , GPRCZ ,
     I           ELAM  , DY    , DH    , DW    , GPRCT ,
     I           GDH   , GDQ   , GDHS  , GDQS  , GDT   ,
     I           GDZ   , GDZM  , GAMX  ,
     I           GCHB  , GCWB  , KB    , KT             )
*
         CALL CUMBMX           !! Cloud Bottom Mass Flux
     M         ( CBMFX , ACWF  , 
     I           ACWFX , IBUOY , FTEST , 
     I           GCYT  , GDSIGD, GDW   , GDQS  ,
     I           KB    , KT                      ) 
*
         CALL CUMMLV           !! Middle level Conv.
     M         ( CMMFX , GCYT  , GCHT  , GCWT  , 
     M           GPRCT , GCLZ  ,
     I           GDH   , GDW   , GDHS  , GDQS  ,
     I           GDZ   , GDZM  , DELP  , 
     I           GAMX  , CBMFX , KT             )
*
         CALL CUMFLX          !! Cloud Mass Flux & Precip.
     M         ( GMFLX , GPRCP , CUMCLW ,
     I           CBMFX , CMMFX , 
     I           GCYM  , GPRCZ , GPRCT  , GCLZ  , 
     I           KT                               )
 2100 CONTINUE 
*
      CALL    CUMCLD            !! Cumulus Cloudiness
     M         ( CUMCLW, 
     O           CUMFRC,
     I           GMFLX , KTMX    )
*
      CALL    CUMDET            !! Cloud Detrainment Heating
     M         ( GTT   , GTQ   ,
     I           GDH   , GDW   , DELP  ,
     I           CBMFX , CMMFX , GCYT  , GCHT  , 
     I           GCWT  , GPRCT ,
     I           KTMX                            )
*
      CALL    CUMDWN            !! Downdfart & Evaporation
     M         ( GTT   , GTQ   , GMFLX , GPRCP ,
     O           GEVPP , GMDD  , GMDDB , 
     I           GDH   , GDW   , GDQS  , GDT   ,
     I           GAM   , GDZM  , DELP  , GCYM  ,
     I           KB    , KBMX  , KTMX            )
*
      CALL    CUMSBH            !! Cloud Subsidence Heating
     M         ( GTT   , GTQ   ,
     I           GDH   , GDQ   , DELP  ,
     I           GMFLX , KTMX            )
*
      CALL    CUMMLT            !! Precipitation Freeze and Melt
     M         ( GTT   , GTQ   ,
     O           GPRCC , GSNWC , 
     I           GPRCP , DELP  ,
     I           GDT   , GDQ   , GDQS  , GAM   ,
     I           KTMX                            )
*
      CALL    CUMUPR            !! Tracer Updraft
     M         ( GTQ   , GPRCC ,
     I           GDQ   , CBMFX , CMMFX , ELAM  ,
     I           GCYT  , GCYM  , GCWT  , GPRCT ,
     I           GPRCP , GEVPP , GDZ   , GDZM  , DELP  ,
     I           KB    , KBMX  , KTMX  , NTRCUM,
     W           GXQ                                     )
*
      CALL    CUMDNR            !! Tracer Downdraft
     M         ( GTQ   , 
     I           GDQ   , GMDD  , GMDDB , DELP  ,
     I           KB    , KBMX  , KTMX  , NTRCUM  )
*
      CALL    CUMSBR            !! Tracer Subsidence
     M         ( GTQ   ,
     I           GDQ   , DELP  ,
     I           GMFLX , KTMX  , NTRCUM  )
*
      CALL HISTIN
     I     ( ACWF , 'CWF',  'cloud work function' , 'J/kg',    'ALEV')
      CALL HISTIN
     I     ( CBMFX, 'CBMFX','cloud-base mass flux','Kg/m**2/s','ALEV')
      CALL HISTIN
     I     ( CMMFX, 'CMMFX','mid-lev cu mass flux','Kg/m**2/s','ALEV')
      CALL HISTIN
     I     ( GMFLX, 'CMFLX','cloud mass flux',    'Kg/m**2/s','AMLEV')
      CALL HISTIN
     I     ( GMDD , 'CDFLX','downdraft mass flux','Kg/m**2/s','AMLEV')
      CALL HISTIN
     I     ( GPRCP, 'FPRCC','cumulus precip.flux','Kg/m**2/s','AMLEV')
*
      RETURN
      END    
***********************************************************************8
      SUBROUTINE CUMBAS   !! cloud base
     O         ( KB    , GCYM  , KBMX  , KTMX  ,
     I           GDH   , GDW   , GDHS  , GDQS  , 
     I           GAM   , GDZM                    )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zccom.F"                /* stand. physical const. */
*
*   [OUTPUT] 
      INTEGER    KB    ( IJSDIM )
      REAL*8     GCYM  ( IJSDIM, KMAX )
      INTEGER    KBMX
      INTEGER    KTMX
*
*   [INPUT] 
      REAL*8     GDH   ( IJSDIM, KMAX )
      REAL*8     GDW   ( IJSDIM, KMAX )
      REAL*8     GDHS  ( IJSDIM, KMAX )
      REAL*8     GDQS  ( IJSDIM, KMAX )
      REAL*8     GAM   ( IJSDIM, KMAX )
      REAL*8     GDZM  ( IJSDIM, KMAX+1 )
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K, IFPAR, JFPAR
      REAL*8     QSL
*
*   [INTERNAL PARM] 
      INTEGER    KLCLB                       !! LCL base level
      INTEGER    KCB                         !! fix cloud bottom
      INTEGER    KBMAX                       !! cloud base max
      INTEGER    KTMAX                       !! cloud top
      INTEGER    KBOFS                       !! cloud base offset
      LOGICAL    OFIRST
      INTEGER    KMAXM1
      PARAMETER (KMAXM1=KMAX-1)
      NAMELIST /NMASCB/ KLCLB, KCB, KBMAX, KTMAX, KBOFS
      DATA       KLCLB  / 1      /
      DATA       KCB    / 0      /
      DATA       KBMAX  / KMAXM1 /
      DATA       KTMAX  / -1     /
      DATA       KBOFS  / 1      /
      DATA       OFIRST / .TRUE. /
*
*   [ONCE] 
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ PCUMA: CLOUD BASE 96/10/03'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMASCB, END=190 )
  190    WRITE  ( JFPAR, NMASCB )
      ENDIF
*
      IF ( KCB .GT. 0 ) THEN
         DO 1100 IJ = 1, IJSDIM
            KB( IJ ) = KCB
 1100    CONTINUE 
      ELSE
         DO 1210 IJ = 1, IJSDIM
            KB( IJ ) = KBMAX
 1210    CONTINUE 
         DO 1220 K = KBMAX-1, KLCLB+1, -1
            DO 1220 IJ = 1, IJSDIM
               QSL = GDQS( IJ,K ) 
     &             + GAM( IJ,K )/(1.+GAM( IJ,K ))/EL
     &               * ( GDH( IJ,KLCLB ) - GDHS( IJ,K ) )
               IF ( GDW( IJ,KLCLB ) .GE. QSL ) THEN
                  KB( IJ ) = K + KBOFS
               ENDIF
 1220    CONTINUE 
      ENDIF
*
      KBMX = 1
      DO 2000 IJ = 1, IJSDIM
         KBMX = MAX( KBMX, KB( IJ ) )
 2000 CONTINUE 
*
      DO 2100 K = 1, KBMX
         DO 2100 IJ = 1, IJSDIM
            IF ( K .LE. KB( IJ ) ) THEN
               GCYM( IJ,K ) = ( GDZM( IJ,K      ) - GDZM( IJ,1 ) )
     &                       /( GDZM( IJ,KB(IJ) ) - GDZM( IJ,1 ) )
            ENDIF
 2100 CONTINUE 
*
      IF ( KTMAX .GE. 0 ) THEN
         KTMX = KTMAX
      ELSE
         KTMX = KMAX
         DO 2300 K = 1, KMAX-1
            DO 2300 IJ = 1, IJSDIM
               IF ( GDHS( IJ,K ) .LE. GDHS( IJ,KLCLB ) ) THEN
                  KTMX = K+1
               ENDIF
 2300    CONTINUE 
      ENDIF
*
      RETURN
      END
**********************************************************************
      SUBROUTINE CUMUP     !! A-S cumulus, structure of clouds
     O         ( GCHB  , GCWB  , DH    , DW    ,
     O           ELAM  , DY    , DYM   ,
     O           GCYT  , GCHT  , GCWT  , GPRCT , 
     I           GDH   , GDW   , GDHS  , GDQS  , GDT   ,
     I           GDZ   , GDZM  , GAM   , GAMX  ,
     I           GCYM  , KB    , KBMX  , KTMX            )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zccom.F"                /* stand. physical const. */
*
*   [OUTPUT] 
      REAL*8     GCHB  ( IJSDIM )
      REAL*8     GCWB  ( IJSDIM )
      REAL*8     DH    ( IJSDIM, KMAX )
      REAL*8     DW    ( IJSDIM, KMAX )
      REAL*8     ELAM  ( IJSDIM, KMAX ) !! entrainment ratio
      REAL*8     DY    ( IJSDIM, KMAX )
      REAL*8     DYM   ( IJSDIM, KMAX )
      REAL*8     GCYT  ( IJSDIM, KMAX ) !! norm. mass flux @top
      REAL*8     GCHT  ( IJSDIM, KMAX ) !! cloud top MSE
      REAL*8     GCWT  ( IJSDIM, KMAX ) !! cloud top q
      REAL*8     GPRCT ( IJSDIM, KMAX ) !! precipitation/M
*
*   [ENTRY OUTPUT] 
      REAL*8     ACWF  ( IJSDIM )       !! cloud work function
      INTEGER    IBUOY ( IJSDIM )       !! buoyancy flag
      REAL*8     GCLZ  ( IJSDIM, KMAX ) !! cloud water
      REAL*8     GPRCZ ( IJSDIM, KMAX ) !! precipitation
      REAL*8     CMMFX ( IJSDIM, KMAX ) !! middle conv. mass flux
*
*   [INPUT] 
      REAL*8     GDH   ( IJSDIM, KMAX )
      REAL*8     GDW   ( IJSDIM, KMAX )   !! humidity 
      REAL*8     GDHS  ( IJSDIM, KMAX )
      REAL*8     GDQS  ( IJSDIM, KMAX )
      REAL*8     GDT   ( IJSDIM, KMAX )   !! temperature T
      REAL*8     GDZ   ( IJSDIM, KMAX )
      REAL*8     GDZM  ( IJSDIM, KMAX+1 )
      REAL*8     GAM   ( IJSDIM, KMAX )
      REAL*8     GAMX  ( IJSDIM, KMAX )
      REAL*8     GCYM  ( IJSDIM, KMAX )
      INTEGER    KB    ( IJSDIM )
      INTEGER    KBMX
      INTEGER    KTMX
*
*   [ENTRY INPUT] 
      REAL*8     GDQ   ( IJSDIM, KMAX )
      REAL*8     GXH   ( IJSDIM, KMAX )
      REAL*8     GXQ   ( IJSDIM, KMAX )
      REAL*8     GXHB  ( IJSDIM )
      REAL*8     GXWB  ( IJSDIM )
      INTEGER    KT
      REAL*8     DELP  ( IJSDIM, KMAX )
      REAL*8     CBMFX ( IJSDIM, KMAX )   !! cloud base mass flux
*
*   [WORK]
      INTEGER    IJ, K, IFPAR, JFPAR, KP1
      REAL*8     DELZ, DELZL, DELZH, PRECR, AA, BB, GCQT
      REAL*8     GCYZ, GCHZ, GCWZ, GCQZ
      REAL*8     PREC0, BUOY, BUOX
      REAL*8     DMH, DMHS, FF
*
*   [INTERNAL PARAM]
      REAL*8     PRECZ0          !! precipitation efficiency
      REAL*8     PRECZH          !! precipitation efficiency
      REAL*8     ELAMIN          !! minimum entrainment ratio
      REAL*8     ELAMAX          !! maximum entrainment ratio
      REAL*8     BUOYMN          !! buoyancy minimum
      REAL*8     TAUML           !! mid. lev. convection adjust time
      REAL*8     FMAXML          !! mid. lev. convection max. flux
      LOGICAL    OFIRST
      NAMELIST   /NMASUP/ PRECZ0, PRECZH,
     &                    ELAMIN, ELAMAX,
     &                    BUOYMN
      NAMELIST   /NMASML/ TAUML,  FMAXML
*   [INTERNAL FUNC]   
      REAL*8    FPREC, Z
      FPREC( Z ) = MIN( MAX( 1.-EXP(-(Z-PRECZ0)/PRECZH), 0.D0 ), 1.D0 )
*
      DATA       PRECZ0, PRECZH / 0.E3 , 4.0E3 / 
      DATA       ELAMIN, ELAMAX / 1.E-5, 1.E-2 /
      DATA       BUOYMN         / 0.     /
      DATA       TAUML          / 7200.  /
      DATA       FMAXML         / 0.01   /
      DATA       OFIRST         / .TRUE. /
*
*   [ONCE] 
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ PCUMA : CLOWD UPDRAFT 96/05/02'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMASUP, END=190 )
  190    WRITE  ( JFPAR, NMASUP )
         ELAMIN = MAX( ELAMIN, 0.D0 )
         ELAMAX = MAX( ELAMAX, 0.D0 )
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMASML, END=290 )
  290    WRITE  ( JFPAR, NMASML )
      ENDIF
*
      CALL RESET( GCHB, IJSDIM )
      CALL RESET( GCWB, IJSDIM )
      DO 1100 K = 1, KBMX
         DO 1100 IJ = 1, IJSDIM
            IF ( K .LT. KB( IJ ) ) THEN
               DELZ  = GCYM( IJ,K+1 ) - GCYM( IJ,K )
               GCHB( IJ ) = GCHB( IJ ) + DELZ * GDH( IJ,K )
               GCWB( IJ ) = GCWB( IJ ) + DELZ * GDW( IJ,K )
            ENDIF
 1100 CONTINUE
*
      CALL RESET ( DH   , IJSDIM*KMAX )
      CALL RESET ( DW   , IJSDIM*KMAX )
      CALL RESET ( DY   , IJSDIM*KMAX )
      CALL RESET ( DYM  , IJSDIM*KMAX )
      CALL RESET ( ELAM , IJSDIM*KMAX )
*
      DO 2100 K = 2, KTMX
         KP1 = MIN( K+1, KMAX )
         DO 2100 IJ = 1, IJSDIM
            IF ( K .GE. KB( IJ ) ) THEN
               DELZL = GDZ ( IJ,K   ) - GDZM( IJ,K  )
               DELZH = GDZM( IJ,KP1 ) - GDZ ( IJ,K  )
               DELZH = MAX( DELZH, 0.D0 )
               DY ( IJ,K   ) = DYM( IJ,K ) + DELZL
               DH ( IJ,K   ) = DH ( IJ,K ) + DELZL*GDH( IJ,K )
               DW ( IJ,K   ) = DW ( IJ,K ) + DELZL*GDW( IJ,K )
               DYM( IJ,KP1 ) = DY ( IJ,K ) + DELZH
               DH ( IJ,KP1 ) = DH ( IJ,K ) + DELZH*GDH( IJ,K )
               DW ( IJ,KP1 ) = DW ( IJ,K ) + DELZH*GDW( IJ,K )
*
               PRECR = FPREC( DY( IJ,K ) )
               AA    = 1./( 1.+ GAM( IJ,K ) )
     &                   *( 1./CP + GDT( IJ,K )/EL*GAM( IJ,K )
     &                          *( EPSVT + 1. - PRECR ) )
               BB    = ( EPSVT + 1. - PRECR )*GDQS( IJ,K ) 
     &                 - EPSVT * GDW( IJ,K )
*
               ELAM( IJ,K )
     &         = ( AA*( GCHB( IJ ) - GDHS( IJ,K ) )
     &            + GDT( IJ,K )
     &              *( BB - (1.-PRECR)*GCWB( IJ )  ) )
     &         / ( AA*( DY( IJ,K )*GDHS( IJ,K ) - DH( IJ,K ) )
     &            - GDT( IJ,K )
     &              *( BB*DY( IJ,K ) - (1.-PRECR)*DW( IJ,K ) ) )
            ENDIF
 2100 CONTINUE 
*         
      CALL RESET ( GCYT,  IJSDIM*KMAX )
      CALL RESET ( GCHT,  IJSDIM*KMAX )
      CALL RESET ( GCWT,  IJSDIM*KMAX )
      CALL RESET ( GPRCT, IJSDIM*KMAX )
*
      DO 3100 K = 2, KTMX
         DO 3100 IJ = 1, IJSDIM
            GCQT = 999.
            IF ( ELAM( IJ,K ) .GT. ELAMIN .AND.
     &           ELAM( IJ,K ) .LE. ELAMAX       ) THEN
               GCYT( IJ,K ) = 1. + ELAM( IJ,K )*DY( IJ,K )
               GCHT( IJ,K ) = GCHB( IJ ) + ELAM( IJ,K )*DH( IJ,K )
               GCWT( IJ,K ) = GCWB( IJ ) + ELAM( IJ,K )*DW( IJ,K )
               GCQT         = GDQS( IJ,K )*GCYT( IJ,K ) 
     &                      + GAMX( IJ,K )
     &                        * ( GCHT( IJ,K ) 
     &                            - GDHS( IJ,K )*GCYT( IJ,K ) )
            ENDIF
            IF ( GCWT( IJ,K ) .GE. GCQT ) THEN
               PRECR         = FPREC( DY( IJ,K ) )
               GPRCT( IJ,K ) = PRECR *( GCWT( IJ,K ) - GCQT )
            ELSE
               ELAM ( IJ,K ) = -999.
            ENDIF
 3100 CONTINUE 
*
      RETURN
*=====================================================================
      ENTRY      CUMCWF    !! Cloud work function
     O         ( ACWF  , IBUOY , GCLZ  , GPRCZ ,
     I           ELAM  , DY    , DH    , DW    , GPRCT ,
     I           GDH   , GDQ   , GDHS  , GDQS  , GDT   ,
     I           GDZ   , GDZM  , GAMX  ,
     I           GCHB  , GCWB  , KB    , KT             )
*
      CALL RESET( ACWF , IJSDIM      )
      CALL RESET( GCLZ , IJSDIM*KMAX )
      CALL RESET( GPRCZ, IJSDIM*KMAX )
*
      DO 4000 IJ = 1, IJSDIM
         IBUOY( IJ )    = 0
         GPRCZ( IJ,KT ) = GPRCT( IJ,KT )
 4000 CONTINUE 
*
      DO 4100 K = KT-1, 1, -1
         DO 4100 IJ = 1, IJSDIM
            IF ( K .GE. KB( IJ ) .AND. ELAM( IJ,KT ) .GE. 0. ) THEN
               GCYZ = 1.         + ELAM( IJ,KT )*DY( IJ,K )
               GCHZ = GCHB( IJ ) + ELAM( IJ,KT )*DH( IJ,K )
               GCWZ = GCWB( IJ ) + ELAM( IJ,KT )*DW( IJ,K )
               GCQZ = GDQS( IJ,K )*GCYZ
     &              + GAMX( IJ,K )*( GCHZ - GDHS( IJ,K )*GCYZ )
               GCQZ = MIN( GCQZ, GCWZ )
*
               PREC0 = FPREC( DY( IJ,K ) )*( GCWZ - GCQZ )
               GPRCZ( IJ,K ) = MIN( PREC0, GPRCZ( IJ,K+1 ) )
               GCLZ ( IJ,K ) = MAX( GCWZ - GCQZ - GPRCZ( IJ,K ), 0.D0 )
*
               BUOY = (   ( GCHZ        - EL*GCQZ        )
     &                  - ( GDH( IJ,K ) - EL*GDQ( IJ,K ) )*GCYZ )
     &               /( CP*GDT( IJ,K ) )
     &               + EPSVT*GCQZ - GCLZ( IJ,K )
     &               - EPSVT*GDQ( IJ,K )*GCYZ
*
               DELZ = GDZM( IJ,K+1 ) - GDZM( IJ,K )
               ACWF( IJ ) = ACWF( IJ ) + BUOY*GRAV*DELZ
*
               IF ( IBUOY( IJ ) .GE. 0 ) THEN
                  IF ( BUOY .LT. BUOYMN ) THEN
                     IBUOY( IJ ) = 1
                  ELSE IF ( IBUOY( IJ ) .GT. 0 ) THEN
                     IBUOY( IJ ) = -1
                  ENDIF
               ENDIF
            ENDIF
 4100 CONTINUE 
*
      RETURN
*=======================================================================
      ENTRY      CUMMLV    !! A-S cumulus, middle level conv.
     M         ( CMMFX , GCYT  , GCHT  , GCWT  ,
     M           GPRCT , GCLZ  ,
     I           GDH   , GDW   , GDHS  , GDQS  , 
     I           GDZ   , GDZM  , DELP  , 
     I           GAMX  , CBMFX , KT             )
*
      K = KT
      DO 6100 IJ = 1, IJSDIM
         DMHS = GDH( IJ,K-1 ) - GDHS( IJ,K )
         IF ( CBMFX( IJ,K ) .LE. 0.D0  .AND.
     &        DMHS          .GT. 0.D0        ) THEN
            GCYT( IJ,K ) = 1.D0
            GCHT( IJ,K ) = GDH( IJ,K-1 )
            GCWT( IJ,K ) = GDW( IJ,K-1 )
            GCQT         = GDQS( IJ,K )
     &                   + GAMX( IJ,K )*( GCHT( IJ,K )-GDHS( IJ,K ) )
            IF ( GCWT( IJ,K ) .GE. GCQT ) THEN
               PRECR = FPREC( GDZ( IJ,K ) - GDZ( IJ,K-1 ) )
               GPRCT( IJ,K ) = PRECR *( GCWT( IJ,K ) - GCQT )
               DMH   = GDH( IJ,K-1 ) - GDH( IJ,K )
               FF    = DMHS/MAX( DMHS, DMH )*DELP( IJ,K )/GRAV
               CMMFX( IJ,K ) = MAX( FF,0.D0 )/TAUML
               CMMFX( IJ,K ) = MIN( CMMFX( IJ,K ), FMAXML ) 
               GCLZ ( IJ,K ) = GCWT( IJ,K ) - GPRCT( IJ,K ) - GCQT
               GCLZ ( IJ,K ) = MAX( GCLZ ( IJ,K ), 0.D0   )
            ENDIF
         ENDIF
 6100 CONTINUE 
*
      RETURN
      END
**********************************************************************
      SUBROUTINE CUMTFX    !! A-S cumulus, test flux
     O         ( GXH   , GXQ   , 
     O           GXHB  , GXWB  , DXH   , DXW   ,
     M           GCYM  ,
     I           GDH   , GDQ   , 
     I           GCHB  , GCWB  , DH    , DW    ,
     I           ELAM  , DYM   , DELP  , FTEST ,
     I           GDZ   , GDZM  , KB    , KT      )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zccom.F"                /* stand. physical const. */
*
*   [OUTPUT] 
      REAL*8     GXH   ( IJSDIM, KMAX )
      REAL*8     GXQ   ( IJSDIM, KMAX )
      REAL*8     GXHB  ( IJSDIM )
      REAL*8     GXWB  ( IJSDIM )
      REAL*8     DXH   ( IJSDIM, KMAX )
      REAL*8     DXW   ( IJSDIM, KMAX )
*
*   [MODIFY] 
      REAL*8     GCYM  ( IJSDIM, KMAX )
*
*   [INPUT] 
      REAL*8     GDH   ( IJSDIM, KMAX ) 
      REAL*8     GDQ   ( IJSDIM, KMAX )
      REAL*8     GCHB  ( IJSDIM )
      REAL*8     GCWB  ( IJSDIM )
      REAL*8     DH    ( IJSDIM, KMAX )
      REAL*8     DW    ( IJSDIM, KMAX )
      REAL*8     ELAM  ( IJSDIM, KMAX )
      REAL*8     DYM   ( IJSDIM, KMAX )
      REAL*8     DELP  ( IJSDIM, KMAX )
      REAL*8     FTEST
      REAL*8     GDZ   ( IJSDIM, KMAX )
      REAL*8     GDZM  ( IJSDIM, KMAX+1 )
      INTEGER    KB    ( IJSDIM )
      INTEGER    KT
*
*   [WORK] 
      INTEGER    IJ, K, KP1
      REAL*8     GTH, GTQ, DELZ, DELZL, DELZH
*
      DO 1100 K = 1, KT
         DO 1100 IJ = 1, IJSDIM
            IF ( K .GT. KB( IJ ) ) THEN
               GCYM( IJ,K ) = 1.D0 + ELAM( IJ,KT )*DYM( IJ,K )
            ENDIF
 1100 CONTINUE
*
      CALL RESET( GXH,        IJSDIM*KT   )
      CALL RESET( GXQ,        IJSDIM*KT   )
      CALL COPY ( GXHB, GCHB, IJSDIM      )
      CALL COPY ( GXWB, GCWB, IJSDIM      )
      CALL RESET( DXH,        IJSDIM*KT   )
      CALL RESET( DXW,        IJSDIM*KT   )
*
      CALL CUMSBH            !! Cloud Subsidence Heating by test-MF
     M         ( GXH   , GXQ   ,
     I           GDH   , GDQ   , DELP  ,
     I           GCYM  , KT              )
*
      DO 3100 K = 1, KT
         KP1 = MIN( K+1, KT )
         DO 3100 IJ = 1, IJSDIM
            GTH = ( CP*GXH( IJ,K ) + EL*GXQ( IJ,K ) )*FTEST
            GTQ =                       GXQ( IJ,K )  *FTEST
            GXH( IJ,K ) = GDH( IJ,K ) + GTH        
            GXQ( IJ,K ) = GDQ( IJ,K ) + GTQ
            IF ( K .LT. KB( IJ ) ) THEN
               DELZ       = GCYM( IJ,K+1 ) - GCYM( IJ,K )
               GXHB( IJ ) = GXHB( IJ ) + DELZ * GTH
               GXWB( IJ ) = GXWB( IJ ) + DELZ * GTQ
            ELSE
               DELZL = GDZ ( IJ,K   ) - GDZM( IJ,K  )
               DELZH = GDZM( IJ,KP1 ) - GDZ ( IJ,K  )
               DELZH = MAX( DELZH, 0.D0 )
               DXH( IJ,K   ) = DXH( IJ,K ) + DELZL*GTH
               DXW( IJ,K   ) = DXW( IJ,K ) + DELZL*GTQ
               DXH( IJ,KP1 ) = DXH( IJ,K ) + DELZH*GTH
               DXW( IJ,KP1 ) = DXW( IJ,K ) + DELZH*GTQ
               DXH( IJ,K   ) = DXH( IJ,K ) + DH( IJ,K )
               DXW( IJ,K   ) = DXW( IJ,K ) + DW( IJ,K )
            ENDIF
 3100 CONTINUE 
*
      RETURN
      END
**********************************************************************
      SUBROUTINE CUMBMX     !! A-S cumulus, bottom mass flux
     M         ( CBMFX , ACWF  , 
     I           ACWFX , IBUOY , FTEST , 
     I           GCYT  , GDSIGD, GDW   , GDQS  ,
     I           KB    , KT                     ) 
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
*
*   [MODIFY] 
      REAL*8     CBMFX ( IJSDIM, KMAX   )     !! cloud base mass flux
      REAL*8     ACWF  ( IJSDIM, KMAX   )     !! cloud work function
*
*   [INPUT] 
      REAL*8     ACWFX ( IJSDIM         )     !! TEST cloud work func
      INTEGER    IBUOY ( IJSDIM         )     !! buoyancy flag
      REAL*8     FTEST                        !! TEST mass flux
      REAL*8     GCYT  ( IJSDIM, KMAX   )     !! norm mass flux @top
      REAL*8     GDSIGD( IJSDIM, KMAX+1 )     !! sigma velocity
      REAL*8     GDW   ( IJSDIM, KMAX   )
      REAL*8     GDQS  ( IJSDIM, KMAX   )
      INTEGER    KB    ( IJSDIM         )
      INTEGER    KT
*
*   [INTERNAL WORK] 
      INTEGER    IJ, IFPAR, JFPAR
      REAL*8     TAUADY, FMAXY, ACWF0Y, RHB
*
*   [INTERNAL PARAM] 
      REAL*8     TAUADJ( 2 )          !! relaxation time
      REAL*8     FMAX  ( 2 )          !! maximum flux
      REAL*8     ACWF0 ( 2 )          !! critical val. of A
      REAL*8     SGDCRT               !! critical val. of SIGDOT
      REAL*8     RHCRT                !! critical val. of RH
      LOGICAL    OFIRST
      REAL*8     EPS
      NAMELIST   /NMASMF/ TAUADJ, FMAX, ACWF0, SGDCRT, RHCRT
      DATA       TAUADJ             / 7200.D0, -999.  /
      DATA       FMAX               /  0.02D0, -999.  /
      DATA       ACWF0              /    0.D0, -999.  /
      DATA       SGDCRT             /    0.D0         /
      DATA       RHCRT              /    0.D0         /
      DATA       OFIRST             / .TRUE.          /
      DATA       EPS                / 1.D-10          /
*
*   [ONCE] 
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' DIAGNOSTIC CLOWD BASE MASSFLUX 96/10/02'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMASMF, END=190 )
  190    WRITE  ( JFPAR, NMASMF )
         IF ( TAUADJ(2) .LT. -900. ) TAUADJ(2) = TAUADJ(1)
         IF ( FMAX  (2) .LT. -900. ) FMAX  (2) = FMAX  (1)
         IF ( ACWF0 (2) .LT. -900. ) ACWF0 (2) = ACWF0 (1)
      ENDIF
*    
      DO 1100 IJ = 1, IJSDIM
         IF ( IBUOY( IJ ) .LT. 0 ) THEN
            ACWF( IJ,KT ) = 0.D0
         ENDIF
 1100 CONTINUE 
*      
      DO 1200 IJ = 1, IJSDIM
         RHB = GDW( IJ,KB(IJ) )/MAX( GDQS( IJ,KB(IJ) ), EPS )
         IF ( RHB           .GE. RHCRT       .AND.
     &        ACWF( IJ,KT ) .GT. ACWFX( IJ )       ) THEN
            IF ( GDSIGD( IJ,KB( IJ ) ) .LT. SGDCRT ) THEN
               TAUADY = TAUADJ( 1 )
               FMAXY  = FMAX  ( 1 )
               ACWF0Y = ACWF0 ( 1 )
            ELSE
               TAUADY = TAUADJ( 2 )
               FMAXY  = FMAX  ( 2 )
               ACWF0Y = ACWF0 ( 2 )
            ENDIF
            CBMFX( IJ,KT ) = FTEST / TAUADY 
     &                     * MAX( ACWF ( IJ,KT )-ACWF0Y,      0.D0 )
     &                     / MAX( ACWF ( IJ,KT )-ACWFX( IJ ), EPS  )
            CBMFX( IJ,KT ) = MAX( CBMFX( IJ,KT ), 0.D0 )
            CBMFX( IJ,KT ) = MIN( CBMFX( IJ,KT ), FMAXY/GCYT( IJ,KT ) )
         ENDIF
 1200 CONTINUE
*
      RETURN
      END
**********************************************************************
      SUBROUTINE CUMFLX     !! A-S cumulus, cloud mass flux
     M         ( GMFLX , GPRCP , CUMCLW ,
     I           CBMFX , CMMFX , 
     I           GCYM  , GPRCZ , GPRCT  , GCLZ  , 
     I           KT                               )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zccom.F"                /* stand. physical const. */
*
*   [MODIFY] 
      REAL*8     GMFLX ( IJSDIM, KMAX+1 ) !! mass flux
      REAL*8     GPRCP ( IJSDIM, KMAX+1 ) !! precipitation
      REAL*8     CUMCLW( IJSDIM, KMAX   ) !! cloud water
*
*   [INPUT] 
      REAL*8     CBMFX ( IJSDIM, KMAX )       !! cloud base mass flux
      REAL*8     CMMFX ( IJSDIM, KMAX )       !! middle conv. mass flux
      REAL*8     GCYM  ( IJSDIM, KMAX )       !! normarized mass flux
      REAL*8     GPRCZ ( IJSDIM, KMAX )       !! precipitation/M
      REAL*8     GPRCT ( IJSDIM, KMAX )       !! precipitation/M
      REAL*8     GCLZ  ( IJSDIM, KMAX )       !! cloud water
      INTEGER    KT                           !! cloud top
*     
*   [INTERNAL WORK] 
      INTEGER    IJ, K
      REAL*8     GPRCM
*
      DO 1100 K = 1, KT
         DO 1100 IJ = 1, IJSDIM
            GMFLX( IJ,K ) = GMFLX( IJ,K ) 
     &                    + GCYM( IJ,K )*CBMFX( IJ,KT )
 1100 CONTINUE
      DO 1200 IJ = 1, IJSDIM
         GMFLX( IJ,KT ) = GMFLX( IJ,KT ) + CMMFX( IJ,KT ) 
 1200 CONTINUE 
*
#ifdef OPT_WTRACE
      DO 2100 K = 1, KT
         DO 2100 IJ = 1, IJSDIM
            GPRCP( IJ,K ) = GPRCP( IJ,K ) 
     &                    + GPRCT( IJ,KT ) * CBMFX( IJ,KT )
 2100 CONTINUE
#else
      DO 2100 K = 2, KT
         DO 2100 IJ = 1, IJSDIM
            GPRCM = ( GPRCZ( IJ,K ) + GPRCZ( IJ,K-1 ) )/2.D0
            GPRCP( IJ,K ) = GPRCP( IJ,K ) 
     &                    +( GPRCZ( IJ,KT ) - GPRCM )*CBMFX( IJ,KT )
 2100 CONTINUE
      DO 2200 IJ = 1, IJSDIM
            GPRCP( IJ,1 ) = GPRCP( IJ,1 ) 
     &                    + GPRCZ( IJ,KT ) * CBMFX( IJ,KT )
 2200 CONTINUE 
#endif
#ifdef OPT_WTRACE
      DO 2300 K = 1, KT
#else
      DO 2300 K = 1, KT-1
#endif
         DO 2300 IJ = 1, IJSDIM
            GPRCP( IJ,K ) = GPRCP( IJ,K ) 
     &                    + GPRCT( IJ,KT ) * CMMFX( IJ,KT )
 2300 CONTINUE 
*
      DO 3100 K = 1, KT
         DO 3100 IJ = 1, IJSDIM
            CUMCLW( IJ,K ) = CUMCLW( IJ,K ) 
     &                     + GCLZ( IJ,K ) * CBMFX( IJ,KT )
 3100 CONTINUE 
      DO 3200 IJ = 1, IJSDIM
         CUMCLW( IJ,KT ) = CUMCLW( IJ,KT ) 
     &                   + GCLZ( IJ,KT ) * CMMFX( IJ,KT )
 3200 CONTINUE 
*
      RETURN
      END
**********************************************************************
      SUBROUTINE CUMDET    !! A-S cumulus, heating by detrain.
     M         ( GTT   , GTQ   ,
     I           GDH   , GDW   , DELP  ,
     I           CBMFX , CMMFX , GCYT  , GCHT  , 
     I           GCWT  , GPRCT ,
     I           KTMX                                   )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zccom.F"                /* stand. physical const. */
*
*   [MODIFY] 
      REAL*8     GTT   ( IJSDIM, KMAX )   !! Temperature tendency 
      REAL*8     GTQ   ( IJSDIM, KMAX )   !! Moisture tendency 
*
*   [INPUT] 
      REAL*8     GDH   ( IJSDIM, KMAX ) 
      REAL*8     GDW   ( IJSDIM, KMAX )   !! humidity q
      REAL*8     DELP  ( IJSDIM, KMAX ) 
      REAL*8     CBMFX ( IJSDIM, KMAX )   !! cloud base mass flux
      REAL*8     CMMFX ( IJSDIM, KMAX )   !! middle lev. mass flux
      REAL*8     GCYT  ( IJSDIM, KMAX )   !! detraining mass flux
      REAL*8     GCHT  ( IJSDIM, KMAX )   !! detraining MSE
      REAL*8     GCWT  ( IJSDIM, KMAX )   !! detraining q
      REAL*8     GPRCT ( IJSDIM, KMAX )   !! precipitation/M
      INTEGER    KTMX
*
      INTEGER    IJ, K
      REAL*8     GTHCI, GTQCI
*
      DO 2100 K = 1, KTMX
         DO 2100 IJ = 1, IJSDIM
            GTHCI
     &     = GRAV/DELP( IJ,K )
     &       * ( CBMFX( IJ,K )+CMMFX( IJ,K ) )
     &       * ( GCHT( IJ,K ) - GCYT( IJ,K )*GDH( IJ,K ) )
*
            GTQCI
     &     = GRAV/DELP( IJ,K )
     &       * ( CBMFX( IJ,K )+CMMFX( IJ,K ) )
     &       * ( GCWT( IJ,K )-GPRCT( IJ,K ) - GCYT( IJ,K )*GDW( IJ,K ) )
*
            GTT( IJ,K ) = GTT( IJ,K ) + ( GTHCI - EL*GTQCI )/CP
            GTQ( IJ,K ) = GTQ( IJ,K ) + GTQCI
 2100 CONTINUE
*
      RETURN
      END
**********************************************************************
      SUBROUTINE CUMSBH    !! A-S cumulus, heating by adiabat. descent
     M         ( GTT   , GTQ   ,
     I           GDH   , GDQ   , DELP  ,
     I           GMFLX , KTMX            )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zccom.F"                /* stand. physical const. */
*
*   [MODIFY] 
      REAL*8     GTT   ( IJSDIM, KMAX )   !! Temperature tendency 
      REAL*8     GTQ   ( IJSDIM, KMAX )   !! Moisture tendency 
*
*   [INPUT] 
      REAL*8     GDH   ( IJSDIM, KMAX ) 
      REAL*8     GDQ   ( IJSDIM, KMAX )   !! humidity q
      REAL*8     DELP  ( IJSDIM, KMAX ) 
      REAL*8     GMFLX ( IJSDIM, KMAX+1 ) !! mass flux
      INTEGER    KTMX
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K, KM, KP
      REAL*8     SBH0, SBQ0, SBH1, SBQ1, FX1
      REAL*8     GTHCI, GTQCI
      REAL*8     FX    ( IJSDIM )
*
      CALL RESET( FX, IJSDIM )
*
      DO 1100 K = KTMX, 1, -1
         KM = MAX( K-1, 1    )
         KP = MIN( K+1, KMAX )
         DO 1100 IJ = 1, IJSDIM
            SBH0 = GMFLX( IJ,K+1 )*( GDH( IJ,KP )-GDH( IJ,K  ) )
            SBQ0 = GMFLX( IJ,K+1 )*( GDQ( IJ,KP )-GDQ( IJ,K  ) )
*
            SBH1 = GMFLX( IJ,K   )*( GDH( IJ,K  )-GDH( IJ,KM ) )
            SBQ1 = GMFLX( IJ,K   )*( GDQ( IJ,K  )-GDQ( IJ,KM ) )
*
            IF ( GMFLX( IJ,K ) .GT. GMFLX( IJ,K+1 ) ) THEN
               FX1 = 0.5D0
            ELSE
#ifdef OPT_SBHCOR
               FX1 = .5D0 * MIN( ABS(SBR0)/MAX( ABS(SBR1),1.D-20 ),
     &                           1.D0 )
#else
               FX1 = 0.D0
#endif
            ENDIF
*
            GTHCI = GRAV/DELP( IJ,K )
     &                 * ( ( 1.D0-FX( IJ ) )*SBH0 + FX1 *SBH1 )
            GTQCI = GRAV/DELP( IJ,K )
     &                 * ( ( 1.D0-FX( IJ ) )*SBQ0 + FX1 *SBQ1 )
*
            GTT( IJ,K ) = GTT( IJ,K ) + ( GTHCI - EL*GTQCI )/CP
            GTQ( IJ,K ) = GTQ( IJ,K ) + GTQCI
*
            FX ( IJ )   = FX1
 1100 CONTINUE
*
      RETURN
      END
********************************************************************
      SUBROUTINE CUMDWN    !! A-S cumulus, down draft & evap.
     M         ( GTT   , GTQ   , GMFLX , GPRCP ,
     O           GEVPP , GMDD  , GMDDB , 
     I           GDH   , GDW   , GDQS  , GDT   ,
     I           GAM   , GDZM  , DELP  , GCYM  ,
     I           KB    , KBMX  , KTMX                   )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zccom.F"                /* stand. physical const. */
*
*   [MODIFY] 
      REAL*8     GTT   ( IJSDIM, KMAX )   !! Temperature tendency 
      REAL*8     GTQ   ( IJSDIM, KMAX )   !! Moisture tendency 
      REAL*8     GMFLX ( IJSDIM, KMAX+1 ) !! mass flux
      REAL*8     GPRCP ( IJSDIM, KMAX+1 ) !! precipitation
*
*   [OUTPUT] 
      REAL*8     GEVPP ( IJSDIM, KMAX )   !! evaporation
      REAL*8     GMDD  ( IJSDIM, KMAX+1 ) !! downdraft mass flux
      REAL*8     GMDDB ( IJSDIM )         !! downdraft mass flux
*
*   [INPUT] 
      REAL*8     GDH   ( IJSDIM, KMAX )
      REAL*8     GDW   ( IJSDIM, KMAX )   !! humidity q
      REAL*8     GDQS  ( IJSDIM, KMAX )
      REAL*8     GDT   ( IJSDIM, KMAX )   !! air temperature T
      REAL*8     GAM   ( IJSDIM, KMAX )
      REAL*8     GDZM  ( IJSDIM, KMAX+1 )
      REAL*8     GCYM  ( IJSDIM, KMAX )
      REAL*8     DELP  ( IJSDIM, KMAX ) 
      INTEGER    KB    ( IJSDIM )
      INTEGER    KBMX
      INTEGER    KTMX
*
*   [INTERNAL WORK] 
      REAL*8     GCHD  ( IJSDIM )         !! downdraft MSE
      REAL*8     GCWD  ( IJSDIM )         !! downdraft q
      INTEGER    KHMIN ( IJSDIM )
      REAL*8     GMFXB ( IJSDIM )
      REAL*8     EVAPT ( IJSDIM )
*
      INTEGER    IJ, K, IFPAR, JFPAR
      REAL*8     GDTW, GDQW, RD, EVPE, GMDDE
      REAL*8     GTHCI, GTQCI
*
*   [INTERNAL PARM] 
      REAL*8     EVAPR                  !! evaporation factor
      REAL*8     REVPDD                 !! rate of DD to evapolation
      REAL*8     RDDMX                  !! norm. flux of downdraft
      REAL*8     VTERM                  !! term. vel. of precip.
      LOGICAL    OFIRST 
      NAMELIST  /NMASDN/ EVAPR, REVPDD, RDDMX, VTERM
      DATA       EVAPR  / 0.3  /
#ifdef OPT_WTRACE
      DATA       REVPDD / 0.0  /
      DATA       RDDMX  / 0.0  /
#else
      DATA       REVPDD / 0.5  /
      DATA       RDDMX  / 0.5  /
#endif
      Data       VTERM  / 10.  /
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' CLOWD DOWNDRAFT 94/05/12'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMASDN, END=190 )
  190    WRITE  ( JFPAR, NMASDN )
      ENDIF
*
      CALL RESET( GMDD  , IJSDIM*(KMAX+1) )
      CALL RESET( GMDDB , IJSDIM )
*
      DO 1100 IJ = 1, IJSDIM
         KHMIN( IJ ) = 0
         GCHD ( IJ ) = GDH  ( IJ,1 )     !! Hmin
         GMFXB( IJ ) = GMFLX( IJ,KB(IJ) )
 1100 CONTINUE 
      DO 1200 K = 2, KTMX
         DO 1200 IJ = 1, IJSDIM
            IF ( GDH( IJ,K ) .LT. GCHD( IJ ) ) THEN
               KHMIN( IJ ) = K
               GCHD ( IJ ) = GDH( IJ,K )
            ENDIF
 1200 CONTINUE 
*
      CALL RESET( EVAPT , IJSDIM )
      CALL RESET( GCHD  , IJSDIM )
      CALL RESET( GCWD  , IJSDIM )
*
      DO 2100 K = KTMX, 1, -1
         DO 2100 IJ = 1, IJSDIM
            GDTW = GDT( IJ,K ) 
     &           - EL/CP* ( GDQS( IJ,K ) - GDW( IJ,K ) )
     &                  / ( 1. + GAM( IJ,K )           )
            GDQW = GDW( IJ,K ) 
     &           +        ( GDQS( IJ,K ) - GDW( IJ,K ) )
     &                  / ( 1. + GAM( IJ,K )           )
*
            IF ( GDQW .GT. GDW( IJ,K ) ) THEN
               RD            = GPRCP( IJ,K+1 )/VTERM
               GEVPP( IJ,K ) = EVAPR*RD*( GDZM(IJ,K+1)-GDZM(IJ,K) )
     &                                 *( GDQW - GDW( IJ,K ) )
               GEVPP( IJ,K ) = MIN( GEVPP( IJ,K ), GPRCP( IJ,K+1 ) )
               EVAPT( IJ )   = EVAPT( IJ   ) + GEVPP( IJ,K )
            ELSE
               GEVPP( IJ,K ) = 0.
               GDQW          = GDW( IJ,K )
            ENDIF
            GPRCP( IJ,K )    = GPRCP( IJ,K ) - EVAPT( IJ )
*               
            IF ( K .GE. KB   ( IJ ) .AND. 
     &           K .LT. KHMIN( IJ )       ) THEN
               GMDDE        = -REVPDD*GEVPP( IJ,K )
     &                      /  MAX( GDQW - GDW(IJ,K), 1.D-10 )
               GMDD( IJ,K ) = GMDD( IJ,K+1 ) + GMDDE
               GMDD( IJ,K ) = MAX( GMDD( IJ,K ), -RDDMX*GMFXB( IJ ) )
               GMDDE        = GMDD( IJ,K ) - GMDD( IJ,K+1 )
               EVPE         = GEVPP( IJ,K ) + GMDDE*( GDQW - GDW(IJ,K) )
               GCHD ( IJ )  = GCHD ( IJ )   + GDH( IJ,K )*GMDDE
               GCWD ( IJ )  = GCWD ( IJ )   + GDQW       *GMDDE
               GMDDB( IJ )  = GMDDB( IJ )   + GMDDE
            ELSE
               GMDD( IJ,K ) = GMDD ( IJ,K+1 )
               EVPE         = GEVPP( IJ,K   )
            ENDIF
*
            GTQCI = GRAV/DELP( IJ,K )*EVPE
            GTT( IJ,K ) = GTT( IJ,K ) - EL*GTQCI/CP
            GTQ( IJ,K ) = GTQ( IJ,K ) + GTQCI
 2100 CONTINUE 
      DO 2200 IJ = 1, IJSDIM
         IF ( GMDDB( IJ ) .NE. 0. ) THEN
            GCHD( IJ ) = GCHD( IJ )/GMDDB( IJ )
            GCWD( IJ ) = GCWD( IJ )/GMDDB( IJ )
         ENDIF
 2200 CONTINUE 
*
      DO 3100 K = KBMX, 1, -1
         DO 3100 IJ = 1, IJSDIM
            IF ( K .LT. KB( IJ ) ) THEN
               GMDD( IJ,K ) = GMDDB( IJ ) * GCYM( IJ,K )
*
               GTHCI
     &             = GRAV/DELP( IJ,K )
     &               *( GMDD( IJ,K ) - GMDD( IJ,K+1 ) )
     &               *( GCHD( IJ )   - GDH ( IJ,K )   )
               GTQCI
     &             = GRAV/DELP( IJ,K )
     &               *( GMDD( IJ,K ) - GMDD( IJ,K+1 ) )
     &               *( GCWD( IJ )   - GDW ( IJ,K )   )
*
               GTT( IJ,K ) = GTT( IJ,K ) + ( GTHCI - EL*GTQCI )/CP
               GTQ( IJ,K ) = GTQ( IJ,K ) + GTQCI
            ENDIF
 3100 CONTINUE
*
      DO 4100 K = 1, KTMX
         DO 4100 IJ = 1, IJSDIM
            GMFLX( IJ,K ) = GMFLX( IJ,K ) + GMDD( IJ,K )
 4100 CONTINUE 
*
      RETURN
      END
**********************************************************************
      SUBROUTINE CUMMLT    !! A-S cumulus, freeze & melt of prec.
     M         ( GTT   , GTQ   ,
     O           GPRCC , GSNWC , 
     I           GPRCP , DELP  ,
     I           GDT   , GDQ   , GDQS  , GAM   ,
     I           KTMX                           )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zccom.F"                /* stand. physical const. */
*
*   [MODIFY] 
      REAL*8     GTT   ( IJSDIM, KMAX )   !! heating rate
      REAL*8     GTQ   ( IJSDIM, KMAX )   !! change in q
*
*   [OUTPUT] 
      REAL*8     GPRCC ( IJSDIM       )   !! rain fall
      REAL*8     GSNWC ( IJSDIM       )   !! snow fall
*
*   [INPUT] 
      REAL*8     GPRCP ( IJSDIM, KMAX+1 ) !! precipitation
      REAL*8     DELP  ( IJSDIM, KMAX )
      REAL*8     GDT   ( IJSDIM, KMAX )   !! temperature T
      REAL*8     GDQ   ( IJSDIM, KMAX )   !! humidity q
      REAL*8     GDQS  ( IJSDIM, KMAX )
      REAL*8     GAM   ( IJSDIM, KMAX )
      INTEGER    KTMX
*
*   [WORK] 
      REAL*8     GDTW, ELFM, RDZI
      INTEGER    IJ, K, IFPAR, JFPAR
*
*   [INTERNAL PARM] 
      REAL*8     TWSNOW                      !! wet-bulb temp. rain/snow
      LOGICAL    OFIRST
      NAMELIST  /NMSNWF/ TWSNOW
      DATA       TWSNOW         / 273.15 /
      DATA       OFIRST         / .TRUE. /
*
*   [ONCE] 
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ CLDMLT: FREEZE AND MELT OF PREC. 95/01/24'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMSNWF, END=290 )
  290    WRITE  ( JFPAR, NMSNWF )
      ENDIF
*
      ELFM = EMELT / CP
      CALL RESET( GPRCC, IJSDIM )
      CALL RESET( GSNWC, IJSDIM )
*
      DO 1100 K = KTMX, 1, -1
         DO 1100 IJ = 1, IJSDIM
            GPRCC( IJ ) = GPRCC( IJ ) 
     &                  + GPRCP( IJ,K ) - GPRCP( IJ,K+1 )
            GDTW        = GDT( IJ,K ) 
     &                  - EL/CP* ( GDQS( IJ,K ) - GDQ( IJ,K ) )
     &                         / ( 1. + GAM( IJ,K )           )
            RDZI        = GRAV/DELP( IJ,K )
            IF ( GDTW .LT. TWSNOW ) THEN
               GTT( IJ,K ) = GTT( IJ,K ) + ELFM * GPRCC( IJ ) * RDZI
               GSNWC( IJ ) = GPRCC( IJ ) + GSNWC( IJ )
               GPRCC( IJ ) = 0.
            ELSE
               GTT( IJ,K ) = GTT( IJ,K ) - ELFM * GSNWC( IJ ) * RDZI
               GPRCC( IJ ) = GPRCC( IJ ) + GSNWC( IJ) 
               GSNWC( IJ ) = 0.               
            ENDIF
 1100 CONTINUE
*      
      RETURN
      END
**********************************************************************
      SUBROUTINE CUMCLD       !! A-S cumulus, cloudiness
     M         ( CUMCLW, 
     O           CUMFRC,
     I           GMFLX , KTMX  )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zccom.F"                /* stand. physical const. */
*
*   [OUTPUT] 
      REAL*8     CUMCLW( IJSDIM, KMAX )   !! cloud water in cumulus
      REAL*8     CUMFRC( IJSDIM )         !! cumulus cloud fraction
*
*   [INPUT] 
      REAL*8     GMFLX ( IJSDIM, KMAX+1 ) !! cumulus mass flux
      INTEGER    KTMX
*
*   [WORK] 
      INTEGER    IJ, K, IFPAR, JFPAR
      REAL*8     CUMF
*      
*   [INTERNAL PARAM] 
      REAL*8     FACLW                    !! Mc->CLW
      REAL*8     CMFMIN                   !! Mc->cloudiness
      REAL*8     CMFMAX                   !! Mc->cloudiness
      REAL*8     CLMIN                    !! cloudiness Min.
      REAL*8     CLMAX                    !! cloudiness Max.
      REAL*8     FACLF
      LOGICAL    OFIRST
      SAVE       FACLF
      NAMELIST   /NMASCL/ FACLW, CMFMIN, CMFMAX, CLMIN, CLMAX
      DATA       FACLW                  / 0.3 /
      DATA       CMFMIN, CMFMAX         / 2.E-3, 3.E-1 /
      DATA       CLMIN , CLMAX          / 1.E-3, 0.4   /
      DATA       OFIRST                 / .TRUE.       /
*
*   [ONCE] 
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' CUMULUS CLOUDINESS 96/05/07'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMASCL, END=190 )
  190    WRITE  ( JFPAR, NMASCL )
         FACLF = (CLMAX-CLMIN)/LOG(CMFMAX/CMFMIN)
      ENDIF
*
      CALL RESET( CUMFRC, IJSDIM )
      DO 1100 K = 1, KTMX
         DO 1100 IJ = 1, IJSDIM
            CUMFRC( IJ ) = MAX( CUMFRC( IJ ), GMFLX( IJ,K ) )
 1100 CONTINUE 
      DO 1200 IJ = 1, IJSDIM
         IF ( CUMFRC( IJ ) .GT. 0. ) THEN
            CUMF         = LOG( MAX( CUMFRC( IJ ),CMFMIN )/CMFMIN )
            CUMFRC( IJ ) = MIN( FACLF*CUMF+CLMIN, CLMAX )
         ENDIF
 1200 CONTINUE 
*
      DO 2100 K = 1, KTMX
         DO 2100 IJ = 1, IJSDIM
            IF ( GMFLX( IJ,K ) .GT. 0. ) THEN
               CUMCLW( IJ,K ) = FACLW*CUMCLW( IJ,K )/GMFLX( IJ,K )
     &                        * CUMFRC( IJ )
            ENDIF
 2100 CONTINUE      
*
      RETURN
      END
**********************************************************************
      SUBROUTINE CUMUPR         !! Tracer Updraft
     M         ( GTR   , GPRCC ,
     I           GDR   , CBMFX , CMMFX , ELAM  ,
     I           GCYT  , GCYM  , GCWT  , GPRCT ,
     I           GPRCP , GEVPP , GDZ   , GDZM  , DELP  ,
     I           KB    , KBMX  , KTMX  , NTRCUM,
     I           GPRCR                                   )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zccom.F"                /* stand. physical const. */
#include        "zpdim.F"
*
*   [MODIFY] 
      REAL*8     GTR   ( IJSDIM, KMAX, NTR )
      REAL*8     GPRCC ( IJSDIM, NTR  )
*
*   [INPUT] 
      REAL*8     GDR   ( IJSDIM, KMAX, NTR )
      REAL*8     CBMFX ( IJSDIM, KMAX   )
      REAL*8     CMMFX ( IJSDIM, KMAX   )
      REAL*8     ELAM  ( IJSDIM, KMAX   )
      REAL*8     GCYT  ( IJSDIM, KMAX   )
      REAL*8     GCYM  ( IJSDIM, KMAX   )
      REAL*8     GCWT  ( IJSDIM, KMAX   )
      REAL*8     GPRCT ( IJSDIM, KMAX   )
      REAL*8     GPRCP ( IJSDIM, KMAX+1 ) !! precipitation
      REAL*8     GEVPP ( IJSDIM, KMAX   )
      REAL*8     GDZ   ( IJSDIM, KMAX   )
      REAL*8     GDZM  ( IJSDIM, KMAX+1 )
      REAL*8     DELP  ( IJSDIM, KMAX   )
      INTEGER    KB    ( IJSDIM )
      INTEGER    KBMX
      INTEGER    KTMX
      INTEGER    NTRCUM
*
*   [WORK]
      REAL*8     GPRCR ( IJSDIM, KMAX )
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K, LT, LTX, IFPAR, JFPAR, KP1
      REAL*8     DELZ, DELZH, DELZL, GCRT, GCRTD
      REAL*8     CXFLX, SCAV, EVPF, GPRCX
      REAL*8     GCRB  ( IJSDIM )
      REAL*8     DR    ( IJSDIM )
*
*   [INTERNAL PARM] 
      INTEGER    NTRX
      PARAMETER (NTRX=NTR-ITL+1)
      REAL*8     FSCAV ( NTRX )
      REAL*8     FENTR ( NTRX )
      REAL*8     FSCUM ( NTRX )
      REAL*8     FSWTR ( NTRX )
      LOGICAL    OFIRST
      NAMELIST  /NMSCAV/ FSCAV , FENTR , FSCUM , FSWTR
*
*   [INTERNAL FUNC] 
      DATA       FSCAV          / NTRX*0.0 /
      DATA       FENTR          / NTRX*0.0 /
      DATA       FSCUM          / NTRX*0.0 /
#ifdef OPT_WTRACE
      DATA       FSWTR          / NTRX*1.0 /
#else
      DATA       FSWTR          / NTRX*0.0 /
#endif
      DATA       OFIRST         / .TRUE.   /
*
*   [ONCE] 
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ PCUMA: TRACER UPDRAFT 96/05/13'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMSCAV, END=390 )
  390    WRITE  ( JFPAR, NMSCAV )
      ENDIF
*
      DO 4000 LT = ITL+1, NTRCUM
         LTX = LT - ITL
*
         CALL RESET( GPRCR, IJSDIM*KMAX )
         CALL RESET( GCRB , IJSDIM      )
         CALL RESET( DR,    IJSDIM      )
*
         DO 1100 K = 1, KBMX
            DO 1100 IJ = 1, IJSDIM
               IF ( K .LT. KB( IJ ) ) THEN
                  DELZ  = GCYM( IJ,K+1 ) - GCYM( IJ,K )
                  GCRB( IJ ) = GCRB( IJ ) + DELZ * GDR( IJ,K,LT )
               ENDIF
 1100    CONTINUE
*
         DO 2100 K = 2, KTMX
            KP1 = MIN( K+1, KMAX )
            DO 2100 IJ = 1, IJSDIM
               IF ( K .GE. KB( IJ ) ) THEN
                  DELZL = GDZ ( IJ,K   ) - GDZM( IJ,K  )
                  DELZH = GDZM( IJ,KP1 ) - GDZ ( IJ,K  )
                  DELZH = MAX( DELZH, 0.D0 )
                  DR( IJ ) = DR  ( IJ ) + DELZL*GDR( IJ,K,LT  )
                  GCRT     = GCRB( IJ ) + ELAM( IJ,K )*DR( IJ )
                  DR( IJ ) = DR  ( IJ ) + DELZH*GDR( IJ,K,LT  )
               ENDIF
*
               IF ( CBMFX( IJ,K ) .LE. 0.D0 ) THEN
                  GCRT = GDR( IJ,K-1,LT ) 
               ENDIF
*
               CXFLX = CBMFX( IJ,K ) + CMMFX( IJ,K )
               IF ( CXFLX .GT. 0.D0 ) THEN
                  SCAV  = FSCUM( LTX )*GPRCT( IJ,K )
                  IF ( GCWT( IJ,K ) .GT. 0.D0 ) THEN
                     SCAV = SCAV 
     &                    + FSWTR( LTX )
     &                      *GPRCT( IJ,K )/GCWT( IJ,K )
                  ENDIF
                  SCAV  = MIN( SCAV, 1.D0 )
                  GCRTD = GCRT * ( 1.D0 - SCAV )
                  GPRCR ( IJ,K ) = SCAV * GCRT * CXFLX
                  GTR( IJ,K,LT ) = GTR( IJ,K,LT )
     &                      + GRAV/DELP( IJ,K )*CXFLX
     &                        *( GCRTD - GCYT( IJ,K )*GDR( IJ,K,LT ) )
               ENDIF
 2100    CONTINUE 
*
         DO 3100 K = KTMX, 1, -1
            DO 3100 IJ = 1, IJSDIM
               IF ( GPRCP( IJ,K+1 ) .GT. 0.D0 ) THEN
                  EVPF  = GEVPP( IJ,K )/GPRCP( IJ,K+1 )
                  GPRCX = GPRCC( IJ,LT )*( 1.D0 - EVPF )
                  GTR( IJ,K,LT ) = GTR ( IJ,K,LT )  
     &                           + GRAV/DELP( IJ,K )
     &                             * ( GPRCC( IJ,LT ) - GPRCX )
                  GPRCC( IJ,LT ) = GPRCX
               ENDIF
               GPRCC( IJ,LT ) = GPRCC( IJ,LT ) + GPRCR( IJ,K )
 3100    CONTINUE 
*
 4000 CONTINUE 
*
      RETURN
      END
********************************************************************
      SUBROUTINE CUMDNR      !! Tracer Downdraft
     M         ( GTR   , 
     I           GDR   , GMDD  , GMDDB , DELP  ,
     I           KB    , KBMX  , KTMX  , NTRCUM  )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zccom.F"                /* stand. physical const. */
#include        "zpdim.F"
*
*   [MODIFY] 
      REAL*8     GTR   ( IJSDIM, KMAX, NTR )   !! Temperature tendency 
*
*   [INPUT] 
      REAL*8     GDR   ( IJSDIM, KMAX, NTR )
      REAL*8     GMDD  ( IJSDIM, KMAX+1 )   !! downdraft mass flux
      REAL*8     GMDDB ( IJSDIM         )   !! downdraft mass flux
      REAL*8     DELP  ( IJSDIM, KMAX   )
      INTEGER    KB    ( IJSDIM )
      INTEGER    KBMX
      INTEGER    KTMX
      INTEGER    NTRCUM
*
*   [INTERNAL WORK] 
      REAL*8     GCRD  ( IJSDIM )           !! downdraft q
      REAL*8     GMDDE, GPRCX, EVPF
      INTEGER    IJ, K, LT
*
      DO 4000 LT = ITL+1, NTRCUM
*
         CALL RESET( GCRD, IJSDIM )
         DO 1100 K = KTMX, 1, -1
            DO 1100 IJ = 1, IJSDIM
               IF ( K .GE. KB( IJ ) ) THEN
                  GMDDE       = GMDD( IJ,K ) - GMDD( IJ,K+1 )
                  GCRD ( IJ ) = GCRD( IJ )   + GDR ( IJ,K,LT )*GMDDE
               ENDIF
 1100    CONTINUE 
         DO 1200 IJ = 1, IJSDIM
            IF ( GMDDB( IJ ) .NE. 0. ) THEN
               GCRD( IJ ) = GCRD( IJ )/GMDDB( IJ )
            ENDIF
 1200    CONTINUE 
*
         DO 2100 K = KBMX, 1, -1
            DO 2100 IJ = 1, IJSDIM
               IF ( K .LT. KB( IJ ) ) THEN
                  GTR( IJ,K,LT ) = GTR( IJ,K,LT ) 
     &                 + GRAV/DELP( IJ,K )
     &                   *( GMDD( IJ,K ) - GMDD( IJ,K+1 ) )
     &                   *( GCRD( IJ )   - GDR( IJ,K,LT ) )
               ENDIF
 2100    CONTINUE
*
 4000 CONTINUE 
*
      RETURN
      END
**********************************************************************
      SUBROUTINE CUMSBR    !! Tracer Subsidence
     M         ( GTR   ,
     I           GDR   , DELP  ,
     I           GMFLX , KTMX  , NTRCUM  )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zccom.F"                /* stand. physical const. */
#include        "zpdim.F"
*
*   [MODIFY] 
      REAL*8     GTR   ( IJSDIM, KMAX, NTR )   !! tracer tendency 
*
*   [INPUT] 
      REAL*8     GDR   ( IJSDIM, KMAX, NTR )   !! tracer
      REAL*8     DELP  ( IJSDIM, KMAX   ) 
      REAL*8     GMFLX ( IJSDIM, KMAX+1 )      !! mass flux
      INTEGER    KTMX
      INTEGER    NTRCUM
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K, KM, KP, LT
      REAL*8     SBR0, SBR1, FX1
      REAL*8     FX    ( IJSDIM )
*
      DO 1000 LT = 2, NTRCUM
         CALL RESET( FX, IJSDIM )
         DO 1100 K = KTMX, 1, -1
            KM = MAX( K-1, 1    )
            KP = MIN( K+1, KMAX )
            DO 1100 IJ = 1, IJSDIM
               SBR0 = GMFLX( IJ,K+1 )
     &                *( GDR( IJ,KP,LT )-GDR( IJ,K ,LT ) )
               SBR1 = GMFLX( IJ,K   )
     &                *( GDR( IJ,K ,LT )-GDR( IJ,KM,LT ) )
*
               IF ( GMFLX( IJ,K ) .GT. GMFLX( IJ,K+1 ) ) THEN
                  FX1 = 0.5D0
               ELSE
#ifdef OPT_SBHCOR
                  FX1 = .5D0 * MIN( ABS(SBR0)/MAX( ABS(SBR1),1.D-20 ),
     &                              1.D0 )
#else
                  FX1 = 0.D0
#endif
               ENDIF
*
               GTR( IJ,K,LT )
     &               = GTR( IJ,K,LT )
     &               + GRAV/DELP( IJ,K )
     &               * ( ( 1.D0-FX( IJ ) )*SBR0 + FX1 *SBR1 )
*
               FX ( IJ ) = FX1
 1100    CONTINUE
 1000 CONTINUE
*
      RETURN
      END
