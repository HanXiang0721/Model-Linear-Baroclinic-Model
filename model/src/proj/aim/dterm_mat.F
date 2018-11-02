* PACKAGE DCIRC !! dynamics: terms of eqs.
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/05/28(numaguti) restructure
*         00/04/10(hiro)     planetary wave model
*         01/06/15(hiro)     PWM incl. moisture tendency
*         05/05/26(hiro)     AIM for baroclinic model
*         05/06/16(hiro)     solenoidal term removed (optional)
*         GF* : basic state
*         GA* : perturbation
**********************************************************************
      SUBROUTINE DYNTRM_MAT     !! dynamics terms with ZM GF*
     O         ( WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
     I           GAU   , GAV   , GAT   , GAPI  , GAQ   ,
     I           GAVOR , GADIV , 
     I           GFU0  , GFV0  , GFT0  , GFPS0 , GFQ0  ,
     I           DELT  , GDZS  , SDTOP , SDBTM , OSLNOID,
     C           SALPHA, SBETA , SKAPPA, ATF   , BTF   , 
     C           TBAR  , TMTX  , CORIOL, FLAPLA         )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* # of wave              */
#include        "zpdim.F"                /* physics etc.           */
#include        "zccom.F"                /* physical constants     */
#include        "zcord.F"                /* coordinate             */
#include        "zhdim.F"                /* # of char. in string */
*
*   [OUTPUT] 
      REAL*8     WTVOR ( NMDIM, KMAX      )  !! tendency of vorticity
      REAL*8     WTDIV ( NMDIM, KMAX      )  !! tendency of divergence D
      REAL*8     WTT   ( NMDIM, KMAX      )  !! tendency of temperature
      REAL*8     WTPI  ( NMDIM            )  !! logPs pi tendency
      REAL*8     WTQ   ( NMDIM, KMAX      )  !! tendency of humidity
*
*   [INPUT] 
      REAL*8     GAU ( IJDIM, KMAX       )  !! perturbation wind
      REAL*8     GAV ( IJDIM, KMAX       )  !! perturbation wind
      REAL*8     GAT ( IJDIM, KMAX       )  !! perturbation temperature 
      REAL*8     GAPI( IJDIM             )  !! perturbation pi
      REAL*8     GAQ ( IJDIM, KMAX       )  !! perturbation humidity
      REAL*8     GAVOR ( IJDIM, KMAX     )  !! vorticity
      REAL*8     GADIV ( IJDIM, KMAX     )  !! divergence D 
*
      REAL*8     GFU0 ( IJDIM, KMAX       )  !! BS zonal wind
      REAL*8     GFV0 ( IJDIM, KMAX       )  !! BS meridional wind
      REAL*8     GFT0 ( IJDIM, KMAX       )  !! BS temperature
      REAL*8     GFPS0( IJDIM             )  !! BS surface pressure
      REAL*8     GFQ0 ( IJDIM, KMAX       )  !! BS specific humidity
*
      REAL*8     GDZS  ( IJDIM )             !! surface topography
      REAL*8     SDTOP ( IJDIM )             !! coefficient for diss.
      REAL*8     SDBTM ( IJDIM )             !! coefficient for diss.
      REAL*8     SALPHA( KMAX  )             !! fact. of hydrostatic eq.
      REAL*8     SBETA ( KMAX  )             !! fact. of hydrostatic eq.
      REAL*8     SKAPPA( KMAX  )             !! fact. of vert. intrp. T
      REAL*8     ATF   ( KMAX  )             !! fact. of vert. intrp. T
      REAL*8     BTF   ( KMAX  )             !! fact. of vert. intrp. T
      REAL*8     TBAR  ( KMAX  )             !! basic temp. (full lev)
      REAL*8     TMTX  ( KMAX, KMAX )        !! for semi-implicit
*
      REAL*8     CORIOL( IJDIM )             !! Corioris factor f
      REAL*8     FLAPLA( NMDIM )             !! Laplacian factor
*
      REAL*8     DELT                        !! time step

      LOGICAL    OSLNOID        !! include solenoidal term in vor.
*
*   [WORK] 
      REAL*8     GFPI  ( IJDIM         )  !! BS pi
      REAL*8     GDX   ( IJDIM, KMAX   )  !! work
      REAL*8     GDY   ( IJDIM, KMAX   )  !! work
      REAL*8     WDX   ( NMDIM, KMAX   )  !! work
      REAL*8     WDY   ( NMDIM, KMAX   )  !! work
      REAL*8     WDS   ( NMDIM         )  !! work

      INTEGER    IJ, K, L, NM, IFPAR, JFPAR
*
*   [INTERNAL SAVE] !! variables for basic state
      REAL*8     GFU  ( IJDIM, KMAX )    !! BS zonal wind
      REAL*8     GFV  ( IJDIM, KMAX )    !! BS meridional wind
      REAL*8     GFT  ( IJDIM, KMAX )    !! BS temperature
      REAL*8     GFPS ( IJDIM       )    !! BS surface pressure
      REAL*8     GFQ  ( IJDIM, KMAX )    !! BS specific humidity
      REAL*8     GFVOR( IJDIM, KMAX )    !! BS for VOR
      REAL*8     GFDIV( IJDIM, KMAX )    !! BS for DIV (output)
      REAL*8     GFSIGD( IJDIM, KMAX+1)  !! tend. of sigma (output)
      REAL*8     GFPID ( IJDIM       )   !! tend. of pai
      REAL*8     GFTV  ( IJDIM, KMAX )   !! virtulal tmp.
      REAL*8     GFPADV( IJDIM, KMAX )   !! advection of pai 
      REAL*8     GFSUMP( IJDIM, KMAX )   !! vert. integ. tend. pi
      REAL*8     GFPIX ( IJDIM       )   !! zonal deriv. of pi
      REAL*8     GFPIY ( IJDIM       )   !! merid. differ. of pi 
*
      SAVE       GFU, GFV, GFT, GFPS, GFQ
      SAVE       GFVOR, GFDIV
      SAVE       GFSIGD, GFPID, GFTV, GFPADV, GFSUMP, GFPIX, GFPIY
*
*   [INTERNAL PARAM.] 
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE.  /

      IF ( OFIRST ) THEN
         CALL   REWNML ( IFPAR , JFPAR )
         WRITE ( JFPAR,* ) ' @@@ DTERM: DYNAMICS EQ. TERMS 98/10/17'
*
*     modify basic state
*
         CALL COPY  ( GFU  , GFU0 , IJDIM*KMAX )
         CALL COPY  ( GFV  , GFV0 , IJDIM*KMAX )
         CALL COPY  ( GFT  , GFT0 , IJDIM*KMAX )
         CALL COPY  ( GFPS , GFPS0, IJDIM      )
         CALL COPY  ( GFQ  , GFQ0 , IJDIM*KMAX )
         CALL MTRUNC( GFU  , KMAX , .TRUE., WDX )
         CALL MTRUNC( GFV  , KMAX , .TRUE., WDX )
         CALL MTRUNC( GFT  , KMAX , .TRUE., WDX )
         CALL MTRUNC( GFPS , 1    , .TRUE., WDS )
         CALL MTRUNC( GFQ  , KMAX , .TRUE., WDX )
*
*     calculation for basic state
*
         CALL UV2VDG
     O        ( GFVOR , GFDIV ,
     I        GFU   , GFV   , .TRUE., .TRUE.,
     W        GDX   , GDY   , WDX   , WDY    )
*
         DO 1100 IJ = 1, IJDIM
            GFPI( IJ ) = LOG( GFPS(IJ) )
 1100    CONTINUE 
*
         CALL HGRAD             !! pi gradient
     O        ( GFPIX , GFPIY ,
     I          GFPI  , 1     ,
     W          WDS            )
*
         CALL PSDOTB            !!  surface pres. change & sigma velocity
     O         ( GFSIGD, GFSUMP, GFPADV, GFPID ,
     I           GFU   , GFV   , GFDIV , GFPIX , GFPIY  )
*
         CALL VIRTMB( GFTV , GFT , GFQ  )
*
         OFIRST = .FALSE.
      ENDIF
*
*     dynamic equation terms
*
      CALL DYNTRM_SUB     !! dynamics terms 
     O         ( WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
     I           GAU   , GAV   , GAT   , GAPI  , GAQ   ,
     I           GAVOR , GADIV , 
     I           GFU   , GFV   , GFT   , GFPS  , GFQ   ,
     I           GFVOR , GFDIV , GFSIGD, GFPID , GFTV  ,
     I           GFPADV, GFSUMP, GFPIX , GFPIY ,
     I           DELT  , GDZS  , SDTOP , SDBTM , OSLNOID,
     C           SALPHA, SBETA , SKAPPA, ATF   , BTF   , 
     C           TBAR  , TMTX  , CORIOL, FLAPLA         )
*
      RETURN
      END
**********************************************************************
      SUBROUTINE DYNTRM_SUB     !! dynamics terms 
     O         ( WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
     I           GAU   , GAV   , GAT   , GAPI  , GAQ   ,
     I           GAVOR , GADIV , 
     I           GFU   , GFV   , GFT   , GFPS  , GFQ   ,
     I           GFVOR , GFDIV , GFSIGD, GFPID , GFTV  ,
     I           GFPADV, GFSUMP, GFPIX , GFPIY ,
     I           DELT  , GDZS  , SDTOP , SDBTM , OSLNOID,
     C           SALPHA, SBETA , SKAPPA, ATF   , BTF   , 
     C           TBAR  , TMTX  , CORIOL, FLAPLA         )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* # of wave              */
#include        "zpdim.F"                /* physics etc.           */
#include        "zccom.F"                /* physical constants     */
#include        "zcord.F"                /* coordinate             */
#include        "zhdim.F"                /* # of char. in string */
*
*   [OUTPUT] 
      REAL*8     WTVOR ( NMDIM, KMAX      )  !! tendency of vorticity
      REAL*8     WTDIV ( NMDIM, KMAX      )  !! tendency of divergence D
      REAL*8     WTT   ( NMDIM, KMAX      )  !! tendency of temperature
      REAL*8     WTPI  ( NMDIM            )  !! logPs pi tendency
      REAL*8     WTQ   ( NMDIM, KMAX      )  !! tendency of humidity
*
*   [INPUT] 
      REAL*8     GAU ( IJDIM, KMAX       )  !! perturbation wind
      REAL*8     GAV ( IJDIM, KMAX       )  !! perturbation wind
      REAL*8     GAT ( IJDIM, KMAX       )  !! perturbation temperature 
      REAL*8     GAPI( IJDIM             )  !! perturbation pi
      REAL*8     GAQ ( IJDIM, KMAX       )  !! perturbation humidity
      REAL*8     GAVOR ( IJDIM, KMAX       )  !! vorticity
      REAL*8     GADIV ( IJDIM, KMAX       )  !! divergence D 
*
      REAL*8     GFU  ( IJDIM, KMAX )    !! BS zonal wind
      REAL*8     GFV  ( IJDIM, KMAX )    !! BS meridional wind
      REAL*8     GFT  ( IJDIM, KMAX )    !! BS temperature
      REAL*8     GFPS ( IJDIM       )    !! BS surface pressure
      REAL*8     GFQ  ( IJDIM, KMAX )    !! BS specific humidity
      REAL*8     GFVOR( IJDIM, KMAX )    !! BS for VOR
      REAL*8     GFDIV( IJDIM, KMAX )    !! BS for DIV (output)
      REAL*8     GFSIGD( IJDIM, KMAX+1)  !! tend. of sigma (output)
      REAL*8     GFPID ( IJDIM        )  !! tend. of pai
      REAL*8     GFTV  ( IJDIM, KMAX  )  !! virtulal tmp.
      REAL*8     GFPADV( IJDIM, KMAX )   !! advection of pai 
      REAL*8     GFSUMP( IJDIM, KMAX )   !! vert. integ. tend. pi
      REAL*8     GFPIX ( IJDIM       )   !! zonal deriv. of pi
      REAL*8     GFPIY ( IJDIM       )   !! merid. differ. of pi 
*
      REAL*8     GDZS  ( IJDIM )             !! surface topography
      REAL*8     SDTOP ( IJDIM )             !! coefficient for diss.
      REAL*8     SDBTM ( IJDIM )             !! coefficient for diss.
      REAL*8     SALPHA( KMAX  )             !! fact. of hydrostatic eq.
      REAL*8     SBETA ( KMAX  )             !! fact. of hydrostatic eq.
      REAL*8     SKAPPA( KMAX  )             !! fact. of vert. intrp. T
      REAL*8     ATF   ( KMAX  )             !! fact. of vert. intrp. T
      REAL*8     BTF   ( KMAX  )             !! fact. of vert. intrp. T
      REAL*8     TBAR  ( KMAX  )             !! basic temp. (full lev)
      REAL*8     TMTX  ( KMAX, KMAX )        !! for semi-implicit
*
      REAL*8     CORIOL( IJDIM )             !! Corioris factor f
      REAL*8     FLAPLA( NMDIM )             !! Laplacian factor
*
      REAL*8     DELT                        !! time step

      LOGICAL    OSLNOID        !! include solenoidal term in vor.
*
*   [INTERNAL] 
      REAL*8     GATV  ( IJDIM, KMAX       )  !! virtulal tmp.
      REAL*8     GSIGD ( IJDIM, KMAX+1     )  !! tendency of sigma
      REAL*8     GPID  ( IJDIM             )  !! tendency of pai
      REAL*8     GAPADV( IJDIM, KMAX )   !! advection of pai
      REAL*8     GSUMP ( IJDIM, KMAX )   !! vert. integ. of tend. of pi
      REAL*8     GAPIX ( IJDIM       )   !! zonal deriv. of pi
      REAL*8     GAPIY ( IJDIM       )   !! merid. differ. of pi
#ifdef OPT_ADDCPL
      REAL*8     GTAUX ( IJDIM       )   !! zonal stress
      REAL*8     GTAUY ( IJDIM       )   !! meridional stress
      REAL*8     GPREC ( IJDIM       )   !! precipitation
#endif
*
*   [WORK] 
      REAL*8     GTUX  ( IJDIM, KMAX   )  !! tend. zonal  adv.
      REAL*8     GTVX  ( IJDIM, KMAX   )  !! tend. merid. adv.
      REAL*8     GTUXN ( IJDIM, KMAX   )  !! tend. zonal  adv.
      REAL*8     GTVXN ( IJDIM, KMAX   )  !! tend. merid. adv.
      REAL*8     GTXX  ( IJDIM, KMAX   )  !! tend.
      REAL*8     GAWT  ( IJDIM, KMAX   )  !! forcing for temp., f(W')
      REAL*8     GAWQ  ( IJDIM, KMAX   )  !! forcing for Q, f(W')
      REAL*8     WTKE  ( NMDIM, KMAX   )  !! KE term
      REAL*8     WDX   ( NMDIM, KMAX   )  !! work
      REAL*8     WDY   ( NMDIM, KMAX   )  !! work
      REAL*8     WDS   ( NMDIM         )  !! work

      INTEGER    IJ, K, L, NM, IFPAR, JFPAR
*
*   [INTERNAL SAVE] !! variables for basic state
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      REAL*8     UVFACT( IJDIM  )        !! factor of u -> U
*
      SAVE       NMO, UVFACT 
*
*   [INTERNAL PARAM.] 
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE.  /

      IF ( OFIRST ) THEN
         CALL   REWNML ( IFPAR , JFPAR )
         WRITE ( JFPAR,* ) ' @@@ DTERM_SUB: DYNAMICS TERMS 05/06/24'
         CALL DSETNM
     O         ( NMO    )
         CALL   DSETUV
     O        ( UVFACT )
*
         CALL RESET( GAWT, IJDIM*KMAX )
         CALL RESET( GAWQ, IJDIM*KMAX )
         OFIRST = .FALSE.
      ENDIF
*
*     internal forcing due to surface T, Q fluxes
*
#ifndef OPT_CLASSIC
#ifdef  OPT_ORHS
      CALL UV2TQF               !! sfc. flux (Ta,Qa,Va -> Fh,Fq)
     O     ( GAWT , GAWQ ,
     I       GAU  , GAV  , GAT  , GAQ  , DELT  )
      CALL TQ2TQF               !! MCA (Ta,Qa -> Fh,Fq)
     M     ( GAWT , GAWQ ,
     I       GAT  , GAQ  , DELT                )
#ifdef OPT_ADDCPL
      CALL UV2TAU               !! U,V -> sfc stress
     O     ( GTAUX , GTAUY ,
     I       GAU   , GAV     )
      CALL RESET( GAWT , IJDIM*KMAX )
      CALL TQ2TQF               !! MCA (Ta,Qa -> Fh,Fq)
     O     ( GAWT , GAWQ ,
     I       GAT  , GAQ  , DELT                )
      CALL Q2PREC               !! heating to precipitation
     O     ( GPREC , 
     I       GAWT              )
#endif
#endif
#endif
*
*    << pressure eq. >>
*
*
      CALL HGRAD                       !! perturbation pi gradient
     O         ( GAPIX , GAPIY ,
     I           GAPI  , 1     ,
     W           WDS            )
*
      CALL PSDOT     !!  surface pres. change & sigma velocity
     O         ( GSIGD , GSUMP , GAPADV, GPID  ,
     I           GAU   , GAV   , GADIV , GAPIX , GAPIY , 
     I           GFU   , GFV   , GFPIX , GFPIY  )
*
      CALL COPY ( GTXX, GPID, IJDIM )
      DO 2100 IJ = 1, IJDIM
         DO 2200 K = 1, KMAX
            GTXX( IJ,1 )  = GTXX( IJ,1 )  + DSIG( K )*GADIV( IJ,K )
 2200    CONTINUE
 2100 CONTINUE 
*
      CALL G2W
     O         ( WTPI  ,
     I           GTXX  ,
     I           '    ', 'POSO',  1  )
*
*    << temperature eq. >>
*
      CALL VIRTMP( GATV , GFT , GFQ , GAT , GAQ )
*
      CALL GRTADV                      !!  temperature advection
     O         ( GTUX  , GTVX  , GTXX  ,
     I           GAT   , GATV  ,
     I           GFT   , GFTV  ,
     I           GAU   , GAV   , GADIV , 
     I           GFU   , GFV   , GFDIV , 
     I           GSIGD , GSUMP , GAPADV,
     I           GFSIGD, GFSUMP, GFPADV,
     C           SALPHA, SBETA , SKAPPA,
     C           ATF   , BTF   , TBAR    )
*
      DO 3100 L = 1, KMAX
         DO 3100 K = 1, KMAX
            DO 3100 IJ = 1, IJDIM
               GTXX( IJ,K )  = GTXX( IJ,K ) + TMTX( K,L )*GADIV( IJ,L )
 3100 CONTINUE 
*
      DO 3150 IJ = 1, IJDIM
         GTXX( IJ,1    )  = GTXX( IJ,1    )
     &                    + SDBTM( IJ ) * GAT( IJ, 2       )
         GTXX( IJ,KMAX )  = GTXX( IJ,KMAX )
     &                    + SDTOP( IJ ) * GAT( IJ, KMAX-1 )
 3150 CONTINUE 
*
      CALL HCONVW
     O         ( WTT   ,
     M           GTUX  , GTVX  , 
     I           KMAX            )
      CALL G2W 
     M         ( WTT   ,
     I           GTXX  , '    ', 'ADDO', KMAX   )
*
*    << momentum eq. >>
*
      CALL GRUADV                      !!  momentum advection
     O         ( GTUX  , GTVX  , GTXX  ,
     O           GTUXN , GTVXN , 
     I           GAU   , GAV   , GAVOR , 
     I           GFU   , GFV   , GFVOR , 
     I           GAPIX , GAPIY , GSIGD , GATV  , GAT   ,
     I           GFPIX , GFPIY , GFSIGD, GFTV  ,
     C           CORIOL, 
     C           SALPHA, SBETA , SKAPPA, TBAR           )
*
      CALL       UV2VDW         !! (U,V) -> (vor.,D)spectral
     O        ( WTVOR , WTDIV ,
     I          GTUX  , GTVX  , .TRUE., .TRUE.,
     W          GSUMP , GAPADV                  )
      IF( .NOT. OSLNOID ) THEN  !! exclude solenoidal term
         CALL    UV2VDW      !! (U,V) -> (vor.,D)spectral
     O        ( WTVOR , WDX   ,
     I          GTUXN , GTVXN , .TRUE., .TRUE.,
     W          GSUMP , GAPADV                  )
      ENDIF
      CALL G2W 
     O        ( WTKE  ,
     I          GTXX  ,
     I          '    ', 'POSO', KMAX )
*
#ifdef OPT_CLASSIC
      CALL RESET( WTQ  , NMDIM*KMAX )
#else
*
*    << moisture eq. >>
*
      CALL GRQADV                      !!  adv. of humidity etc.
     O         ( GTUX  , GTVX  , GTXX  ,
     I           GAQ   , GFQ   ,
     I           GAU   , GAV   , GADIV , GSIGD , 
     I           GFU   , GFV   , GFDIV , GFSIGD    )
*
      CALL HCONVW
     O         ( WTQ   ,
     M           GTUX  , GTVX  , 
     I           KMAX            )
*
      CALL G2W
     M         ( WTQ   ,
     I           GTXX  , '    ', 'ADDO', KMAX )
*
      DO 4100 K = 1 , KMAX
         DO 4100 NM = 1 , NMDIM
            WTDIV ( NM,K ) =  WTDIV ( NM,K ) 
     &                     -  WTKE  ( NM,K ) * FLAPLA( NM )
 4100 CONTINUE
*
*    << sfc. flux and cumulus convection >>
*
      CALL W2G
     O     ( GAWT ,
     I       WTT  , '    ', 'POSO', KMAX )
      CALL W2G
     O     ( GAWQ ,
     I       WTQ  , '    ', 'POSO', KMAX )
      CALL UV2TQF               !! sfc. flux (Ta,Qa,Va -> Fh,Fq)
     M     ( GAWT , GAWQ ,
     I       GAU  , GAV  , GAT  , GAQ  , DELT  )
      CALL TQ2TQF               !! MCA (Ta,Qa -> Fh,Fq)
     M     ( GAWT , GAWQ ,
     I       GAT  , GAQ  , DELT                )
      CALL G2W
     O     ( WTT  ,
     I       GAWT , '    ', 'POSO', KMAX )
      CALL G2W
     O     ( WTQ  ,
     I       GAWQ , '    ', 'POSO', KMAX )
      CALL RESET( GTUX  , IJDIM*KMAX  )
      CALL RESET( GTVX  , IJDIM*KMAX  )
      CALL UV2UVF
     O     ( GTUX(1,1), GTVX(1,1),
     I       GAU  , GAV  , DELT  )
      CALL UV2VDW         !! (U,V) -> (vor.,D)spectral
     O     ( WDX   , WDY   ,
     I       GTUX  , GTVX  , .TRUE., .TRUE.,
     W       GSUMP , GAPADV                  )
      DO 4200 NM = 1 , NMDIM
         WTVOR ( NM,1 ) =  WTVOR ( NM,1 ) + WDX( NM,1 ) 
         WTDIV ( NM,1 ) =  WTDIV ( NM,1 ) + WDY( NM,1 ) 
 4200 CONTINUE
#endif
*
*    << linear damping in grid space >>
*
#ifdef OPT_GRDDMP
      CALL ADD_GRDDMP
     M     ( WTVOR , WTDIV , WTT   , WTQ   ,
     I       GAVOR , GADIV , GAT   , GAQ   , GDZS , DELT  )
#endif
*
      RETURN
      END
**********************************************************************
      SUBROUTINE WAVE2GRID      !! perturbation wave -> grid
     O        ( GAU   , GAV   , GAT   , GAPI  , GAQ   ,
     O          GAVOR , GADIV ,
     I          WAVOR , WADIV , WAT   , WAPI  , WAQ   ,
     W          WDX   , WDY                              )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* # of wave              */
#include        "zpdim.F"                /* physics etc.           */
#include        "zccom.F"                /* physical constants     */
#include        "zcord.F"                /* coordinate             */
#include        "zhdim.F"                /* # of char. in string */
*
*   [OUTPUT] 
      REAL*8     GAU ( IJDIM, KMAX       )  !! perturbation wind
      REAL*8     GAV ( IJDIM, KMAX       )  !! perturbation wind
      REAL*8     GAT ( IJDIM, KMAX       )  !! perturbation temperature 
      REAL*8     GAPI( IJDIM             )  !! perturbation pi
      REAL*8     GAQ ( IJDIM, KMAX       )  !! perturbation humidity
      REAL*8     GAVOR ( IJDIM, KMAX     )  !! vorticity
      REAL*8     GADIV ( IJDIM, KMAX     )  !! divergence D 
*
*   [INPUT] 
      REAL*8     WAVOR ( NMDIM, KMAX   )  !! vorticity
      REAL*8     WADIV ( NMDIM, KMAX   )  !! divergence D
      REAL*8     WAT   ( NMDIM, KMAX   )  !! temperature
      REAL*8     WAPI  ( NMDIM         )  !! logPs pi
      REAL*8     WAQ   ( NMDIM, KMAX   )  !! humidity
*
*   [WORK] 
      REAL*8     WDX   ( NMDIM, KMAX   )  !! work
      REAL*8     WDY   ( NMDIM, KMAX   )  !! work

      INTEGER    IFPAR, JFPAR
*
*   [INTERNAL PARAM.] 
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE.  /
*
      IF ( OFIRST ) THEN
         CALL   REWNML ( IFPAR , JFPAR )
         WRITE ( JFPAR,* ) 
     &        ' @@@ WAVE2GRID: PERTURBATION IN GRID 07/25/05'
         OFIRST = .FALSE.
      ENDIF
*
      CALL VDW2UV               !! (vor.,D)spectral -> (U,V)
     O     ( GAU   , GAV   ,
     I       WAVOR , WADIV , .TRUE. , .TRUE.  ,
     W       WDX   , WDY                        )
      CALL W2G 
     O     ( GAVOR ,
     I       WAVOR ,
     I       '     ', 'POSO', KMAX  )
      CALL W2G 
     O     ( GADIV ,
     I       WADIV ,
     I       '     ', 'POSO', KMAX  )
      CALL W2G 
     O     ( GAT   ,
     I       WAT   ,
     I       '     ', 'POSO', KMAX  )
      CALL W2G 
     O     ( GAPI  ,
     I       WAPI  ,
     I       '     ', 'POSO', 1    )
#ifdef OPT_CLASSIC
      CALL RESET( GAQ , IJDIM*KMAX )
#else
      CALL W2G 
     O     ( GAQ   ,
     I       WAQ   ,
     I       '     ', 'POSO', KMAX  )
#endif
*
      RETURN
      END

