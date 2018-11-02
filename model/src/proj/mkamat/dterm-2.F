* PACKAGE DCIRC !! dynamics: terms of eqs.
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/05/28(numaguti) restructure
*         00/04/10(hiro)     planetary wave model
*         01/06/15(hiro)     PWM incl. moisture tendency
*         GF* : basic state
*         GA* : perturbation
**********************************************************************
      SUBROUTINE DYNTRM    !! dynamics terms
     O         ( WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
     O           GSIGD , GPID  , GAVOR , GADIV , GATV  ,
     O           GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     M           GBU   , GBV   , GBT   , GBPS  , GBQ   ,
     I           DELT  ,
     I           GFU   , GFV   , GFT   , GFPS  , GFQ   ,
     I           GFPSM , SDTOP , SDBTM ,
     C           SALPHA, SBETA , SKAPPA, ATF   , BTF   , 
     C           TBAR  , TMTX  , CORIOL, FLAPLA,
     W           GTUX  , GTVX  , GTXX  , WTKE  ,
     I           OMATV , OMATD , OMATT , OMATP , OMATQ , 
     I           WTUNTT, WTUNTF, WTUNTP )
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
      REAL*8     GSIGD ( IJDIM, KMAX+1     )  !! tendency of sigma
      REAL*8     GPID  ( IJDIM             )  !! tendency of pai
      REAL*8     GAVOR ( IJDIM, KMAX       )  !! vorticity
      REAL*8     GADIV ( IJDIM, KMAX       )  !! divergence D 
      REAL*8     GATV  ( IJDIM, KMAX       )  !! virtulal tmp.
      REAL*8     GBU ( IJDIM, KMAX       )  !! perturbation wind
      REAL*8     GBV ( IJDIM, KMAX       )  !! perturbation wind
      REAL*8     GBT ( IJDIM, KMAX       )  !! perturbation temperature 
      REAL*8     GBPS( IJDIM             )  !! perturbation sfc. pressure
      REAL*8     GBQ ( IJDIM, KMAX       )  !! perturbation humidity
*
*   [INPUT] 
      REAL*8     GFU ( IJDIM, KMAX )        !! BS zonal wind
      REAL*8     GFV ( IJDIM, KMAX )        !! BS meridional wind
      REAL*8     GFT ( IJDIM, KMAX )        !! BS temperature
      REAL*8     GFPS( IJDIM )              !! BS surface pressure
      REAL*8     GFPSM ( IJDIM     )        !! ZMBS surface pressure
      REAL*8     GFQ ( IJDIM, KMAX       )  !! BS specific humidity
      REAL*8     GAU ( IJDIM, KMAX       )  !! perturbation wind
      REAL*8     GAV ( IJDIM, KMAX       )  !! perturbation wind
      REAL*8     GAT ( IJDIM, KMAX       )  !! perturbation temperature 
      REAL*8     GAPS( IJDIM             )  !! perturbation sfc. pressure
      REAL*8     GAQ ( IJDIM, KMAX       )  !! perturbation humidity
*
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
*
      REAL*8     WTUNTT( NMDIM, KMAX   )  !! unit matrix 
      REAL*8     WTUNTF( NMDIM, KMAX   )  !! zero matrix (dumy)
      REAL*8     WTUNTP( NMDIM         )  !! unit matrix 
      LOGICAL    OMATV, OMATD, OMATT, OMATP, OMATQ
*
*   [WORK] 
      REAL*8     GTUX  ( IJDIM, KMAX   )  !! tend. zonal  adv.
      REAL*8     GTVX  ( IJDIM, KMAX   )  !! tend. merid. adv.
      REAL*8     GTXX  ( IJDIM, KMAX   )  !! tend.
      REAL*8     GFTXX ( IJDIM, KMAX   )  !! tend.
      REAL*8     GAWT  ( IJDIM, KMAX   )  !! forcing for temp., f(W')
      REAL*8     GAWQ  ( IJDIM, KMAX   )  !! forcing for Q, f(W')
#ifndef OPT_CLASSIC
      REAL*8     GFST  ( IJDIM, KMAX   )  !! steady forcing for T, f(Ts)
      REAL*8     GFSQ  ( IJDIM, KMAX   )  !! steady forcing for Q, f(Ts)
#endif
      REAL*8     WTKE  ( NMDIM, KMAX   )  !! KE term
      REAL*8     WDX   ( NMDIM, KMAX   )  !! work
      REAL*8     WDY   ( NMDIM, KMAX   )  !! work
*
*   [INTERNAL WORK]
      REAL*8     GAPADV( IJDIM, KMAX )   !! advection of pai
      REAL*8     GSUMP ( IJDIM, KMAX )   !! vert. integ. of tend. of pi
      REAL*8     GAPIX ( IJDIM       )   !! zonal deriv. of pi
      REAL*8     GAPIY ( IJDIM       )   !! merid. differ. of pi
      INTEGER    IJ, K, L, NM, IFPAR, JFPAR
#ifdef OPT_ADDCPL
      REAL*8     GTAUX ( IJDIM       )   !! zonal stress
      REAL*8     GTAUY ( IJDIM       )   !! meridional stress
      REAL*8     GPREC ( IJDIM       )   !! precipitation
#endif
*
*   [INTERNAL SAVE] !! variables for basic state
      REAL*8     GFVOR( IJDIM, KMAX )    !! BS for VOR
      REAL*8     GFDIV( IJDIM, KMAX )    !! BS for DIV (output)
      REAL*8     GFSIGD( IJDIM, KMAX+1)  !! tend. of sigma (output)
      REAL*8     GFPID ( IJDIM        )  !! tend. of pai
      REAL*8     GFTV  ( IJDIM, KMAX  )  !! virtulal tmp.
      REAL*8     GFPADV( IJDIM, KMAX )   !! advection of pai 
      REAL*8     GFSUMP( IJDIM, KMAX )   !! vert. integ. tend. pi
      REAL*8     GFPIX ( IJDIM       )   !! zonal deriv. of pi
      REAL*8     GFPIY ( IJDIM       )   !! merid. differ. of pi 

      REAL*8     WSVOR ( NMDIM, KMAX      )  !! vorticity forcing
      REAL*8     WSDIV ( NMDIM, KMAX      )  !! divergence forcing
      REAL*8     WST   ( NMDIM, KMAX      )  !! temperature forcing
      REAL*8     WSPI  ( NMDIM            )  !! logPs forcing
      REAL*8     WSQ   ( NMDIM, KMAX      )  !! humidity forcing
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      REAL*8     UVFACT( IJDIM  )        !! factor of u -> U
*
      SAVE       GFVOR, GFDIV
      SAVE       GFSIGD, GFPID, GFTV, GFPADV, GFSUMP, GFPIX, GFPIY
      SAVE       NMO, UVFACT 
*
*   [INTERNAL PARAM.] 
*
      REAL*8     FACT(5)        !! factor for perturbations
      CHARACTER  WVFRC *(NFILN) !! file name for perturbation
      CHARACTER  FSFRC *(NFILN) !! file name for steady forcing
      CHARACTER  GSFRC *(NFILN) !! file name for total forcing
      INTEGER        NFRC       !! assigned no. of perturbation
      INTEGER        NSFRC      !! assigned no. of forcing
      LOGICAL    OEP            !! only calculate EP flux?
      LOGICAL    OFRC           !! apply steady forcing (for moist)
      LOGICAL    OFIRST
      NAMELIST  /NMWFRC/ WVFRC, NFRC, FACT, OEP
      NAMELIST  /NMSFRC/ FSFRC, GSFRC, NSFRC
      DATA       WVFRC  / '     ' /
      DATA       FSFRC  / '     ' /
      DATA       GSFRC  / '     ' /
      DATA       FACT   / 1.D0,1.D0,1.D0,1.D0,1.D0  /
      DATA       NFRC   / 1       /
      DATA       NSFRC / 1       /
      DATA       OFIRST / .TRUE.  /
      DATA       OEP    / .FALSE. /
      DATA       OFRC   / .FALSE. /

      IF ( OFIRST ) THEN
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMWFRC, END=480 )
 480     WRITE  ( JFPAR, NMWFRC)
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMSFRC, END=490 )
 490     WRITE  ( JFPAR, NMSFRC)
         WRITE ( JFPAR,* ) ' @@@ DTERM: DYNAMICS EQ. TERMS 98/10/17'
         CALL DSETNM
     O         ( NMO    )
         CALL   DSETUV
     O        ( UVFACT )

#ifdef OPT_WVFRC
         CALL MTRUNC( GFU , KMAX, WTKE )
         CALL MTRUNC( GFV , KMAX, WTKE )
         CALL MTRUNC( GFT , KMAX, WTKE )
         CALL MTRUNC( GFPS, 1   , WSPI )
         CALL MTRUNC( GFQ , KMAX, WTKE )
#endif
*
*     calculation for basic state
*
         CALL UV2VDG
     O        ( GFVOR , GFDIV ,
     I        GFU   , GFV   , .TRUE., .TRUE.,
     W        GTUX  , GTVX  , WTVOR , WTDIV  )
*
         DO 1100 IJ = 1, IJDIM
            IF( GFPSM(IJ) .EQ. 0.D0 ) THEN
               GFTXX( IJ,1 )  = LOG( GFPS( IJ ) )
            ELSE
               GFTXX( IJ,1) = 
     &              LOG( GFPS(IJ)+GFPSM(IJ) ) - LOG( GFPSM(IJ) )
            ENDIF
 1100    CONTINUE 
*
         CALL HGRAD             !! pi gradient
     O        ( GFPIX , GFPIY ,
     I        GFTXX   , 1     ,
     W        WTKE           )
*
         CALL PSDOTB            !!  surface pres. change & sigma velocity
     O         ( GFSIGD, GFSUMP, GFPADV, GFPID ,
     I           GFU   , GFV   , GFDIV , GFPIX , GFPIY  )
*
         CALL VIRTMB( GFTV , GFT , GFQ  )
*
         CALL RESET( WSVOR, NMDIM*KMAX )
         CALL RESET( WSDIV, NMDIM*KMAX )
         CALL RESET( WST  , NMDIM*KMAX )
         CALL RESET( WSPI , NMDIM )
         CALL RESET( WSQ  , NMDIM*KMAX )

         CALL RESET( GAWT, IJDIM*KMAX )
         CALL RESET( GAWQ, IJDIM*KMAX )
         OFIRST = .FALSE.
      ENDIF
*
      CALL RESET( GAVOR, IJDIM*KMAX )
      CALL RESET( GADIV, IJDIM*KMAX )
*
#ifdef OPT_MKMAT
*
*     make perturbation matrices
*
      CALL RESET( GAU  , IJDIM*KMAX )
      CALL RESET( GAV  , IJDIM*KMAX )
      CALL RESET( GAT  , IJDIM*KMAX )
      CALL RESET( GAPS , IJDIM )
      CALL RESET( GAQ  , IJDIM*KMAX )
*
*     wave to grid
*
      IF( OMATV ) THEN
         CALL VDW2UV            !! (vor.,D) -> (U,V)
     O        ( GAU   , GAV   ,
     I        WTUNTT , WTUNTF , .TRUE., .FALSE.,
     W        WDX   , WDY                     )
      ELSE IF ( OMATD ) THEN
         CALL VDW2UV            !! (vor.,D) -> (U,V)
     O        ( GAU   , GAV   ,
     I        WTUNTF , WTUNTT , .FALSE., .TRUE.,
     W        WDX   , WDY                     )
      ENDIF
      
      IF( OMATT ) THEN
         CALL W2G
     O        ( GAT   ,
     I        WTUNTT  , '    ', 'POSO', KMAX )
      ENDIF
      
      IF( OMATP ) THEN
         CALL W2G
     O        ( GAPS  ,
     I        WTUNTP  , '    ', 'POSO', 1    )
      ENDIF

      IF( OMATQ ) THEN
         CALL W2G
     O        ( GAQ   ,
     I        WTUNTT  , '    ', 'POSO', KMAX )
      ENDIF
#endif
*
#ifdef OPT_WVFRC
*
#ifndef OPT_CLASSIC
*
*     read steady forcing
*
      CALL RESET( GAU , IJDIM*KMAX )
      CALL RESET( GAV , IJDIM*KMAX )
      CALL RESET( GFST, IJDIM*KMAX )
      CALL RESET( GFSQ, IJDIM*KMAX )
      CALL RDSFRC
     O     ( GAVOR  , GADIV  , GAT  , GAPS  , GAQ  , OFRC ,
     I       FSFRC  , NSFRC, DELT    )
      IF ( OFRC ) THEN
         CALL G2W
     O        ( WTVOR,
     I          GAVOR,
     I          '    ', 'POSO',  KMAX  )
         CALL G2W
     O        ( WTDIV,
     I          GADIV,
     I          '    ', 'POSO',  KMAX  )
         CALL G2W
     O        ( WTT  ,
     I          GAT  ,
     I          '    ', 'POSO',  KMAX  )
         CALL G2W
     O        ( WTPI ,
     I          GAPS ,
     I          '    ', 'POSO',  1     )
         CALL G2W
     O        ( WTQ  ,
     I          GAQ  ,
     I          '    ', 'POSO',  KMAX  )
         CALL COPY  ( GFST , GAT ,  IJDIM*KMAX )
         CALL COPY  ( GFSQ , GAQ ,  IJDIM*KMAX )
         CALL RESET ( GAT  , IJDIM*KMAX )
         CALL RESET ( GAPS , IJDIM      )
         CALL RESET ( GAQ  , IJDIM*KMAX )
      ENDIF
#endif
*
*     read perturbations
*
      CALL RDWFRC
     O     ( GAU  , GAV  , GAT  , GAPS  , GAQ  ,
     I       WVFRC, NFRC, FACT    )
      CALL MTRUNC( GAU , KMAX, WTKE )
      CALL MTRUNC( GAV , KMAX, WTKE )
      CALL MTRUNC( GAT , KMAX, WTKE )
      CALL MTRUNC( GAPS, 1   , WSPI )
      CALL MTRUNC( GAQ , KMAX, WTKE )
*
#endif
*
      CALL COPY( GBU , GAU ,  IJDIM*KMAX )
      CALL COPY( GBV , GAV ,  IJDIM*KMAX )
      CALL COPY( GBT , GAT ,  IJDIM*KMAX )
      CALL COPY( GBPS, GAPS,  IJDIM )
      CALL COPY( GBQ , GAQ ,  IJDIM*KMAX )
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
      CALL WRSFRC
     I     ( GAWT,  GSFRC, 'GAWT', IDIM,  JDIM, KMAX )
      CALL WRSFRC
     I     ( GAWQ,  GSFRC, 'GAWQ', IDIM,  JDIM, KMAX )
      CALL WRSFRC
     I     ( GFST,  GSFRC, 'GFST', IDIM,  JDIM, KMAX )
      CALL WRSFRC
     I     ( GFSQ,  GSFRC, 'GFSQ', IDIM,  JDIM, KMAX )
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
      CALL WRSFRC
     I     ( GTAUX ,  GSFRC, 'TAUX', IDIM,  JDIM, 1   )
      CALL WRSFRC
     I     ( GTAUY ,  GSFRC, 'TAUY', IDIM,  JDIM, 1   )
      CALL WRSFRC
     I     ( GPREC ,  GSFRC, 'PREC', IDIM,  JDIM, 1   )
#endif
*
#endif
#endif
*
      IF ( OFRC ) RETURN
*
*    << circulations >>
*
      CALL UV2VDG               !! UV-->VOR.DIV.
     O     ( GAVOR , GADIV ,
     I     GAU   , GAV   , .TRUE., .TRUE.,
     W     GTUX  , GTVX  , WTVOR , WTDIV  )
*
      DO 1200 IJ = 1, IJDIM
         GTXX( IJ,1 ) = GAPS(IJ)
 1200 CONTINUE 
      CALL HGRAD                       !! perturbation pi gradient
     O         ( GAPIX , GAPIY ,
     I           GTXX , 1     ,
     W           WTKE           )
*
      CALL PSDOT     !!  surface pres. change & sigma velocity
     O         ( GSIGD , GSUMP , GAPADV, GPID  ,
     I           GAU   , GAV   , GADIV , GAPIX , GAPIY , 
     I           GFU   , GFV   , GFPIX , GFPIY  )
*
*    << pressure eq. >>
*
      CALL COPY ( GTXX, GPID, IJDIM )
CC#ifndef OPT_ORHS !! comment out on 01/19/03
      DO 2100 IJ = 1, IJDIM
         DO 2200 K = 1, KMAX
            GTXX( IJ,1 )  = GTXX( IJ,1 )  + DSIG( K )*GADIV( IJ,K )
 2200    CONTINUE
 2100 CONTINUE 
CC#endif
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
     I           OEP   , 
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
      IF( OEP ) THEN
         CALL RESET( GTXX, IJDIM*KMAX)
      ENDIF
      CALL G2W 
     M         ( WTT   ,
     I           GTXX  , '    ', 'ADDO', KMAX   )
*
*    << momentum eq. >>
*
      CALL GRUADV                      !!  momentum advection
     O         ( GTUX  , GTVX  , GTXX  ,
     I           GAU   , GAV   , GAVOR , 
     I           GFU   , GFV   , GFVOR , 
     I           GAPIX , GAPIY , GSIGD , GATV  , GAT   ,
     I           GFPIX , GFPIY , GFSIGD , GFTV ,
     I           OEP   ,
     C           CORIOL, 
     C           SALPHA, SBETA , SKAPPA, TBAR           )
*
      IF( OEP ) THEN
         DO 3300 K = 1, KMAX
            DO 3300 IJ = 1, IJDIM
               GTXX( IJ, K) = GTUX( IJ, K) * UVFACT( IJ )
 3300    CONTINUE
         CALL G2W
     O         ( WTVOR ,
     I           GTXX  ,
     I           'YGRA', 'POS ', KMAX )
         DO 3400 K = 1, KMAX
            DO 3400 IJ = 1, IJDIM
               GTUX( IJ, K) = -2.D0 * GAV( IJ, K) * GFV( IJ, K) 
 3400    CONTINUE
         CALL G2W
     O         ( WTVOR ,
     I           GTUX  ,
     I           'XGRA', 'ADD ', KMAX )
         CALL W2G 
     O         ( GTUX  ,
     I           WTVOR  ,
     I           '     ', 'POSO', KMAX )
         DO 3500 K = 1, KMAX
            DO 3500 IJ = 1, IJDIM
               GTUX( IJ, K) = GTUX( IJ, K) * UVFACT( IJ )
 3500    CONTINUE
         CALL G2W 
     O         ( WTVOR  ,
     I           GTUX  ,
     I           'YGRA ', 'POSO', KMAX )
*
         CALL G2W
     O         ( WTDIV ,
     I           GTXX  ,
     I           'XGRA', 'NEG ', KMAX )
         DO 3600 K = 1, KMAX
            DO 3600 IJ = 1, IJDIM
               GTVX( IJ, K) = 2.D0 * GAU( IJ, K) * GFU( IJ, K) 
 3600    CONTINUE
         CALL G2W
     O         ( WTDIV ,
     I           GTVX  ,
     I           'YGRA', 'ADD ', KMAX )
         CALL W2G 
     O         ( GTVX  ,
     I           WTDIV  ,
     I           '     ', 'POSO', KMAX )
         DO 3700 K = 1, KMAX
            DO 3700 IJ = 1, IJDIM
               GTVX( IJ, K) = GTVX( IJ, K) * UVFACT( IJ )
 3700    CONTINUE
         CALL G2W 
     O         ( WTDIV  ,
     I           GTVX  ,
     I           'YGRA ', 'POSO', KMAX )
      ELSE
         CALL       UV2VDW      !! (U,V) -> (vor.,D)spectral
     O        ( WTVOR , WTDIV ,
     I          GTUX  , GTVX  , .TRUE., .TRUE.,
     W          GSUMP , GAPADV                  )
         CALL G2W 
     O        ( WTKE  ,
     I          GTXX  ,
     I          '    ', 'POSO', KMAX )
      ENDIF
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
#ifndef OPT_CLASSIC
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
     W     GSUMP , GAPADV                  )
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
     I       GAVOR , GADIV , GAT   , GAQ   , DELT  )
#endif
*
      RETURN
      END
***********************************************************************
      SUBROUTINE MTRUNC    !! truncate at m=MTR
     M         ( GX      ,
     I           KDIMD   ,
     W           WX         )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
#include        "zcord.F"                /* coordinate             */
*
*   [INPUT] 
      INTEGER    KDIMD
*
*   [MODIFY] 
      REAL*8     GX  ( IJDIM, KDIMD ) !! grid value
*
*   [WORK] 
      REAL*8     WX  ( NMDIM, KDIMD )       !! wave
*
*   [INTERNAL WORK] 
      INTEGER    L, M, LEND, K
      INTEGER    IFPAR, JFPAR
*
*   [INTERNAL SAVE] 
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      SAVE       NMO
*
*   [INTERNAL PARM] 
      LOGICAL    OTRUN  
      INTEGER    MTRN
      LOGICAL    OFIRST
      NAMELIST  /NMMTRN/ OTRUN, MTRN
      DATA       OTRUN  / .FALSE. /
      DATA       MTRN   / MMAX / 
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMMTRN, END=290 )
  290    WRITE  ( JFPAR, NMMTRN )
*
         IF( OTRUN ) 
     &        WRITE ( JFPAR,* )
     $        ' @@@ MTRUNC: TRUNCATION AT ZONAL WAV #:',MTRN
         CALL DSETNM
     O         ( NMO   )
      ENDIF
*
      IF( .NOT. OTRUN ) RETURN
*
      CALL G2W
     O     ( WX    ,
     I       GX    ,
     I       '    ', 'POSO', KDIMD )
*
      DO 1000 K = 1, KDIMD
         DO 1100 M = MTRN+1 , MMAX, MINT
            LEND = MIN( LMAX, NMAX-M )
            DO 1110 L = 0 , LEND
               WX( NMO(1,M,L),K )  = 0.D0
               WX( NMO(2,M,L),K )  = 0.D0
 1110       CONTINUE
 1100    CONTINUE
 1000 CONTINUE 
*
      CALL W2G 
     O     ( GX    ,
     I       WX    ,
     I       '    ', 'POSO', KDIMD )
*
      RETURN
      END
***********************************************************************
      SUBROUTINE MTRUN0    !! truncate at m=0 (zonal mean)
     M         ( GX    ,
     I           KDIMD          )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zcord.F"                /* coordinate             */
*
*
*   [INPUT] 
      INTEGER    KDIMD
*
*   [MODIFY] 
      REAL*8     GX  ( IDIM,JDIM,KDIMD ) !! grid value
*
*   [INTERNAL WORK] 
      INTEGER    I, J, K
      INTEGER    IFPAR, JFPAR
      REAL*8     GXM
*
*   [INTERNAL PARM] 
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         WRITE ( JFPAR,* )
     $        ' @@@ MTRUN0: SET ZONAL MEAN 04/05/31 '
      ENDIF
*
      DO 1000 K = 1, KDIMD
         DO 1100 J = 1, JDIM
            GXM = 0.D0
            DO 1200 I = 1, IDIM
               GXM = GXM + GX( I,J,K ) / DBLE( IDIM )
 1200       CONTINUE
            DO 1300 I = 1, IDIM
               GX( I,J,K ) = GXM
 1300       CONTINUE
 1100    CONTINUE
 1000 CONTINUE 
*
*
      RETURN
      END
***********************************************************************
      SUBROUTINE VIRTMB    !! basic state virtual temperature
     O         ( GFTV  ,
     I           GFT   , GFQ     )
*
*   [PARAM] 
#include        "zcdim.F"
#include        "zpdim.F"                /* physics etc. */
#include        "zccom.F"
*
*   [OUTPUT]
      REAL*8     GFTV  ( IJDIM, KMAX )        !! virtual temperature
*
*   [INPUT]
      REAL*8     GFT   ( IJDIM, KMAX       )  !! temperature T
      REAL*8     GFQ   ( IJDIM, KMAX       )  !! humidity 
*
*   [ENTRY OUTPUT]
      REAL*8     GATV  ( IJDIM, KMAX )        !! virtual temperature
*
*   [ENTRY INPUT]
      REAL*8     GAT   ( IJDIM, KMAX       )  !! temperature T
      REAL*8     GAQ   ( IJDIM, KMAX       )  !! humidity 
*
*   [INTERNAL WORK]
      INTEGER    IJ, K
*
*   [INTERNAL SAVE] 
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ VIRTMB: VERTUAL TEMP. 01/06/15'
         OFIRST = .FALSE.
      ENDIF
*
      DO 1100 K = 1, KMAX
         DO 1100 IJ = 1, IJDIM
            GFTV( IJ,K ) = GFT( IJ,K )
     &                   * ( 1.+ EPSVT*GFQ( IJ,K ) )
 1100 CONTINUE
*
      RETURN
*=======================================================================
      ENTRY VIRTMP         !! perturbation virtual temperature
     O         ( GATV  ,
     I           GFT   , GFQ    , 
     I           GAT   , GAQ     )
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ VIRTMP: VERTUAL TEMP. 01/06/15'
         OFIRST = .FALSE.
      ENDIF
*
      DO 1200 K = 1, KMAX
         DO 1200 IJ = 1, IJDIM
            GATV( IJ,K ) = GAT( IJ,K ) + EPSVT * 
     &                   ( GAT( IJ,K ) * GFQ( IJ,K ) 
     &                   + GFT( IJ,K ) * GAQ( IJ,K ) )
 1200 CONTINUE
*
      RETURN
      END
**********************************************************************
      SUBROUTINE RDSFRC    !!  get steady forcing
     O         ( GSFV  , GSFD  , GSFT  , GSFP  , GSFQ  , OFRC ,
     I           FSFRC , NSFRC, DELT    )
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* # of wave              */
#include        "zpdim.F"                /* physics etc.           */
#include        "zccom.F"                /* physical constants     */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT]
      REAL*8     GSFV ( IJDIM, KMAX )    !! steady forcing for vor.
      REAL*8     GSFD ( IJDIM, KMAX )    !! steady forcing for div.
      REAL*8     GSFT ( IJDIM, KMAX )    !! steady forcing for temp.
      REAL*8     GSFP ( IJDIM       )    !! steady forcing for LnPs
      REAL*8     GSFQ ( IJDIM, KMAX )    !! steady forcing for humidity
      LOGICAL    OFRC                    !! apply steady forcing
*
*   [INPUT]
      CHARACTER  FSFRC * 70              !! filename steady forcing
      REAL*8     DELT                    !! time step
      INTEGER  NSFRC            !! assigned no. of forcing
*
*   [WORK]
      REAL*8     GSST ( IJDIM )          !! steady SST forcing
      REAL*8     GWG  ( IJDIM )          !! steady soil wetness forcing
      REAL*4     X     ( IMAX, JMAX    ) !! temporary for forcing data
      INTEGER    IJ, I, J, N
*
*
      IF( FSFRC(1:1) .EQ. ' ' ) THEN
         WRITE( 6, *) '    ### NO STEADY FORCING APPLIED ### '
         OFRC = .FALSE. 
         RETURN
      ELSE
         OPEN( 88, FILE = FSFRC, FORM='UNFORMATTED',
     &        STATUS='UNKNOWN' ) 
         WRITE( 6, *) '    $$$ STEADY FORCING FILE:', FSFRC,' $$$'
         CALL RESET ( GSFV , IJDIM*KMAX )
         CALL RESET ( GSFD , IJDIM*KMAX )
         CALL RESET ( GSFT , IJDIM*KMAX )
         CALL RESET ( GSFP , IJDIM      )
         CALL RESET ( GSFQ , IJDIM*KMAX )
*     
*     read forcing data
*
         DO 100 N = 1, NSFRC-1
            READ( 88 ) X
            READ( 88 ) X
 100     CONTINUE
*
         READ( 88 ) X           !! SST anomaly
         IJ = 0
         DO 310 J = 1, JMAX
            DO 320 I = 1, IMAX
               IJ = IJ + 1
               GSST( IJ ) = DBLE( X( I, J) )
 320        CONTINUE
            IJ = IJ + 1
            GSST( IJ ) = DBLE( X( 1, J) )
 310     CONTINUE
         write(6,*) 'sst anom.'
*     
         READ( 88 ) X           !! soil wetness
         IJ = 0
         DO 330 J = 1, JMAX
            DO 340 I = 1, IMAX
               IJ = IJ + 1
               GWG( IJ ) = DBLE( X( I, J) )
 340        CONTINUE
            IJ = IJ + 1
            GWG( IJ ) = DBLE( X( 1, J) )
 330     CONTINUE
         write(6,*) 'soil wetness anom.'
*     
         CALL TS2TQF
     O        ( GSFT , GSFQ ,
     I          GSST , DELT    )
*
         CALL WG2TQF
     M        ( GSFT , GSFQ ,
     I          GWG  , DELT    )
*
         CLOSE( 88 )
         OFRC = .TRUE. 
      ENDIF


      RETURN
      END
**********************************************************************
      SUBROUTINE RDWFRC    !!  get perturbations
     O         ( GDU  , GDV  , GDT  , GDPS , GDQ  ,
     I           WVFRC, NFRC, FACT   )
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* # of wave              */
#include        "zpdim.F"                /* physics etc.           */
#include        "zccom.F"                /* physical constants     */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT]
      REAL*8     GDU ( IJDIM, KMAX )    !! perturbation for u
      REAL*8     GDV ( IJDIM, KMAX )    !! perturbation for v
      REAL*8     GDT ( IJDIM, KMAX )    !! perturbation for T
      REAL*8     GDPS( IJDIM       )    !! perturbation for Ps
      REAL*8     GDQ ( IJDIM, KMAX )    !! perturbation for q
*
*   [INPUT]
      CHARACTER  WVFRC * 70             !! perturbation filename
      REAL*8     FACT( 5 )              !! factor
      INTEGER NFRC              !! assigned no. of perturbation
*
*   [WORK]
      REAL*4     DATG( IMAX, JDIM )
      INTEGER    K, IJ, I, J, N
*
*
      CALL RESET ( GDU , IJDIM*KMAX )
      CALL RESET ( GDV , IJDIM*KMAX )
      CALL RESET ( GDT , IJDIM*KMAX )
      CALL RESET ( GDPS, IJDIM      )
      CALL RESET ( GDQ , IJDIM*KMAX )
*
      IF( WVFRC(1:1) .EQ. ' ' ) THEN
         WRITE( 6, *) '    ### NO PERTURBATION SPECIFIED ###'
         RETURN
      ELSE
         OPEN( 79, FILE = WVFRC, FORM='UNFORMATTED',
     &        STATUS='UNKNOWN' ) 
         WRITE( 6, *) '    $$$ PERTURBATIONG FILE:', WVFRC,' $$$'
*     
*     read perturbation data
*
         DO 70 N = 1, NFRC-1
#ifdef OPT_CLASSIC
            DO 80 K = 1, KMAX*4+1
               READ( 79 ) DATG
 80         CONTINUE
#else
            DO 90 K = 1, KMAX*5+1
               READ( 79 ) DATG
 90         CONTINUE
#endif
 70      CONTINUE
*
         DO 100 K = 1, KMAX
            READ( 79 ) DATG
            IJ = 0
            DO 110 J = 1, JDIM
               DO 120 I = 1, IMAX
                  IJ = IJ + 1
                  GDU( IJ, K) = FACT(1) * DBLE( DATG( I, J) )
 120           CONTINUE
               IJ = IJ + 1
               GDU( IJ, K) = GDU( IJ-IMAX, K)
 110        CONTINUE
 100     CONTINUE
*
         DO 200 K = 1, KMAX
            READ( 79 ) DATG
            IJ = 0
            DO 210 J = 1, JDIM
               DO 220 I = 1, IMAX
                  IJ = IJ + 1
                  GDV( IJ, K) = FACT(2) * DBLE( DATG( I, J) )
 220           CONTINUE
               IJ = IJ + 1
               GDV( IJ, K) = GDV( IJ-IMAX, K)
 210        CONTINUE
 200     CONTINUE
*
         DO 300 K = 1, KMAX
            READ( 79 ) DATG
            IJ = 0
            DO 310 J = 1, JDIM
               DO 320 I = 1, IMAX
                  IJ = IJ + 1
                  GDT( IJ, K) = FACT(3) * DBLE( DATG( I, J) )
 320           CONTINUE
               IJ = IJ + 1
               GDT( IJ, K) = GDT( IJ-IMAX, K)
 310        CONTINUE
 300     CONTINUE
*
         DO 330 K = 1, KMAX
            READ( 79 ) DATG     !! dummy for geopotential
 330     CONTINUE
*
         READ( 79 ) DATG
         IJ = 0
         DO 400 J = 1, JDIM
            DO 410 I = 1, IMAX
               IJ = IJ + 1
               GDPS( IJ ) = FACT(4) * DBLE( DATG( I, J) )
 410        CONTINUE
            IJ = IJ + 1
            GDPS( IJ ) = GDPS( IJ-IMAX )
 400     CONTINUE
*
#ifndef OPT_CLASSIC
         DO 500 K = 1, KMAX
            READ( 79 ) DATG
            IJ = 0
            DO 510 J = 1, JDIM
               DO 520 I = 1, IMAX
                  IJ = IJ + 1
                  GDQ( IJ, K) = FACT(5) * DBLE( DATG( I, J) )
 520           CONTINUE
               IJ = IJ + 1
               GDQ( IJ, K) = GDQ( IJ-IMAX, K)
 510        CONTINUE
 500     CONTINUE
#endif
         CLOSE( 79 )
      ENDIF
*
      RETURN
      END
**********************************************************************
      SUBROUTINE SPECSM    !!  remove highest components
     M         ( GX  ,  KMAXD ,
     I           LCUT,
     W           WX              )
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* # of wave              */
*
*   [INPUT]
      INTEGER    KMAXD
      INTEGER    LCUT
      CHARACTER  CSMD*(*)
*
*   [MODIFY]
      REAL*8     GX ( IDIM,JDIM,KMAXD )    !!
*
*   [SAVE]
      INTEGER    NMO   ( 2, 0:NMAX, 0:NMAX ) !! order of spect. suffix
      SAVE       NMO
*
*   [INTERNAL WORK]
      REAL*8     GY ( IDIM,JDIM,KMAXD )    !!
      REAL*8     WX( NMDIM, KMAXD)
      REAL*8     W0, W1
      INTEGER    I, J, K, L, M, LEND
      INTEGER    I1, I2, J1, J2
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /

      IF( OFIRST ) THEN
         OFIRST = .FALSE.
         CALL DSETNM
     O         ( NMO   )
      ENDIF
*
      CALL G2W
     O     ( WX  ,
     I       GX  , '    ', 'POS ', KMAXD )
*
      DO 1000 K = 1, KMAXD
         DO 1000 M = 0 , MMAX, MINT
            LEND = MIN( LMAX, NMAX-M )
            DO 1100 L = 0 , LEND
               IF ( L .GT. LCUT ) THEN
                  WX( NMO(1,M,L),K )  = 0.D0
                  WX( NMO(2,M,L),K )  = 0.D0
               ENDIF
 1100       CONTINUE
 1000 CONTINUE
*     
      CALL W2G
     O     ( GX  ,
     I       WX  , '    ', 'POSO', KMAXD )
*
      RETURN
*=======================================================================
      ENTRY GRIDSM         !! smoothing on grid
     O         ( GX  , KMAXD , CSMD ,
     W           GY                   )

      CALL COPY( GY , GX , IDIM*JDIM*KMAXD)
      W0 = 0.7D0
      W1 = ( 1.D0 - W0 ) / 2.D0
      DO 1500 K = 1, KMAXD
         IF( CSMD(1:1) .EQ. 'X' ) THEN
            DO 1600 J = 1, JDIM
               DO 1700 I = 1, IDIM
                  I1 = I - 1
                  I2 = I + 1
                  IF( I .EQ. 1    ) I1 = IDIM
                  IF( I .EQ. IDIM ) I2 = 1
                  GX( I,J,K ) = W0 * GY( I,J,K )
     &                        + W1 * GY( I1,J,K ) + W1 * GY( I2,J,K ) 
 1700          CONTINUE
 1600       CONTINUE
         ELSEIF( CSMD(1:1) .EQ. 'Y' ) THEN
            DO 1800 I = 1, IDIM
               DO 1900 J = 1, JDIM
                  J1 = J - 1
                  J2 = J + 1
                  IF( J .EQ. 1    ) J1 = J
                  IF( J .EQ. JDIM ) J2 = J
                  GX( I,J,K ) = W0 * GY( I,J,K )
     &                        + W1 * GY( I,J1,K ) + W1 * GY( I,J2,K ) 
 1900          CONTINUE
 1800       CONTINUE
         ENDIF
 1500 CONTINUE

      RETURN
      END
