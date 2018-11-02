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
      SUBROUTINE DYNTRM_AIM_FULL    !! dynamics terms with full GF*
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
      SAVE       GFSIGD, GFPID, GFTV, GFPADV, GFSUMP
      SAVE       GFPIX, GFPIY
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
      SUBROUTINE  DIAGW               !! diagnose omega
     O     ( GAW  ,
     I       GAU  , GAV  , GADIV , GAPS , 
     I       GFU  , GFV  ,         GFPS   )
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
      REAL*8     GAW ( IJDIM, KMAX      )  !! perturbation omega
*
*   [INPUT] 
      REAL*8     GFU ( IJDIM, KMAX       )  !! BS zonal wind
      REAL*8     GFV ( IJDIM, KMAX       )  !! BS meridional wind
      REAL*8     GFPS( IJDIM             )  !! BS surface pressure

      REAL*8     GAU ( IJDIM, KMAX       )  !! perturbation wind
      REAL*8     GAV ( IJDIM, KMAX       )  !! perturbation wind
      REAL*8     GADIV ( IJDIM, KMAX     )  !! divergence D 
      REAL*8     GAPS( IJDIM             )  !! perturbation sfc. pressure
*
*   [WORK] 
      REAL*8     GFVOR ( IJDIM, KMAX     )  !! BS vorticity 
      REAL*8     GFDIV ( IJDIM, KMAX     )  !! BS divergence D 

      REAL*8     GFSIGD( IJDIM, KMAX+1)  !! tend. of sigma (output)
      REAL*8     GFPID ( IJDIM       )   !! tend. of pai
      REAL*8     GFPADV( IJDIM, KMAX )   !! advection of pai 
      REAL*8     GFSUMP( IJDIM, KMAX )   !! vert. integ. tend. pi
      REAL*8     GFPI  ( IJDIM       )   !! ln Ps
      REAL*8     GFPIX ( IJDIM       )   !! zonal deriv. of pi
      REAL*8     GFPIY ( IJDIM       )   !! merid. differ. of pi 

      REAL*8     GSIGD ( IJDIM, KMAX+1)  !! tendency of sigma
      REAL*8     GPID  ( IJDIM       )   !! tendency of pai
      REAL*8     GAPADV( IJDIM, KMAX )   !! advection of pai
      REAL*8     GSUMP ( IJDIM, KMAX )   !! vert. integ. of tend. of pi
      REAL*8     GAPI  ( IJDIM       )   !! ln Ps
      REAL*8     GAPIX ( IJDIM       )   !! zonal deriv. of pi
      REAL*8     GAPIY ( IJDIM       )   !! merid. differ. of pi

      REAL*8     GTUX  ( IJDIM, KMAX   )  !! work
      REAL*8     GTVX  ( IJDIM, KMAX   )  !! work
      REAL*8     WDX   ( NMDIM, KMAX   )  !! work
      REAL*8     WDY   ( NMDIM, KMAX   )  !! work
      REAL*8     WDS   ( NMDIM         )  !! work

      REAL*8     FACA, PADVA, PADVF
      INTEGER    IFPAR, JFPAR
      INTEGER    IJ, K
*
*   [INTERNAL PARAM.] 
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE.  /

      IF ( OFIRST ) THEN
         CALL   REWNML ( IFPAR , JFPAR )
         WRITE ( JFPAR,* ) ' @@@ DIAGW: DIAGNOSIS OF OMEGA 05/05/30'
         OFIRST = .FALSE.
      ENDIF
*
      CALL UV2VDG
     O      ( GFVOR , GFDIV ,
     I        GFU   , GFV   , .FALSE., .TRUE.,
     W        GTUX  , GTVX  , WDX    , WDY     )
*
      DO 1100 IJ = 1, IJDIM
         GFPI( IJ ) = LOG( GFPS( IJ ) )
         GAPI( IJ ) = LOG( GFPS( IJ ) + GAPS( IJ ) ) - GFPI( IJ )
 1100 CONTINUE

      CALL HGRAD                !! pi gradient
     O        ( GFPIX , GFPIY ,
     I          GFPI  , 1     ,
     W          WDS            )
*
      CALL PSDOTB               !!  surface pres. change & sigma velocity
     O         ( GFSIGD, GFSUMP, GFPADV, GFPID ,
     I           GFU   , GFV   , GFDIV , GFPIX , GFPIY  )
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
*    << diagnostic for omega >>
*
      FACA = SIG( KMAX ) - SIGM( KMAX+1 )
      DO 5000 IJ = 1, IJDIM
         PADVA = ( GADIV( IJ,KMAX ) + GAPADV( IJ,KMAX ) ) * FACA
         PADVF = ( GFDIV( IJ,KMAX ) + GFPADV( IJ,KMAX ) ) * FACA
         GAW( IJ,KMAX ) = 
     &        ( SIG( KMAX )*GAPADV( IJ,KMAX ) - PADVA ) * GFPS( IJ ) 
     &      + ( SIG( KMAX )*GFPADV( IJ,KMAX ) - PADVF ) * GAPS( IJ ) 
 5000 CONTINUE
      DO 5100 K = 1, KMAX-1
         DO 5100 IJ = 1, IJDIM
            FACA = SIG( K ) - SIGM( K+1 )
            PADVA = ( GADIV( IJ,K ) + GAPADV( IJ,K ) ) * FACA
            PADVF = ( GFDIV( IJ,K ) + GFPADV( IJ,K ) ) * FACA
            GAW( IJ,K ) = 
     &           ( SIG(K)*GAPADV( IJ,K )-(GSUMP( IJ,K+1 )+PADVA) )
     &           * GFPS( IJ )
     &         + ( SIG(K)*GFPADV( IJ,K )-(GFSUMP( IJ,K+1 )+PADVF) )
     &           * GAPS( IJ )
 5100 CONTINUE
*
*
      RETURN
      END
**********************************************************************
      SUBROUTINE  DIAGWB        !! diagnose total omega
     O     ( GFW  ,
     I       GFU  , GFV  , GFPS   )
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
      REAL*8     GFW ( IJDIM, KMAX      )  !! perturbation omega
*
*   [INPUT] 
      REAL*8     GFU ( IJDIM, KMAX       )  !! BS zonal wind
      REAL*8     GFV ( IJDIM, KMAX       )  !! BS meridional wind
      REAL*8     GFPS( IJDIM             )  !! BS surface pressure
*
*   [WORK] 
      REAL*8     GFVOR ( IJDIM, KMAX     )  !! BS vorticity 
      REAL*8     GFDIV ( IJDIM, KMAX     )  !! BS divergence D 

      REAL*8     GFSIGD( IJDIM, KMAX+1)  !! tend. of sigma (output)
      REAL*8     GFPID ( IJDIM       )   !! tend. of pai
      REAL*8     GFPADV( IJDIM, KMAX )   !! advection of pai 
      REAL*8     GFSUMP( IJDIM, KMAX )   !! vert. integ. tend. pi
      REAL*8     GFPI  ( IJDIM       )   !! ln Ps
      REAL*8     GFPIX ( IJDIM       )   !! zonal deriv. of pi
      REAL*8     GFPIY ( IJDIM       )   !! merid. differ. of pi 

      REAL*8     GTUX  ( IJDIM, KMAX   )  !! work
      REAL*8     GTVX  ( IJDIM, KMAX   )  !! work
      REAL*8     WDX   ( NMDIM, KMAX   )  !! work
      REAL*8     WDY   ( NMDIM, KMAX   )  !! work
      REAL*8     WDS   ( NMDIM         )  !! work

      REAL*8     FACA, PADVF
      INTEGER    IFPAR, JFPAR
      INTEGER    IJ, K
*
*   [INTERNAL PARAM.] 
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE.  /

      IF ( OFIRST ) THEN
         CALL   REWNML ( IFPAR , JFPAR )
         WRITE ( JFPAR,* ) ' @@@ DIAGWB: DIAGNOSIS OF OMEGA 05/06/28'
         OFIRST = .FALSE.
      ENDIF
*
      CALL UV2VDG
     O      ( GFVOR , GFDIV ,
     I        GFU   , GFV   , .FALSE., .TRUE.,
     W        GTUX  , GTVX  , WDX    , WDY     )
*
      DO 1100 IJ = 1, IJDIM
         GFPI( IJ ) = LOG( GFPS( IJ ) )
 1100 CONTINUE

      CALL HGRAD                !! pi gradient
     O        ( GFPIX , GFPIY ,
     I          GFPI  , 1     ,
     W          WDS            )
*
      CALL PSDOTB               !!  surface pres. change & sigma velocity
     O         ( GFSIGD, GFSUMP, GFPADV, GFPID ,
     I           GFU   , GFV   , GFDIV , GFPIX , GFPIY  )
*
*    << diagnostic for omega >>
*
      FACA = SIG( KMAX ) - SIGM( KMAX+1 )
      DO 5000 IJ = 1, IJDIM
         PADVF = ( GFDIV( IJ,KMAX ) + GFPADV( IJ,KMAX ) ) * FACA
         GFW( IJ,KMAX ) = 
     &      + ( SIG( KMAX )*GFPADV( IJ,KMAX ) - PADVF ) * GFPS( IJ ) 
 5000 CONTINUE
      DO 5100 K = 1, KMAX-1
         DO 5100 IJ = 1, IJDIM
            FACA = SIG( K ) - SIGM( K+1 )
            PADVF = ( GFDIV( IJ,K ) + GFPADV( IJ,K ) ) * FACA
            GFW( IJ,K ) = 
     &         + ( SIG(K)*GFPADV( IJ,K )-(GFSUMP( IJ,K+1 )+PADVF) )
     &           * GFPS( IJ )
 5100 CONTINUE
*
*
      RETURN
      END
***********************************************************************
      SUBROUTINE MTRUNC    !! truncate at m=MTR
     M         ( GX      ,
     I           KDIMD   ,  OZMSW   ,
     W           WX                    )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
#include        "zcord.F"                /* coordinate             */
*
*   [INPUT] 
      INTEGER    KDIMD
      LOGICAL    OZMSW          !! retain higher or lower zonal waves
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
     $        ' @@@ MTRUNC: TRUNCATION AT ZONAL WAVE #:',MTRN
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
      IF( OZMSW ) THEN          !! retain lower zonal waves
         DO 1000 K = 1, KDIMD
            DO 1100 M = MTRN+1 , MMAX, MINT
               LEND = MIN( LMAX, NMAX-M )
               DO 1200 L = 0 , LEND
                  WX( NMO(1,M,L),K )  = 0.D0
                  WX( NMO(2,M,L),K )  = 0.D0
 1200          CONTINUE
 1100       CONTINUE
 1000    CONTINUE 
      ELSE
         DO 2000 K = 1, KDIMD
            DO 2100 M = 0, MTRN, MINT
               LEND = MIN( LMAX, NMAX-M )
               DO 2200 L = 0 , LEND
                  WX( NMO(1,M,L),K )  = 0.D0
                  WX( NMO(2,M,L),K )  = 0.D0
 2200          CONTINUE
 2100       CONTINUE

 2000    CONTINUE 
      ENDIF
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
*========================================================
      ENTRY MTRUNW              !! remove zonal mean
     M         ( GX    ,
     I           KDIMD          )
*
      DO 2000 K = 1, KDIMD
         DO 2100 J = 1, JDIM
            GXM = 0.D0
            DO 2200 I = 1, IDIM
               GXM = GXM + GX( I,J,K ) / DBLE( IDIM )
 2200       CONTINUE
            DO 2300 I = 1, IDIM
               GX( I,J,K ) = GX( I,J,K ) - GXM
 2300       CONTINUE
 2100    CONTINUE
 2000 CONTINUE 
*
*
      RETURN
      END
