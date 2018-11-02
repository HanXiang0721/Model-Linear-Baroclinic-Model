* PACKAGE DADMN !! dynamics: main routine
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/06/03(numaguti) tracer
*         96/06/06(numaguti) remove FIXMAS/MASFIX
*         01/06/15(hiro)     including moisture equation
**********************************************************************
      SUBROUTINE DYNMCS    !!  dynamics
     M         ( GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     M           GBU   , GBV   , GBT   , GBPS  , GBQ   ,
     O           GSIGD , GPID  , 
     O           GFU   , GFV   , GFT   , GFPS  , GFQ   ,
     I           TIME  , TFSEND, DELT  , OINIT , GDZS  ,           
     I           FFRC  , OPER  , NFCS  , OVD   ,
     I           FSFRC , OFRC  , OSSTF , NSFCS )
*
*               semi-implicit time integration
*               implicit diffusion
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* # of wave              */
#include        "zpdim.F"                /* physics etc.           */
#include        "zhdim.F"                /* # of char. in string   */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
*
*   [MODIFY]
      REAL*8     GAU   ( IJDIM, KMAX      )  !! westerly u
      REAL*8     GAV   ( IJDIM, KMAX      )  !! southern wind v
      REAL*8     GAT   ( IJDIM, KMAX      )  !! temperature T
      REAL*8     GAPS  ( IJDIM            )  !! surface pressure
      REAL*8     GAQ   ( IJDIM, KMAX      )  !! humidity etc.
*        : grid value(t) <GENGD>  grid value(t+dt)
*
      REAL*8     GBU   ( IJDIM, KMAX      )  !! westerly u
      REAL*8     GBV   ( IJDIM, KMAX      )  !! southern wind v
      REAL*8     GBT   ( IJDIM, KMAX      )  !! temperature T
      REAL*8     GBPS  ( IJDIM            )  !! surface pressure
      REAL*8     GBQ   ( IJDIM, KMAX      )  !! humidity etc.
*        : grid value(t-dt) <MOVGD>  grid value(t)
*
*   [OUTPUT] 
      REAL*8     GSIGD ( IJDIM, KMAX+1    )  !! sigma velocity
      REAL*8     GPID  ( IJDIM            )  !! pi velocity
*
*   [INPUT]
      REAL*8     GFU   ( IJDIM, KMAX      )  !! BS westerly u
      REAL*8     GFV   ( IJDIM, KMAX      )  !! BS southery wind v
      REAL*8     GFT   ( IJDIM, KMAX      )  !! BS temperature
      REAL*8     GFPS  ( IJDIM            )  !! BS surface pressure
      REAL*8     GFQ   ( IJDIM, KMAX      )  !! BS humidity

      REAL*8     TIME                        !! time
      REAL*8     TFSEND                      !! finish t steady forcing
      REAL*8     DELT                        !! time step delta(t)
      LOGICAL    OINIT                       !! initialized condition ?
      REAL*8     GDZS  ( IJDIM )             !! surface topography
      CHARACTER  FFRC * 70                   !! filename perturbation
      CHARACTER  FSFRC * 70                  !! filename steady forcing
      LOGICAL    OPER                        !! add perturbation ?
      LOGICAL    OFRC                        !! add steady forcing ?
      LOGICAL    OSSTF                        !! is forcing SST?
      LOGICAL    OVD                         !! initial pert. = vor,div
      INTEGER    NFCS                        !! no. case init. pert.
      INTEGER    NSFCS                       !! no. case steady forcing
*
*   [INTERNAL WORK] 
      REAL*8     GAVOR ( IJDIM, KMAX   )  !! vorticity
      REAL*8     GADIV ( IJDIM, KMAX   )  !! divergence
      REAL*8     GATV  ( IJDIM, KMAX   )  !! virtual temperature
      REAL*8     GDX   ( IJDIM, KMAX+1 )  !! work
      REAL*8     GDY   ( IJDIM, KMAX   )  !! work
      REAL*8     GDZ   ( IJDIM, KMAX   )  !! work
      REAL*8     WDX   ( NMDIM, KMAX   )  !! work
      REAL*8     WDY   ( NMDIM, KMAX   )  !! work
      REAL*8     WTVOR ( NMDIM, KMAX   )  !! tendency of vorticity
      REAL*8     WTDIV ( NMDIM, KMAX   )  !! tendency of divergence D
      REAL*8     WTT   ( NMDIM, KMAX   )  !! tendency of temperature
      REAL*8     WTPI  ( NMDIM         )  !! logPs pi tendency
      REAL*8     WTQ   ( NMDIM, KMAX   )  !! tendency of humidity
*
      INTEGER    NM
      REAL*8     DELI
*
*   [INTERNAL SAVE] 
      REAL*8     DIFV  ( NMDIM, KMAX   )  !! h-diff. coef (vor)
      REAL*8     DIFD  ( NMDIM, KMAX   )  !! h-diff. coef (div)
      REAL*8     DIFT  ( NMDIM, KMAX   )  !! h-diff. coef. of heat
      REAL*8     DIFQ  ( NMDIM, KMAX   )  !! h-diff. coef. of water
      REAL*8     WPHIS ( NMDIM  )         !! surface G.P.
*
      SAVE       DIFV, DIFD, DIFT, DIFQ, WPHIS
*
      REAL*8     SALPHA( KMAX )           !! fact. of hydrostatic eq.
      REAL*8     SBETA ( KMAX )           !! fact. of hydrostatic eq.
      REAL*8     SKAPPA( KMAX )           !! fact. of vert. intrp. T
      REAL*8     ATF   ( KMAX )           !! fact. of vert. intrp. T
      REAL*8     BTF   ( KMAX )           !! fact. of vert. intrp. T
      REAL*8     TBAR  ( KMAX )           !! basic temp. (full lev)
      REAL*8     GG    ( KMAX )           !! for semi-implicit
      REAL*8     TMTX  ( KMAX, KMAX )     !! for semi-implicit
      REAL*8     WHMTX ( KMAX, KMAX )     !! for semi-implicit
      REAL*8     GCMTX ( KMAX, KMAX )     !! for semi-implicit
      SAVE       SALPHA, SBETA, SKAPPA, ATF, BTF, TBAR
      SAVE       GG, TMTX, WHMTX, GCMTX
*
      REAL*8     CORIOL( IJDIM )          !! Corioris factor f
      REAL*8     FLAPLA( NMDIM )          !! Laplacian factor
      SAVE       CORIOL, FLAPLA
*     
*   [INTERNAL PARM] 
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
      INTRINSIC  DBLE
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DADMN: DYNAMICS CONTROL 98/11/13'
         OFIRST = .FALSE.
*
         CALL RDIFRC      !!  read initial perturbation
     M        ( GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     M          GBU   , GBV   , GBT   , GBPS  , GBQ   ,
     I          OPER  , FFRC  , NFCS    )
*
         CALL SPSTUP      !! spherical harmonic functions
         CALL DSETDF      !! horizontal diffusion
     O          ( DIFV  , DIFD  , DIFT  , DIFQ  ,
     I            GFU     )
*
         CALL DSETVD      !! vertical difference/semi implicit
     O        ( SALPHA, SBETA , SKAPPA,
     O          ATF   , BTF   , TBAR  , 
     O          GG    , TMTX  , WHMTX , GCMTX ,
     I          GFT    )
*
         CALL DSETCO  
     O         ( CORIOL )
         CALL DSETLP
     O         ( FLAPLA )
         CALL DSETVDF
*
         CALL G2W
     O         ( WPHIS ,
     I           GDZS  ,
     F           '    ', 'POS ', 1     )
         DO 2000 NM = 1, NMDIM
***            WPHIS( NM ) = WPHIS( NM )*GRAV
            WPHIS( NM ) = 0.D0
 2000    CONTINUE 
      ENDIF
*
      DELI = 1.D0/(2.D0*DELT)
      CALL HISTIF
     I   ( GBU,'DUDYN','dynamics u-tendency','m/s**2', 'ALEV',-DELI)
      CALL HISTIF
     I   ( GBV,'DVDYN','dynamics v-tendency','m/s**2', 'ALEV',-DELI)
      CALL HISTIF
     I   ( GBT,'DTDYN','dynamics T-tendency','K/s',    'ALEV',-DELI)
      CALL HISTIF
     I   ( GBPS,'DPDYN','dynamics Ps-tendency','hPa/s','ASFC',-DELI)
*
*  << tendency term >>
*
      CALL DYNTRM    !! dynamics terms
     O         ( WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
     O           GSIGD , GPID  , GAVOR , GADIV , GATV  , 
     I           GFU   , GFV   , GFT   , GFPS  , GFQ   ,
     I           GBU   , GBV   , GBT   , GBPS  , GBQ   ,
     I           TFSEND, FSFRC , OFRC  , OSSTF , NSFCS , OVD   ,
     M           TIME  ,
     M           GAU   , GAV   , GAT   , GAPS  , GAQ   , 
     C           SALPHA, SBETA , SKAPPA, ATF   , BTF   , 
     C           TBAR  , TMTX  , CORIOL, FLAPLA,
     W           GDX   , GDY   , GDZ   , WDX   )
*
      CALL DDIAG                        !!  diagnostic output
     I         ( GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     I           GAVOR , GADIV , GATV  , GSIGD , GPID  ,
     I           GDZS  , 
     W           GDX   , GDY                             )
*
*  << time integration >>
*
      CALL DYNSTP
     M         ( GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     M           GBU   , GBV   , GBT   , GBPS  , GBQ   ,
     M           WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
     I           GFPS  , 
     I           DELT  , OINIT , 
     I           DIFV  , DIFD  , DIFT  , DIFQ  , FLAPLA,
     C           WPHIS , GG    , TMTX  , WHMTX , GCMTX ,
     W           GDX   , GDY   , WDX   , WDY            )
*
      CALL HISTAD ( GAU         , 'DUDYN'  ,  DELI )
      CALL HISTAD ( GAV         , 'DVDYN'  ,  DELI )
      CALL HISTAD ( GAT         , 'DTDYN'  ,  DELI )
      CALL HISTAD ( GAPS        , 'DPDYN'  ,  DELI )
      CALL HISTAD ( GAQ         , 'DQDYN'  ,  DELI )
      CALL HISTAD ( GAQ         , 'DLDYN'  ,  DELI )
*
      RETURN
      END
