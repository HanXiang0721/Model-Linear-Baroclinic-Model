* PACKAGE DADMN !! dynamics: main routine
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/06/03(numaguti) tracer
*         96/06/06(numaguti) remove FIXMAS/MASFIX
**********************************************************************
      SUBROUTINE DYNMCS    !!  dynamics
     M         ( GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     M           GBU   , GBV   , GBT   , GBPS  , GBQ   ,
     O           GSIGD , GPID  ,
     I           TIME  , DELT  , OINIT , OANM  , GDZS    )
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
      REAL*8     GLAPS ( IJDIM            )  !! log surface pressure
      REAL*8     GAQ   ( IJDIM, KMAX, NTR )  !! humidity etc.
*        : grid value(t) <GENGD>  grid value(t+dt)
*
      REAL*8     GBU   ( IJDIM, KMAX      )  !! westerly u
      REAL*8     GBV   ( IJDIM, KMAX      )  !! southern wind v
      REAL*8     GBT   ( IJDIM, KMAX      )  !! temperature T
      REAL*8     GBPS  ( IJDIM            )  !! surface pressure
      REAL*8     GLBPS ( IJDIM            )  !! log surface pressure
      REAL*8     GBQ   ( IJDIM, KMAX, NTR )  !! humidity etc.
*        : grid value(t-dt) <MOVGD>  grid value(t)
*
*   [OUTPUT] 
      REAL*8     GSIGD ( IJDIM, KMAX+1    )  !! sigma velocity
      REAL*8     GPID  ( IJDIM            )  !! pi velocity
*
*   [INPUT]
      REAL*8     TIME                        !! time
      REAL*8     DELT                        !! time step delta(t)
      LOGICAL    OINIT                       !! initialized condition ?
      LOGICAL    OANM                        !! anomaly output?
      REAL*8     GDZS  ( IJDIM )             !! surface topography
*
*   [ENTRY]
      REAL*8     GRFV ( IJDIM, KMAX )        !! R-forcing for vor.
      REAL*8     GRFD ( IJDIM, KMAX )        !! R-forcing for div.
      REAL*8     GRFT ( IJDIM, KMAX )        !! R-forcing for temp.
      REAL*8     GRFP ( IJDIM       )        !! R-forcing for LnPs
      REAL*4     X    ( IMAX, JMAX  )        !! temporary for forcing data
      INTEGER    I, J, K, IJ
      CHARACTER  RSFRC*90
      CHARACTER  CFTYPE*8
      LOGICAL    OADD
      SAVE       GRFV, GRFD, GRFT, GRFP
*
*   [INTERNAL WORK] 
      REAL*8     GAVOR ( IJDIM, KMAX   )  !! vorticity
      REAL*8     GADIV ( IJDIM, KMAX   )  !! divergence
      REAL*8     GATV  ( IJDIM, KMAX   )  !! virtual temperature
      REAL*8     GAW   ( IJDIM, KMAX   )  !! p-velocity
      REAL*8     GBI   ( IJDIM, KMAX   )  !! baroclinicity
      REAL*8     GTH   ( IJDIM, KMAX   )  !! potential temp
      REAL*8     GDX   ( IJDIM, KMAX+1 )  !! work
      REAL*8     GDY   ( IJDIM, KMAX   )  !! work
      REAL*8     GDZ   ( IJDIM, KMAX   )  !! work
      REAL*8     TFLX  ( IJDIM, KMAX   )  !! Newtonian cooling
      REAL*8     WDX   ( NMDIM, KMAX   )  !! work
      REAL*8     WDY   ( NMDIM, KMAX   )  !! work
      REAL*8     WTVOR ( NMDIM, KMAX   )  !! tendency of vorticity
      REAL*8     WTDIV ( NMDIM, KMAX   )  !! tendency of divergence D
      REAL*8     WTT   ( NMDIM, KMAX   )  !! tendency of temperature
      REAL*8     WTPI  ( NMDIM         )  !! logPs pi tendency
*
      INTEGER    LT
      INTEGER    NM
      REAL*8     DELI
*
*   [INTERNAL SAVE] 
      REAL*8     DIFV  ( NMDIM, KMAX   )  !! h-diff. coef (vor)
      REAL*8     DIFD  ( NMDIM, KMAX   )  !! h-diff. coef (div)
      REAL*8     DIFT  ( NMDIM, KMAX   )  !! h-diff. coef. of heat
      REAL*8     DIFQ  ( NMDIM, KMAX   )  !! h-diff. coef. of water
      REAL*8     WPHIS ( NMDIM  )         !! surface G.P.
      REAL*8     RSTT  ( KMAX   )         !! restoring to T_te 
      REAL*8     RSTQ  ( KMAX   )         !! restoring to q(RH*T_te) 
      REAL*8     RSTRH ( KMAX   )         !! restoring RH
      REAL*8     FCTT
      SAVE       DIFV, DIFD, DIFT, DIFQ, WPHIS
      SAVE       RSTT, RSTQ, RSTRH
      SAVE       FCTT
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
      DATA       FCTT   / 1.D0   /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DADMN: DYNAMICS CONTROL 98/10/17'
         OFIRST = .FALSE.
*
         CALL SPSTUP      !! spherical harmonic functions
         CALL DSETDF      !! horizontal diffusion
     O          ( DIFV  , DIFD  , DIFT  , DIFQ  ,
     I            GAU     )
*
* added on 09/30/03
         CALL DSETRST     !! restoring to rad-equilibrium T
     O         ( RSTT  , RSTQ  , RSTRH      )
*
         CALL DSETVD      !! vertical difference/semi implicit
     O        ( SALPHA, SBETA , SKAPPA,
     O          ATF   , BTF   , TBAR  , 
     O          GG    , TMTX  , WHMTX , GCMTX ,
     I          GAT     )
*
         CALL DSETCO  
     O         ( CORIOL )
         CALL DSETLP
     O         ( FLAPLA )
CCX         CALL DSETVDF           !! vertical diffusion
*
         CALL G2W
     O         ( WPHIS ,
     I           GDZS  ,
     F           '    ', 'POS ', 1     )
         DO 1000 NM = 1, NMDIM
            WPHIS( NM ) = WPHIS( NM )*GRAV
 1000    CONTINUE 
*
*     reset V,D,pi forcing when restoring T,Q
*
         IF( RSTT( 1 ) .GT. 0.D0 ) THEN
            CALL RESET ( GRFV , IJDIM*KMAX )
            CALL RESET ( GRFD , IJDIM*KMAX )
            CALL RESET ( GRFP , IJDIM      )
            FCTT = 0.D0
         ENDIF

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
      CALL HISTIF
     I     ( GLBPS,'DLPDYN','dynamics LogPs-tendency','hPa/s','ASFC',
     $     -DELI) 
      CALL HISTIF
     I   ( GBQ,'DQDYN','dynamics q-tendency','1/s',    'ALEV',-DELI)
      CALL HISTIF
     I (GBQ(1,1,ITL),'DLDYN','dynamics l-tendency','1/s','ALEV',-DELI)
*
*  << tendency term >>
*
      CALL DYNTRM    !! dynamics terms
     O         ( WTVOR , WTDIV , WTT   , WTPI  ,
     O           GSIGD , GPID  , GAVOR , GADIV , GATV  , GAW  ,
     O           TFLX  ,
     I           GAU   , GAV   , GAT   , GAPS  , GAQ   , 
     I           GRFV  , GRFD  , GRFT  , GRFP  ,
     I           GBT   ,
     I           TIME  , OANM  , RSTT  , FCTT  ,
     C           SALPHA, SBETA , SKAPPA, ATF   , BTF   , 
     C           TBAR  , TMTX  , CORIOL, FLAPLA,
     W           GDX   , GDY   , GDZ   , WDX             )
*
      CALL GSIGBI         !!  baroclinicity
     O         ( GBI   , GTH   ,
     I           GAT   , GAPS  ,
     I           GAU   , GAV   , CORIOL,
     C           ATF   , BTF              )
*
      CALL DDIAG                        !!  diagnostic output
     I         ( GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     I           GAVOR , GADIV , GATV  , GSIGD , GPID  ,
     I           GAW   , GBI   , GTH   , GDZS  , TFLX  , 
     I           OANM  ,
     W           GDX   , GDY                             )
*
*  << tracer >>
*
#ifdef OPT_MOIST
      DO 2100 LT = 1, NTR
         CALL TRACER
     M         ( GAQ(1,1,LT)   , GBQ(1,1,LT)   ,
     I           GAU   , GAV   , GRFT  , GAPS  , GADIV , GSIGD ,
     I           DELT  , OINIT , DIFQ  , RSTQ  , RSTRH , LT    ,
     W           GDX   , GDY   , GDZ   , WDX   , WDY    )
 2100 CONTINUE 
#endif
*
*  << time integration >>
*
      CALL DYNSTP
     M         ( GAU   , GAV   , GAT   , GAPS  , GLAPS ,
     M           GBU   , GBV   , GBT   , GBPS  , GLBPS ,
     M           GAQ   ,
     I           WTVOR , WTDIV , WTT   , WTPI  ,
     I           DELT  , OINIT , 
     I           DIFV  , DIFD  , DIFT  , DIFQ  , FLAPLA,
     C           WPHIS , GG    , TMTX  , WHMTX , GCMTX ,
     W           GDX   , GDY   , WDX   , WDY            )
*
      CALL HISTAD ( GAU         , 'DUDYN',  DELI )
      CALL HISTAD ( GAV         , 'DVDYN',  DELI )
      CALL HISTAD ( GAT         , 'DTDYN',  DELI )
      CALL HISTAD ( GAPS        , 'DPDYN',  DELI )
      CALL HISTAD ( GLAPS       , 'DLPDYN', DELI )
      CALL HISTAD ( GAQ         , 'DQDYN',  DELI )
      CALL HISTAD ( GAQ(1,1,ITL), 'DLDYN',  DELI )
*
      RETURN
*=====================================================================
      ENTRY      RDFRC        !! read forcing 
     I         ( RSFRC  , CFTYPE , OADD )
*
*
*
      WRITE ( 6,* ) ' @@@ RDRFRC: READ FORCING 01/10/19'

#ifdef OPT_RWRIT
      RETURN
#endif
      IF( RSFRC(1:1) .NE. ' ' ) THEN
         OPEN( 87, FILE = RSFRC, FORM='UNFORMATTED',
     &        STATUS='UNKNOWN' ) 
         WRITE( 6, *) '    $$$ ',CFTYPE,' FORCING FILE:', RSFRC,' $$$'
      ELSE
         WRITE( 6, *) '    ### ',CFTYPE,' FORCING NOT APPLIED ### '
         WRITE( 6, *) 
         RETURN
      ENDIF
*
      IF( .NOT. OADD ) THEN
         CALL RESET ( GRFV , IJDIM*KMAX )
         CALL RESET ( GRFD , IJDIM*KMAX )
         CALL RESET ( GRFT , IJDIM*KMAX )
         CALL RESET ( GRFP , IJDIM      )
      ENDIF
*
      DO 100 K = 1, KMAX
         READ( 87 ) X           !! vorticity
         IJ = 0
         DO 110 J = 1, JMAX
            DO 120 I = 1, IMAX
               IJ = IJ + 1
               GRFV( IJ, K) = GRFV( IJ, K) + DBLE( X( I, J) )
 120        CONTINUE
            IJ = IJ + 1
            GRFV( IJ, K) = GRFV( IJ, K) + DBLE( X( 1, J) )
 110     CONTINUE
 100  CONTINUE
      write(6,*) 'vor.'
*     
      DO 130 K = 1, KMAX
         READ( 87 ) X           !! divergence
         IJ = 0
         DO 140 J = 1, JMAX
            DO 150 I = 1, IMAX
               IJ = IJ + 1
               GRFD( IJ, K) = GRFD( IJ, K) + DBLE( X( I, J) )
 150        CONTINUE
            IJ = IJ + 1
            GRFD( IJ, K) = GRFD( IJ, K) + DBLE( X( 1, J) )
 140     CONTINUE
 130  CONTINUE
      write(6,*) 'div.'
*     
      DO 160 K = 1, KMAX
         READ( 87 ) X           !! temperature
         IJ = 0
         DO 170 J = 1, JMAX
            DO 180 I = 1, IMAX
               IJ = IJ + 1
               GRFT( IJ, K) = GRFT( IJ, K) + DBLE( X( I, J) )
 180        CONTINUE
            IJ = IJ + 1
            GRFT( IJ, K) = GRFT( IJ, K) + DBLE( X( 1, J) )
 170     CONTINUE
 160  CONTINUE
      write(6,*) 'tmp.'
*     
      READ( 87 ) X              !! Ps
      IJ = 0
      DO 190 J = 1, JMAX
         DO 200 I = 1, IMAX
            IJ = IJ + 1
            GRFP( IJ   ) = GRFP( IJ ) + DBLE( X( I, J) )
 200     CONTINUE
         IJ = IJ + 1
         GRFP( IJ   ) = GRFP( IJ ) + DBLE( X( 1, J) )
 190  CONTINUE
      write(6,*) 'lnps'
      CLOSE( 87 )
      WRITE( 6, *) 
*
      RETURN
      END
