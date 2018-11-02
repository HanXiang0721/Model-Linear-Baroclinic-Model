* PACKAGE DADMN !! dynamics: main routine
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/06/03(numaguti) tracer
*         96/06/06(numaguti) remove FIXMAS/MASFIX
*         01/06/15(hiro)     PWM incl. moisture
**********************************************************************
      SUBROUTINE DYNMCS    !!  dynamics
     M         ( GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     I           GFU   , GFV   , GFT   , GFPS  , GFQ   ,
     I           GFPSM ,
     O           GSIGD , GPID  ,
     I           TIME  , DELT  , GDZS  )
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
*   [OUTPUT] 
      REAL*8     GSIGD ( IJDIM, KMAX+1    )  !! sigma velocity
      REAL*8     GPID  ( IJDIM            )  !! pi velocity
*
*   [INPUT]
      REAL*8     GFU   ( IJDIM, KMAX      )  !! BS westerly u 
      REAL*8     GFV   ( IJDIM, KMAX      )  !! BS southern wind v
      REAL*8     GFT   ( IJDIM, KMAX      )  !! BS temperature
      REAL*8     GFPS  ( IJDIM            )  !! BS surface pressure
      REAL*8     GFPSM ( IJDIM            )  !! ZMBS surface pressure
      REAL*8     GFQ   ( IJDIM, KMAX      )  !! BS humidity etc.

      REAL*8     TIME                        !! time
      REAL*8     DELT                        !! time step delta(t)
      REAL*8     GDZS  ( IJDIM )             !! surface topography
*
*   [INTERNAL]
      REAL*8     GAU   ( IJDIM, KMAX      )  !! westerly u
      REAL*8     GAV   ( IJDIM, KMAX      )  !! southern wind v
      REAL*8     GAT   ( IJDIM, KMAX      )  !! temperature T
      REAL*8     GAPS  ( IJDIM            )  !! surface pressure
      REAL*8     GAQ   ( IJDIM, KMAX      )  !! humidity etc.
      REAL*8     GBU   ( IJDIM, KMAX      )  !! westerly u
      REAL*8     GBV   ( IJDIM, KMAX      )  !! southern wind v
      REAL*8     GBT   ( IJDIM, KMAX      )  !! temperature T
      REAL*8     GBPS  ( IJDIM            )  !! surface pressure
      REAL*8     GBQ   ( IJDIM, KMAX      )  !! humidity etc.
      SAVE       GBU, GBV, GBT, GBPS, GBQ
*        : grid value(t) perturbation
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
      INTEGER    IFPAR, JFPAR
      INTEGER    NM
*
*   [INTERNAL SAVE] 
      REAL*8     DIFV  ( NMDIM, KMAX   )  !! h-diff. coef (vor)
      REAL*8     DIFD  ( NMDIM, KMAX   )  !! h-diff. coef (div)
      REAL*8     DIFT  ( NMDIM, KMAX   )  !! h-diff. coef. of heat
      REAL*8     DIFQ  ( NMDIM, KMAX   )  !! h-diff. coef. of water
      REAL*8     WPHIS ( NMDIM  )         !! surface G.P.
      REAL*8     WTUNTT( NMDIM, KMAX   )  !! unit matrix
      REAL*8     WTUNTF( NMDIM, KMAX   )  !! zero matrix (dumy) 
      REAL*8     WTUNTP( NMDIM         )  !! unit matrix 
*
      REAL*8     SDTOP ( IJDIM         ) !! coefficient for diss.
      REAL*8     SDBTM ( IJDIM         ) !! coefficient for diss.
*
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
*
      SAVE       DIFV, DIFD, DIFT, DIFQ, WPHIS
      SAVE       SDTOP, SDBTM
      SAVE       NMO
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
      REAL*8     TDMPC          !! damping coefficient for temperature
      SAVE       SALPHA, SBETA, SKAPPA, ATF, BTF, TBAR
      SAVE       GG, TMTX, WHMTX, GCMTX
      SAVE       TDMPC
*
      REAL*8     CORIOL( IJDIM )          !! Coriolis factor f
      REAL*8     FLAPLA( NMDIM )          !! Laplacian factor
      SAVE       CORIOL, FLAPLA
*     
*   [INTERNAL PARM] 
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE.  /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DADMN: DYNAMICS CONTROL 98/10/30'
         OFIRST = .FALSE.
*
         CALL DSETNM
     O         ( NMO    )
         CALL SPSTUP      !! spherical harmonic functions
         CALL G2W
     O         ( WPHIS ,
     I           GDZS  ,
     F           '    ', 'POS ', 1     )
         DO 200 NM = 1, NMDIM
            WPHIS( NM ) = WPHIS( NM )*GRAV
  200    CONTINUE 
*
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
         CALL DSETBTD
     O         ( SDTOP , SDBTM , TDMPC ,
     I           GFT     )
         CALL DSETVDF
*
      ENDIF
*
*  << tendency term >>
*
C      CALL DYNTRM    !! dynamics terms
C     O         ( WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
C     O           GSIGD , GPID  , GAVOR , GADIV , GATV  , 
C     O           GAU   , GAV   , GAT   , GAPS  , GAQ   ,
C     M           GBU   , GBV   , GBT   , GBPS  , GBQ   ,
C     I           DELT  ,
C     I           GFU   , GFV   , GFT   , GFPS  , GFQ   , 
C     I           GFPSM , SDTOP , SDBTM ,
C     C           SALPHA, SBETA , SKAPPA, ATF   , BTF   , 
C     C           TBAR  , TMTX  , CORIOL, FLAPLA,
C     W           GDX   , GDY   , GDZ   , WDX   ,
C     I           OMATV , OMATD , OMATT , OMATP , OMATQ ,
C     I           WTUNTT, WTUNTF, WTUNTP )
*
*  << time integration >>
*
      CALL DYNSTP
     M         ( GBU   , GBV   , GBT   , GBPS  , GBQ   ,
     M           WTVOR , WTDIV , WTT   , WTPI  , WTQ   ,
     I           DELT  , 
     I           DIFV  , DIFD  , DIFT  , DIFQ  , FLAPLA,
     I           TDMPC ,
     C           WPHIS , GG    , TMTX  , WHMTX , GCMTX , 
     W           GDX   , GDY   , WDX   , WDY            )
*
      RETURN
      END
