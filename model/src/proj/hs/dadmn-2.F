* PACKAGE DADMN !! dynamics: main routine
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/06/03(numaguti) tracer
*         96/06/06(numaguti) remove FIXMAS/MASFIX
*         05/05/24(hiro)     for initializing BS
**********************************************************************
      SUBROUTINE DYNMCS    !!  dynamics
     O         ( OCONV ,
     M           GAT   , GAPS  , 
     I           GAU   , GAV   , GAQ   ,
     I           TIME  , DELT  , OINIT , GDZS    )
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
      LOGICAL    OCONV          !! convergence flag
*
*   [MODIFY]
      REAL*8     GAT   ( IJDIM, KMAX      )  !! temperature T
      REAL*8     GAPS  ( IJDIM            )  !! surface pressure
*        : grid value(t) <GENGD>  grid value(t+dt)
*
*   [INPUT]
      REAL*8     GAU   ( IJDIM, KMAX      )  !! westerly u
      REAL*8     GAV   ( IJDIM, KMAX      )  !! southern wind v
      REAL*8     GAQ   ( IJDIM, KMAX      )  !! humidity etc.
      REAL*8     TIME                        !! time
      REAL*8     DELT                        !! time step delta(t)
      LOGICAL    OINIT                       !! initialized condition ?
      REAL*8     GDZS  ( IJDIM )             !! surface topography
*
*   [INTERNAL WORK] 
      REAL*8     GAZ   ( IJDIM, KMAX      )  !! geopotential height
      REAL*8     WTDIV ( NMDIM, KMAX   )  !! tendency of divergence D
*
      REAL*8     SDTOP ( IJDIM         ) !! coefficient for diss.
      REAL*8     SDBTM ( IJDIM         ) !! coefficient for diss.
      REAL*8     TDMPC          !! damping coefficient for temperature
*
      INTEGER    NM
*
*   [INTERNAL SAVE] 
      REAL*8     WPHIS ( NMDIM  )         !! surface G.P.
      SAVE       WPHIS
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
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DADMN: DYNAMICS CONTROL 98/10/17'
         OFIRST = .FALSE.
*
         CALL SPSTUP      !! spherical harmonic functions
         CALL DSETCO  
     O         ( CORIOL )
         CALL DSETLP
     O         ( FLAPLA )
         CALL DSETVD            !! vertical difference/semi implicit
     O        ( SALPHA, SBETA , SKAPPA,
     O          ATF   , BTF   , TBAR  , 
     O          GG    , TMTX  , WHMTX , GCMTX ,
     I          GAT     )
         CALL DSETVDF           !! vertical diffusion
*
         CALL G2W
     O         ( WPHIS ,
     I           GDZS  ,
     F           '    ', 'POS ', 1     )
         DO 1000 NM = 1, NMDIM
            WPHIS( NM ) = WPHIS( NM )*GRAV
 1000    CONTINUE 
      ENDIF
*
*  << NG divergent tendency >>
*
      CALL DYNTRM    !! dynamics terms
     O         ( WTDIV , 
     I           GAU   , GAV   , GAT   , GAPS  , GAQ   , 
     I           TIME  , 
     C           SALPHA, SBETA , SKAPPA, ATF   , BTF   , 
     C           TBAR  , TMTX  , CORIOL, FLAPLA           )
*
*  << modify temperature and Ps >>
*
      CALL DYNSTP
     O         ( OCONV ,
     M           GAT   , GAPS  ,
     I           WTDIV , 
     I           FLAPLA, WPHIS , GG       )
*
      CALL DSETBTD
     O         ( SDTOP , SDBTM , TDMPC ,
     I           GAT     )
*
C      CALL DDIAG                        !!  diagnostic output
C     O          (  GAZ   ,
C     I             GAT   , GAQ   , GDZS   )
*
*
      RETURN
      END
