* PACKAGE DCIRC !! dynamics: terms of eqs.
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/05/28(numaguti) restructure
**********************************************************************
      SUBROUTINE DYNTRM    !! dynamics terms
     O         ( WTVOR , WTDIV , WTT   , WTPI  ,
     O           GSIGD , GPID  , GAVOR , GADIV , GATV  ,
     I           GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     C           SALPHA, SBETA , SKAPPA, ATF   , BTF   , 
     C           TBAR  , TMTX  , CORIOL, FLAPLA,
     W           GTUX  , GTVX  , GTXX  , WTKE           )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* # of wave              */
#include        "zpdim.F"                /* physics etc.           */
#include        "zccom.F"                /* physical constants     */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT] 
      REAL*8     WTVOR ( NMDIM, KMAX      )  !! tendency of vorticity
      REAL*8     WTDIV ( NMDIM, KMAX      )  !! tendency of divergence D
      REAL*8     WTT   ( NMDIM, KMAX      )  !! tendency of temperature
      REAL*8     WTPI  ( NMDIM            )  !! logPs pi tendency
*
      REAL*8     GSIGD ( IJDIM, KMAX+1     )  !! tendency of sigma
      REAL*8     GPID  ( IJDIM             )  !! tendency of pai
      REAL*8     GAVOR ( IJDIM, KMAX       )  !! vorticity
      REAL*8     GADIV ( IJDIM, KMAX       )  !! divergence D 
      REAL*8     GATV  ( IJDIM, KMAX       )  !! virtulal tmp.
*
*   [INPUT] 
      REAL*8     GAU   ( IJDIM, KMAX       )  !! westerly u
      REAL*8     GAV   ( IJDIM, KMAX       )  !! southern wind v
      REAL*8     GAT   ( IJDIM, KMAX       )  !! temperature T
      REAL*8     GAPS  ( IJDIM             )  !! surface pressure
      REAL*8     GAQ   ( IJDIM, KMAX, NTR  )  !! virtual temperature
*
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
*   [WORK] 
      REAL*8     GTUX  ( IJDIM, KMAX   )  !! tend. zonal  adv.
      REAL*8     GTVX  ( IJDIM, KMAX   )  !! tend. merid. adv.
      REAL*8     GTXX  ( IJDIM, KMAX   )  !! tend.
      REAL*8     WTKE  ( NMDIM, KMAX   )  !! KE term
*
*   [INTERNAL WORK]
      REAL*8     GAPADV( IJDIM, KMAX )   !! advection of pai
      REAL*8     GSUMP ( IJDIM, KMAX )   !! vert. integ. of tend. of pi
      REAL*8     GAPIX ( IJDIM       )   !! zonal deriv. of pi
      REAL*8     GAPIY ( IJDIM       )   !! merid. differ. of pi
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K, L, NM
*
*   [INTERNAL SAVE] 
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DTERM: DYNAMICS EQ. TERMS 96/05/28'
         OFIRST = .FALSE.
      ENDIF
*
*    << circulations >>
*
      CALL UV2VDG
     O         ( GAVOR , GADIV ,
     I           GAU   , GAV   , .TRUE., .TRUE.,
     W           GTUX  , GTVX  , WTVOR , WTDIV  )
*
      DO 1100 IJ = 1, IJDIM
         GTXX( IJ,1 ) = LOG( GAPS( IJ ) )
 1100 CONTINUE 
      CALL HGRAD                       !! surface pressure gradient
     O         ( GAPIX , GAPIY ,
     I           GTXX  , 1     ,
     W           WTKE           )
*
      CALL PSDOT     !!  surface pres. change & sigma velocity
     O         ( GSIGD , GSUMP , GAPADV, GPID  ,
     I           GAU   , GAV   , GADIV , GAPIX , GAPIY  )
*
*    << pressure eq. >>
*
      CALL COPY ( GTXX, GPID, IJDIM )
      DO 2100 K = 1, KMAX
         DO 2100 IJ = 1, IJDIM
            GTXX( IJ,1 ) = GTXX( IJ,1 ) + DSIG( K )*GADIV( IJ,K )
 2100 CONTINUE 
*
      CALL G2W
     O         ( WTPI  ,
     I           GTXX  ,
     I           '    ', 'POSO',  1  )
*
*    << temperature eq. >>
*
      CALL VIRTMD
     O         ( GATV  ,
     I           GAT   , GAQ     )
*
      CALL GRTADV                      !!  temperature advection
     O         ( GTUX  , GTVX  , GTXX  ,
     I           GAT   , GATV  ,
     I           GAU   , GAV   , GADIV , 
     I           GSIGD , GSUMP , GAPADV, 
     C           SALPHA, SBETA , SKAPPA,
     C           ATF   , BTF   , TBAR    )
*
      DO 3100 L = 1, KMAX
         DO 3100 K = 1, KMAX
            DO 3100 IJ = 1, IJDIM
               GTXX( IJ,K ) = GTXX( IJ,K ) + TMTX( K,L )*GADIV( IJ,L )
 3100 CONTINUE 
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
     I           GAU   , GAV   , GAVOR , 
     I           GAPIX , GAPIY , GSIGD , GATV  , GAT   ,
     C           CORIOL, 
     C           SALPHA, SBETA , SKAPPA, TBAR           )
*
      CALL       UV2VDW    !! (U,V) -> (vor.,D)spectral
     O         ( WTVOR , WTDIV ,
     I           GTUX  , GTVX  , .TRUE., .TRUE.,
     W           GSUMP , GAPADV                  )
*
      CALL G2W
     O         ( WTKE  ,
     I           GTXX  ,
     I           '    ', 'POSO', KMAX )
*
      DO 4100 K = 1 , KMAX
         DO 4100 NM = 1 , NMDIM
            WTDIV ( NM,K ) =  WTDIV ( NM,K ) 
     &                     -  WTKE  ( NM,K ) * FLAPLA( NM )
 4100 CONTINUE
*
      RETURN
      END
