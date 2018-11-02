* PACKAGE DCIRC !! dynamics: terms of eqs.
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/05/28(numaguti) restructure
*         05/05/24(hiro)     for initializing BS
**********************************************************************
      SUBROUTINE DYNTRM    !! dynamics terms
     O         ( WTDIV , 
     I           GAU   , GAV   , GAT   , GAPS  , GAQ   , 
     I           TIME  , 
     C           SALPHA, SBETA , SKAPPA, ATF   , BTF   , 
     C           TBAR  , TMTX  , CORIOL, FLAPLA           )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* # of wave              */
#include        "zpdim.F"                /* physics etc.           */
#include        "zccom.F"                /* physical constants     */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT] 
      REAL*8     WTDIV ( NMDIM, KMAX      )  !! tendency of divergence D
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
      REAL*8     TIME                        !! time
*
*   [WORK] 
      REAL*8     GSIGD ( IJDIM, KMAX+1     )  !! tendency of sigma
      REAL*8     GAVOR ( IJDIM, KMAX       )  !! vorticity
      REAL*8     GADIV ( IJDIM, KMAX       )  !! divergence D 
      REAL*8     GAPIX ( IJDIM             )  !! zonal deriv. of pi
      REAL*8     GAPIY ( IJDIM             )  !! merid. differ. of pi
      REAL*8     GATV  ( IJDIM, KMAX       )  !! virtulal tmp.
      REAL*8     GTUX  ( IJDIM, KMAX       )  !! tend. zonal  adv.
      REAL*8     GTVX  ( IJDIM, KMAX       )  !! tend. merid. adv.
      REAL*8     GTXX  ( IJDIM, KMAX       )  !! tend.
      REAL*8     GWX   ( IJDIM, KMAX       )   !! work
      REAL*8     GWY   ( IJDIM, KMAX       )   !! work
      REAL*8     WTKE  ( NMDIM, KMAX       )  !! KE term
      REAL*8     WDX   ( NMDIM, KMAX       )  !! work
      REAL*8     WDY   ( NMDIM, KMAX       )  !! work

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
     W           GTUX  , GTVX  , WDX , WDY           )
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
     O         ( GSIGD , 
     I           GAU   , GAV   , GADIV , GAPIX , GAPIY  )
*
      CALL VIRTMD
     O         ( GATV  ,
     I           GAT   , GAQ     )
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
     O         ( WDX   , WTDIV ,
     I           GTUX  , GTVX  , .FALSE., .TRUE.,
     W           GWX   , GWY                            )
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
*
      RETURN
      END
