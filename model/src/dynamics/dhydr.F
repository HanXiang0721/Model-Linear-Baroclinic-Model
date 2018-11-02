* PACKAGE DHYDR       !! dynamics: hydrostatics
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
***********************************************************************
      SUBROUTINE WHYDRO    !! hydrostatic relation (spectral)
     O         ( WDPHI ,
     I           WDT    )
*
*                  Arakawa & Suarez vert.diffr in sigma
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM                  */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT] 
      REAL*8     WDPHI ( NMDIM, KMAX )       !! altitude phi
*   [ENTRY OUTPUT] 
      REAL*8     GDZ   ( IJDIM, KMAX )       !! altitude Z
*
*   [INPUT] 
      REAL*8     WDT   ( NMDIM, KMAX )       !! temperature T
*   [ENTRY INPUT] 
      REAL*8     GDT   ( IJDIM, KMAX )       !! temperature T
*
*   [ENTRY INPUT] 
      REAL*8     GDZS  ( IJDIM )             !! surface topography
*
*   [INTERNAL WORK] 
      INTEGER    IJ, K, NM
*
*   [INTERNAL SAVE] 
      REAL*8     SALPHA( KMAX )              !! fact. of hydrostatic eq.
      REAL*8     SBETA ( KMAX )              !! fact. of hydrostatic eq.
      SAVE       SALPHA, SBETA
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DHYDR: HYDROSTATICS 94/11/25'
         OFIRST = .FALSE.
         CALL DSETVH
     O        ( SALPHA, SBETA  )
      ENDIF
*
      DO 1100 NM = 1, NMDIM
            WDPHI ( NM,1 ) = CP * SALPHA( 1 ) * WDT ( NM,1 )
 1100 CONTINUE
*
      DO 1200 K = 2, KMAX
         DO 1200 NM = 1, NMDIM
            WDPHI ( NM,K ) = WDPHI( NM,K-1 )
     &                          + CP * SALPHA( K   ) * WDT ( NM,K   )
     &                          + CP * SBETA ( K-1 ) * WDT ( NM,K-1 )
 1200 CONTINUE
*
      RETURN
*======================================================================
      ENTRY      GHYDRO    !! hydrostatic relation (grid)
     O         ( GDZ   ,
     I           GDT   , GDZS   )
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DHYDR: HYDROSTATICS 94/11/25'
         OFIRST = .FALSE.
         CALL DSETVH
     O        ( SALPHA, SBETA  )
      ENDIF
*
      DO 2100 IJ = 1, IJDIM
         GDZ ( IJ,1 ) = GDZS( IJ )
     &                + CP * SALPHA( 1 ) * GDT ( IJ,1 )/GRAV
 2100 CONTINUE
*
      DO 2200 K = 2, KMAX
         DO 2200 IJ = 1, IJDIM
            GDZ ( IJ,K ) = GDZ( IJ,K-1 )
     &                     +(  CP * SALPHA( K   ) * GDT ( IJ,K   ) 
     &                       + CP * SBETA ( K-1 ) * GDT ( IJ,K-1 ) )
     &                     / GRAV
 2200 CONTINUE
*
      RETURN
      END
