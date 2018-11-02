* PACKAGE DDIFC  !! dynamics: fric. heat,p-level diff. correc.
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*
**********************************************************************
      SUBROUTINE CORDIF    !! p-level diff. correc.
     M         ( GDQ   ,
     I           WDPI  , DELT  , 
     C           DIFQ  , NTXX  ,
     W           GTPDIF, WTPDIF          )
*
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
#include        "zpdim.F"                /* physics etc. */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
      INTEGER    NTXX
*
*   [MODIFY]
      REAL*8     GDQ   ( IJDIM, KMAX, NTXX ) !! temp., humidity etc.
*
*   [INPUT]
      REAL*8     WDPI  ( NMDIM  )            !! log Ps=pi
      REAL*8     DELT                        !! time step delta(t)
      REAL*8     DIFQ  ( NMDIM, KMAX  )      !! h-diff. coef.
*
*   [WORK]
      REAL*8     WTPDIF( NMDIM, KMAX )       !! pi  tend. of diff.
      REAL*8     GTPDIF( IJDIM, KMAX )       !! pi  tend. of diff.
*
      INTEGER    NM, K, LT, IJ, KP, KM
      REAL*8     GDDS
*
*   [INTERNAL SAVE] 
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DDIFC: DIFFUSION CORRECTIONS 93/11/09'
         OFIRST = .FALSE.
      ENDIF
*
      IF ( KMAX .EQ. 1 ) RETURN
*
      DO 2100 K = 1, KMAX
         DO 2100 NM = 1, NMDIM
            WTPDIF( NM,K ) 
     &      = - DIFQ( NM,K ) * WDPI( NM ) * SIG( K ) * 2.D0 *DELT
 2100 CONTINUE
      CALL W2G
     O         ( GTPDIF,
     I           WTPDIF,
     I           '    ', 'POS ', KMAX )
*
      DO 2200 LT = 1, NTXX
         DO 2200 K = 1, KMAX
            KP = MIN( K+1, KMAX )
            KM = MAX( K-1, 1    )
            DO 2210 IJ = 1, IJDIM         
               GDDS  = ( GDQ( IJ,KP,LT )-GDQ( IJ,KM,LT ) ) 
     &               / ( SIG(    KP    )-SIG(    KM    ) )
               GDQ( IJ,K,LT ) = GDQ( IJ,K,LT ) - GDDS*GTPDIF( IJ,K )
 2210       CONTINUE
 2200 CONTINUE 
*
      RETURN
      END
**********************************************************************
      SUBROUTINE CORFRC         !! friction heat of horiz. diff.
     M         ( GDT   ,
     M           WDVOR , WDDIV , 
     I           GDU   , GDV   , DELT  ,
     C           DIFV  , DIFD  , 
     W           GTUDIF, GTVDIF, WDX   , WDY    )
*
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
*
*   [MODIFY]
      REAL*8     GDT   ( IJDIM, KMAX )   !! temperature T(t)
      REAL*8     WDVOR ( NMDIM, KMAX )   !! vorticity
      REAL*8     WDDIV ( NMDIM, KMAX )   !! divergence D
*
*   [INPUT]
      REAL*8     GDU   ( IJDIM, KMAX )   !! westerly u
      REAL*8     GDV   ( IJDIM, KMAX )   !! southern wind v
      REAL*8     DELT                    !! time step delta(t)
*
      REAL*8     DIFV  ( NMDIM, KMAX )   !! h-diff. coef (vor)
      REAL*8     DIFD  ( NMDIM, KMAX )   !! h-diff. coef (div)
*
*   [WORK] 
      REAL*8     GTUDIF( IJDIM, KMAX )   !! diff. tend. of wind U
      REAL*8     GTVDIF( IJDIM, KMAX )   !! diff. tend. of wind V
      REAL*8     WDX   ( NMDIM, KMAX )   !! work
      REAL*8     WDY   ( NMDIM, KMAX )   !! work
*
*   [INTERNAL WORK]
      INTEGER    IJ, K, NM
*
*  << WTDTRM : tend. term by diff. >>
*
      DO 1100 K = 1, KMAX
         DO 1100 NM = 1, NMDIM
            WDVOR( NM,K ) = - DIFV( NM,K )* WDVOR( NM,K )
            WDDIV( NM,K ) = - DIFD( NM,K )* WDDIV( NM,K )
 1100 CONTINUE
*
*  << GTDTRM : grid val. of h-diff. tend. >>
*
      CALL       VDW2UV      !! (vor.,D)spectral -> (U,V)
     O         ( GTUDIF, GTVDIF,
     I           WDVOR , WDDIV , .TRUE., .TRUE.,
     W           WDX   , WDY                     )
*
*  << FRCWRM : frictional heat >>
*
      DO 3100 K = 1, KMAX
         DO 3100 IJ = 1, IJDIM
            GDT ( IJ,K ) = GDT ( IJ,K )
     &                    - (   GDU ( IJ,K ) * GTUDIF( IJ,K )
     &                        + GDV ( IJ,K ) * GTVDIF( IJ,K )  )
     &                     / CP * 2.D0 * DELT
 3100 CONTINUE
*
      RETURN
      END
