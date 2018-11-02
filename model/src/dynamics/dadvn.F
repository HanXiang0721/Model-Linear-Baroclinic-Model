* PACKAGE  DADVN   !! dynamics: time advancing + time filter
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/05/29(numaguti) restructure
*
***********************************************************************
      SUBROUTINE DADVNC    !! GA->GB time advancing + time filter
     M         ( GBU   , GBV   , GBT   , GBPS  ,
     I           GAU   , GAV   , GAT   , GAPS   )
*
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc. */
*
*    [MODIFY]
      REAL*8     GBU   ( IJDIM*KMAX      )    !! westerly u(t-delta(t))
      REAL*8     GBV   ( IJDIM*KMAX      )    !! southery wind v(t-dt)
      REAL*8     GBT   ( IJDIM*KMAX      )    !! temp. T(t-dt)
      REAL*8     GBPS  ( IJDIM           )    !! surface pressure (t-dt)
*
*   [INPUT] 
      REAL*8     GAU   ( IJDIM*KMAX      )    !! westerly u(t)
      REAL*8     GAV   ( IJDIM*KMAX      )    !! southern wind v(t)
      REAL*8     GAT   ( IJDIM*KMAX      )    !! temperature T(t)
      REAL*8     GAPS  ( IJDIM           )    !! surface pressure (t)
*
*   [ENTRY INPUT] 
      REAL*8     GBQX  ( IJDIM*KMAX      )    !! specific humidity q(t)
      REAL*8     GAQX  ( IJDIM*KMAX      )    !! specific humidity q(t)
      REAL*8     GBQ   ( IJDIM*KMAX, NTR )    !! specific humidity q(t)
      REAL*8     GAQ   ( IJDIM*KMAX, NTR )    !! specific humidity q(t)

*    [INTERNAL WORK]
      REAL*8     TFILB, TFILA
      INTEGER    IJ, IJK, LT, IFPAR, JFPAR
*
*   [INTERNAL SAVE] 
      REAL*8     TFIL                        !! factor of time filter
      LOGICAL    OFIRST
      NAMELIST  /NMTFIL/ TFIL
      DATA       TFIL   / 5.D-2 /
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DADVN: TIME ADVANCE & FILTER 96/05/29'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMTFIL, END=90 )
   90    WRITE  ( JFPAR, NMTFIL )
      ENDIF
*
      TFILB = TFIL  /( 1.D0 - TFIL )
      TFILA = 1.D0 - TFILB
*
      DO 1100 IJK = 1, IJDIM*KMAX
         GBU  ( IJK ) = TFILB * GBU ( IJK )
     &                + TFILA * GAU ( IJK )
 1100 CONTINUE 
*
      DO 1200 IJK = 1, IJDIM*KMAX
         GBV  ( IJK ) = TFILB * GBV ( IJK )
     &                + TFILA * GAV ( IJK )
 1200 CONTINUE
*
      DO 1300 IJK = 1, IJDIM*KMAX
         GBT  ( IJK ) = TFILB * GBT ( IJK )
     &                + TFILA * GAT ( IJK )
 1300 CONTINUE 
*
      DO 1400 IJ = 1, IJDIM
         GBPS ( IJ  ) = TFILB * GBPS( IJ  )
     &                + TFILA * GAPS( IJ  )
 1400 CONTINUE
*
      RETURN
*=================================================================
      ENTRY      QADVNC   !! GA->GB time advancing + time filter
     M         ( GBQX  ,
     I           GAQX    )
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ DADVN: TIME ADVANCE & FILTER 96/05/29'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMTFIL, END=190 )
  190    WRITE  ( JFPAR, NMTFIL )
      ENDIF
*
      TFILB = TFIL  /( 1.D0 - TFIL )
      TFILA = 1.D0 - TFILB
*
      DO 1700 IJK = 1, IJDIM*KMAX
         GBQX( IJK ) = TFILB * GBQX( IJK )
     &               + TFILA * GAQX( IJK )
 1700 CONTINUE 
*
      RETURN
*=================================================================
      ENTRY      TFILT     !! time filter
     M         ( GBU   , GBV   , GBT   , GBPS  , GBQ   ,
     I           GAU   , GAV   , GAT   , GAPS  , GAQ     )
*
      TFILA =        TFIL
      TFILB = 1.D0 - TFIL
*
      DO 2100 IJK = 1, IJDIM*KMAX
         GBU  ( IJK ) = TFILB * GBU ( IJK )
     &                + TFILA * GAU ( IJK )
 2100 CONTINUE 
*
      DO 2200 IJK = 1, IJDIM*KMAX
         GBV  ( IJK ) = TFILB * GBV ( IJK )
     &                + TFILA * GAV ( IJK )
 2200 CONTINUE
*
      DO 2300 IJK = 1, IJDIM*KMAX
         GBT  ( IJK ) = TFILB * GBT ( IJK )
     &                + TFILA * GAT ( IJK )
 2300 CONTINUE 
*
      DO 2400 IJ = 1, IJDIM
         GBPS ( IJ  ) = TFILB * GBPS( IJ  )
     &                + TFILA * GAPS( IJ  )
 2400 CONTINUE
*
      DO 2500 LT = 1, NTR
         DO 2510 IJK = 1, IJDIM*KMAX
            GBQ  ( IJK,LT ) = TFILB * GBQ ( IJK,LT )
     &                      + TFILA * GAQ ( IJK,LT )
 2510    CONTINUE 
 2500 CONTINUE 
*
      RETURN
*=================================================================
      ENTRY      QFILT     !! time filter
     M         ( GBQ   ,
     I           GAQ     )
*
      TFILA =        TFIL
      TFILB = 1.D0 - TFIL
*
      DO 3500 LT = 1, NTR
         DO 3510 IJK = 1, IJDIM*KMAX
            GBQ  ( IJK,LT ) = TFILB * GBQ ( IJK,LT )
     &                      + TFILA * GAQ ( IJK,LT )
 3510    CONTINUE 
 3500 CONTINUE 
*
      RETURN
      END
