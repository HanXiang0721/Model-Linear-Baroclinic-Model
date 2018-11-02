*  PACKAGE AHIST !! admin: output
*
*   [HIS]  96/06/25(numaguti) AGCM5.4f
***********************************************************************
      SUBROUTINE AHSTIN    !! output standard history 
     I         ( GDU   , GDV   , GDT   , GDPS  , GDQ   ,
     I           GFU   , GFV   , GFT   , GFPS  , GFQ   ,
     I           OANM                                     )
*
*   [PARAM] 
#include        "zcdim.F"
#include        "zpdim.F"                /* physics etc. */
#include        "zhdim.F"
*      
*   [INPUT] 
      REAL*8     GDU   ( IJKDIM )       !! westerly u
      REAL*8     GDV   ( IJKDIM )       !! southern wind v
      REAL*8     GDT   ( IJKDIM )       !! temperature
      REAL*8     GDPS  ( IJDIM  )       !! surface pressure
      REAL*8     GDQ   ( IJKDIM, NTR )  !! humidity q

      REAL*8     GFU   ( IJKDIM )       !! westerly u
      REAL*8     GFV   ( IJKDIM )       !! southern wind v
      REAL*8     GFT   ( IJKDIM )       !! temperature
      REAL*8     GFPS  ( IJDIM  )       !! surface pressure
      REAL*8     GFQ   ( IJKDIM, NTR )  !! humidity q

      LOGICAL    OANM                   !! anomaly output?
*      
*   [WORK]
      REAL*8     GXU   ( IJKDIM )       !! westerly u
      REAL*8     GXV   ( IJKDIM )       !! southern wind v
      REAL*8     GXT   ( IJKDIM )       !! temperature
      REAL*8     GXPS  ( IJDIM  )       !! surface pressure
      REAL*8     GXQ   ( IJKDIM, NTR )  !! humidity q
      INTEGER    IJ, IJK, N
*
*   [INTERNAL WORK] 
      CHARACTER  HITEM *(NCC)
      CHARACTER  HTITL *(NCC)
      INTEGER    LT
*
      IF( OANM ) THEN
         DO 1000 IJK = 1, IJKDIM
            GXU( IJK ) = GDU( IJK ) - GFU( IJK )
            GXV( IJK ) = GDV( IJK ) - GFV( IJK )
            GXT( IJK ) = GDT( IJK ) - GFT( IJK )
 1000    CONTINUE
         DO 1100 IJ = 1, IJDIM
            GXPS( IJ ) = GDPS( IJ ) - GFPS( IJ )
 1100    CONTINUE
         DO 1200 N = 1, NTR
            DO 1200 IJK = 1, IJKDIM
               GXQ( IJK,N ) = GDQ( IJK,N ) - GFQ( IJK,N )
 1200    CONTINUE
      ELSE
         CALL COPY ( GXU  , GDU  , IJKDIM     )
         CALL COPY ( GXV  , GDV  , IJKDIM     )
         CALL COPY ( GXT  , GDT  , IJKDIM     )
         CALL COPY ( GXPS , GDPS , IJDIM      )
         CALL COPY ( GXQ  , GDQ  , IJKDIM*NTR )
      ENDIF
*
      CALL     HISTIN
     I       ( GXU, 'U ', 'u-velocity       ' ,'m/s  ', 'ALEV'   )
      CALL     HISTIN
     I       ( GXV, 'V ', 'v-velocity       ' ,'m/s  ', 'ALEV'   )
      CALL     HISTIN
     I       ( GXT, 'T ', 'temperature      ' ,'K    ', 'ALEV'   )
      CALL     HISTIN
     I       ( GXPS,'PS', 'surface pressure ' ,'mb   ', 'ASFC'   )
      CALL     HISTIN
     I       ( GXQ, 'Q ', 'specific humidity' ,'kg/kg', 'ALEV'   )
      CALL     HISTIN
     I       ( GXQ, 'QT', 'total water mix.',  'kg/kg', 'ALEV'   )
      IF ( ITL .GE. 2 ) THEN
         CALL  HISTIN
     I       ( GXQ(1,ITL), 'QL', 'liquid water' ,'kg/kg', 'ALEV' )
         CALL  HISTAD
     I       ( GXQ(1,ITL), 'QT', 1.D0 )
      ENDIF
      DO 2000 LT = ITL+1, NTR
         CALL CATNUM( HITEM, 'Q',       LT-ITL, '(I2.2)' )
         CALL CATNUM( HTITL, 'tracer ', LT-ITL, '(I2.2)' )
         CALL HISTIN
     I       ( GXQ(1,LT), HITEM, HTITL, 'kg/kg', 'ALEV'  )
 2000 CONTINUE 
*
      RETURN
      END
*********************************************************************
      SUBROUTINE ACHKV     !! valid range monitor
     I         ( GDU   , GDV   , GDT   , GDPS  , GDQ   ,
     I           HLAB  , OALL                            )
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc. */
*
*   [INPUT] 
      REAL*8     GDU   ( IJKDIM )       !! westerly u
      REAL*8     GDV   ( IJKDIM )       !! southern wind v
      REAL*8     GDT   ( IJKDIM )       !! temperature
      REAL*8     GDPS  ( IJDIM  )       !! surface pressure
      REAL*8     GDQ   ( IJKDIM, NTR )  !! humidity q
      CHARACTER  HLAB *(*)
      LOGICAL    OALL
*
      CALL CHKVAL(GDU,     IDIM,JDIM,KMAX, -2.D2, 2.D2,'U' ,HLAB,OALL)
      CALL CHKVAL(GDV,     IDIM,JDIM,KMAX, -2.D2, 2.D2,'V' ,HLAB,OALL)
      CALL CHKVAL(GDT,     IDIM,JDIM,KMAX,  1.D2, 4.D2,'T' ,HLAB,OALL)
      CALL CHKVAL(GDPS,    IDIM,JDIM,1   ,  4.D2,11.D2,'PS',HLAB,OALL)
      CALL CHKVAL(GDQ(1,1),IDIM,JDIM,KMAX,-1.D-2,5.D-2,'Q' ,HLAB,OALL)
      IF ( ITL .GE. 2 ) THEN
        CALL CHKVAL(GDQ(1,ITL),IDIM,JDIM,KMAX,
     &                                    -1.D-2,5.D-2,'QL',HLAB,OALL)
      ENDIF
      IF ( OALL ) THEN
         CALL DIAGP ( GDU,      'GDU',  KMAX,'(G13.5)'  )
         CALL DIAGP ( GDV,      'GDV',  KMAX,'(G13.5)'  )
         CALL DIAGP ( GDT,      'GDT',  KMAX,'(G13.5)'  )
         CALL DIAGP ( GDPS,     'GDPS',   1 ,'(G13.5)'  )
         CALL DIAGP ( GDQ,      'GDQ',  KMAX,'(G13.5)'  )
         IF ( ITL .GE. 2 ) THEN
            CALL DIAGP ( GDQ(1,ITL), 'GDQL', KMAX,'(G13.5)'  )
         ENDIF
      ENDIF
*
      RETURN
      END
