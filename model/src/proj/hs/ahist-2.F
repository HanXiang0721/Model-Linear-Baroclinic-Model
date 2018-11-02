*  PACKAGE AHIST !! admin: output
*
*   [HIS]  96/06/25(numaguti) AGCM5.4f
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
