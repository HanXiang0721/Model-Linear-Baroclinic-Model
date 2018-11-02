*     PACKAGE ASTRT  !! input initial data
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/04/30(numaguti) TSTART
*         96/06/14(numaguti) TEND
*         96/06/17(numaguti) AGCM5.4f
*         05/05/27(hiro)     rename variables
**********************************************************************
      SUBROUTINE RDSTRT    !! read initial data
     O         ( GFU   , GFV   , GFT   , GFPS  , GFQ   , 
     O           OINIT ,
     M           TSTART                                  )
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc. */
#include        "zhdim.F"                /* # of char. in string */
*
*   [OUTPUT]
      REAL*8     GFU   ( IJDIM, KMAX      )  !! westerly u
      REAL*8     GFV   ( IJDIM, KMAX      )  !! southern wind v
      REAL*8     GFT   ( IJDIM, KMAX      )  !! temperature T
      REAL*8     GFPS  ( IJDIM            )  !! surface pressure
      REAL*8     GFQ   ( IJDIM, KMAX      )  !! humidity q
*        : basic state grid value(t)
      LOGICAL    OINIT                       !! initialized condition ?
*
*   [MODIFY] 
      REAL * 8   TSTART                      !! initial time
*
*   [INTERNAL WORK] 
      INTEGER    IEODA, IEODB
*
      CALL RESET( GFU,  IJDIM*KMAX )
      CALL RESET( GFV,  IJDIM*KMAX )
      CALL RESET( GFT,  IJDIM*KMAX )
      CALL RESET( GFPS, IJDIM      )
      CALL RESET( GFQ,  IJDIM*KMAX )
      CALL RDGDST 
     O         ( GFU   , GFV   , GFT   , GFPS  , GFQ   , IEODA ,
     M           TSTART,
     I           'GA'                                           )
      IF ( IEODA .GE. 1 ) THEN
         WRITE (6,*) 
     &          ' ### RDSTRT: No Initial Data'
         CALL XABORT( 1 )
         RETURN
      ENDIF
      OINIT = .FALSE.
*
      RETURN
      END
**********************************************************************
      SUBROUTINE RDGDST    !! read initial data
     O         ( GDU   , GDV   , GDT   , GDPS  , GDQ   , IEOD  ,
     M           TSTART, 
     I           HCLAS                                          )
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc. */
#include        "zhdim.F"                /* # of char. in string */
*
*   [OUTPUT]
      REAL*8     GDU   ( IJDIM, KMAX      )  !! westerly u
      REAL*8     GDV   ( IJDIM, KMAX      )  !! southern wind v
      REAL*8     GDT   ( IJDIM, KMAX      )  !! temperature T
      REAL*8     GDPS  ( IJDIM            )  !! surface pressure
      REAL*8     GDQ   ( IJDIM, KMAX      )  !! humidity q
      INTEGER    IEOD                        !! end of data flag
*
*   [MODIFY] 
      REAL * 8   TSTART                      !! initial time
*
*   [INPUT] 
      CHARACTER  HCLAS *(*)                  !! GA or GF
*
*   [INTERNAL WORK]
      REAL * 8   TINIT0                !! time of initial data
      REAL * 8   TINIT1                !! time of initial data
      REAL * 8   TIME, TIMER
      INTEGER    IFLSTR, IEODX, KLEVS
      CHARACTER  HDFMT *(NCC)
      CHARACTER  HITEM *(NCC)
      CHARACTER  HITEMD*(NCC)
      CHARACTER  HX *2
*
*   [INTERNAL PARAM]
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
      CALL OPNINI         !! open initial data
     O         ( IFLSTR, TINIT0, TINIT1, HDFMT ,
     I           TSTART, HCLAS                  )
      HX = HCLAS
*
      IEOD = 1
 1100 CONTINUE 
         HITEM = HX//'U'
         CALL GDRDTS
     O         ( GDU   , IEODX , 
     O           HITEMD, TIMER , KLEVS ,
     I           IFLSTR, HITEM , HDFMT ,
     I           IMAX  , JMAX  , TINIT0, TINIT1 ,
     I           IDIM  , JDIM  , KMAX             )
         IF ( IEODX  .GT. 0 ) RETURN
         IF ( TINIT0 .LT. 0 ) TSTART = TIMER
*
         HITEM = HX//'V'
         CALL GDRDTS
     O         ( GDV   , IEODX ,
     O           HITEMD, TIME  , KLEVS ,
     I           IFLSTR, HITEM , HDFMT , 
     I           IMAX  , JMAX  , TIMER , TIMER  ,
     I           IDIM  , JDIM  , KMAX             )
         IF ( IEODX  .GT. 0 ) RETURN
*
         HITEM = HX//'T'
         CALL GDRDTS
     O         ( GDT   , IEODX ,
     O           HITEMD, TIME  , KLEVS ,
     I           IFLSTR, HITEM , HDFMT , 
     I           IMAX  , JMAX  , TIMER , TIMER  ,
     I           IDIM  , JDIM  , KMAX             )
         IF ( IEODX  .GT. 0 ) RETURN
*
         HITEM = HX//'PS'
         CALL GDRDTS
     O         ( GDPS  , IEODX ,
     O           HITEMD, TIME  , KLEVS ,
     I           IFLSTR, HITEM , HDFMT , 
     I           IMAX  , JMAX  , TIMER , TIMER  ,
     I           IDIM  , JDIM  , 1               )
         IF ( IEODX .GT. 0 ) RETURN
*
         HITEM = HX//'Q'
         CALL GDRDTS
     O         ( GDQ   , IEODX ,
     O           HITEMD, TIME  , KLEVS ,
     I           IFLSTR, HITEM , HDFMT , 
     I           IMAX  , JMAX  , TIMER , TIMER  ,
     I           IDIM  , JDIM  , KMAX             )
         IF ( IEODX .GT. 0 ) THEN
            WRITE (6,*) ' ### RDSTRT: No Initial Data For ', HITEM
            RETURN
         ENDIF
         IEOD = 0
*
      IF ( TINIT0 .LT. 0 ) THEN
         GOTO 1100
      ENDIF
*
      RETURN
      END
