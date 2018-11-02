*     PACKAGE ASTRT  !! input initial data
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/04/30(numaguti) TSTART
*         96/06/14(numaguti) TEND
*         96/06/17(numaguti) AGCM5.4f
**********************************************************************
      SUBROUTINE RDSTRT    !! read initial data
     O         ( GAU   , GAV   , GAT   , GAPS  , GAQ   , 
     O           GBU   , GBV   , GBT   , GBPS  , GBQ   , 
     O           OINIT ,
     M           TSTART                                  )
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc. */
#include        "zhdim.F"                /* # of char. in string */
*
*   [OUTPUT]
      REAL*8     GAU   ( IJDIM, KMAX      )  !! westerly u
      REAL*8     GAV   ( IJDIM, KMAX      )  !! southern wind v
      REAL*8     GAT   ( IJDIM, KMAX      )  !! temperature T
      REAL*8     GAPS  ( IJDIM            )  !! surface pressure
      REAL*8     GAQ   ( IJDIM, KMAX, NTR )  !! humidity q
*        : grid value(t)
*
      REAL*8     GBU   ( IJDIM, KMAX      )  !! westerly u
      REAL*8     GBV   ( IJDIM, KMAX      )  !! southern wind v
      REAL*8     GBT   ( IJDIM, KMAX      )  !! temperature T
      REAL*8     GBPS  ( IJDIM            )  !! surface pressure
      REAL*8     GBQ   ( IJDIM, KMAX, NTR )  !! humidity q
*        : grid value(t-dt)
*
      LOGICAL    OINIT                       !! initialized condition ?
*
*   [MODIFY] 
      REAL * 8   TSTART                      !! initial time
*
*   [INTERNAL WORK] 
      INTEGER    IEODA, IEODB
*
      CALL RESET( GAU,  IJDIM*KMAX )
      CALL RESET( GAV,  IJDIM*KMAX )
      CALL RESET( GAT,  IJDIM*KMAX )
      CALL RESET( GAPS, IJDIM*KMAX )
      CALL RESET( GAQ,  IJDIM*KMAX*NTR )
      CALL RDGDST 
     O         ( GAU   , GAV   , GAT   , GAPS  , GAQ   , IEODA ,
     M           TSTART,
     I           'GA'                                           )
      IF ( IEODA .GE. 1 ) THEN
         WRITE (6,*) 
     &          ' ### RDSTRT: No Initial Data'
         CALL XABORT( 1 )
         RETURN
      ENDIF
*
      CALL RDGDST 
     O         ( GBU   , GBV   , GBT   , GBPS  , GBQ   , IEODB ,
     M           TSTART,
     I           'GB'                                           )
      IF ( IEODB .GE. 1 ) THEN
         WRITE (6,*) 
     &          ' ### RDSTRT: No Initial GB Data. GA=GB Assumed'
         CALL COPY ( GBU   , GAU   , IJDIM*KMAX     )
         CALL COPY ( GBV   , GAV   , IJDIM*KMAX     )
         CALL COPY ( GBT   , GAT   , IJDIM*KMAX     )
         CALL COPY ( GBPS  , GAPS  , IJDIM          )
         CALL COPY ( GBQ   , GAQ   , IJDIM*KMAX*NTR )
         OINIT = .TRUE.
      ELSE
         OINIT = .FALSE.
      ENDIF
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
      REAL*8     GDQ   ( IJDIM, KMAX, NTR )  !! humidity q
      INTEGER    IEOD                        !! end of data flag
*
*   [MODIFY] 
      REAL * 8   TSTART                      !! initial time
*
*   [INPUT] 
      CHARACTER  HCLAS *(*)                  !! GA or GB
*
*   [INTERNAL WORK]
      REAL * 8   TINIT0                !! time of initial data
      REAL * 8   TINIT1                !! time of initial data
      REAL * 8   TIME, TIMER
      INTEGER    IFLSTR, IEODX, KLEVS, LT
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
         DO 3100 LT = 1, NTR
            HITEM = HX//'Q**'
            WRITE ( HITEM(4:5), '(I2.2)' ) LT
            IF ( LT .EQ. ITL ) HITEM = HX//'QL'
            IF ( LT .EQ. 1   ) HITEM = HX//'Q'
            CALL GDRDTS
     O         ( GDQ(1,1,LT)   , IEODX ,
     O           HITEMD, TIME  , KLEVS ,
     I           IFLSTR, HITEM , HDFMT , 
     I           IMAX  , JMAX  , TIMER , TIMER  ,
     I           IDIM  , JDIM  , KMAX             )
            IF ( IEODX .GT. 0 ) THEN
               WRITE (6,*) ' ### RDSTRT: No Initial Data For ', HITEM
               RETURN
            ENDIF
            IEOD = 0
 3100    CONTINUE 
*
      IF ( TINIT0 .LT. 0 ) THEN
         GOTO 1100
      ENDIF
*
      RETURN
      END
***********************************************************************
      SUBROUTINE WRRSTR    !! write restart data
     I         ( GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     I           GBU   , GBV   , GBT   , GBPS  , GBQ   ,
     I           TIME  , TSTART, OEND                    )
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc. */
#include        "zhdim.F"                /* # of char. in string */
*
*   [INPUT]
      REAL*8     GAU   ( IJDIM, KMAX      )  !! westerly u
      REAL*8     GAV   ( IJDIM, KMAX      )  !! southern wind v
      REAL*8     GAT   ( IJDIM, KMAX      )  !! temperature T
      REAL*8     GAPS  ( IJDIM            )  !! surface pressure
      REAL*8     GAQ   ( IJDIM, KMAX, NTR )  !! humidity q
*        : grid value(t)
*
      REAL*8     GBU   ( IJDIM, KMAX      )  !! westerly u
      REAL*8     GBV   ( IJDIM, KMAX      )  !! southern wind v
      REAL*8     GBT   ( IJDIM, KMAX      )  !! temperature T
      REAL*8     GBPS  ( IJDIM            )  !! surface pressure
      REAL*8     GBQ   ( IJDIM, KMAX, NTR )  !! humidity q
*        : grid value(t-dt)
*
      REAL * 8   TIME                        !! time
      REAL * 8   TSTART                      !! start t of calculation
      LOGICAL    OEND                        !! END of STEP
*
      CALL WRGDST
     I         ( GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     I           TIME  , TSTART, OEND  , 'GA'            )
      CALL WRGDST
     I         ( GBU   , GBV   , GBT   , GBPS  , GBQ   ,
     I           TIME  , TSTART, OEND  , 'GB'            )
*
#ifndef OPT_NOPHYSICS
      CALL WRRSTP   !! write physical restart file
     I         ( TIME  , TSTART, OEND   )    
#endif
*
      RETURN
      END
***********************************************************************
      SUBROUTINE WRGDST    !! write restart data (GD)
     I         ( GDU   , GDV   , GDT   , GDPS  , GDQ   ,
     I           TIME  , TSTART, OEND  , HCLAS           )
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc. */
#include        "zhdim.F"                /* # of char. in string */
*
*   [INPUT]
      REAL*8     GDU   ( IJDIM, KMAX      )  !! westerly u
      REAL*8     GDV   ( IJDIM, KMAX      )  !! southern wind v
      REAL*8     GDT   ( IJDIM, KMAX      )  !! temperature T
      REAL*8     GDPS  ( IJDIM            )  !! surface pressure
      REAL*8     GDQ   ( IJDIM, KMAX, NTR )  !! humidity q
      REAL * 8   TIME                        !! time
      REAL * 8   TSTART                      !! start t of calculation
      LOGICAL    OEND                        !! END of STEP
      CHARACTER  HCLAS *(*)
*
*   [INTERNAL WORK] 
      INTEGER    JFLRST                 !! output file
      CHARACTER  HDFMT  *(NCC)          !! output format
      CHARACTER  HITEM  *(NCC)
      CHARACTER  HTITL  *(NCC*2)
      CHARACTER  HX     *2
      INTEGER    LT
*
*    [INTERNAL PARAM]
      REAL * 8   TDUR
      DATA       TDUR   / 1.D0  /
*
      CALL OPNRST         !! open restart data
     O         ( JFLRST, HDFMT ,
     I           TIME  , TSTART, OEND  , HCLAS  )
      IF ( JFLRST .LE. 0 ) RETURN
*
      HX = HCLAS
*
      HITEM = HX//'U'
      CALL GDWRIT
     I        ( GDU   ,
     I          HITEM , 'u-velocity'  ,'m/s'  ,
     I          TIME  , TDUR  , JFLRST, HDFMT ,
     I          IMAX  , JMAX  , 
     I          IDIM  , JDIM  , KMAX  , 'ALEV'  )
*
      HITEM = HX//'V'
      CALL GDWRIT
     I        ( GDV   ,
     I          HITEM , 'v-velocity'  , 'm/s' ,
     I          TIME  , TDUR  , JFLRST, HDFMT ,
     I          IMAX  , JMAX  ,
     I          IDIM  , JDIM  , KMAX  , 'ALEV'  )
*
      HITEM = HX//'T'
      CALL GDWRIT
     I        ( GDT   ,
     I          HITEM , 'temperature' ,'K'  ,
     I          TIME  , TDUR  , JFLRST, HDFMT ,
     I          IMAX  , JMAX  ,
     I          IDIM  , JDIM  , KMAX  , 'ALEV'  )
*
      HITEM = HX//'PS'
      CALL GDWRIT
     I        ( GDPS  ,
     I          HITEM , 'surface pressure','hPa',
     I          TIME  , TDUR  , JFLRST, HDFMT ,
     I          IMAX  , JMAX  ,
     I          IDIM  , JDIM  , 1     , 'ASFC'  )
*
      DO 3100 LT = 1, NTR
         HITEM = HX//'Q**'
         WRITE ( HITEM(4:5),   '(I2.2)' ) LT
         IF ( LT .EQ. ITL ) HITEM = HX//'QL'
         IF ( LT .EQ. 1   ) HITEM = HX//'Q'
         HTITL = 'tracer No.**'
         WRITE ( HTITL(11:12), '(I2.2)' ) LT
         IF ( LT .EQ. ITL ) HTITL = 'specific cloud water'
         IF ( LT .EQ. 1   ) HTITL = 'specific humidity'
         CALL GDWRIT
     I        ( GDQ(1,1,LT),
     I          HITEM , HTITL , 'kg/kg' ,
     I          TIME  , TDUR  , JFLRST, HDFMT ,
     I          IMAX  , JMAX  ,
     I          IDIM  , JDIM  , KMAX  , 'ALEV'  )
 3100 CONTINUE 
*
      RETURN
      END
