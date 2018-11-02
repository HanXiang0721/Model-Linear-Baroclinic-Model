*     PACKAGE ASTRT  !! input initial data
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/04/30(numaguti) TSTART
*         96/06/14(numaguti) TEND
*         96/06/17(numaguti) AGCM5.4f
*         05/05/24(hiro)     for initializing BS
**********************************************************************
      SUBROUTINE RDSTRT    !! read initial data
     O         ( GAU   , GAV   , GAT   , GAPS  , GAQ   , 
     O           OINIT                                   )
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc. */
#include        "zhdim.F"                /* # of char. in string */
*
*   [INPUT/OUTPUT]
      REAL*8     GAU   ( IJDIM, KMAX      )  !! westerly u
      REAL*8     GAV   ( IJDIM, KMAX      )  !! southern wind v
      REAL*8     GAT   ( IJDIM, KMAX      )  !! temperature T
      REAL*8     GAPS  ( IJDIM            )  !! surface pressure
      REAL*8     GAQ   ( IJDIM, KMAX      )  !! humidity q
*        : grid value(t)
*
      LOGICAL    OINIT                       !! initialized condition ?
*
*   [INTERNAL WORK] 
      INTEGER    IFPAR, JFPAR
      INTEGER    I, J, K, IJ, N
      REAL*4     X   ( IMAX,JMAX       )     !! temporary array
      REAL*4     X3  ( IMAX,JMAX,KMAX  )     !! temporary array
      CHARACTER  HEAD( 64 )*16
*
*   [INTERNAL SAVE] 
      CHARACTER  HEADU( 64 )*16
      CHARACTER  HEADV( 64 )*16
      CHARACTER  HEADT( 64 )*16
      CHARACTER  HEADP( 64 )*16
      CHARACTER  HEADQ( 64 )*16
      SAVE       HEADU, HEADV, HEADT, HEADP, HEADQ
*
*   [INTERNAL PARM] 
      CHARACTER  FBS*90         !! initial BS (Gtool3)
      CHARACTER  FBSGT*90       !! modified BS (Gtool3)
      CHARACTER  FBSGR*90       !! modified BS (GrADS)
      LOGICAL    OFIRST
      NAMELIST  /NMBS/    FBS
      NAMELIST  /NMMODBS/ FBSGT, FBSGR
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ RDSTRT: READ BASIC STATE 05/05/24'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMBS, END=190 )
  190    WRITE  ( JFPAR, NMBS )
         READ   ( IFPAR, NMMODBS, END=290 )
  290    WRITE  ( JFPAR, NMMODBS )
      ENDIF
*
      IF( FBS(1:1) .NE. ' ' ) THEN
         OPEN( 20, FILE = FBS, FORM='UNFORMATTED',
     &        STATUS='OLD' ) 
      ELSE
         WRITE( 6, *) '    ### NO BASIC STATE ### '
         WRITE( 6, *) 
         CALL XABORT( 1 )
         RETURN
      ENDIF
*
      CALL RESET( GAU,  IJDIM*KMAX )
      CALL RESET( GAV,  IJDIM*KMAX )
      CALL RESET( GAT,  IJDIM*KMAX )
      CALL RESET( GAPS, IJDIM      )
      CALL RESET( GAQ,  IJDIM*KMAX )
      OINIT = .TRUE.
*
*     U
*
      READ( 20 ) HEAD
cc      READ( 20 ) (((X3(I,J,K),I=1,IMAX),J=1,JMAX),K=1,KMAX)
      READ( 20 ) X3
      DO N = 1, 64
         HEADU( N ) = HEAD( N )
      ENDDO
      DO 100 K = 1, KMAX
         IJ = 0
         DO 110 J = 1, JMAX
            DO 120 I = 1, IMAX
               IJ = IJ + 1
               GAU( IJ,K ) = DBLE( X3( I,J,K ) )
 120        CONTINUE
            IJ = IJ + 1
            GAU( IJ,K ) = DBLE( X3( 1,J,K ) )
 110     CONTINUE
 100  CONTINUE
*
*     V
*
      READ( 20 ) HEAD
      READ( 20 ) X3
      DO N = 1, 64
         HEADV( N ) = HEAD( N )
      ENDDO
      DO 130 K = 1, KMAX
         IJ = 0
         DO 140 J = 1, JMAX
            DO 150 I = 1, IMAX
               IJ = IJ + 1
               GAV( IJ,K ) = DBLE( X3( I,J,K ) )
 150        CONTINUE
            IJ = IJ + 1
            GAV( IJ,K ) = DBLE( X3( 1,J,K ) )
 140     CONTINUE
 130  CONTINUE
*
*     T
*
      READ( 20 ) HEAD
      READ( 20 ) X3
      DO N = 1, 64
         HEADT( N ) = HEAD( N )
      ENDDO
      DO 160 K = 1, KMAX
         IJ = 0
         DO 170 J = 1, JMAX
            DO 180 I = 1, IMAX
               IJ = IJ + 1
               GAT( IJ,K ) = DBLE( X3( I,J,K ) )
 180        CONTINUE
            IJ = IJ + 1
            GAT( IJ,K ) = DBLE( X3( 1,J,K ) )
 170     CONTINUE
 160  CONTINUE
*
*     Ps
*
      READ( 20 ) HEAD
      READ( 20 ) X
      DO N = 1, 64
         HEADP( N ) = HEAD( N )
      ENDDO
      IJ = 0
      DO 200 J = 1, JMAX
         DO 210 I = 1, IMAX
            IJ = IJ + 1
            GAPS( IJ ) = DBLE( X( I,J ) )
 210     CONTINUE
         IJ = IJ + 1
         GAPS( IJ ) = DBLE( X( 1,J ) )
 200  CONTINUE
*
*     Q
*
      READ( 20 ) HEAD
      READ( 20 ) X3
      DO N = 1, 64
         HEADQ( N ) = HEAD( N )
      ENDDO
      DO 220 K = 1, KMAX
         IJ = 0
         DO 230 J = 1, JMAX
            DO 240 I = 1, IMAX
               IJ = IJ + 1
               GAQ( IJ,K ) = DBLE( X3( I,J,K ) )
 240        CONTINUE
            IJ = IJ + 1
            GAQ( IJ,K ) = DBLE( X3( 1,J,K ) )
 230     CONTINUE
 220  CONTINUE
*
      CLOSE( 20 )
*
*
      RETURN
*======================================================================
      ENTRY WMODBS              !! write modified state
     I         ( GAU   , GAV   , GAT   , GAPS  , GAQ    )
*
*
*
      WRITE ( 6,* ) ' @@@ WMODBS: WRITE BASIC STATE 05/05/24'
*
      IF( FBSGT(1:1) .NE. ' ' .AND. FBSGR(1:1) .NE. ' ' ) THEN
         OPEN( 86, FILE = FBSGT, FORM='UNFORMATTED',
     &        STATUS='UNKNOWN' ) 
         OPEN( 87, FILE = FBSGR, FORM='UNFORMATTED',
     &        STATUS='UNKNOWN' ) 
      ELSE
         WRITE( 6, *) '    ### MODIFIED STATE NOT DAMPED ### '
         WRITE( 6, *) 
         RETURN
      ENDIF
*
      DO 300 K = 1, KMAX
         IJ = 0
         DO 310 J = 1, JMAX
            DO 320 I = 1, IMAX
               IJ = IJ + 1
               X ( I,J   ) = SNGL( GAU( IJ,K ) )
               X3( I,J,K ) = SNGL( GAU( IJ,K ) )
 320        CONTINUE
            IJ = IJ + 1
 310     CONTINUE
         WRITE( 87 ) X           !! temperature
 300  CONTINUE
      WRITE( 86 ) HEADU
      WRITE( 86 ) (((X3(I,J,K),I=1,IMAX),J=1,JMAX),K=1,KMAX)
*     
      DO 330 K = 1, KMAX
         IJ = 0
         DO 340 J = 1, JMAX
            DO 350 I = 1, IMAX
               IJ = IJ + 1
               X ( I,J   ) = SNGL( GAV( IJ,K ) )
               X3( I,J,K ) = SNGL( GAV( IJ,K ) )
 350        CONTINUE
            IJ = IJ + 1
 340     CONTINUE
         WRITE( 87 ) X           !! meridional wind
 330  CONTINUE
      WRITE( 86 ) HEADV
      WRITE( 86 ) (((X3(I,J,K),I=1,IMAX),J=1,JMAX),K=1,KMAX)
*     
      DO 360 K = 1, KMAX
         IJ = 0
         DO 370 J = 1, JMAX
            DO 380 I = 1, IMAX
               IJ = IJ + 1
               X ( I,J   ) = SNGL( GAT( IJ,K ) )
               X3( I,J,K ) = SNGL( GAT( IJ,K ) )
 380        CONTINUE
            IJ = IJ + 1
 370     CONTINUE
         WRITE( 87 ) X           !! temperature
 360  CONTINUE
      WRITE( 86 ) HEADT
      WRITE( 86 ) (((X3(I,J,K),I=1,IMAX),J=1,JMAX),K=1,KMAX)
*     
      IJ = 0
      DO 390 J = 1, JMAX
         DO 400 I = 1, IMAX
            IJ = IJ + 1
            X( I,J ) = SNGL( GAPS( IJ ) )
 400     CONTINUE
         IJ = IJ + 1
 390  CONTINUE
      WRITE( 87 ) X             !! surface pressure
      WRITE( 86 ) HEADP
      WRITE( 86 ) ((X(I,J),I=1,IMAX),J=1,JMAX)
*
      DO 410 K = 1, KMAX
         IJ = 0
         DO 420 J = 1, JMAX
            DO 430 I = 1, IMAX
               IJ = IJ + 1
               X ( I,J   ) = SNGL( GAQ( IJ,K ) )
               X3( I,J,K ) = SNGL( GAQ( IJ,K ) )
 430        CONTINUE
            IJ = IJ + 1
 420     CONTINUE
         WRITE( 87 ) X           !! humidity
 410  CONTINUE
      WRITE( 86 ) HEADQ
      WRITE( 86 ) (((X3(I,J,K),I=1,IMAX),J=1,JMAX),K=1,KMAX)
*
      CLOSE( 86 )
      CLOSE( 87 )
*     
      RETURN
      END
**********************************************************************
      SUBROUTINE RDSTRT_OLD    !! read initial data
     O         ( GAU   , GAV   , GAT   , GAPS  , GAQ   , 
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
      OINIT = .TRUE.
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
