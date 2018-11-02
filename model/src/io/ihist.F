* PACKAGE IHIST           !!  history package
*
*   [HIS] 92/06/18(numaguti)
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/12/04(numaguti) PSMLT
*         97/08/01(numaguti) MOSAIC
***********************************************************************
      SUBROUTINE HISTRY    !! output history data
*
*     store and output by time averaging.
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zidim.F"                /* number of output data */
#include        "zhdim.F"                /* No. of character */
#include        "zmdim.F"                /* mosaic */
*
*    [ENTRY INPUT]
      LOGICAL    OALL                         !! unconditional output
      INTEGER    IOGRP                        !! output group
      REAL*8     GDATA  ( * )                 !! data
      CHARACTER  HITEM *(*)                   !! name of variables
      CHARACTER  HTITL *(*)                   !! title
      CHARACTER  HUNIT *(*)                   !! unit
      CHARACTER  HCORD *(*)                   !! axis coordinates
      REAL*8     RFACT                        !! factor
      LOGICAL    OADD                         !! add or set
      REAL*8     RMISSZ                       !! missing value
*   [ENTRY OUTPUT] 
      LOGICAL    OOUTD                        !! output or not
*
*    [INTERNAL WORK]
      CHARACTER  ITEM  *(NCC)                 !! name of variables
      CHARACTER  FILE  *(NFILN)               !! file name
      INTEGER    START ( 6 )                  !! sart time
      INTEGER    END   ( 6 )                  !! finish time
      REAL*8     TINTV                        !! output interval
      REAL*8     TAVRG                        !! accumulation interval
      CHARACTER  TUNIT *(NCC)                 !! time unit
      INTEGER    XSEL                         !! selected type of X
      INTEGER    YSEL                         !! selected type of Y
      INTEGER    ZSEL                         !! selected type of Z
      INTEGER    MSEL                         !! selected type of MOZ
      CHARACTER  DFMT  *(NCC)                 !! output format
      REAL*8     FACT                         !! factor
      LOGICAL    POUT                         !! output at p-level
      LOGICAL    PSMLT                        !! multiply Ps
      INTEGER    DIUR                         !! interval of diurnal out
      LOGICAL    SQUARE                       !! square output
      CHARACTER  HTIMES*20, HTIMEE*20
      CHARACTER  HDSET *(NCC)
*
      REAL * 8   TIME, TIMEA, TIMEM
      INTEGER    IH, ID, IHZ
      INTEGER    IFPAR, JFPAR, JFILE, IERR, IMAXD, JMAXD, KMAXD
      REAL*8     DELTZ, RMULT, DELTY
      LOGICAL    OOUT, OOUTA
      CHARACTER  HAX  *(NCC), HAY  *(NCC), HAZ  *(NCC)
      NAMELIST  /NMHIST/ ITEM  , FILE  , 
     &                   XSEL  , YSEL  , ZSEL  , MSEL  ,
     &                   TINTV , TAVRG , TUNIT , START , END   , 
     &                   DFMT  , FACT  , POUT  , PSMLT , 
     &                   DIUR  , SQUARE
*
*   [EXTERNAL FUNC] 
      LOGICAL    OINTVL
*
*   [INTERNAL SAVE] 
      REAL*8     GOUT   ( NHDIM )            !! accumulation area
      INTEGER    INDUR  ( NHDIM )            !! duration of accumulation
      REAL * 8   TDUR   ( NHIST )            !! duration of accumulation
      REAL * 8   TBEGIN ( NHIST )            !! start accumulation
      INTEGER    INNUM  ( NHIST )            !! accumulated # of times
      REAL * 8   TPREV  ( NHIST )            !! time of last adding
      REAL * 8   TPREVO ( NHIST )            !! time of last output
      REAL*8     TDELT  ( NHIST )            !! add into delta(t)
*
      CHARACTER  HITEMR ( NHISR )*(NCC)      !! name of var.(examined)
      CHARACTER  HITEMH ( NHIST )*(NCC)      !! name of var.(registered)
      CHARACTER  HTITLH ( NHIST )*(NCC*2)    !! discribe variables
      CHARACTER  HUNITH ( NHIST )*(NCC)      !! unit
      CHARACTER  HCORDH ( NHIST )*(NCC)      !! axis coordinates
      INTEGER    IOGRPH ( NHIST )            !! output group
      INTEGER    INDEXH ( NHIST )            !! starting suffix
      INTEGER    ISIZEH ( NHIST )            !! size
      INTEGER    JFILEH ( NHIST )            !! file unit  number
      REAL * 8   TSTRTH ( NHIST )            !! sart time
      REAL * 8   TENDH  ( NHIST )            !! finish time
      REAL*8     TINTVH ( NHIST )            !! output interval
      REAL*8     TAVRGH ( NHIST )            !! duration of accumulation
      CHARACTER  HTUNIH ( NHIST )*(NCC)      !! time unit
      INTEGER    IXSELH ( NHIST )            !! selected type of X
      INTEGER    IYSELH ( NHIST )            !! selected type of Y
      INTEGER    IZSELH ( NHIST )            !! selected type of Z
      INTEGER    IMSELH ( NHIST )            !! selected type of MOZ
      CHARACTER  HDFMTH ( NHIST )*(NCC)      !! data format
      REAL*8     FACTH  ( NHIST )            !! factor
      LOGICAL    OPOUTH ( NHIST )            !! output at P-level
      LOGICAL    OPSMLH ( NHIST )            !! multiply by Ps
      INTEGER    IDIURH ( NHIST )            !! intv. of diurnal var.
      LOGICAL    OSQUH  ( NHIST )            !! square output
      INTEGER    IHMAX                       !! No. of registration
      INTEGER    IHRMAX                      !! # of examined regist.
      REAL*8     RMISS
      REAL*8     RMISS0
      REAL*8     DMISS
      INTEGER    IOGRPZ
      INTEGER    INDX
      LOGICAL    OFIRST, OADDED

      SAVE       GOUT  , INDUR , TDUR  , TBEGIN, INNUM , TPREV ,
     &           TPREVO, TDELT
      SAVE       HITEMR, HTITLH, HUNITH, HCORDH, IOGRPH, INDEXH,
     &           ISIZEH, JFILEH, TSTRTH, TENDH , TINTVH, TAVRGH,
     &           HTUNIH, IXSELH, IYSELH, IZSELH, IMSELH, HDFMTH,
     &           FACTH , OPOUTH, OPSMLH, IDIURH, OSQUH
      SAVE       IHMAX , IHRMAX, RMISS , RMISS0, DMISS , IOGRPZ,
     &           INDX 
*
      DATA       HITEMR / NHISR*' ' /
      DATA       HITEMH / NHIST*' ' /
      DATA       IHMAX, IHRMAX / 0,0 /
      DATA       RMISS  / -999.D0 /
      DATA       RMISS0 / -999.D0 /
      DATA       DMISS  / -999.D0 /
      DATA       IOGRPZ / 1       /
      DATA       INDX   / 1       /
      DATA       OFIRST, OADDED / .TRUE., .FALSE. /
*
*===================================================================
      ENTRY      HISTOU    !! output history data
     I         ( OALL   )
*
      CALL GETTIM ( TIME )
      CALL GETRUN ( HDSET )
*
      DO 4500 IH = 1, IHMAX
         IF ( IOGRPH( IH ) .NE. IOGRPZ .AND. (.NOT.OALL) ) GOTO 4500
         IF ( TIME .LE. TPREVO( IH )                     ) GOTO 4500
*
         OOUT = OINTVL          !! time step passed ?
     I        ( TIME, TPREVO(IH), TSTRTH(IH), TINTVH(IH), HTUNIH(IH) )
*
         IF ( ( OOUT .OR. OALL ) .AND. INNUM( IH ) .GT. 0 ) THEN
*
            IF ( TDUR( IH ) .GT. 0. .AND. TAVRGH( IH ) .GT. 0. ) THEN
               DO 1100 ID = INDEXH( IH ), INDEXH( IH )+ISIZEH( IH )-1
                  IF ( INDUR( ID ) .NE. 0 ) THEN
                     GOUT ( ID ) = GOUT ( ID ) / DBLE( INDUR( ID ) )
                  ELSE
                     GOUT ( ID ) = DMISS
                  ENDIF
 1100          CONTINUE
               TIMEM = ( TBEGIN( IH ) + TIME )/2.
            ELSE
               DO 1200 ID = INDEXH( IH ), INDEXH( IH )+ISIZEH( IH )-1
                  IF ( INDUR( ID ) .EQ. 0 ) THEN
                     GOUT ( ID ) = DMISS
                  ENDIF
 1200          CONTINUE
               TIMEM = TIME
            ENDIF
*
            CALL HISIZE
     O         ( IMAXD , JMAXD , KMAXD ,
     O           HAX   , HAY   , HAZ   ,
     I           HCORDH(IH), IXSELH(IH), IYSELH(IH), IZSELH(IH), 
     I           IMSELH(IH), OPOUTH(IH), IDIURH(IH)             )
*
            IF ( HDFMTH(IH)(1:1) .EQ. '(' ) THEN
               CALL GZWRIT
     I         ( GOUT( INDEXH(IH) ),
     I           HITEMH(IH), HTITLH(IH), HUNITH(IH), HDSET,
     I           TIMEM , TDUR(IH), JFILEH(IH), HDFMTH(IH),
     I           HAX   , HAY   , HAZ   ,
     I           IMAXD , JMAXD , IMAXD , JMAXD, KMAXD   )
            ELSE
               CALL GTZWRT
     I         ( GOUT( INDEXH(IH) ),
     I           HITEMH(IH), HTITLH(IH), HUNITH(IH), HDSET,
     I           TIMEM , TDUR(IH), JFILEH(IH), HDFMTH(IH),
     I           TBEGIN( IH )  , TPREV( IH ) ,
     I           HAX   , HAY   , HAZ   ,
     I           IMAXD , JMAXD , IMAXD , JMAXD, KMAXD    )
            ENDIF
*
            CALL RESET ( GOUT ( INDEXH(IH) ),  ISIZEH(IH) )
            CALL IRESET( INDUR( INDEXH(IH) ),  ISIZEH(IH) )
            TDUR  ( IH ) = 0.
            TBEGIN( IH ) = TIME
            INNUM ( IH ) = 0
            TPREVO( IH ) = TIME
*
         ELSE IF ( INDEXH( IH ) .GT. 0 ) THEN
            CALL    CSSAFT
     O            ( TIMEA ,
     I              TIME  , ABS(TAVRGH(IH)), HTUNIH(IH)  )
*
            OOUTA = OINTVL       !! time step passed ?
     I        ( TIMEA, TPREVO(IH), TSTRTH(IH), TINTVH(IH), HTUNIH(IH) )
*
            IF ( .NOT. OOUTA .OR. TAVRGH(IH) .EQ. 0. ) THEN
               CALL RESET ( GOUT ( INDEXH(IH) ),  ISIZEH(IH) )
               CALL IRESET( INDUR( INDEXH(IH) ),  ISIZEH(IH) )
               TDUR  ( IH ) = 0.
               TBEGIN( IH ) = TIME
               INNUM ( IH ) = 0
            ENDIF
*
         ENDIF
*
         IF ( ( TENDH(IH) .GT. 0    ) .AND. 
     &        ( TIME .GE. TENDH(IH) )       )  THEN
            INDEXH( IH ) = - INDEXH( IH )
         ENDIF
*
 4500 CONTINUE
*
*  NO RETURN -- CONTINUE
*======================================================================
      ENTRY      HISTRP    !!  show entries of history output
*
      IF ( OADDED ) THEN
         OADDED = .FALSE.
*
         WRITE ( 6,* ) ' ####### HISTORY USAGE REPORT ########'
*
         WRITE ( 6,4610 )     'ITEM',     'FILE',
     &                        'XSEL',     'YSEL',    'ZSEL', 
     &                        'START ',   'END   ', 
     &                        'TINTV ',   'TAVRG ',
     &                        'SIZE'
         DO 4600 IH = 1, IHMAX
            CALL CSS2CC ( HTIMES, TSTRTH(IH) )
            CALL CSS2CC ( HTIMEE, TENDH (IH) )
            WRITE ( 6, 4620 ) HITEMH(IH), JFILEH(IH),
     &                        IXSELH(IH), IYSELH(IH), IZSELH(IH),
     &                        HTIMES(1:10), HTIMEE(1:10),
     &                        TINTVH(IH), TAVRGH(IH),
     &                        ISIZEH(IH)
 4600    CONTINUE
         WRITE ( 6,* )  '    TOTAL NUMBER = ', IHMAX,
     &                  '    TOTAL SIZE   = ', INDX-1
      ENDIF
*
 4610 FORMAT(' ',A4,11X, A4, 3(1X,A4), 4(5X,A6),               5X, A4 )
 4620 FORMAT(' ',A16,1X, I2, 3(1X,I4), 2(1X,A10), 2(1X,G10.3), 1X, I8 )
*
      RETURN
*======================================================================
      ENTRY      HISTAX    !!  add into history output
     I         ( GDATA ,
     I           HITEM , RFACT , OADD  )
*
*          < 5. search HITEM and add >
*
      CALL GETTIM ( TIME )
*
      DO 5100 IH = 1, IHMAX
         IF ( ( HITEM .EQ. HITEMH( IH ) ) .AND. 
     &        ( INDEXH( IH ) .GT. 0     )         ) THEN
*
            DELTZ       = TIME - TPREV( IH )
            TPREV( IH ) = TIME
            IF ( DELTZ .NE. 0 ) THEN
               TDELT ( IH ) = NINT  ( DELTZ )
               TDUR  ( IH ) = TDUR  ( IH ) + DELTZ
               INNUM ( IH ) = INNUM ( IH ) + 1
            ENDIF
            IF ( OADD ) THEN
               DELTY = 0.
            ELSE
               DELTY = TDELT( IH )
            ENDIF
*
            IF ( TAVRGH(IH) .GT. 0. ) THEN
               RMULT = TDELT( IH )*RFACT*FACTH( IH )
            ELSE
               RMULT = RFACT*FACTH( IH )
            ENDIF
*
            CALL HSTADD
     M         ( GOUT( INDEXH( IH ) )  , INDUR( INDEXH( IH ) ) ,
     I           GDATA     , RMULT     , RMISS     , 
     I           OPOUTH(IH), OPSMLH(IH), IDIURH(IH), 
     I           DELTY     , OSQUH (IH),
     I           HCORDH(IH), IXSELH(IH), IYSELH(IH), IZSELH(IH),
     I           IMSELH(IH)                                      )
*
         ENDIF
 5100 CONTINUE
*
      RETURN
*======================================================================
      ENTRY      HISTRG    !!  register history output
     O         ( OOUTD ,
     I           HITEM , HTITL , HUNIT , HCORD  )
*
      IF ( OFIRST ) THEN
         OFIRST = .FALSE.
         DO 6000 IH = 1, NHIST
            TPREVO( IH ) = 0.
 6000    CONTINUE
      ENDIF
*
      OOUTD = .FALSE.
*
      DO 6100 IH = 1, IHMAX
         IF ( ( HITEM .EQ. HITEMH( IH ) ) .AND. 
     &        ( INDEXH( IH ) .GT. 0     )         ) THEN
            OOUTD = .TRUE.
            RETURN
         ENDIF
 6100 CONTINUE 
      DO 6200 IH = 1, IHRMAX
         IF ( HITEM .EQ. HITEMR( IH ) ) THEN
            OOUTD = .FALSE.
            RETURN
         ENDIF
 6200 CONTINUE
*
      CALL GETTIM ( TIME )
      IHRMAX = IHRMAX + 1
      IF ( IHRMAX .GT. NHISR ) THEN
         WRITE (6,*) ' ### HISTRG: NUMBER OF HIST ENTRY OVER:',IHRMAX
         CALL XABORT( 0 )
         RETURN
      ENDIF
      HITEMR( IHRMAX ) = HITEM
*
      WRITE ( 6,* ) ' *** HISTORY CONTROL ', HITEM, ' :'
      CALL   REWNML ( IFPAR , JFPAR )
*
 7100 CONTINUE
         CALL HQDEFP
     O     ( FILE  ,
     O       START , END   , TINTV , TAVRG , TUNIT , 
     O       XSEL  , YSEL  , ZSEL  , MSEL  , 
     O       DFMT  , FACT  , POUT  , PSMLT , DIUR   )
         ITEM   = ' '
         SQUARE = .FALSE.
*
         READ   ( IFPAR, NMHIST, END=7190, ERR=7190 )
*
         IF ( ITEM .EQ. HITEM .AND. FILE .NE. ' ' ) THEN
            OOUTD = .TRUE.
            WRITE ( JFPAR, NMHIST )
*
            CALL IFLOPN
     O         ( JFILE , IERR   ,
     I           FILE  , 'WRITE', DFMT )
            IF ( IERR .NE. 0 ) THEN
               WRITE (6,*) ' ### HISTRG: FILE OPEN ERROR :'//FILE
               CALL XABORT( 0 )
               RETURN
            ENDIF
*
            IHMAX = IHMAX + 1
            IF ( IHMAX .GT. NHIST ) THEN
               WRITE (6,*) ' ### HISTRG: NUMBER OF HIST OVER:',IHMAX
               CALL XABORT( 0 )
               RETURN
            ENDIF
*
            CALL HISIZE
     O         ( IMAXD , JMAXD , KMAXD ,
     O           HAX   , HAY   , HAZ   ,
     I           HCORD , XSEL  , YSEL  , ZSEL  , 
     I           MSEL  , POUT  , DIUR            )
*
            INDEXH( IHMAX ) = 0
            HITEMH( IHMAX ) = HITEM
            HTITLH( IHMAX ) = HTITL
            HUNITH( IHMAX ) = HUNIT
            HCORDH( IHMAX ) = HCORD
            IOGRPH( IHMAX ) = IOGRPZ
            ISIZEH( IHMAX ) = IMAXD*JMAXD*KMAXD
            JFILEH( IHMAX ) = JFILE
            IF ( START(1) .GE. 0 ) THEN
               CALL CYH2SS ( TSTRTH(IHMAX), START )
            ELSE
               TSTRTH(IHMAX) = TIME
            ENDIF
            CALL CYH2SS ( TENDH(IHMAX), END )
            TINTVH( IHMAX ) = TINTV 
            TAVRGH( IHMAX ) = TAVRG 
            HTUNIH( IHMAX ) = TUNIT 
            IXSELH( IHMAX ) = XSEL 
            IYSELH( IHMAX ) = YSEL 
            IZSELH( IHMAX ) = ZSEL 
            IMSELH( IHMAX ) = MSEL 
            HDFMTH( IHMAX ) = DFMT 
            FACTH ( IHMAX ) = FACT 
            OPOUTH( IHMAX ) = POUT
            OPSMLH( IHMAX ) = PSMLT
            IDIURH( IHMAX ) = DIUR
            OSQUH ( IHMAX ) = SQUARE
*     
            OADDED = .TRUE.
*
         ENDIF
*
         GOTO 7100
*
 7190 CONTINUE
*
      IF ( .NOT. OOUTD ) RETURN
*
*  NO RETURN -- CONTINUE
*===================================================================
      ENTRY      HISTRZ    !! initialize history output
*
      CALL GETTIM ( TIME )
*
      DO 8600 IH = 1, IHMAX
         CALL    CSSAFT
     O         ( TIMEA ,
     I           TIME  , ABS(TAVRGH(IH)), HTUNIH(IH)  )
*
         IF ( ( INDEXH( IH ) .EQ. 0  ) .AND.
     &        ( TIMEA .GE. TSTRTH(IH) )        ) THEN
*
            INDX = 1
            DO 8610 IHZ = 1, IHMAX
               IF ( INDEXH( IHZ ) .GT. 0 ) THEN
                  INDX = MAX( INDX, INDEXH(IHZ)+ISIZEH(IHZ) )
               ELSE IF ( INDEXH( IHZ ) .LT. 0 .AND.
     &                   ISIZEH( IHZ ) .EQ. ISIZEH( IH ) ) THEN
                  INDX = ABS( INDEXH( IHZ ) )
                  ISIZEH( IHZ ) = 0
                  GOTO 8690
               ENDIF
 8610       CONTINUE 
*
            IF ( INDX+ISIZEH(IH) .GT. NHDIM+1 ) THEN
               WRITE (6,*) ' ### HISTRG: HISTORY AREA FULL:',
     &                    INDX+ISIZEH(IH)
               CALL XABORT(0)
               RETURN
            ENDIF
*
 8690       CONTINUE 
            INDEXH( IH ) = INDX
            CALL RESET ( GOUT ( INDEXH(IH) ), ISIZEH(IH) )
            CALL IRESET( INDUR( INDEXH(IH) ), ISIZEH(IH) )
            TDUR  ( IH ) = 0.
            TBEGIN( IH ) = TIME
            INNUM ( IH ) = 0
            TPREV ( IH ) = TIME
            TPREVO( IH ) = TSTRTH(IH)
            TDELT ( IH ) = 0.
            INDX  = INDX + ISIZEH( IH )
         ENDIF
 8600 CONTINUE 
*
      RETURN
*======================================================================
      ENTRY      HISTGR    !! set history output group
     I         ( IOGRP  )
*
      IOGRPZ = IOGRP
*
      RETURN
*======================================================================
      ENTRY      HISTMS    !! set history missing value
     I         ( RMISSZ )
*
      RMISS = RMISSZ
*
      RETURN
*======================================================================
      ENTRY      HISTMR    !! reset history missing value to default
*
      RMISS = RMISS0
*
      RETURN
      END
*********************************************************************
      SUBROUTINE HISTRT    !! initialize history output
     I         ( IOGRP  )
*   [INPUT] 
      INTEGER    IOGRP
*
      CALL       HISTGR( IOGRP  )
      CALL       HISTRZ
*
      RETURN
      END
*********************************************************************
      SUBROUTINE HISTIN    !!  add/register history output
     I         ( GDATA ,
     I           HITEM , HTITL , HUNIT , HCORD   )
*
*   [INPUT] 
      REAL*8     GDATA  ( * )                 !! data
      CHARACTER  HITEM *(*)                   !! name of variables
      CHARACTER  HTITL *(*)                   !! title
      CHARACTER  HUNIT *(*)                   !! unit
      CHARACTER  HCORD *(*)                   !! axis coordinates
*   [ENTRY INPUT] 
      REAL*8     RFACT
*   [WORK] 
      LOGICAL    OOUTD
      SAVE
*
      CALL       HISTRG      !!  register history output
     I         ( OOUTD ,
     I           HITEM , HTITL , HUNIT , HCORD   )
*
      IF ( OOUTD ) THEN
         CALL       HISTAX      !!  add into history output
     I         ( GDATA ,
     I           HITEM , 1.D0  , .FALSE. )
      ENDIF
*
      RETURN
*===================================================================
      ENTRY      HISTIF    !!  add/register history output with factor
     I         ( GDATA ,
     I           HITEM , HTITL , HUNIT , HCORD  , RFACT )
*
      CALL       HISTRG      !!  register history output
     I         ( OOUTD ,
     I           HITEM , HTITL , HUNIT , HCORD   )
*
      IF ( OOUTD ) THEN
         CALL       HISTAX      !!  add into history output
     I         ( GDATA ,
     I           HITEM , RFACT , .FALSE. )
      ENDIF
*
      RETURN
*===================================================================
      ENTRY      HISTAD    !!  add into history output with factor
     I         ( GDATA ,
     I           HITEM , RFACT )
*
      CALL       HISTAX         !!  add into history output
     I         ( GDATA ,
     I           HITEM , RFACT , .TRUE. )
*
      RETURN
      END
**********************************************************************
      SUBROUTINE HQDEFP    !! default parameter for history output
     O         ( HFILEX,
     O           ISTRTX, IENDX , TINTVX, TAVRGX, HTUNIX, 
     O           IXSELX, IYSELX, IZSELX, IMSELX,
     O           HDFMTX, FACTX , OPOUTX, OPSMLX, IDIURX )
*
*   [PARAM] 
#include        "zcdim.F"                 /* # of grid point & wave */
#include        "zidim.F"                 /* history */
#include        "zhdim.F"                 /* No. of character */
*
*   [OUTPUT]
      CHARACTER  HFILEX *(*)         !! file name
      INTEGER    ISTRTX( 6 )         !! sart time
      INTEGER    IENDX ( 6 )         !! finish time
      REAL*8     TINTVX              !! output interval
      REAL*8     TAVRGX              !! accumulation interval
      CHARACTER  HTUNIX *(*)         !! time unit
      INTEGER    IXSELX              !! selected type of X
      INTEGER    IYSELX              !! selected type of Y
      INTEGER    IZSELX              !! selected type of Z
      INTEGER    IMSELX              !! selected type of MOZ
      CHARACTER  HDFMTX *(*)         !! output format
      REAL*8     FACTX               !! factor
      LOGICAL    OPOUTX              !! output at p-level
      LOGICAL    OPSMLX              !! multiply Ps
      INTEGER    IDIURX              !! intv. of diurnal var.
*
*   [INTERNAL WORK] 
      INTEGER    IFPAR, JFPAR
      INTEGER    ID
*
*   [INTERNAL PARM] 
      CHARACTER  FILE    *(NFILN)    !! file name
      INTEGER    START ( 6 )         !! sart time
      INTEGER    END   ( 6 )         !! finish time
      REAL*8     TINTV               !! output interval
      REAL*8     TAVRG               !! accumulation interval
      CHARACTER  TUNIT   *(NCC)      !! time unit
      INTEGER    XSEL                !! selected type of X
      INTEGER    YSEL                !! selected type of Y
      INTEGER    ZSEL                !! selected type of Z
      INTEGER    MSEL                !! selected type of MOZ
      CHARACTER  DFMT    *(NCC)      !! output format
      REAL*8     FACT                !! factor
      LOGICAL    POUT                !! output at p-level
      LOGICAL    PSMLT               !! multiply Ps
      INTEGER    DIUR                !! intv. of diurnal var.
      LOGICAL    OFIRST
      NAMELIST  /NMHISD/ FILE  ,
     &                   START , END   , TINTV , TAVRG , TUNIT ,
     &                   XSEL  , YSEL  , ZSEL  , MSEL  ,
     &                   DFMT  , FACT  , POUT  , PSMLT , DIUR
      DATA       OFIRST        / .TRUE. /
      DATA       FILE          / ' ' /
      DATA       START         / -999,0,0,0,0,0  /
      DATA       END           / -999,0,0,0,0,0  /
      DATA       TINTV, TAVRG  / 1.D0, 1.D0 /
      DATA       TUNIT         / 'DAY' /
      DATA       XSEL, YSEL, ZSEL / 3*-1 /
      DATA       MSEL          / 0     /
      DATA       DFMT          / 'UR4' /
      DATA       FACT          / 1.D0  /
      DATA       POUT          / .FALSE.  /
      DATA       PSMLT         / .FALSE.  /
      DATA       DIUR          / 0 /
*
*   [ONCE] 
      IF ( OFIRST ) THEN
         WRITE (6,*) ' @@@ IHDEF: HISTORY DEFAULT 93/12/07'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMHISD, END=190 )
  190    WRITE  ( JFPAR, NMHISD )
         CALL   REWNML ( IFPAR , JFPAR )
      ENDIF
*
      HFILEX = FILE  
      DO 1110 ID = 1, 6
         ISTRTX( ID ) = START( ID )
         IENDX ( ID ) = END  ( ID )
 1110 CONTINUE 
      TINTVX = TINTV 
      TAVRGX = TAVRG 
      HTUNIX = TUNIT 
      IXSELX = XSEL  
      IYSELX = YSEL  
      IZSELX = ZSEL  
      IMSELX = MSEL  
      HDFMTX = DFMT  
      FACTX  = FACT  
      OPOUTX = POUT
      OPSMLX = PSMLT
      IDIURX = DIUR
*
      RETURN
      END
