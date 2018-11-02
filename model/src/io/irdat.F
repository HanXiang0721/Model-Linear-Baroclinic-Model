* PACKAGE IRDAT  !! i/o  readexternal file
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
************************************************************************
      SUBROUTINE RDDAT     !! read external file
     M         ( GDATA , ISET  ,
     I           TIME  , HITEM , HCORD , DEFALV, OINTPD  )
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zhdim.F"                /* No. of character       */
#include        "zrdim.F"                /* irdat size             */
*
*   [OUTPUT] 
      REAL*8     GDATA  ( * )
      INTEGER    ISET                    !! -1:no assignment,0:default
*                                        !!  1:succeed in reading file
*   [INPUT] 
      REAL * 8   TIME
      CHARACTER  HITEM *(*)
      CHARACTER  HCORD *(*)
      REAL*8     DEFALV
      LOGICAL    OINTPD
*
*   [INTERNAL WORK] 
      INTEGER    IR, IRZ, IFPAR, JFPAR
      INTEGER    IERR, IEOD
      INTEGER    IMAXD , JMAXD , KMAXD
      INTEGER    IDIMS , JDIMS , IDIME , JDIME
      INTEGER    ISIZE
      CHARACTER  HAX*(NCC), HAY*(NCC), HAZ*(NCC)
      INTEGER    INDX
      LOGICAL    OFIRST
*      
*   [INTERNAL PARM] 
      CHARACTER  ITEM  *(NCC)                 !! name of variables
      CHARACTER  ITEMD *(NCC)                 !! name of variables
      CHARACTER  FILE  *(NFILN)               !! file name
      REAL*8     DEFALT                       !! default value
      INTEGER    XSEL                         !! selected type of X
      INTEGER    YSEL                         !! selected type of Y
      INTEGER    ZSEL                         !! selected type of Z
      INTEGER    MSEL                         !! selected mosaic
      INTEGER    START ( 6 )                  !! sart time
      INTEGER    END   ( 6 )                  !! finish time
      CHARACTER  DFMT  *(NCC)                 !! output format
      REAL*8     FACT                         !! factor
      LOGICAL    PIN                          !! output at p-level
      INTEGER    DIUR                         !! interval of diurnal out
      LOGICAL    OINTRP                       !! interpolate ?
      LOGICAL    CLIMAT                       !! climatology ?
      NAMELIST  /NMDATA/ ITEM  , ITEMD , FILE  , DEFALT,
     &                   XSEL  , YSEL  , ZSEL  , MSEL  ,
     &                   START , END   , 
     &                   DFMT  , FACT  ,
     &                   PIN   , DIUR  , OINTRP, CLIMAT
*      
*   [INTERNAL SAVE] 
      REAL*8     GD0    ( NRDIM )
      REAL*8     GD1    ( NRDIM )
      REAL * 8   T0     ( NRDAT )
      REAL * 8   T1     ( NRDAT )
      CHARACTER  HITEMR ( NRDAT )*(NCC)
      CHARACTER  HITEMD ( NRDAT )*(NCC)
      CHARACTER  HCORDR ( NRDAT )*(NCC)
      INTEGER    IMAXDR ( NRDAT )
      INTEGER    JMAXDR ( NRDAT )
      INTEGER    KMAXDR ( NRDAT )
      INTEGER    ISIZER ( NRDAT )
      REAL*8     DEFALR ( NRDAT )
      LOGICAL    OCLIMR ( NRDAT )
      CHARACTER  HDFMTR ( NRDAT )*(NCC)
      REAL*8     FACTR  ( NRDAT )
      LOGICAL    OPINR  ( NRDAT )
      INTEGER    IDIURR ( NRDAT )
      LOGICAL    OINTPR ( NRDAT )
      INTEGER    INDEXR ( NRDAT )
      INTEGER    IFILER ( NRDAT )
      REAL * 8   TSTRTR ( NRDAT )
      REAL * 8   TENDR  ( NRDAT )
      REAL*8     F0     ( NRDAT )
      CHARACTER  HITMRR ( NRITM )*(NCC)
      INTEGER    IRMAX
      INTEGER    IRRMAX
      SAVE       GD0, GD1
      SAVE       T0, T1, HITEMR, HITEMD, HCORDR, IMAXDR, JMAXDR, 
     &           KMAXDR, ISIZER, DEFALR, OCLIMR, HDFMTR, FACTR , 
     &           OPINR , IDIURR, OINTPR, INDEXR, IFILER, TSTRTR, 
     &           TENDR , F0    
      SAVE       HITMRR
      DATA       IRMAX, IRRMAX / 0, 0 /
*
      ISET = -1
*
      IR = 0
      DO 1100 IRZ = 1, IRMAX
         IF ( HITEM .EQ. HITEMR( IRZ ) ) THEN
            IR = IRZ
         ENDIF
 1100 CONTINUE 
*
      IF ( IR .EQ. 0 ) THEN
*
         DO 2000 IRZ = 1, IRRMAX
            IF ( HITEM .EQ. HITMRR( IRZ ) ) THEN
               RETURN
            ENDIF
 2000    CONTINUE 
*
         IF ( IRRMAX .GE. NRITM ) THEN
            WRITE (6,*) ' ### RDDAT: NRITM IS TOO SMALL', IRRMAX
            CALL XABORT(0)
            RETURN
         ENDIF
         IRRMAX = IRRMAX + 1
         HITMRR( IRRMAX ) = HITEM
         WRITE ( 6,* ) ' *** READ DATA CONTROL ', HITEM, ' :'
         CALL   REWNML ( IFPAR , JFPAR )
*
 2100    CONTINUE 
            ITEM   = ' '
            ITEMD  = ' '
            FILE   = ' ' 
            DEFALT = DEFALV
            XSEL   = -1
            YSEL   = -1
            ZSEL   = -1
            CALL IRESET(START,6)
            START(1) = -999
            CALL IRESET(END  ,6)            
            END  (1) = -999
            DFMT   = 'UR4'
            FACT   = 1.
            PIN    = .FALSE.
            DIUR   = 0
            OINTRP = OINTPD
            CLIMAT = .TRUE.
            READ   ( IFPAR, NMDATA, END=4900 )
            IF ( ITEM .EQ. HITEM ) THEN
               WRITE  ( JFPAR, NMDATA )
*
               IF ( IRMAX .GE. NRDAT ) THEN
                  WRITE (6,*) ' ### RDDAT: NUMBER OF RDAT OVER', IRMAX
                  CALL XABORT(0)
                  RETURN
               ENDIF
               IRMAX = IRMAX + 1
               IR    = IRMAX
               HITEMR( IR ) = HITEM
               IF ( ITEMD .EQ. ' ' ) THEN
                  HITEMD( IR ) = HITEM
               ELSE
                  HITEMD( IR ) = ITEMD                  
               ENDIF
               INDEXR( IR ) = 0
*
               CALL IFLOPN
     O         ( IFILER( IR ), IERR   ,
     I           FILE        , 'READ' , DFMT )
*
               IF ( IERR .EQ. 0 ) THEN
                  IF ( IFILER(IR) .GT. 0 ) CALL FREWND( IFILER(IR) )
                  CALL HISIZE
     O            ( IMAXD , JMAXD , KMAXD ,
     O              HAX   , HAY   , HAZ   ,
     I              HCORD , XSEL  , YSEL  , ZSEL  , 
     I              MSEL  , PIN   , DIUR           )
               ELSE
                  WRITE (6,*) ' ### FILE of ', HITEM, ' NOT FOUND :',
     &                  ' DEFAULT ASSUMED '
                  IFILER(IR) = 0
               ENDIF
*
               HCORDR( IR ) = HCORD
               ISIZER( IR ) = IMAXD*JMAXD*KMAXD
               IMAXDR( IR ) = IMAXD
               JMAXDR( IR ) = JMAXD
               KMAXDR( IR ) = KMAXD
               DEFALR( IR ) = DEFALT
               HDFMTR( IR ) = DFMT 
               FACTR ( IR ) = FACT 
               OPINR ( IR ) = PIN
               IDIURR( IR ) = DIUR
               OINTPR( IR ) = OINTRP
               OCLIMR( IR ) = CLIMAT
               T0    ( IR ) = -99
               T1    ( IR ) = -999
               IF ( START(1) .GE. 0 ) THEN
                  CALL CYH2SS ( TSTRTR(IR), START )
               ELSE
                  TSTRTR(IR) = TIME
               ENDIF
               CALL CYH2SS ( TENDR (IR), END )
            ELSE
               GOTO 2100
            ENDIF
*
      ENDIF
*
      IF ( INDEXR( IR ) .EQ. 0  .AND. TIME .GE. TSTRTR( IR ) 
     &                          .AND. IFILER( IR ) .NE. 0    ) THEN
         INDX = 1
         DO 3100 IRZ = 1, IRMAX
            IF      ( INDEXR( IRZ ) .GT. 0 ) THEN
               INDX = MAX( INDX, INDEXR(IRZ)+MAX(ISIZER(IRZ),0) )
            ELSE IF ( INDEXR( IRZ ) .LT. 0 .AND.
     &                ISIZER( IRZ ) .EQ. ISIZER( IR ) ) THEN
               INDX = ABS( INDEXR( IRZ ) )
               ISIZER( IRZ ) = 0
               GOTO 3190
            ENDIF
 3100    CONTINUE 
*
         IF ( INDX+ISIZER(IR) .GT. NRDIM+1 ) THEN
            WRITE (6,*) ' ### RDDAT: READ DATA AREA FULL:',
     &                    INDX+ISIZER(IR)
            CALL XABORT(0)
            RETURN
         ENDIF
*
 3190    CONTINUE 
         INDEXR( IR ) = INDX
*
         OFIRST = .TRUE. 
      ELSE
         OFIRST = .FALSE.
      ENDIF
*
      CALL       HQCSLX( IDIMS, IDIME, HCORDR(IR) )
      CALL       HQCSLY( JDIMS, JDIME, HCORDR(IR) )
      CALL       HQCDMZ( KMAXD,        HCORDR(IR) )
*
      IF ( INDEXR( IR ) .GT. 0 ) THEN 
         IF ( IDIMS .LE. 1 .AND. JDIMS .LE. 1 ) THEN
            CALL RDCON2          !! read external data and interpolation
     O         ( F0 (IR)        , IEOD           ,
     M           GD0(INDEXR(IR)), GD1(INDEXR(IR)), 
     M           T0 (IR)        , T1 (IR)        ,
     I           TIME           , HITEMD(IR)     , IFILER(IR)     , 
     I           DEFALR(IR)     , OCLIMR(IR)     , HDFMTR(IR)     ,
     I           OINTPR(IR)     , FACTR (IR)     , OFIRST         ,
     I           IMAXDR(IR)     , JMAXDR(IR)     , KMAXDR(IR)      )
         ENDIF
*
         CALL RDATIN
     O         ( GDATA          ,
     O           F0 (IR)        , GD0(INDEXR(IR)), GD1(INDEXR(IR)), 
     I           OPINR (IR)     , IDIURR(IR)     , HCORDR(IR)     ,
     I           IMAXDR(IR)     , JMAXDR(IR)     , KMAXDR(IR)      )
*
         IF ( TENDR(IR) .GT. 0 .AND. TIME .GE. TENDR(IR) ) THEN
            INDEXR( IR ) = - INDEXR( IR )
         ENDIF
         IF ( IEOD .NE. 0 .OR.
     &        (T0(IR) .LT. 0.D0 .AND. T1(IR) .LT. 0.D0) ) THEN
            ISET = 0
         ELSE
            ISET = 1
         ENDIF
      ELSE
         ISET = 0
         ISIZE = (IDIME-IDIMS+1)*(JDIME-JDIMS+1)*KMAXD
         CALL SETV( GDATA, DEFALR(IR), ISIZE )
      ENDIF
*
 4900 CONTINUE 
*         
      RETURN
      END
************************************************************************
      SUBROUTINE RDCON2    !! read external data and interpolation
     O         ( FACT0 , IEOD  ,
     M           GDIN0 , GDIN1 , 
     M           T0    , T1    ,
     I           TIME  , HITEM , IFILE , 
     I           DEFVAL, OCLIM , HDFMT , 
     I           OINTRP, FACT  , OFIRST,
     I           IDIMD , JDIMD , KDIMD   )
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zhdim.F"                /* No. of character */
      INTEGER    IDIMD
      INTEGER    JDIMD
      INTEGER    KDIMD
*
*   [OUTPUT] 
      REAL*8     FACT0
      INTEGER    IEOD
*
*   [MODIFY] 
      REAL*8     GDIN0 ( IDIMD*JDIMD*KDIMD )
      REAL*8     GDIN1 ( IDIMD*JDIMD*KDIMD )
      REAL * 8   T0
      REAL * 8   T1
*   [INPUT] 
      REAL * 8   TIME
      CHARACTER  HITEM *(*)
      INTEGER    IFILE
      REAL*8     DEFVAL
      LOGICAL    OCLIM
      CHARACTER  HDFMT *(*)
      LOGICAL    OINTRP
      REAL*8     FACT
      LOGICAL    OFIRST
*
*   [INTERNAL WORK] 
      LOGICAL    ONEXT
      INTEGER    IY0, IY1, IY, IDY0, IDY1, IDY, NDY
      INTEGER    IJK
      INTEGER    IMAXD, JMAXD, KLEVS
      CHARACTER  HITEMD *(NCC)
      INTEGER    IFIRST
      INTEGER    IREWND
*
      REAL*8     DMISS
      DATA       DMISS / -999.D0 /
*
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' *** RDCON2: FIRST READ ITEM= ', HITEM
         CALL SETV ( GDIN1, DEFVAL, IDIMD*JDIMD*KDIMD )
         IFIRST = 2
      ELSE
         IFIRST = 0
      ENDIF
      IREWND = 0
*
C     IMAXD = MIN( IMAX, IDIMD )
C     JMAXD = MIN( JMAX, JDIMD )
      IMAXD = IDIMD
      JMAXD = JDIMD
*
 1100 CONTINUE
*
      ONEXT = .FALSE.
      IF ( IFIRST .GT. 0 ) THEN
         ONEXT  = .TRUE.
         IFIRST = IFIRST - 1
      ELSE  IF ( T1 .NE. T0 ) THEN
         IF ( OCLIM ) THEN
            CALL CPERPO ( .FALSE. )
            CALL CSS2YD ( IY0, IDY0, T0   )
            CALL CSS2YD ( IY1, IDY1, T1   )
            CALL CPERPO ( .TRUE. )
            CALL CSS2YD ( IY , IDY , TIME )
            CALL CDAYYR ( NDY , IY  )
            IF      ( IDY0.LE.IDY .AND. IDY.LE.IDY1 ) THEN
               ONEXT = .FALSE.
               IF ( IDY0 .NE. IDY1 ) THEN
                  FACT0 = DBLE(IDY1-IDY)/DBLE(IDY1-IDY0)
               ELSE
                  FACT0 = 1.
               ENDIF
            ELSE IF ( IDY0 .GT. IDY1 ) THEN
               IF ( IDY0.LE.IDY .AND. IDY.LE.(IDY1+NDY) ) THEN
                  ONEXT = .FALSE.
                  FACT0 = DBLE((IDY1+NDY)-IDY)/DBLE((IDY1+NDY)-IDY0)
               ELSE IF ( IDY0.LE.(IDY+NDY) .AND.
     &                   (IDY+NDY).LE.(IDY1+NDY)   ) THEN
                  ONEXT = .FALSE.
                  FACT0 = DBLE(IDY1+NDY-(IDY+NDY))
     &                   /DBLE(IDY1+NDY-IDY0)
               ELSE
                  ONEXT = .TRUE.
               ENDIF
            ELSE
               ONEXT = .TRUE.
            ENDIF
         ELSE IF ( TIME .GE. T1 ) THEN
            ONEXT = .TRUE.
         ELSE
            FACT0 = ( T1 - TIME )/( T1 - T0 )
         ENDIF
      ELSE
         FACT0 = 0.
      ENDIF
*
      IF ( ONEXT ) THEN
         CALL COPY ( GDIN0, GDIN1, IDIMD*JDIMD*KDIMD )
         T0 = T1
         CALL GDREAD
     O         ( GDIN1 , IEOD  ,
     O           HITEMD, T1    , KLEVS ,
     I           IFILE , HITEM , HDFMT ,
     I           IMAXD , JMAXD , 
     I           IDIMD , JDIMD , KDIMD  )
         IF ( (IEOD .NE. 0) .AND. (IFILE .GT. 0) )THEN
            IF ( IREWND .LE. 0 ) THEN
               IREWND = IREWND + 1
               CALL FREWND ( IFILE )
               CALL GDREAD
     O         ( GDIN1 , IEOD  ,
     O           HITEMD, T1    , KLEVS ,
     I           IFILE , HITEM , HDFMT ,
     I           IMAXD , JMAXD , 
     I           IDIMD , JDIMD , KDIMD  )
            ELSE
               IEOD = 999
               WRITE (6,*) ' ### NO MORE VALID FILE FOR ',
     &                     HITEM, 'TIME =', T0
               CALL COPY ( GDIN1, GDIN0, IDIMD*JDIMD*KDIMD )
               T0    = -1.
               T1    = -1.
               FACT0 =  0.
               RETURN
            ENDIF
         ENDIF
*
         IF ( IEOD .NE. 0 ) THEN
            WRITE (6,*) ' ### FILE of ', HITEM, ' NOT VALID :',
     &                  ' DEFAULT ASSUMED '
            CALL  XABORT(0)
            CALL  SETV ( GDIN1, DEFVAL, IDIMD*JDIMD*KDIMD )
            T0    = -1.
            T1    = -1.
            FACT0 =  0.
         ELSE
            DO 2100 IJK = 1, IDIMD*JDIMD*KDIMD
               IF ( GDIN1( IJK ) .EQ. DMISS ) THEN
                  GDIN1( IJK ) = DEFVAL
               ENDIF
 2100       CONTINUE 
            GOTO 1100
         ENDIF
      ENDIF
*
      IF ( .NOT. OINTRP ) THEN
         IF ( FACT0 .LE. 0.5 ) THEN
            FACT0 = 0.D0
         ELSE
            FACT0 = FACT
         ENDIF
      ELSE
         FACT0 = FACT0 * FACT
      ENDIF
*
      RETURN
      END
************************************************************************
      SUBROUTINE RDATIN       !! data reading and time interpolation
     O         ( GDATA ,
     I           F0    , GIN0  , GIN1  ,
     I           OPIN  , IDIUR , HCORD ,
     I           IMAXDR, JMAXDR, KMAXDR )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zidim.F"                /* number of output data */
#include        "zhdim.F"                /* No. of character */
*
*   [OUTPUT] 
      REAL*8     GDATA  ( * )
*
*   [INPUT] 
      REAL*8     F0
      REAL*8     GIN0    ( * )
      REAL*8     GIN1    ( * )
      LOGICAL    OPIN 
      INTEGER    IDIUR
      CHARACTER  HCORD *(*)
      INTEGER    IMAXDR, JMAXDR, KMAXDR
*
*   [INTERNAL WORK] 
      REAL*8     GDATAW ( NGDWRK )            !! work
      INTEGER    IDIMS , JDIMS
      INTEGER    IDIME , JDIME
      INTEGER    IMAXD , JMAXD , KMAXD, IDIMD, JDIMD
      INTEGER    ISIZE
      INTEGER    KMAXDD
      CHARACTER  HCORDY *(NCC)
*
      IF ( OPIN  .AND. ( HCORD.EQ.'ALEV' .OR. HCORD.EQ.'AMLEV' ) ) THEN
         HCORDY = 'APLEV'
      ELSE
         HCORDY = HCORD
      ENDIF
*
      CALL       HQCSLX( IDIMS, IDIME, HCORDY )
      CALL       HQCSLY( JDIMS, JDIME, HCORDY )
      CALL       HQCDMX( IMAXD, IDIMD, HCORDY )
      CALL       HQCDMY( JMAXD, JDIMD, HCORDY )
      CALL       HQCDMZ( KMAXD,        HCORDY )
      ISIZE = (IDIME-IDIMS+1)*(JDIME-JDIMS+1)*KMAXD
*
      CALL RDATF0
     O         ( GDATAW,
     I           F0    , GIN0  , GIN1  ,     
     I           IMAXDR, JMAXDR, KMAXDR,
     I           IDIMS , JDIMS , IDIME , JDIME  )
*
      KMAXDD = KMAXDR
*
      IF ( IDIUR .NE. 0 .AND. KMAXD .EQ. 1 ) THEN
         CALL    DIURSL      !! diurnal variation
     M         ( GDATAW, IMAXD , JMAXD , KMAXDD,
     I           IDIUR , HCORD , 
     I           IDIMS , JDIMS , IDIME , JDIME  )
      ENDIF
*
      IF ( KMAXDR .LT. KMAXD ) THEN
         CALL ZEXT
     M         ( GDATAW, IMAXD , JMAXD , KMAXDD,
     I           KMAXD ,
     I           IDIMS , JDIMS , IDIME , JDIME  )
      ENDIF
*
      IF ( OPIN  .AND. ( HCORD.EQ.'ALEV' .OR. HCORD.EQ.'AMLEV' ) ) THEN
         CALL P2SINT
     O         ( GDATA , KMAXD ,
     I           GDATAW, KMAXDD, HCORD ,
     I           IDIMS , JDIMS , IDIME , JDIME )
      ELSE
         CALL COPY( GDATA, GDATAW, ISIZE )
      ENDIF
*
      RETURN
      END
