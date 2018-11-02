* PACKAGE IGTIO   !! interface for input/output for GTOOL3
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/12/02(numaguti) data larger than area
******************************************************************
      SUBROUTINE GTZRED    !! read in GTOOL3 format
     O         ( GDATA , IEOD  , 
     O           IMAXZ , JMAXZ , KLEVS ,
     O           HITEMD, HTITL , HUNIT , HDSET , 
     O           TIME  , TDUR  , HDFMTD,
     O           HALON , HALAT , HASIG ,
     I           IFILE , HITEM , HDFMT ,
     I           IDIMD , JDIMD , KDIMD  )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zidim.F"                /* number of output data */
#include        "zhdim.F"                /* No. of character */
      INTEGER    IDIMD , JDIMD , KDIMD
*
*   [OUTPUT] 
      REAL*8     GDATA ( IDIMD, JDIMD, KDIMD )  !! data
      INTEGER    IEOD
      INTEGER    IMAXZ
      INTEGER    JMAXZ
      INTEGER    KLEVS
      CHARACTER  HITEMD *(*)
      CHARACTER  HTITL  *(*)                    !! title
      CHARACTER  HUNIT  *(*)                    !! unit
      CHARACTER  HDSET  *(*)                    !! name of dataset
      REAL * 8   TIME                           !! time
      REAL * 8   TDUR                           !! representative time
      CHARACTER  HDFMTD *(*)                    !! data format
      CHARACTER  HALON *(*)
      CHARACTER  HALAT *(*)
      CHARACTER  HASIG *(*)
*
*   [INPUT] 
      INTEGER    IFILE
      CHARACTER  HITEM  *(*)                    !! name for identify
      CHARACTER  HDFMT  *(*)                    !! data format
*
*   [ENTRY INPUT] 
      REAL * 8   TIME1                          !! time
      REAL * 8   TIME2                          !! time
      REAL*8     DMIN
      REAL*8     DMAX
      REAL*8     DIVS
      REAL*8     DIVL
      INTEGER    ISTYPE
      INTEGER    JFILE                          !! output file No.
      INTEGER    IMAXD
      INTEGER    JMAXD
*
*   [INTERNAL WORK] 
      REAL * 8   DDATA ( NGDWRK )
      REAL * 4   SDATA ( NGDWRK )
      COMMON    /COMIGT/ DDATA
      EQUIVALENCE (DDATA,SDATA)
*
*   [INTERNAL SAVE] 
      REAL*8     VMISS
      DATA       VMISS / -999. /
*
      CALL GTZZRD    !! read in GTOOL3 format
     O         ( GDATA , IEOD  , 
     O           IMAXZ , JMAXZ , KLEVS ,
     O           HITEMD, HTITL , HUNIT , HDSET , 
     O           TIME  , TDUR  , HDFMTD,
     O           HALON , HALAT , HASIG ,
     I           IFILE , HITEM , HDFMT ,
     I           IDIMD , JDIMD , KDIMD ,
     W           DDATA , SDATA , NGDWRK          )
*
      RETURN
*=================================================================
      ENTRY      GTZWRT    !! write in GTOOL3 format
     I         ( GDATA ,
     I           HITEM , HTITL , HUNIT , HDSET ,
     I           TIME  , TDUR  , JFILE , HDFMT ,
     I           TIME1 , TIME2 ,
     I           HALON , HALAT , HASIG ,
     I           IMAXD , JMAXD , 
     I           IDIMD , JDIMD , KDIMD          )
*
      CALL     GTZZWT                     !! write
     I         ( GDATA ,
     I           HITEM , HTITL , HUNIT ,
     I           HDSET , VMISS , VMISS ,
     I           VMISS , VMISS , 1     ,
     I           TIME  , TDUR  , JFILE , HDFMT ,
     I           TIME1 , TIME2 ,
     I           HALON , HALAT , HASIG ,
     I           IMAXD , JMAXD , 
     I           IDIMD , JDIMD , KDIMD ,
     W           DDATA , SDATA , NGDWRK         )
*
      RETURN
*=================================================================
      ENTRY      GTZXWT    !! write in GTOOL3 format
     I         ( GDATA ,
     I           HITEM , HTITL , HUNIT ,
     I           HDSET , DMIN  , DMAX  ,
     I           DIVS  , DIVL  , ISTYPE,
     I           TIME  , TDUR  , JFILE , HDFMT ,
     I           TIME1 , TIME2 ,
     I           HALON , HALAT , HASIG ,
     I           IMAXD , JMAXD , 
     I           IDIMD , JDIMD , KDIMD          )
*
      CALL     GTZZWT                     !! write
     I         ( GDATA ,
     I           HITEM , HTITL , HUNIT ,
     I           HDSET , DMIN  , DMAX  ,
     I           DIVS  , DIVL  , ISTYPE,
     I           TIME  , TDUR  , JFILE , HDFMT ,
     I           TIME1 , TIME2 ,
     I           HALON , HALAT , HASIG ,
     I           IMAXD , JMAXD , 
     I           IDIMD , JDIMD , KDIMD ,
     W           DDATA , SDATA , NGDWRK         )
*
      RETURN
      END
******************************************************************
      SUBROUTINE GTZZRD    !! read in GTOOL3 format
     O         ( GDATA , IEOD  , 
     O           IMAXZ , JMAXZ , KLEVS ,
     O           HITEMD, HTITL , HUNIT , HDSET , 
     O           TIME  , TDUR  , HDFMTD,
     O           HALON , HALAT , HASIG ,
     I           IFILE , HITEM , HDFMT ,
     I           IDIMD , JDIMD , KDIMD ,
     W           DDATA , SDATA , NGDWRK         )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zhdim.F"                /* No. of character */
      INTEGER    IDIMD , JDIMD , KDIMD
      INTEGER    NGDWRK
*
*   [OUTPUT] 
      REAL*8     GDATA ( IDIMD, JDIMD, KDIMD )  !! data
      INTEGER    IEOD
      INTEGER    IMAXZ
      INTEGER    JMAXZ
      INTEGER    KLEVS
      CHARACTER  HITEMD *(*)
      CHARACTER  HTITL  *(*)                    !! title
      CHARACTER  HUNIT  *(*)                    !! unit
      CHARACTER  HDSET  *(*)                    !! name of dataset
      REAL * 8   TIME                           !! time
      REAL * 8   TDUR                           !! representative time
      CHARACTER  HDFMTD *(*)                    !! data format
      CHARACTER  HALON *(*)
      CHARACTER  HALAT *(*)
      CHARACTER  HASIG *(*)
*
*   [INPUT] 
      INTEGER    IFILE
      CHARACTER  HITEM  *(*)                    !! name for identify
      CHARACTER  HDFMT  *(*)                    !! data format
*
*   [ENTRY INPUT] 
      REAL * 8   TIME1                          !! time
      REAL * 8   TIME2                          !! time
      REAL*8     DMIN
      REAL*8     DMAX
      REAL*8     DIVS
      REAL*8     DIVL
      INTEGER    ISTYPE
      INTEGER    JFILE                          !! output file No.
      INTEGER    IMAXD
      INTEGER    JMAXD
*
*   [WORK] 
      REAL * 8   DDATA ( NGDWRK )
      REAL * 4   SDATA ( NGDWRK )
*
*   [INTERNAL WORK] 
      INTEGER    I, J, K, IJK, IJKNUM, IERR
      CHARACTER  HEAD  ( NDC )*(NCC)
      CHARACTER  HITEMZ*(NCC)
      CHARACTER  HDFMTZ*(NCC)
      INTEGER    IDATE ( 6 )
      INTEGER    IDATE1( 6 )
      INTEGER    IDATE2( 6 )
      INTEGER    IDATEN( 6 )
      CHARACTER  HTITLZ*(2*NCC)
      CHARACTER  HTIME *20
      INTEGER    IMINY, JMINY, KMINY, IMAXY, JMAXY, KMAXY
      INTEGER    ITDUR
*
*   [INTERNAL SAVE] 
      REAL*8     VMISS
      LOGICAL    OFIRST
#ifdef CODE_IBM
      REAL*8     REALMN
      DATA       REALMN / 1.2E-38 /
#endif
      DATA       VMISS  / -999.  /
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE (6,*) ' @@@ IGTIO: GTOOL3 I/O PACKAGE 96/12/02'
         OFIRST = .FALSE.
      ENDIF
*
 2000 CONTINUE 
      READ ( IFILE, IOSTAT=IEOD ) HEAD
*
      IF ( IEOD .EQ. 0 ) THEN
         HDSET  = HEAD( 2)
         HITEMZ = HEAD( 3)
*
         IF ( HITEM .NE. ' ' .AND. HITEMZ .NE. HITEM ) THEN
            READ ( IFILE )
            GOTO 2000
         ENDIF
         HITEMD = HITEMZ
*
         HTITL  = HEAD(14)//HEAD(15)
         HUNIT  = HEAD(16)
         HALON  = HEAD(29) 
         HALAT  = HEAD(32)
         HASIG  = HEAD(35)
         HDFMTZ = HEAD(38)
         HDFMTD = HDFMTZ
         READ ( HEAD(28), '(I16)' ) ITDUR
         TDUR = ITDUR
*
         READ ( HEAD(30), '(I16)' ) IMINY
         READ ( HEAD(31), '(I16)' ) IMAXY
         READ ( HEAD(33), '(I16)' ) JMINY
         READ ( HEAD(34), '(I16)' ) JMAXY
         READ ( HEAD(36), '(I16)' ) KMINY
         READ ( HEAD(37), '(I16)' ) KMAXY
         IMAXZ  = IMAXY-IMINY+1
         JMAXZ  = JMAXY-JMINY+1
         KLEVS  = KMAXY-KMINY+1
         IJKNUM = IMAXZ*JMAXZ*KLEVS
*
         IF ( IMINY.NE.1 .OR. JMINY.NE.1 .OR. KMINY.NE.1 ) THEN
            WRITE (6,*) ' *** GTZZRD: DIM BASE IS NOT 1 ',
     &                  IMINY, JMINY, KMINY
         ENDIF
*
         IERR = 0
*-------------------------------------------------
*        hujun, 20130630
         write(6,*) 'igtio.F: IERR=0 is done!'
*-------------------------------------------------
         IF ( IMAXZ .GT. IDIMD .OR. 
     &        JMAXZ .GT. JDIMD  ) THEN
            WRITE (6,*) ' ### GTZZRD: AREA TOO SMALL FOR I,J',
     &                  ' DATA:', IMAXZ, JMAXZ, KLEVS,
     &                  ' AREA:', IDIMD, JDIMD, KDIMD
            CALL XABORT( 0 )
            IERR = 1
         ENDIF
*
         IF ( KLEVS .GT. KDIMD ) THEN
            WRITE (6,*) ' ### GTZZRD: AREA TOO SMALL FOR K',
     &                  ' DATA:', KLEVS, ' AREA:', KDIMD
            WRITE (6,*) ' ### DATA TRUNCATED for ', HITEM
         ENDIF
         KLEVS  = MIN( KDIMD, KLEVS )
         IJKNUM = IMAXZ*JMAXZ*KLEVS
*
         IF ( IJKNUM .GT. NGDWRK ) THEN
            WRITE (6,*) ' ### GTZZRD: AREA TOO SMALL ',
     &                  ' DATA:', IJKNUM, ' AREA:', NGDWRK
            CALL XABORT( 0 )
            IERR = 1
         ENDIF
*
         IF ( IERR .NE. 0 ) THEN
            READ ( IFILE, IOSTAT=IEOD ) 
            IEOD = 1
            RETURN
         ENDIF
*
         IF ( HDFMTZ(3:3) .EQ. '8' ) THEN 
            READ ( IFILE, IOSTAT=IEOD ) 
     &             ( DDATA(IJK), IJK=1, IJKNUM )
            IF ( IEOD .EQ. 0 ) THEN
               IJK = 0
               DO 2100 K = 1, KLEVS
                  DO 2100 J = 1, JMAXZ
                     DO 2100 I = 1, IMAXZ
                        IJK = IJK + 1
                        GDATA( I,J,K ) = DDATA( IJK )
 2100          CONTINUE
            ENDIF
         ELSE
            READ ( IFILE, IOSTAT=IEOD ) 
     &             ( SDATA(IJK), IJK=1, IJKNUM )
            IF ( IEOD .EQ. 0 ) THEN
               IJK = 0
               DO 2200 K = 1, KLEVS
                  DO 2200 J = 1, JMAXZ
                     DO 2200 I = 1, IMAXZ
                        IJK = IJK + 1
                        GDATA( I,J,K ) = SDATA( IJK )
 2200          CONTINUE
            ENDIF
         ENDIF
*
         READ ( HEAD(27), 2600 ) (IDATE(I),I=1,6)
 2600    FORMAT( I4.4,I2.2,I2.2,1X,I2.2,I2.2,I2.2 )
         CALL CYH2SS ( TIME, IDATE )
      ENDIF
*
      IEOD = ABS( IEOD )
*-----------------------------------------------
*     hujun, 20130630
      write(6,*) 'igtio.F: IERR=',IEOD
*-----------------------------------------------
      IF ( IEOD .EQ. 0 ) THEN
         CALL CSS2CC( HTIME, TIME )
         WRITE (6,*) ' *** READIN ', HITEMZ, ' TIME= ', HTIME,
     &               ' DSET= ', HDSET
      ENDIF
*     
      RETURN
*=================================================================
      ENTRY      GTZZWT    !! write in GTOOL3 format
     I         ( GDATA ,
     I           HITEM , HTITL , HUNIT ,
     I           HDSET , DMIN  , DMAX  ,
     I           DIVS  , DIVL  , ISTYPE,
     I           TIME  , TDUR  , JFILE , HDFMT ,
     I           TIME1 , TIME2 ,
     I           HALON , HALAT , HASIG ,
     I           IMAXD , JMAXD , 
     I           IDIMD , JDIMD , KDIMD ,
     W           DDATA , SDATA , NGDWRK         )
*
      IF ( OFIRST ) THEN
         WRITE (6,*) ' @@@ IGTIO: GTOOL3 I/O PACKAGE 93/12/07'
         OFIRST = .FALSE.
      ENDIF
*
      IJKNUM = IMAXD*JMAXD*KDIMD
      IF ( IJKNUM .GT. NGDWRK ) THEN
         WRITE (6,*) ' ### GTZZWT: AREA TOO SMALL ',
     &                  ' DATA:', IJKNUM, ' AREA:', NGDWRK
         CALL XABORT( 0 )
      ENDIF
*
      CALL CPERPO ( .FALSE. )
      CALL CSS2YH ( IDATE , TIME  )
      CALL CSS2YH ( IDATE1, TIME1 )
      CALL CSS2YH ( IDATE2, TIME2 )
      CALL CPERPO ( .TRUE.  )
      HTITLZ= HTITL
      CALL YDATE  ( IDATEN )
*
      WRITE ( HEAD( 1), '(I16)' ) 9010
      HEAD( 2) = HDSET
      HEAD( 3) = HITEM
      DO 800 I = 4, 11
         HEAD( I) = ' '
  800 CONTINUE 
      WRITE ( HEAD(12), '(I16)' ) 1
      WRITE ( HEAD(13), '(I16)' ) 1
      HEAD(14) = HTITLZ(1:NCC)
      HEAD(15) = HTITLZ(NCC+1:2*NCC)
      HEAD(16) = HUNIT
      DO 810 I = 17, 24
         HEAD( I) = ' '
  810 CONTINUE 
      WRITE ( HEAD(25), '(I16)' ) NINT(TIME/3600.)
      HEAD(26) = 'HOUR'
      WRITE ( HEAD(27), 820 ) (IDATE(I),I=1,6)
  820 FORMAT( I4.4,I2.2,I2.2,' ',I2.2,I2.2,I2.2 )
      WRITE ( HEAD(28), '(I16)'   ) NINT(TDUR/3600.)
      IF ( HALON .EQ. 'AV' .OR. HALON(1:1) .EQ. '=' ) THEN
         HEAD(29) = ' '
         WRITE ( HEAD(30), '(I16)' ) 1         
         WRITE ( HEAD(31), '(I16)' ) 1         
         HEAD( 4) = 'LON'//HALON
         HEAD(17) = 'LON'//HALON
      ELSE
         HEAD(29) = HALON
         WRITE ( HEAD(30), '(I16)' ) 1         
         WRITE ( HEAD(31), '(I16)' ) IMAXD
      ENDIF
      IF ( HALAT .EQ. 'AV' .OR. HALAT(1:1) .EQ. '=' ) THEN
         HEAD(32) = ' '
         WRITE ( HEAD(33), '(I16)' ) 1         
         WRITE ( HEAD(34), '(I16)' ) 1         
         HEAD( 5) = 'LAT'//HALAT
         HEAD(18) = 'LAT'//HALAT
      ELSE
         HEAD(32) = HALAT
         WRITE ( HEAD(33), '(I16)' ) 1         
         WRITE ( HEAD(34), '(I16)' ) JMAXD
      ENDIF
      IF ( HASIG .EQ. 'AV' .OR. HASIG(1:1) .EQ. '=' ) THEN
         HEAD(35) = ' '
         WRITE ( HEAD(36), '(I16)' ) 1         
         WRITE ( HEAD(37), '(I16)' ) 1         
         HEAD( 6) = 'LEV'//HASIG
         HEAD(19) = 'LEV'//HASIG
      ELSE
         HEAD(35) = HASIG
         WRITE ( HEAD(36), '(I16)' ) 1         
         WRITE ( HEAD(37), '(I16)' ) KDIMD
      ENDIF
      HEAD(38) = HDFMT
      WRITE ( HEAD(39), '(E16.7)' ) VMISS
      WRITE ( HEAD(40), '(E16.7)' ) DMIN
      WRITE ( HEAD(41), '(E16.7)' ) DMAX
      WRITE ( HEAD(42), '(E16.7)' ) DIVS
      WRITE ( HEAD(43), '(E16.7)' ) DIVL
      WRITE ( HEAD(44), '(I16)'   ) ISTYPE
      HEAD(45) = ' '
      WRITE ( HEAD(46), '(I16)'   ) 0
      WRITE ( HEAD(47), '(E16.7)' ) 0.      
      WRITE ( HEAD(48), 820 ) (IDATE1(I),I=1,6)
      WRITE ( HEAD(49), 820 ) (IDATE2(I),I=1,6)
      DO 840 I = 50, 59
         HEAD( I) = ' '
  840 CONTINUE 
      WRITE ( HEAD(60), 820 ) (IDATEN(I),I=1,6)
      HEAD(61) = 'AGCM5.4'
      WRITE ( HEAD(62), 820 ) (IDATEN(I),I=1,6)
      HEAD(63) = 'AGCM5.4'
      WRITE ( HEAD(64), '(I16)' ) IMAXD*JMAXD*KDIMD
*
      HDFMTZ = HDFMT
      IF ( HDFMTZ(3:3) .EQ. '8' ) THEN
         IJK = 0
         DO 1100 K = 1, KDIMD
            DO 1100 J = 1, JMAXD
               DO 1100 I = 1, IMAXD
                  IJK = IJK + 1
                  DDATA( IJK ) = GDATA( I,J,K )
 1100    CONTINUE 
         WRITE ( JFILE ) HEAD
         WRITE ( JFILE ) ( DDATA(IJK), IJK=1, IMAXD*JMAXD*KDIMD )
      ELSE 
         IJK = 0
         DO 1200 K = 1, KDIMD
            DO 1200 J = 1, JMAXD
               DO 1200 I = 1, IMAXD
                  IJK = IJK + 1
                  SDATA( IJK ) = GDATA( I,J,K )
#ifdef CODE_IBM
                  IF ( ABS(SDATA(IJK)) .LT. REALMN ) THEN
                     SDATA( IJK ) = 0.
                  ENDIF
#endif
 1200    CONTINUE
         WRITE ( JFILE ) HEAD
         WRITE ( JFILE ) ( SDATA(IJK), IJK=1, IMAXD*JMAXD*KDIMD )
      ENDIF
*
      CALL CSS2CC( HTIME, TIME )
      WRITE (6,*) ' *** OUTPUT ', HITEM, ' TIME= ', HTIME
*
      RETURN
      END
