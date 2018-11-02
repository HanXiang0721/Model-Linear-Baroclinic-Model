* PACKAGE IRWGD !! i/o  input routine
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*   [HIS] 96/06/17(numaguti) AGCM5.4f
*********************************************************************
      SUBROUTINE GDREAD    !! read data
     O         ( GDATA , IEOD  ,
     O           HITEMD, TIME  , KLEVS ,
     I           IFILE , HITEM , HDFMT ,
     I           IMAXD , JMAXD , 
     I           IDIMD , JDIMD , KDIMD  )
*
*   [PARAM] 
      INTEGER    IDIMD
      INTEGER    JDIMD
      INTEGER    KDIMD
#include        "zhdim.F"                   /* No. of character */
*
*    [OUTPUT]    
      REAL*8     GDATA ( IDIMD, JDIMD, KDIMD ) !! data
      INTEGER    IEOD                          !! exist:0, no data:1
      CHARACTER  HITEMD *(*)
      REAL * 8   TIME                          !! time
      INTEGER    KLEVS
*
*    [INPUT]    
      INTEGER    IFILE                       !! file unit  number
      CHARACTER  HITEM *(*)                  !! selected value of ITEM
      CHARACTER  HDFMT  *(*)                 !! data format
      INTEGER    IMAXD, JMAXD
*
*    [INTERNAL WORK]
      CHARACTER  HTITL  *(NCC*2)                !! title
      CHARACTER  HUNIT  *(NCC)                  !! unit
      CHARACTER  HDSET  *(NCC)                  !! name of dataset
      REAL * 8   TDUR                           !! representative time
*
      CALL       GDREDX    !! read data
     O         ( GDATA , IEOD  ,
     O           HITEMD, HTITL , HUNIT , HDSET , 
     O           TIME  , TDUR  , KLEVS ,
     I           IFILE , HITEM , HDFMT ,
     I           IMAXD , JMAXD , 
     I           IDIMD , JDIMD , KDIMD           )
*
      RETURN
      END
*********************************************************************
      SUBROUTINE GDRDTS    !! read data & time select
     O         ( GDATA , IEOD  ,
     O           HITEMD, TIME  , KLEVS ,
     I           IFILE , HITEM , HDFMT ,
     I           IMAXD , JMAXD , TSEL0 , TSEL1 , 
     I           IDIMD , JDIMD , KDIMD          )
*
*   [PARAM] 
      INTEGER    IDIMD
      INTEGER    JDIMD
      INTEGER    KDIMD
#include        "zhdim.F"                   /* No. of character */
*
*    [OUTPUT]    
      REAL*8     GDATA ( IDIMD, JDIMD, KDIMD ) !! data
      INTEGER    IEOD                          !! exist:0, no data:1
      CHARACTER  HITEMD *(*)
      REAL * 8   TIME                          !! time
      INTEGER    KLEVS
*
*    [INPUT]    
      INTEGER    IFILE                       !! file unit  number
      CHARACTER  HITEM *(*)                  !! selected value of ITEM
      CHARACTER  HDFMT  *(*)                 !! data format
      INTEGER    IMAXD, JMAXD
      REAL * 8   TSEL0, TSEL1                !! select time
*
*    [INTERNAL WORK]
      CHARACTER  HTITL  *(NCC*2)                !! title
      CHARACTER  HUNIT  *(NCC)                  !! unit
      CHARACTER  HDSET  *(NCC)                  !! name of dataset
      REAL * 8   TDUR                           !! representative time
*
 1100 CONTINUE 
         CALL   GDREDX         !! read data
     O         ( GDATA , IEOD  ,
     O           HITEMD, HTITL , HUNIT , HDSET , 
     O           TIME  , TDUR  , KLEVS ,
     I           IFILE , HITEM , HDFMT ,
     I           IMAXD , JMAXD , 
     I           IDIMD , JDIMD , KDIMD           )
         IF ( IEOD .EQ. 0  ) THEN
            IF (    ((TSEL0.GE.0).AND.(TIME.LT.TSEL0))
     &          .OR.((TSEL1.GE.0).AND.(TIME.GT.TSEL1)) ) THEN
                     GOTO 1100
            ENDIF
         ENDIF
*
      RETURN
      END
*********************************************************************
      SUBROUTINE GDREDX    !! read data
     O         ( GDATA , IEOD  ,
     O           HITEMD, HTITL , HUNIT , HDSET , 
     O           TIME  , TDUR  , KLEVS ,
     I           IFILE , HITEM , HDFMT ,
     I           IMAXD , JMAXD , 
     I           IDIMD , JDIMD , KDIMD           )
*
*   [PARAM] 
      INTEGER    IDIMD
      INTEGER    JDIMD
      INTEGER    KDIMD
#include        "zhdim.F"                   /* No. of character */
*
*    [OUTPUT]    
      REAL*8     GDATA ( IDIMD, JDIMD, KDIMD ) !! data
      INTEGER    IEOD                          !! exist:0, no data:1
      CHARACTER  HITEMD *(*)
      CHARACTER  HTITL  *(NCC*2)                !! title
      CHARACTER  HUNIT  *(NCC)                  !! unit
      CHARACTER  HDSET  *(NCC)                  !! name of dataset
      REAL * 8   TIME                           !! time
      REAL * 8   TDUR                           !! representative time
      INTEGER    KLEVS
*
*    [INPUT]    
      INTEGER    IFILE                       !! file unit  number
      CHARACTER  HITEM *(*)                  !! selected value of ITEM
      CHARACTER  HDFMT  *(*)                 !! data format
      INTEGER    IMAXD, JMAXD
*
*    [INTERNAL WORK]
      INTEGER    I, J, K
      INTEGER    IMAXZ, JMAXZ
      CHARACTER  HDFMTD *(NCC)                  !! data format
      CHARACTER  HAX   *(NCC)
      CHARACTER  HAY   *(NCC)
      CHARACTER  HAZ   *(NCC)
*
      IF ( IFILE .EQ. 0 ) THEN
         IEOD = 1
         RETURN
      ENDIF
*
      IF ( HDFMT(1:1) .EQ. '(' ) THEN
         CALL GZREAD            !! read
     O         ( GDATA , IEOD  , 
     O           IMAXZ , JMAXZ , KLEVS ,
     O           HITEMD, HTITL , HUNIT , HDSET , 
     O           TIME  , TDUR  , HDFMTD,
     O           HAX   , HAY   , HAZ   ,
     I           IFILE , HITEM , HDFMT ,
     I           IDIMD , JDIMD , KDIMD  )
      ELSE
         CALL GTZRED            !! read (GTOOL3)
     O         ( GDATA , IEOD  , 
     O           IMAXZ , JMAXZ , KLEVS ,
     O           HITEMD, HTITL , HUNIT , HDSET , 
     O           TIME  , TDUR  , HDFMTD,
     O           HAX   , HAY   , HAZ   ,
     I           IFILE , HITEM , HDFMT ,
     I           IDIMD , JDIMD , KDIMD  )
      ENDIF
*
      IF ( IEOD .EQ. 0 ) THEN
         IF ( IMAXZ.NE.IMAXD .OR. JMAXZ.NE.JMAXD ) THEN
            WRITE (6,*) ' ### GDREAD: RESOLUTION MISMATCH: ', 
     &                  HITEMD,
     &                  ' DATA:', IMAXZ, JMAXZ, 
     &                  ' REQ:',  IMAXD, JMAXD
            CALL XABORT( 1 )
            IEOD = 1
            RETURN
         ENDIF
*
         DO 3100 K = 1, KLEVS
            DO 3130 I = IMAXD+1, IDIMD
               DO 3130 J = 1, JDIMD
                  GDATA( I,J,K ) = GDATA( 1,J,K )
 3130       CONTINUE
            DO 3140 J = JMAXD+1, JDIMD
               DO 3140 I = 1, IDIMD
                  GDATA( I,J,K ) = GDATA( I,1,K )
 3140       CONTINUE
 3100    CONTINUE
      ENDIF
*
      RETURN
      END
*********************************************************************
      SUBROUTINE GDWRIT    !! write data
     I         ( GDATA ,
     I           HITEM , HTITL , HUNIT ,
     I           TIME  , TDUR  , JFILE , HDFMT ,
     I           IMAXD , JMAXD ,
     I           IDIMD , JDIMD , KDIMD , HCORD  )
*
*   [PARAM] 
      INTEGER    IDIMD
      INTEGER    JDIMD
      INTEGER    KDIMD
#include        "zhdim.F"                   /* No. of character */
*
*    [INPUT]     
      REAL*8     GDATA ( IDIMD, JDIMD, KDIMD )  !! data
      CHARACTER  HITEM  *(*)                    !! name for identify
      CHARACTER  HTITL  *(*)                    !! title
      CHARACTER  HUNIT  *(*)                    !! unit
      REAL * 8   TIME                           !! time
      REAL * 8   TDUR                           !! representative time
      INTEGER    JFILE                          !! output file No.
      CHARACTER  HDFMT  *(*)                    !! data format
      REAL * 8   TIME1                          !! time
      REAL * 8   TIME2                          !! time
      CHARACTER  HCORD  *(*)                    !! axis coordinates
      INTEGER    IMAXD, JMAXD
*
*    [INTERNAL WORK]
      CHARACTER  HAX   *(NCC)
      CHARACTER  HAY   *(NCC)
      CHARACTER  HAZ   *(NCC)
      CHARACTER  HDSET *(NCC)
*
      IF ( JFILE .EQ. 0 ) THEN
         RETURN
      ENDIF
*
      CALL GETRUN( HDSET )
*
      CALL HQCNMX( HAX, HCORD )
      CALL HQCNMY( HAY, HCORD )
      CALL HQCNMZ( HAZ, HCORD )
*
      IF ( HDFMT(1:1) .EQ. '(' ) THEN
         CALL GZWRIT            !! write
     I         ( GDATA ,
     I           HITEM , HTITL , HUNIT , HDSET ,
     I           TIME  , TDUR  , JFILE , HDFMT ,
     I           HAX   , HAY   , HAZ   ,
     I           IMAXD , JMAXD , 
     I           IDIMD , JDIMD , KDIMD          )
      ELSE
         CALL GTZWRT            !! write (GTOOL3)
     I         ( GDATA ,
     I           HITEM , HTITL , HUNIT , HDSET ,
     I           TIME  , TDUR  , JFILE , HDFMT ,
     I           TIME  , TIME  ,
     I           HAX   , HAY   , HAZ   ,
     I           IMAXD , JMAXD , 
     I           IDIMD , JDIMD , KDIMD          )
      ENDIF
*
      RETURN
      END
