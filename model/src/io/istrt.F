*   PACKAGE ISTRT  !! initial data I/O manager
*
*   [HIS] 96/06/17(numaguti) AGCM5.4f
**********************************************************************
      SUBROUTINE OPNINI    !! open initial data
     O         ( IFILE , TINIT0, TINIT1, HDFMT ,
     I           TSTART, HCLAS                  )
*
*   [PARAM]
#include        "zhdim.F"                /* # of char. in string */
*
*   [OUTPUT]
      INTEGER    IFILE                 !! file ID
      REAL * 8   TINIT0                !! time of initial data min.
      REAL * 8   TINIT1                !! time of initial data max.
      CHARACTER  HDFMT*(NCC)           !! data format
*
*   [INPUT] 
      REAL * 8   TSTART                !! initial time
      CHARACTER  HCLAS *(*)            !! class (ALL,GA,GB,PHY,OCN)

*
*   [INTERNAL WORK]
      REAL * 8   TINITR                !! time of initial data
      REAL * 8   SDTBFR, SDTAFT
      INTEGER    IFPAR, JFPAR, IERR
      INTEGER    ITINIT( 6 )
      CHARACTER  HFILE*(NFILN)
*
*   [INTERNAL PARAM]
      CHARACTER  CLASS*(NCC)           !! class (ALL,GA,GB,PHY,OCN)
      CHARACTER  FILE *(NFILN)         !! input file :initial value
      INTEGER    TINIT ( 6 )           !! time of initial data
      REAL*8     DTBFR                 !! backward acceptable error
      REAL*8     DTAFTR                !! forward acceptable error
      CHARACTER  TUNIT *(NCC)          !! unit time
      CHARACTER  DFMT  *(NCC)          !! data format
      LOGICAL    OFIRST 
      NAMELIST /NMINIT/ CLASS, FILE, TINIT, DTBFR, DTAFTR, TUNIT, DFMT
      DATA       CLASS  / 'ALL' /
      DATA       FILE   / 'ST'  /
      DATA       TINIT  / -999, 1, 1, 0, 0, 0 /
      DATA       DTBFR  / 0  /
      DATA       DTAFTR / 10 /
      DATA       TUNIT  / 'DAY ' /
      DATA       DFMT   / 'UR'   /
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         OFIRST = .FALSE.
         WRITE ( 6,* ) ' @@@ OPNINI : OPEN INITIAL DATA  96/06/17'
      ENDIF
*
      CALL REWNML( IFPAR, JFPAR )
 1100 CONTINUE 
         READ ( IFPAR, NMINIT, END=1900 )
         IF ( CLASS .EQ. 'ALL' .OR. CLASS .EQ. HCLAS ) THEN
            HFILE = FILE
            HDFMT = DFMT
            CALL CYH2SS( TINITR, TINIT ) 
            CALL CXX2SS( SDTBFR, DTBFR,  TUNIT , TINITR )
            CALL CXX2SS( SDTAFT, DTAFTR, TUNIT , TINITR )
            WRITE( JFPAR, NMINIT )
         ENDIF
         GOTO 1100
 1900 CONTINUE 
*
      CALL IFLOPN
     O         ( IFILE , IERR  ,
     I           HFILE , 'READ', HDFMT  )
      IF ( IFILE .GT. 0 ) CALL FREWND( IFILE )
*
      IF ( TINITR .LT. 0 ) THEN
         TINITR = TSTART
      ENDIF
      IF ( TINITR .GE. 0 ) THEN
         CALL CSS2YH( ITINIT, TINITR )
         WRITE ( 6,* ) ' ### TRY TO READ START AT T = ', ITINIT
         TINIT0 = TINITR - SDTBFR
         TINIT1 = TINITR + SDTAFT
      ELSE
         WRITE ( 6,* ) ' ### TRY TO READ LAST DATA'
         TINIT0 = -1.D0
         TINIT1 = -1.D0
      ENDIF
*
      RETURN
      END
**********************************************************************
      SUBROUTINE OPNRST    !! open restart data
     O         ( JFILE , HDFMT ,
     I           TIME  , TSTART, OEND  , HCLAS  )
*
*   [PARAM]
#include        "zhdim.F"                /* # of char. in string */
*
*   [OUTPUT]
      INTEGER    JFILE                 !! file ID ( 0: no output )
      CHARACTER  HDFMT*(NCC)           !! data format
*
*   [INPUT] 
      REAL * 8   TIME                  !! time   
      REAL * 8   TSTART                !! initial time
      LOGICAL    OEND                  !! END of STEP
      CHARACTER  HCLAS *(*)            !! class (ALL,GA,GB,PHY,OCN)
*
*   [INTERNAL WORK] 
      INTEGER    ICLAS, IFPAR, JFPAR, IERR, IC
      CHARACTER  HFILE *(NFILN)
      LOGICAL    OOUT
*
*   [EXTERNAL FUNC] 
      LOGICAL    OINTVL
*
*   [INTERNAL SAVE]
      INTEGER    NCLAS
      PARAMETER (NCLAS=6)
      INTEGER    JFILEI(NCLAS)
      REAL*8     TINTVI(NCLAS)
      CHARACTER  HTUNTI(NCLAS) *(NCC)
      REAL * 8   TSTRTI(NCLAS)
      CHARACTER  HDFMTI(NCLAS) *(NCC) 
      LOGICAL    OVERWI(NCLAS)
      REAL * 8   TPREVI(NCLAS)  
      SAVE       JFILEI, TINTVI, TSTRTI, HDFMTI
*
*   [INTERNAL PARAM]
      CHARACTER  CLASS*(NCC)           !! class (ALL,GA,GB,PHY,OCN)
      CHARACTER  FILE *(NFILN)         !! output file :restart
      REAL*8     TINTV                 !! interval
      CHARACTER  TUNIT  *(NCC)         !! unit
      INTEGER    START  ( 6 )          !! start output
      CHARACTER  DFMT  *(NCC)          !! output format
      LOGICAL    OVERWT                !! overwrite ?
      LOGICAL    OFIRST
      NAMELIST /NMRSTR/ CLASS, FILE, TINTV, TUNIT, START, DFMT, OVERWT
      DATA       CLASS  / 'ALL' /
      DATA       FILE   / 'RST' /
      DATA       TINTV  / 10.   /
      DATA       TUNIT  / 'DAY' /
      DATA       START  / -999, 1, 1, 0, 0, 0 /
      DATA       DFMT   / 'UR8' /
      DATA       OVERWT / .FALSE.  /
      DATA       JFILEI / NCLAS*-1 /
      DATA       OFIRST / .TRUE.   /
*
      IF ( OFIRST ) THEN
         OFIRST = .FALSE.
         WRITE ( 6,* ) ' @@@ OPNRST : OPEN RESTART FILE  96/06/17'
      ENDIF
*
      ICLAS = 6
      IF (HCLAS .EQ. 'GA' ) ICLAS = 1
      IF (HCLAS .EQ. 'GB' ) ICLAS = 2
      IF (HCLAS .EQ. 'PHY') ICLAS = 3
      IF (HCLAS .EQ. 'OCN') ICLAS = 4
*
      IF ( JFILEI( ICLAS ) .LT. 0 ) THEN
         CALL REWNML( IFPAR, JFPAR )
 1100    CONTINUE 
            READ ( IFPAR, NMRSTR, END=1800 )
            IF ( CLASS .EQ. 'ALL' .OR. CLASS .EQ. HCLAS ) THEN
               HFILE = FILE
               HDFMTI(ICLAS) = DFMT
               OVERWI(ICLAS) = OVERWT
               IF ( START(1) .GE. 0 ) THEN
                  CALL CYH2SS( TSTRTI(ICLAS), START ) 
               ELSE
                  TSTRTI(ICLAS) = TSTART
               ENDIF
               TINTVI(ICLAS) = TINTV
               HTUNTI(ICLAS) = TUNIT
               WRITE( JFPAR, NMRSTR )
            ENDIF
            GOTO 1100
 1800    CONTINUE 
*
         CALL IFLOPN
     O         ( JFILEI(ICLAS)  , IERR  ,
     I           HFILE , 'WRITE', HDFMTI(ICLAS) )
         TPREVI(ICLAS) = TSTRTI(ICLAS)
*
         DO 1900 IC = 1, ICLAS-1
            IF ( JFILEI( IC ) .EQ. JFILEI( ICLAS ) ) THEN
               OVERWI(ICLAS) = .FALSE.
            ENDIF
 1900    CONTINUE 
      ENDIF
*
      HDFMT = HDFMTI(ICLAS)
      JFILE = JFILEI(ICLAS)
      IF ( OEND ) THEN
         IF ( TIME .EQ. TPREVI(ICLAS) ) THEN
            JFILE = 0
         ENDIF
      ELSE
         OOUT = OINTVL          !! time step passed ?
     I         ( TIME  , TPREVI(ICLAS), TSTRTI(ICLAS), 
     I           TINTVI(ICLAS), HTUNTI(ICLAS)          )
         IF ( OOUT ) THEN
            TPREVI(ICLAS) = TIME
         ELSE
            JFILE = 0
         ENDIF
      ENDIF
*
      IF ( JFILE .GT. 0 .AND. OVERWI(ICLAS) ) THEN
         CALL FREWND( JFILE )
      ENDIF
*
      RETURN
      END
