* PACKAGE IGZIO   !! i/o interface with format
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*
******************************************************************
      SUBROUTINE GZWRIT    !! write with FORMAT
     I         ( GDATA ,
     I           HITEM , HTITL , HUNIT , HDSET ,
     I           TIME  , TDUR  , JFILE , HDFMT ,
     I           HALON , HALAT , HASIG ,
     I           IMAXD , JMAXD , 
     I           IDIMD , JDIMD , KDIMD           )
*
*   [PARAM] 
#include        "zhdim.F"                /* No. of character */
      INTEGER    IDIMD , JDIMD , KDIMD
      INTEGER    IMAXD , JMAXD
*
*   [INPUT] 
      REAL*8     GDATA ( IDIMD, JDIMD, KDIMD )  !! data
      CHARACTER  HITEM  *(*)                    !! name for identify
      CHARACTER  HTITL  *(*)                    !! title
      CHARACTER  HUNIT  *(*)                    !! unit
      CHARACTER  HDSET  *(*)                    !! name of dataset
      REAL * 8   TIME                           !! time
      REAL * 8   TDUR                           !! representative time
      INTEGER    JFILE                          !! output file No.
      CHARACTER  HDFMT  *(*)                    !! data format
      CHARACTER  HALON *(*)
      CHARACTER  HALAT *(*)
      CHARACTER  HASIG *(*)
*
*   [ENTRY OUTPUT] 
      INTEGER    IEOD
      INTEGER    IMAXZ
      INTEGER    JMAXZ
      INTEGER    KLEVS
      CHARACTER  HITEMD *(*)
      CHARACTER  HDFMTD *(*)                    !! data format
*
*   [ENTRY INPUT] 
      INTEGER    IFILE
*
*   [INTERNAL WORK] 
      INTEGER    I, J, K, IL, KLEVZ
      CHARACTER  HTIME *20
      CHARACTER  HTDUR *20
      CHARACTER  HNUM  *20
      INTEGER    NH
      PARAMETER (NH=80) 
      CHARACTER  HEAD  *(NH)
      CHARACTER  HDFMTZ*(NCC)
      CHARACTER  HITEMZ*(NCC)
      INTEGER    IDATE ( 6 )
*
*   [EXTERNAL FUNC] 
      INTEGER    LENC
*
*   [INTERNAL SAVE] 
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE (6,*) ' @@@ IGZIO: FORMATTED I/O PACKAGE 94/05/20'
         OFIRST = .FALSE.
      ENDIF
*
      CALL CPERPO ( .FALSE. )
      CALL CSS2YH ( IDATE, TIME )
      CALL CPERPO ( .TRUE.  )
      WRITE ( HTIME, 600 ) (IDATE(I),I=1,6)
  600 FORMAT( I4.4,'/',I2.2,'/',I2.2,'-',I2.2,':',I2.2,':',I2.2 )
      WRITE ( HTDUR, '(G12.5)' ) TDUR
      CALL CLADJ(HTDUR)
*
      IF ( HITEM .NE. ' ' ) THEN
         HEAD(1:NH) = HITEM
      ELSE
         HEAD(1:NH) = 'NONE'
      ENDIF
      IL            = LENC(HEAD)+1
      IF ( HDSET .NE. ' ' ) THEN
         HEAD(IL+1:NH) = HDSET
      ELSE
         HEAD(IL+1:NH) = 'NONE'
      ENDIF
      IL            = LENC(HEAD)+1
      HEAD(IL+1:NH) = HTIME
      IL            = LENC(HEAD)+1
      HEAD(IL+1:NH) = HTDUR
      IL            = LENC(HEAD)+1
      HEAD(IL+1:NH) = HDFMT
      IL            = LENC(HEAD)+1
*
      WRITE ( JFILE, '(A)' ) HEAD(1:IL)
*
      WRITE ( HNUM , '(I5)'   ) IMAXD
      CALL CLADJ(HNUM)
      HEAD          = HNUM
      IL            = LENC(HEAD)+1
      WRITE ( HNUM , '(I5)'   ) JMAXD
      CALL CLADJ(HNUM)
      HEAD(IL+1:NH) = HNUM
      IL            = LENC(HEAD)+1
      WRITE ( HNUM , '(I5)'   ) KDIMD
      CALL CLADJ(HNUM)
      HEAD(IL+1:NH) = HNUM
      IL            = LENC(HEAD)+1
      IF ( HALON .NE. ' ' ) THEN
         HEAD(IL+1:NH) = HALON
      ELSE
         HEAD(IL+1:NH) = 'NIL'
      ENDIF
      IL            = LENC(HEAD)+1
      IF ( HALAT .NE. ' ' ) THEN
         HEAD(IL+1:NH) = HALAT
      ELSE
         HEAD(IL+1:NH) = 'NIL'
      ENDIF
      IL            = LENC(HEAD)+1
      IF ( HASIG .NE. ' ' ) THEN
         HEAD(IL+1:NH) = HASIG
      ELSE
         HEAD(IL+1:NH) = 'NIL'
      ENDIF
      IL            = LENC(HEAD)+1
      IF ( HUNIT .NE. ' ' ) THEN
         HEAD(IL+1:NH) = HUNIT
      ELSE
         HEAD(IL+1:NH) = 'NONE'
      ENDIF
      IL            = LENC(HEAD)+1
      IF ( HTITL .NE. ' ' ) THEN
         HEAD(IL+1:NH) = HTITL
      ELSE
         HEAD(IL+1:NH) = 'NONE'
      ENDIF
      IL            = LENC(HEAD)
*
      WRITE ( JFILE, '(A)' ) HEAD(1:IL)
*
      DO 1300 K = 1, KDIMD
         DO 1300 J = 1, JMAXD
            WRITE ( JFILE, HDFMT ) 
     &            ( GDATA(I,J,K), I = 1, IMAXD)
 1300 CONTINUE
*
      WRITE (6,*) ' *** OUTPUT ', HITEM, ' TIME= ', HTIME
*
      RETURN
*==================================================================
      ENTRY      GZREAD    !! read with FORMAT
     O         ( GDATA , IEOD  , 
     O           IMAXZ , JMAXZ , KLEVS ,
     O           HITEMD, HTITL , HUNIT , HDSET , 
     O           TIME  , TDUR  , HDFMTD,
     O           HALON , HALAT , HASIG ,
     I           IFILE , HITEM , HDFMT ,
     I           IDIMD , JDIMD , KDIMD          )
* 
      IF ( OFIRST ) THEN
         WRITE (6,*) ' @@@ IGZIO: FORMATTED I/O PACKAGE 94/05/20'
         OFIRST = .FALSE.
      ENDIF
*
 2000 CONTINUE 
*      
      READ ( IFILE, '(A)', IOSTAT=IEOD ) HEAD
*
      IF ( IEOD .EQ. 0 ) THEN
         CALL RDTKNH( HEAD, HITEMZ )
         CALL RDTKNH( HEAD, HDSET  )
         CALL RDTKNH( HEAD, HTIME  )
         CALL RDTKNR( HEAD, TDUR   )
         CALL RDTKNH( HEAD, HDFMTZ )

         READ ( IFILE, '(A)', IOSTAT=IEOD ) HEAD

         CALL RDTKNI( HEAD, IMAXZ  )
         CALL RDTKNI( HEAD, JMAXZ  )
         CALL RDTKNI( HEAD, KLEVS  )
         CALL RDTKNH( HEAD, HALON  )
         CALL RDTKNH( HEAD, HALAT  )
         CALL RDTKNH( HEAD, HASIG  )
         CALL RDTKNH( HEAD, HUNIT  )
         HTITL = HEAD
         CALL CLADJ(HTITL)
*
         KLEVZ = MIN( KDIMD, KLEVS )
*
         IF ( IMAXZ .GT. IDIMD .OR. JMAXZ .GT. JDIMD ) THEN
            WRITE (6,*) ' ### GTZRED: AREA TOO SMALL ',
     &                  ' DATA:', IMAXZ, JMAXZ,
     &                  ' AREA:', IDIMD, JDIMD
            CALL XABORT( 0 )
            IEOD = 1
            RETURN
         ENDIF
*
         DO 2300 K = 1, KLEVZ
            DO 2300 J = 1, JMAXZ
               READ ( IFILE, HDFMTZ, IOSTAT=IEOD ) 
     &              ( GDATA(I,J,K), I = 1, IMAXZ)
 2300    CONTINUE
         DO 2400 K = KLEVZ+1, KLEVS
            DO 2400 J = 1, JMAXZ
               READ ( IFILE, HDFMTZ, IOSTAT=IEOD ) 
     &              ( GDATA(I,J,KLEVZ), I = 1, IMAXZ) 
 2400    CONTINUE
*
         IF ( HITEM .NE. ' ' .AND. HITEMZ .NE. HITEM ) THEN
            GOTO 2000
         ENDIF
*
         IF ( KLEVS .GT. KDIMD ) THEN
            WRITE (6,*) ' ### GTZRED: AREA TOO SMALL ',
     &                  ' DATA:', KLEVS, 
     &                  ' AREA:', KDIMD
CC          CALL XABORT( 0 )
            IEOD = 1
            RETURN
         ENDIF
*
         HITEMD = HITEMZ
         READ ( HTIME, 2600 ) (IDATE(I),I=1,6)
 2600    FORMAT( I4.4,1X,I2.2,1X,I2.2,1X,I2.2,1X,I2.2,1X,I2.2 )
         CALL CYH2SS ( TIME, IDATE )
*
      ENDIF
*
      IEOD = ABS( IEOD )
      IF ( IEOD .EQ. 0 ) THEN
         WRITE (6,*) ' *** READIN ', HITEMZ, ' TIME= ', HTIME,
     &               ' DSET= ', HDSET
      ENDIF
*     
      RETURN
      END
