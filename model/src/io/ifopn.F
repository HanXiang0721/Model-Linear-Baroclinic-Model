* PACKAGE IFOPN  !! open file
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*
************************************************************************
      SUBROUTINE IFLOPN    !!  open file
     O         ( IFILE , IERR  ,
     I           HFILE , HACT  , HDFMT  )
*     
*   [PARAM] 
#include        "zhdim.F"                /* No. of character */
*
*   [OUTPUT] 
      INTEGER    IFILE                       !! unit number
      INTEGER    IERR                        !! error not equal 0
*
*   [INPUT] 
      CHARACTER  HFILE *(*)                  !! file name
      CHARACTER  HACT  *(*)                  !! read/write
      CHARACTER  HDFMT *(*)                  !! format
*
*   [INTERNAL WORK] 
      INTEGER    IFILED                      !! specified unit number
      CHARACTER  HFILX *(NFILN)              !! file name
      CHARACTER  HFORM *(NCC)                !! format
      LOGICAL    OEXIST, OPEND, OPENDX
      INTEGER    ICOUNT
      INTEGER    IFPAR, JFPAR
*
#ifdef CODE_ENDIAN
      INTEGER    XFINQN, XFINQU, IOPND
#endif      
*
*   [INTERNAL SAVE] 
      INTEGER    IFILMN, IFILMX
      LOGICAL    OFIRST
      SAVE       IFILMN, IFILMX, OFIRST
      NAMELIST  /NMFILE/ IFILMN, IFILMX
      DATA       IFILMN, IFILMX / 20, 99 /
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE (6,*) ' @@@ IFOPN: OPEN FILE 93/12/14'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMFILE, END=190 )
  190    WRITE  ( JFPAR, NMFILE )         
      ENDIF
*
      IFILED = -1
*
      HFILX = HFILE
      IF ( ( HFILX(1:1).GE.'0' ).AND.( HFILX(1:1).LE.'9' ) ) THEN
         IF ( ( HFILX(2:2).GE.'0' ).AND.( HFILX(2:2).LE.'9' ) ) THEN 
            IF ( HFILX(3:3) .EQ. ':' .OR. HFILX(3:3) .EQ. ' ' ) THEN
               READ ( HFILX(1:2), '(I2)' ) IFILED
               HFILX = HFILX(4:NFILN)
            ENDIF
         ELSE IF ( HFILX(2:2) .EQ. ':' .OR. HFILX(2:2) .EQ. ' ' ) THEN
            READ ( HFILX(1:1), '(I2)' ) IFILED
               HFILX = HFILX(3:NFILN)
         ENDIF
      ELSE IF ( HFILX .EQ. 'CON' ) THEN
         IERR = 0
         IF ( HACT .EQ. 'READ' ) THEN
            IFILE = 5
         ELSE
            IFILE = 6
         ENDIF
         RETURN
      ELSE IF ( HFILX .EQ. ' ' ) THEN
         IERR = 0
         IFILE = 0
         RETURN
      ENDIF
*
      IF ( HFILX .EQ. ' ' ) THEN
         IERR = 0
         IFILE = IFILED
         WRITE ( 6,* ) ' *** FILE CONNECTED UNIT = ', IFILE
         RETURN
      ENDIF
*
      CALL MKFILN ( HFILX  )                 !! subst. fname w/ env.var.
*
      IF ( HDFMT(1:1) .EQ. '(' ) THEN 
         HFORM = 'FORMATTED'
      ELSE
         HFORM = 'UNFORMATTED'
      ENDIF
*
#ifdef CODE_ENDIAN
      IF ( HFORM .EQ. 'UNFORMATTED' ) THEN
         OPEND=.FALSE.
         IOPND=XFINQN(HFILE,IFILE)
         IF ( IOPND .NE. 0 ) OPEND=.TRUE.
      ELSE
#endif
         INQUIRE ( FILE=HFILX  , IOSTAT=IERR, EXIST=OEXIST,
     &             OPENED=OPEND, NUMBER=IFILE               )
*
         IF ( .NOT. OEXIST ) THEN
            IF      ( HACT.EQ.'READ' ) THEN
               WRITE (6,*) ' ### IFOPN: FILE '//HFILX//' NOT FOUND'
               IERR  = 1
               IFILE = 0
               RETURN
            ELSE IF ( HACT.EQ.'TEST' ) THEN
               IERR  = 1
               IFILE = 0
               RETURN
            ENDIF
         ENDIF
#ifdef CODE_ENDIAN
      ENDIF
#endif
      IF ( .NOT. OPEND ) THEN
         IFILE  = IFILED
         IF ( IFILE .LT. 0 ) IFILE = IFILMN
         ICOUNT = 0
 2100    CONTINUE
            ICOUNT = ICOUNT + 1
#ifdef CODE_ENDIAN
            IOPND=XFINQU(IFILE)
            IF ( IOPND .NE. 0 ) THEN
#else
            INQUIRE ( UNIT=IFILE, OPENED=OPENDX )
            IF ( OPENDX ) THEN
#endif
               IFILE = IFILE+1
               IF ( IFILE .GT. IFILMX ) IFILE=IFILMN
               IF ( ICOUNT .GT. 100 ) THEN
                  WRITE (6,*) ' ### I/O UNIT FULL'
                  IFILE = 0
                  IERR  = 1
                  RETURN
               ENDIF
               GOTO 2100
            ENDIF
*
            CALL FOPEN
     O         ( IERR  ,
     I           IFILE , HFILX , HACT  , HFORM  , 'SEQUENTIAL' )
*
         IF ( IERR .NE. 0 ) THEN
            WRITE (6,*) ' ### FILE OPEN ERROR :', IERR
         ENDIF
*
      ELSE
         IERR = 0
      ENDIF
*
      IF ( IERR .EQ. 0 ) THEN
         WRITE ( 6,* ) ' *** FILE OPEND for ', HACT,' UNIT = ', IFILE,
     &                 ' NAME = ', HFILX
         IF ( IFILED .GE. 0 .AND. IFILE .NE. IFILED ) THEN
            WRITE ( 6,* ) ' *** UNIT NUMBER IS MODIFIED FROM ', IFILED
         ENDIF
      ENDIF
*     
      RETURN
      END
***********************************************************************
      SUBROUTINE MKFILN    !! subst. fname w/ env.var.
     M         ( HFILE )
*
*   [PARAM] 
#include        "zhdim.F"                /* No. of character */
*
*   [MODIFY] 
      CHARACTER  HFILE  *(*)
*
*   [INTERNAL WORK] 
      CHARACTER  HFILX  *(NFILN)
      INTEGER    NFNAM, NFILE, I, II, IE, NENV, IEN
      CHARACTER  HENVN  *(NCC)
      CHARACTER  HENVV  *(NFILN)
*
*   [EXTERNAL FUNC] 
      INTEGER    LENC
*
*   [INTERNAL PARM] 
      CHARACTER  HCQUO, HCENV
      DATA       HCQUO, HCENV / '"', '$' /
*
      HFILX = HFILE
      HFILE = ' '
      NFNAM = LENC( HFILX )
      NFILE = LEN ( HFILE )
      I     = 1
      II    = 1
*
 1100 CONTINUE 
         IF      ( HFILX(I:I) .EQ. HCQUO ) THEN
            I = I + 1
         ELSE IF ( HFILX(I:I) .EQ. HCENV ) THEN
            HENVN = ' '
            DO 1200 IE = I+1, NFNAM
               IF ( ((HFILX(IE:IE) .LT. 'A') .OR.
     &               (HFILX(IE:IE) .GT. 'Z')) .AND.
     &               (HFILX(IE:IE) .NE. '_')       ) THEN
                  IF ( IE .GT. I+1 ) THEN
                     HENVN = HFILX(I+1:IE-1)
                  ENDIF
                  I = IE
                  GOTO 1300
               ENDIF
 1200       CONTINUE 
            HENVN = HFILX(I+1:NFNAM)
            I = NFNAM+1
*
 1300       CONTINUE 
            CALL GETENV ( HENVN, HENVV )
            IF ( HENVV .NE. ' ' ) THEN
               NENV = LENC( HENVV )
               IEN  = MIN( II+NENV-1, NFILE )
               HFILE(II:IEN) = HENVV
               II = IEN+1
            ELSE
               WRITE (6,*) ' ### MKFILN: UNDEFINED ENVIRONMENT VAR. :',
     &                      HENVN
            ENDIF
         ELSE
            HFILE(II:II) = HFILX(I:I)
            I  = I + 1
            II = II + 1
         ENDIF
      IF ( (I. LE. NFNAM) .AND. (II .LE. NFILE) ) GOTO 1100
*
      RETURN
      END
