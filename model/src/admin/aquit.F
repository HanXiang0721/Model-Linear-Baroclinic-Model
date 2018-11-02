* PACKAGE AQUIT  !! admin: check for compulsive exit
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*
*********************************************************************
      SUBROUTINE QUITCK    !! check for compulsive exit
     O         ( OQUIT , TIME   )
*
*   [PARAM] 
#include        "zhdim.F"                   /* No. of character */
*
*   [OUTPUT] 
      LOGICAL    OQUIT
      REAL * 8   TIME
*
*   [INTERNAL WORK] 
      INTEGER    IFPAR, JFPAR
      INTEGER    IFILE, IERR, IOS, I
      INTEGER    ITIME( 6 )
      REAL * 8   TIMEQ
      LOGICAL    OCHK
*
*   [EXTERNAL FUNC] 
      LOGICAL    OINTVL
*
*   [INTERNAL SAVE] 
      REAL * 8   TSTRTR, TPREV
      SAVE       TSTRTR, TPREV
*
*   [INTERNAL PARM] 
      CHARACTER  QUIT  *(NFILN)
      REAL*8     TINTV                 !! interval
      CHARACTER  TUNIT  *(NCC)         !! unit
      INTEGER    START  ( 6 )          !! start
      LOGICAL    OFIRST
      NAMELIST  /NMQUIT/ QUIT, TINTV, TUNIT
      DATA       QUIT   / 'QUIT' /
      DATA       TINTV  /  1    /
      DATA       TUNIT  / 'HOUR' /
      DATA       START  / -999, 1, 1, 0, 0, 0 /
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE (6,*) ' @@@ AQUIT: CHECK QUIT BY FORCE 93/12/25'
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMQUIT, END=190 )
  190    WRITE  ( JFPAR, NMQUIT )
*
         IF ( START(1) .GE. 0 ) THEN
            CALL CYH2SS( TSTRTR, START  )
         ELSE
            TSTRTR = TIME
         ENDIF
         TPREV = TIME
      ENDIF
*
      OQUIT = .FALSE.
      IF ( QUIT .EQ. ' ' ) THEN
         RETURN
      ENDIF
      IF ( TINTV .GT. 0 ) THEN
         OCHK = OINTVL( TIME  , TPREV , TSTRTR, TINTV, TUNIT )
         IF ( .NOT. OCHK ) THEN
            RETURN
         ENDIF
      ENDIF
*
      CALL IFLOPN
     O         ( IFILE , IERR  ,
     I           QUIT  , 'TEST', '(A)'  )
      IF ( IERR .EQ. 0 ) THEN
         READ ( IFILE, 2600, IOSTAT=IOS ) (ITIME(I),I=1,6)
 2600    FORMAT( I4.4,1X,I2.2,1X,I2.2,1X,I2.2,1X,I2.2,1X,I2.2 )
C2600    FORMAT( I4.4,I2.2,I2.2,1X,I2.2,I2.2,I2.2 )
         CLOSE( IFILE )
         IF ( IOS .EQ. 0 ) THEN
            CALL CYH2SS( TIMEQ, ITIME )
            IF ( TIME .GE. TIMEQ ) THEN
               OQUIT = .TRUE.
            ENDIF
         ELSE
            OQUIT = .TRUE.
         ENDIF
      ENDIF
*
      RETURN
      END
