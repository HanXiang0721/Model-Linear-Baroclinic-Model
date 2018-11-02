* PACKAGE YS-unix  !!  machine dependent routine for HITAC
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         93/12/25(numaguti) AGCM5.3
*********************************************************************
      SUBROUTINE FOPEN     !! open file
     O         ( IOS   ,
     I           IFILE , HFILE , HACT  , HFORM  , HACCSS )
*
*   [OUTPUT] 
      INTEGER    IOS
*   [INPUT] 
      INTEGER    IFILE
      CHARACTER  HFILE  *(*)
      CHARACTER  HACT   *(*)
      CHARACTER  HFORM  *(*)
      CHARACTER  HACCSS *(*)
*      
      IF ( HACT .EQ. 'TEST' ) THEN
         OPEN ( UNIT=IFILE , FILE=HFILE, IOSTAT=IOS,
     &          ACTION='READ', FORM=HFORM,
     &          ACCESS='SEQUENTIAL'                 )
      ELSE
         OPEN ( UNIT=IFILE , FILE=HFILE, IOSTAT=IOS,
     &          ACTION=HACT, FORM=HFORM,
     &          ACCESS='SEQUENTIAL'                 )
      ENDIF
*
      RETURN
      END
*********************************************************************
      SUBROUTINE FREWND     !! rewind file
     I         ( IFILE )
*   [INPUT] 
      INTEGER    IFILE
*
      REWIND( IFILE )
*
      RETURN
      END
*********************************************************************
*    SUBROUTINE ERRTRA     !! error traceback
*   [INTRINSIC FUNC] 
*********************************************************************
      SUBROUTINE YCLOCP    !! output of CPU time
     I         ( HREM   )
*
*   [OUTPUT] 
      CHARACTER  HREM*(*)                    !! title
*
*   [INTERNAL SAVE] 
      REAL*8     CPUOLD                      !! CPU time at last call
      REAL*8     VPUOLD                      !! VPU time at last call
      DATA       CPUOLD, VPUOLD / 0.  , 0.  /
      SAVE
*
*   [INTERNAL WORK] 
      REAL*8     CPUTIM                      !! scalar time
      REAL*8     VPUTIM                      !! vector time
*
      CALL YCLOCK( CPUTIM, VPUTIM )
      WRITE ( 6,9000 ) HREM, CPUTIM - CPUOLD, VPUTIM - VPUOLD,
     &                       CPUTIM         , VPUTIM
 9000 FORMAT( ' ',A8,': CPU/VPU TIME = ', 2E10.5, ' : ', 2E10.5 )
      CPUOLD = CPUTIM
      VPUOLD = VPUTIM
*
      RETURN
      END
********************************************************************
      SUBROUTINE YCLOCK    !! get CPU time
     O         ( CPUTIM, VPUTIM )
*
*   [OUTPUT] 
      REAL*8     CPUTIM                      !! scalar time
      REAL*8     VPUTIM                      !! vector time
*
      CALL XCLOCK ( CPUTIM, 5 )
      CALL VCLOCK ( VPUTIM, 5 )
*
      RETURN
*======================================================================
      ENTRY      YCLOCL    !! clear CPU time
*
*      << clear CPU time >>
*
      CALL XCLOCK
      CALL VCLOCK
*
      RETURN
      END
***********************************************************************
      SUBROUTINE GETENV    !! get environmental variables
     M         ( HENVN , HENVV  )
*
*   [MODIFY] 
      CHARACTER  HENVN *(*)
      CHARACTER  HENVV *(*)
*
      HENVV = ' '
*
      RETURN
      END
***********************************************************************
      SUBROUTINE REWNML    !! rewind NAMELIST-input file
     O         ( IFILE, JFILE )
*
*   [OUTPUT] 
      INTEGER    IFILE
      INTEGER    JFILE
*
*   [INTERNAL SAVE] 
      INTEGER    IFILEZ, JFILEZ
      DATA       IFILEZ / 5 /
      DATA       JFILEZ / 6 /
      SAVE
*
      REWIND ( IFILEZ, ERR = 1999 )
      IFILE = IFILEZ
      JFILE = JFILEZ
      RETURN
*
 1999 IF ( IFILEZ .EQ. 5 ) THEN
         WRITE (6,*) ' ### REWNML: UNIT 5 MAY BE A STANDARD INPUT'
         IFILE = IFILEZ
         JFILE = JFILEZ
      ELSE
         WRITE (6,*) ' ### REWNML: ERROR IN REWINDING'
         CALL XABORT( 0 )
         RETURN
      ENDIF
*
      RETURN
*======================================================================
      ENTRY      SETNML    !!  set NAMELIST-input filename
     I         ( IFILE, JFILE )
*
      IFILEZ = IFILE
      JFILEZ = JFILE
*
      RETURN
      END
***********************************************************************
      SUBROUTINE YPREP     !! sys.dep. initialization
*
      RETURN
      END
***********************************************************************
      SUBROUTINE YFINE     !! sys.dep. final treatment
*
      RETURN
      END
***********************************************************************
      SUBROUTINE YDATE     !! get present time & date
     O         ( IDATE )
*
*   [OUTPUT] 
      INTEGER    IDATE ( 6 )
*
*   [INTERNAL WORK] 
      CHARACTER  HDATE1 *12
      CHARACTER  HTIME1 *12
*
      CALL  DATE ( HDATE1 )
      READ( HDATE1, 600 ) IDATE(1), IDATE(2), IDATE(3)
  600 FORMAT(I2,1X,I2,1X,I2)
      IDATE(1) = IDATE(1) + 1900
      CALL  CLOCK( HTIME1, 1 )
      READ( HTIME1, 600 ) IDATE(4), IDATE(5), IDATE(6)
*
      RETURN
      END
