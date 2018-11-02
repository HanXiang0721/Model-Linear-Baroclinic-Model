* PACKAGE YSUN   !!   machine dependent routine for Sun Fortran
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         93/12/25(numaguti) AGCM5.3
*
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
      IF ( HACT .EQ. 'APPEND' ) THEN
         OPEN ( UNIT=IFILE , FILE=HFILE, IOSTAT=IOS,
     &          FORM=HFORM,
     &          ACCESS='APPEND'                       )
      ELSE
         OPEN ( UNIT=IFILE , FILE=HFILE, IOSTAT=IOS,
     &          FORM=HFORM,
     &          ACCESS=HACCSS                         )
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
      SUBROUTINE ERRTRA    !! error traceback
*
*   [INTERNAL WORK] 
      INTEGER    IPID, IRET
*
*   [INTRINSIC FUNC] 
      INTEGER    GETPID
      INTEGER    KILL
*      
      IPID=GETPID()
      IRET=KILL(IPID,5)
*
      RETURN
      END
*********************************************************************
      SUBROUTINE YCLOCP    !! output of CPU time
     I         ( HREM   )
*
*   [INPUT] 
      CHARACTER  HREM*(*)                    !! title
*
*   [INTERNAL WORK] 
      REAL*8     CPUTIM                      !! time passage
      REAL*8     VPUTIM                      !! user time
*
*   [INTERNAL SAVE]  
      REAL*8     CPUOLD                      !! time from last call
      REAL*8     VPUOLD                      !! user time at last call
      SAVE
      DATA       CPUOLD, VPUOLD / 0.  , 0.   /
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
      REAL*8     CPUTIM                      !! time passage
      REAL*8     VPUTIM                      !! user time
*
*   [INTERNAL WORK] 
      REAL * 4   TARRAY( 2 )
*
*   [INTRINSIC FUNC] 
      REAL * 4   ETIME
*
*   [INTERNAL SAVE] 
      REAL*8     TICKS
      REAL*8     TICK0, TUSR0                !! start
      SAVE
      DATA       TICK0, TUSR0 / 0.  , 0.    /
*
      TICKS  = ETIME( TARRAY )
      CPUTIM = TICKS - TICK0
      VPUTIM = TARRAY( 1 ) - TUSR0
*
      RETURN
*======================================================================
      ENTRY      YCLOCL    !! clear CPU time
*
      TICK0 = ETIME( TARRAY )
      TUSR0 = TARRAY( 1 )
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
      SAVE
      DATA       IFILEZ / 5 /
      DATA       JFILEZ / 6 /
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
      FUNCTION   ERRHANDLE !! treatment for error
     I         ( SIG   , CODE  , CONTEXT ) 
*
      INTEGER    ERRHANDLE
      INTEGER    SIG, CODE, CONTEXT(5)
*
      CALL ABORT
*
      END
***********************************************************************
      SUBROUTINE YFINE     !! sys.dep. final treatment
*
      RETURN
      END
***********************************************************************
      SUBROUTINE YDATE     !! get present time & date
     O         ( IDATEX )
*
*   [OUTPUT] 
      INTEGER      IDATEX( 6 )
*   [INTERNAL WORK] 
      INTEGER * 4  IMON, IDAY, IYEAR, ITIMES( 3 ), I
*
      CALL  IDATE ( IMON, IDAY, IYEAR )
      IF ( IYEAR .LT. 100 ) IYEAR = IYEAR + 1900
      CALL  ITIME ( ITIMES )
      IDATEX( 1 ) = IYEAR
      IDATEX( 2 ) = IMON
      IDATEX( 3 ) = IDAY
      DO 1100 I = 4, 6
         IDATEX( I ) = ITIMES( I-3 )
 1100 CONTINUE 
*
      RETURN
      END
S( 3 ), I
*
      CALL  IDATE ( IMON, IDAY, IYEAR )
      IF ( IYEAR .LT. 100 ) IYEAR = IYEAR + 1900
      CALL  ITIME ( ITIMES )
      IDATEX( 1 ) = IYEAR
      IDATEX( 2 ) = IMON
      IDATEX( 3 ) = IDAY
      DO 1100 I = 4, 6
         IDATEX( I ) = ITIMES( I-3 )
 1100 CONTINUE 
*
      RETURN
      END
