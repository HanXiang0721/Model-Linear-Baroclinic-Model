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
#ifdef CODE_ENDIAN
      INTEGER    IERR, XFOPN, XFADD
*
      IF ( HFORM .EQ. 'UNFORMATTED' ) THEN
         IF ( HACT .EQ. 'APPEND' ) THEN
            IERR=XFOPN(IFILE,HFILE,2)
         ELSE IF ( HACT .EQ. 'WRITE' ) THEN
            IERR=XFOPN(IFILE,HFILE,1)
         ELSE 
            IERR=XFOPN(IFILE,HFILE,0)
         ENDIF
      ELSE
         IERR=XFADD(IFILE,HFILE)
#endif
         IF ( HACT .EQ. 'APPEND' ) THEN
            OPEN ( UNIT=IFILE , FILE=HFILE, IOSTAT=IOS,
     &             FORM=HFORM,
     &             ACCESS='APPEND'                       )
         ELSE
            OPEN ( UNIT=IFILE , FILE=HFILE, IOSTAT=IOS,
     &             FORM=HFORM,
     &             ACCESS=HACCSS                         )
         ENDIF
#ifdef CODE_ENDIAN
      ENDIF
#endif
*
      RETURN
      END
*********************************************************************
      SUBROUTINE FREWND     !! rewind file
     I         ( IFILE )
*   [INPUT] 
      INTEGER    IFILE
*   [INTERNAL WORK] 
      INTEGER    IRET, XFREW
*
      IRET=XFREW( IFILE )
      IF ( IRET .EQ. 1 ) THEN
         REWIND( IFILE )
      ENDIF
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
*   [INTERNAL SAVE] 
      REAL*8     TICKS
      REAL*8     CLOCKS

      REAL*8     TICK0, TUSR0                !! start
      SAVE
      DATA       TICK0, TUSR0 / 0.  , 0.    /
*
      TICKS  = CLOCKS()
      CPUTIM = TICKS - TICK0
      VPUTIM = 0.
*
      RETURN
*======================================================================
      ENTRY      YCLOCL    !! clear CPU time
*
      TICK0 = CLOCKS()
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
      INTEGER * 4  IDATES( 6 ), I
*
      CALL  TIMES( IDATES )
      IF ( IDATES(1) .LT. 100 ) IDATES(1) = IDATES(1) + 1900
      DO 1100 I = 1, 6
         IDATEX( I ) = IDATES( I )
 1100 CONTINUE 
*
      RETURN
      END
