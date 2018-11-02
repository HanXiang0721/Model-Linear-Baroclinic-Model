* PACKAGE YSX3   !!   machine dependent routine for NEC SX4
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
**                 123456789012345678
      CALL MESPUT('### USER ERROR ###',18, 1)  
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
*   [INTERNAL SAVE]  
      REAL*8     CPUOLD                      !! CPU time at last call
      DATA       CPUOLD / 0.   /
      SAVE
*
*   [INTERNAL WORK] 
      REAL*8     CPUTIM                      !! CPU time
      REAL*8     VPUTIM                      !! VPU time
*
      CALL YCLOCK( CPUTIM, VPUTIM )
      WRITE ( 6,9000 ) HREM, CPUTIM - CPUOLD,
     &                       CPUTIM
 9000 FORMAT( ' ',A8,': CPU TIME = ', E10.5, ' : ', E10.5 )
      CPUOLD = CPUTIM
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
*   [INTERNAL SAVE] 
      REAL * 8   CPUT0, CPUT1
      DATA       CPUT0 / 0.   /
      SAVE       CPUT0
*
      CALL CLOCK  ( CPUT1 )
      CPUTIM = CPUT1 - CPUT0
      VPUTIM = 0.  
*
      RETURN
*======================================================================
      ENTRY      YCLOCL    !! clear CPU time
*
      CALL CLOCK ( CPUT0 )
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
      CALL ERROPT ( 215,215, 2,2,2,2,2,2 )
*       underflow
      CALL ERROPT ( 251,251, 2,2,2,2,2,2 )
      CALL ERROPT ( 268,268, 2,2,2,2,2,2 )
      CALL ERROPT ( 271,271, 2,2,2,2,2,2 )
*       error abort count
      CALL ERRCNT ( 100 )
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
      CHARACTER  HDATE1 *8
      CHARACTER  HTIME1 *8

*
      CALL  DATIM ( HDATE1, HTIME1 )
      READ( HDATE1, 600 ) IDATE(1), IDATE(2), IDATE(3)
  600 FORMAT(I2,1X,I2,1X,I2)
      IDATE(1) = IDATE(1) + 1900
      READ( HTIME1, 600 ) IDATE(4), IDATE(5), IDATE(6)
*
      RETURN
      END
