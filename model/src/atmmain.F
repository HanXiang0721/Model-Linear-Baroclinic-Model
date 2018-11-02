*  PACKAGE ATMMAIN !! AGCM main routine
*
*   [HIS] 96/06/14(numaguti) AGCM5.4f
***********************************************************************
      PROGRAM    AGCM      !! AGCM main routine
*
*   [VAR] 
      REAL * 8   TIME
      REAL * 8   TEND
*
*   [WORK] 
      LOGICAL    OQUIT
*
      WRITE ( 6,* ) ' @@@ LINEAR BAROCLINIC MODEL @@@ 99/01/01'
      CALL YPREP                       !! sys.dep. initialization
      CALL CALNDR                      !! initilize calendar
*
*   < Initialize >
*
      CALL ATMINI
     O       ( TIME, TEND  )
*
 8000 CONTINUE 
      IF ( TIME .LT. TEND ) THEN
*
*      < Atmospheric Step >
*
         CALL ATMOS
     M      ( TIME, TEND  )
*
         CALL QUITCK( OQUIT, TIME )
         IF ( .NOT. OQUIT ) GOTO 8000
      ENDIF
*
      CALL ATMFIN( TIME   )
*
      CALL SETTIM( TIME   )
      CALL HISTOU( .TRUE. )            !! output data
      CALL CLCOUT                      !! output of CPU time
      CALL YFINE                       !! sys.dep. final treatment
*
      STOP
      END
