*  PACKAGE GMAIN !! GCM main routine
*
*   [HIS] 96/06/14(numaguti) AGCM5.4f
***********************************************************************
      PROGRAM    GCM       !! GCM main routine
*
*   [VAR] 
      REAL * 8   TIME
      REAL * 8   TEND
      REAL * 8   TSTP
      REAL * 8   TIMEA
      REAL * 8   TIMEO
*
*   [WORK] 
      INTEGER    IFPAR, JFPAR
      LOGICAL    OQUIT
*
*   [PARM] 
      REAL * 8   TSTEP
      REAL*8     STEP
      CHARACTER  TUNIT*4
      NAMELIST  /NMMAIN/ STEP, TUNIT
      DATA       STEP   / 1      /
      DATA       TUNIT  / 'DAY'  /
*
      WRITE ( 6,* ) ' @@@ AGCM5.4 COUPLED @@@ 96/06/17'
      CALL YPREP                       !! sys.dep. initialization
      CALL CALNDR                      !! initilize calendar
*
      CALL  REWNML ( IFPAR , JFPAR )
      READ  ( IFPAR, NMMAIN, END=190 )
  190 WRITE ( JFPAR, NMMAIN )
      CALL CXX2SS( TSTEP, STEP, TUNIT, 0.D0 )
*
*   < Initialize >
*
      CALL ATMINI
     O       ( TIME  , TEND    )
      CALL OCNINI
     M       ( TIME  , TEND    )
*
      TIMEA = TIME
      TIMEO = TIME
      TSTP  = TIME
 8000 CONTINUE 
      IF ( TIMEA .LT. TEND .OR. TIMEO .LT. TEND ) THEN
         TSTP  = MIN( ANINT( TSTP + TSTEP ), TEND )
*
*      < Atmospheric Step >
*
 8100    CONTINUE 
         IF  ( TIMEA .LT. TSTP ) THEN
            CALL ATMOS
     M         ( TIMEA, TSTP  )
            GOTO 8100
         ENDIF
*
*      < Oceanic Step >
*
 8200    CONTINUE 
         IF  ( TIMEO .LT. TSTP ) THEN
            CALL OCEAN
     M         ( TIMEO, TSTP  )
            GOTO 8200                  
         ENDIF
*
         TIME = TSTP
         CALL QUITCK( OQUIT, TIME )
         IF ( .NOT. OQUIT ) GOTO 8000
      ENDIF
*
      CALL ATMFIN( TIMEA  )
      CALL OCNFIN( TIMEO  )
*
      CALL SETTIM( TEND   )
      CALL HISTOU( .TRUE. )            !! output data
      CALL CLCOUT                      !! output of CPU time
      CALL YFINE                       !! sys.dep. final treatment
*
      STOP
      END
