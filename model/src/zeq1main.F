* [hiro] modified on 2004/04/14
* [hiro] incorpolated to LBM package on 2004/06/14
c****************************zeq1main.f*********************************
c  driver routine for coupled model
c note time convention in months; 0.5 corresponds to mid January 1960.
c***********************************************************************
*
#include      "zeq_common.F"
*

*
*   [VAR] 
      REAL * 8   TIME
      REAL * 8   TEND
*
*   [WORK] 
      LOGICAL    OQUIT
*
*    [INTERNAL WORK]
      REAL*8  TD
      INTEGER IFPAR, JFPAR
      INTEGER IT1, NT
      INTEGER I, J
      INTEGER NTIMES            !! undefined?
*
*    [INTERNAL PARAM]
      REAL*8  TFIND

      NAMELIST /NMTITL/ HEADER
      NAMELIST /NMSTAT/ NSTART, TFIND

      DATA   HEADER / '      ' /
      DATA   TFIND  / 1.D20   /
*
      WRITE ( 6,* ) ' @@@ mLBM-CZ COUPLED MODEL @@@ 04/07/01'
      CALL YPREP                       !! sys.dep. initialization
      CALL CALNDR                      !! initilize calendar

      CALL   REWNML ( IFPAR , JFPAR )
      READ   ( IFPAR, NMTITL, END=190 )
  190 WRITE  ( JFPAR, NMTITL )
      CALL   REWNML ( IFPAR , JFPAR )
      READ   ( IFPAR, NMSTAT, END=290 )
  290 WRITE  ( JFPAR, NMSTAT )

      CALL OPENFL

      WRITE( JFPAR,* ) ' @@@ MODEL INITIAL SETUP @@@ '

* read control paramters
      NATM = 2
      IF( NSTART .LT. 0 ) GO TO 999
* if history input is being used for startup, call rdhist...............
      IF( NSTART .GT. 0 ) CALL RDHIST(0,TFIND)
      NATMR = NATM
* if nstart=1 or nstart=2, time is gotten from the history file.........
* otherwise (nstart=3) time is set to tzero from input data file........
      IF( NSTART .EQ. 3 .OR. NSTART .EQ. 0 ) THEN
         TD = TZERO
      ELSE
         TZERO = TD
      ENDIF

      NT  = 0
      IT1 = TD + 120.501
      IT  = MOD( IT1,12 )
      MP  = IT + 1
      IF( IT .EQ. 0) IT = 12
      TY  = TD + 120.501 - IT1

      CALL SETUP
      CALL RDBASE               !! read mean fields
      CALL CONSTC               !! initialize ocean dynamics model
      ISTART=0
      CALL STRESS(TD)           !! initial stress field

* if one wanted to overwrite any fields with data, it would be done here
* based on the paramter IC2... routine INSERT would be created to do it.
*     IF( IC2 .GT. 0 ) CALL INSERT
* initial call to ocean force routine and dynamics update routine mloop
* are special if starting up from rest - ie, if not restarting..........
      CALL CFORCE

      WRITE( JFPAR,1080 ) NT,TD,IT
 
      IF( NSTART .GT. 0 ) THEN
         ISTART=1
         GO TO 400
      ELSE
         GO TO 350
      ENDIF
*
*     this is the top of main loop for all but the first cycle
* 
 300  CALL SSTA(TD)             !! update the ssta field 
      CALL ZATMC(TD)            !! update the winds
      CALL STRESS(TD)           !! compute the wind stress anomalies
      CALL CFORCE               !! stress forcing into ocean model
 350  CALL MLOOP                !! update the ocean dynamics

 400  CONTINUE
*
      WRITE( JFPAR,1090 ) NT,TD,IT

*
      CALL WRGRD(NT)            !! history output
      CALL WRNINO(NT,TD)        !! nino3 SSTA diagnosis
* 
      IF( NT .GE. NTIMES ) GO TO 900 !! if it's time to stop

* if not, loop again

      NT  = NT + 1
      TD  = TZERO + FLOAT( NT ) * DTD
      IT1 = TD + 120.501
      IT  = MOD( IT1,12 )
      MP  = IT + 1
      IF( IT .EQ. 0 ) IT = 12
      TY  = TD + 120.501 - IT1
      GO TO 300

 900  CALL WRHIST               !! time to finish up
*
 999  CALL CLCOUT               !! output of CPU time
      CALL YFINE                !! sys.dep. final treatment

 1080 FORMAT( ' TIMESTEP (INITIAL)', I6, ' TIME =',F8.3, 
     $     ' MONTHS = ', I5, ' INTEGER MONTH' )
 1090 FORMAT( ' TIMESTEP', I6, ' TIME =',F8.3, ' MONTHS = ',
     $     I5, ' INTEGER MONTH' )

      STOP
      END
 
