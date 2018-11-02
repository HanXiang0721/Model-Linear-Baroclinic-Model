* PACKAGE ASTUP !! admin: initial set up of atmos 
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/04/30(numaguti) TSTART, TEND
*         96/07/04(numaguti) AGCM5.4f
*         01/11/13(hiro)     for solver
***********************************************************************
      SUBROUTINE ASETUP    !! initial set up of atmos 
     O         ( TSTART, TEND )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zhdim.F"                /* # of char. in string   */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
*
*   [OUTPUT] 
      REAL * 8   TSTART                      !! start t of calculation
      REAL * 8   TEND                        !! finish t of calculation
*
      CALL PCONST ( .TRUE. )           !! physical constants
      CALL ASETCO                      !! set coordinate
      CALL SETPAR                      !! time of experiment etc.
     O         ( TSTART, TEND )
*
      RETURN
*================================================================
      ENTRY SETCONS             !! set constant (for solver)
*
      CALL PCONST ( .FALSE. )   !! physical constants
*
      RETURN
      END
***********************************************************************
      SUBROUTINE SETPAR    !! set up atmos parameters
     O         ( TSTART, TEND )
*
*   [PARAM] 
#include        "zcdim.F"                    /* # of grid point & wave */
#include        "zhdim.F"                    /* # of char. in string */
#include        "zccom.F"                    /* stand. physical const. */
*
*   [OUTPUT] 
      REAL * 8   TSTART                      !! start t of calculation
      REAL * 8   TEND                        !! finish t of calculation
*
*   [ENTRY OUTPUT] 
      CHARACTER  HRUN*(NCC)                  !! title of experiment
*
*    [INTERNAL WORK]
      INTEGER    IFPAR, JFPAR
      CHARACTER  HTIME*20
*
*   [INTERNAL SAVE] 
      REAL * 8   TSTRTX
*
*   [INTERNAL PARM] 
      CHARACTER  RUN *(NCC)                  !! title of experiment
      INTEGER    START ( 6 )                 !! start t of calculation
      INTEGER    END   ( 6 )                 !! finish t of calculation
      INTEGER    PERPET( 6 )                 !! Perpetual DATE
      REAL*8     DUR                         !! calculation time
      CHARACTER  TUNIT*(NCC)                 !! time unit
      NAMELIST  /NMRUN/  RUN
      NAMELIST  /NMTIME/ START, END, PERPET, DUR, TUNIT
      DATA       RUN      / 'TEST001' /
      DATA       START    / -1, 1, 1, 0, 0, 0 /
      DATA       END      / -1, 1, 1, 0, 0, 0 /
      DATA       PERPET   / -1, 1, 1, 0, 0, 0 /
      DATA       DUR      / -999 /
      DATA       TUNIT    / 'DAY' /
*
      DATA       TSTRTX   / 0.D0 /
*
      WRITE (6,*) ' @@@ ASETUP : SET EXP. PARAMETERS 96/04/30'
*
*   < 1. title of experiment >
*
      CALL   REWNML ( IFPAR , JFPAR )
      READ   ( IFPAR, NMRUN , END=1190 )
 1190 WRITE  ( JFPAR, NMRUN  )
*
*   < 2. control time >
*
      CALL   REWNML ( IFPAR , JFPAR )
      READ   ( IFPAR, NMTIME, END=3190 )
 3190 WRITE  ( JFPAR, NMTIME )
*
      CALL  CYH2SS
     O         ( TSTART,
     I           START  )
*
      IF ( DUR .GE. 0 ) THEN
         CALL  CSSAFT      !! calendar, time advancing
     O         ( TEND  ,
     I           TSTART, DUR   , TUNIT  )
      ELSE
         CALL  CYH2SS
     O         ( TEND  ,
     I           END    )
      ENDIF
*
      IF ( PERPET(1) .GE. 0 ) THEN
         CALL    CPERPT
     I         ( PERPET(1), PERPET(2), PERPET(3) )
      ENDIF
*
      IF ( TSTART .GE. 0.D0 ) THEN
         CALL CSS2CC ( HTIME, TSTART )
         WRITE ( 6,* ) ' ### MODEL START TIME = ', HTIME
         CALL CSS2CC ( HTIME, TEND   )
         WRITE ( 6,* ) ' ### MODEL END   TIME = ', HTIME
      ENDIF
*
      CALL SETTIM( TSTART )      !! adjust time
      TSTRTX = TSTART
*
      RETURN
*================================================================
      ENTRY      TIMADJ               !! adjust time for experiment 
     M         ( TSTART, TEND  )
*
      TSTART = MAX( TSTART, 0.d0 )
*
      IF ( TSTART .NE. TSTRTX ) THEN
         IF ( DUR .GE. 0 ) THEN
            CALL  CSSAFT        !! calendar, time advancing
     O         ( TEND  ,
     I           TSTART, DUR   , TUNIT  )
         ENDIF
*
         CALL CSS2CC ( HTIME, TSTART )
         WRITE ( 6,* ) ' ### MODEL START TIME (revised) = ', HTIME
         CALL CSS2CC ( HTIME, TEND   )
         WRITE ( 6,* ) ' ### MODEL END   TIME (revised) = ', HTIME
      ENDIF
*     
      CALL SETTIM( TSTART )      !! adjust time
*
      RETURN
*================================================================
      ENTRY      GETRUN    !! get name of experiment
     O         ( HRUN    )
*
      HRUN = RUN
*
      RETURN
*================================================================
      ENTRY      SETRUN    !! set name of experiment
     I         ( HRUN    )
*
      RUN = HRUN
*
      RETURN
      END
