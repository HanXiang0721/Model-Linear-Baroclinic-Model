*  PACKAGE AADMIN !! admin: main routine
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/04/30(numaguti) TSTART, TEND
*         96/06/06(numaguti) change FIXMAS for accurate restart
*         96/06/19(numaguti) AGCM5.4f
*         05/05/24(hiro)     for initializing BS
***********************************************************************
      SUBROUTINE ATMOS         !! AGCM step
     M         ( TIME  , 
     I           TEND    )
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc.           */
#include        "zhdim.F"                /* # of char. in string   */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
#ifdef SYS_HITAC
#include        "zwdim.F"                /* work */
#endif
*
*   [MODIFY]       
      REAL * 8   TIME                  !! time
      REAL * 8   TEND                  !! end time
*
*   [INTERNAL SAVE] 
      REAL*8     GAU   ( IJDIM, KMAX      )  !! westerly
      REAL*8     GAV   ( IJDIM, KMAX      )  !! southern wind
      REAL*8     GAT   ( IJDIM, KMAX      )  !! temperature
      REAL*8     GAPS  ( IJDIM            )  !! surface pressure
      REAL*8     GAQ   ( IJDIM, KMAX, NTR )  !! humidity etc.
*        : grid value(t) <DYNMCS>  grid value(t+dt)
*
      INTEGER    ISTEP                 !! serial No. of step
      REAL*8     DELT                  !! time step delta(t)
      LOGICAL    OINIT                 !! initializing time or not
*        : time etc.
*
      REAL * 8   TSTART                !! start time
      REAL*8     GDZS  ( IJDIM )       !! surface topography
*
      SAVE       GAU, GAV, GAT, GAPS, GAQ
      SAVE       ISTEP, DELT, OINIT
      SAVE       TSTART, GDZS
*
*   [INTERNAL WORK] 
      INTEGER    IFPAR, JFPAR, ISET
      CHARACTER  HTIME*20
      REAL * 8   TIMEZ
*
#ifndef OPT_NOCOMMON
#ifdef SYS_HITAC
      COMMON     WORK
      REAL*8     WORK( NWORK )
#endif
#endif
*
*   [INTERNAL PARM] 
      CHARACTER  RSFRC*90
      CHARACTER  FSFRC*90
      LOGICAL    OCHECK, OCKALL
      LOGICAL    OCONV                 !! convergence flag
      NAMELIST  /NMCHCK/ OCHECK, OCKALL
      DATA       OCHECK, OCKALL / .FALSE., .FALSE. /
      DATA       ISTEP          / 0 /
      DATA       OCONV           / .FALSE. /
*
      ISTEP = ISTEP + 1
      CALL CSS2CC ( HTIME, TIME )
      WRITE (6,*) ' *** STEP=', ISTEP, ' TIME=', HTIME
      CALL SETTIM ( TIME )
      CALL HISTRT (    1 )
*
*     << START : start of step >>
*
      CALL TIMSTP               !! control time
     M         ( DELT  , OINIT ,
     I           TIME  , GAU   , GAV   , ISTEP  )
*
      TIMEZ = ANINT( TIME + DELT )
      IF ( TIMEZ .GT. TEND ) THEN
         TIMEZ = TEND
         DELT  = TIMEZ - TIME
      ENDIF
*
      CALL SETPSF               !! set output Ps
     I         ( GAPS   )     
*
*     << DYNMCS : dynamics >>
*     
      IF ( OCHECK ) THEN
         CALL ACHKV 
     I         ( GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     I           'before DYNMCS', OCKALL                 )
      ENDIF
*
      CALL CLCSTR ( 'DYNMCS' )
      CALL DYNMCS               !! dynamics
     O         ( OCONV ,
     M           GAT   , GAPS  , 
     I           GAU   , GAV   , GAQ   ,
     I           TIME  , DELT  , OINIT , GDZS    )
C      CALL MASFIX
C     M         ( GAPS  , GAQ   , 
C     I           DELT            )
      CALL CLCEND ( 'DYNMCS' )
*
      IF( OCONV ) THEN
         WRITE ( 6,* ) ' @@@ AADMN: ITERATION CONVERGED '
*
         TIME = TEND
         RETURN
      ENDIF
*
      IF ( .NOT. OINIT ) THEN
*
         TIME = TIMEZ
         CALL SETTIM ( TIME )
*
         CALL HISTOU( .FALSE. ) !! output data
*
      ENDIF
*     
      RETURN
*======================================================================
*     Initialize
*======================================================================
      ENTRY      ATMINI                !! AGCM initialize
     O         ( TIME  , TEND  )
*
*  << SETPUP : initial setting >>
*
      WRITE ( 6,* ) ' @@@ AADMN: LBM2.0 HS INITIALIZE MAIN 05/05/24'
*
      CALL   REWNML ( IFPAR , JFPAR )
      READ   ( IFPAR, NMCHCK, END=180 )
  180 WRITE  ( JFPAR, NMCHCK )
*
      CALL ASETUP                      !! initial setup of atmos.
     O         ( TSTART, TEND )
*
      CALL RDSTRT                      !! read initial data
     O         ( GAU   , GAV   , GAT   , GAPS  , GAQ   , 
     O           OINIT                                   )
*
      CALL TIMADJ                      !! adjust time for experiment 
     M         ( TSTART, TEND  )
      TIME = TSTART
*
      CALL RESET( GDZS, IJDIM )
      CALL RDDAT
     O         ( GDZS  , ISET  ,
     I           TIME  , 'GRZ' , 'ASFC', 0.D0  , .TRUE.  )
*
      CALL ACHKV
     I         ( GAU   , GAV   , GAT   , GAPS  , GAQ   , 
     I           'start GA'    , .TRUE.                  )
      ISTEP = 0
*
      RETURN
*=====================================================================
*     Finalize
*=====================================================================
      ENTRY      ATMFIN        !! AGCM finalize
     I         ( TIME   )
*
*     dump modified basic state
*
      CALL WMODBS               !! write modified state
     I     ( GAU   , GAV   , GAT   , GAPS  , GAQ    )
*
      RETURN
      END
