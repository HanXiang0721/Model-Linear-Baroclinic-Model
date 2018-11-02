*  PACKAGE AADMIN !! admin: main routine
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/04/30(numaguti) TSTART, TEND
*         96/06/06(numaguti) change FIXMAS for accurate restart
*         96/06/19(numaguti) AGCM5.4f
*         96/09/18(numaguti) GSIGD,GPID
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
      REAL*8     GBU   ( IJDIM, KMAX      )  !! westerly
      REAL*8     GBV   ( IJDIM, KMAX      )  !! southern wind
      REAL*8     GBT   ( IJDIM, KMAX      )  !! temperature
      REAL*8     GBPS  ( IJDIM            )  !! surface pressure
      REAL*8     GBQ   ( IJDIM, KMAX, NTR )  !! humidity etc.
*        : grid value(t-dt) <DYNMCS>  grid value(t)
*
      REAL*8     GFU   ( IJDIM, KMAX      )  !! westerly
      REAL*8     GFV   ( IJDIM, KMAX      )  !! southern wind
      REAL*8     GFT   ( IJDIM, KMAX      )  !! temperature
      REAL*8     GFPS  ( IJDIM            )  !! surface pressure
      REAL*8     GFQ   ( IJDIM, KMAX, NTR )  !! humidity etc.
*        : basic state variables (=initial fields)
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
      SAVE       GBU, GBV, GBT, GBPS, GBQ
      SAVE       GFU, GFV, GFT, GFPS, GFQ
      SAVE       ISTEP, DELT, OINIT
      SAVE       TSTART, GDZS
*
*   [INTERNAL WORK] 
      REAL*8     GSIGD ( IJDIM, KMAX+1    )  !! sigma velocity
      REAL*8     GPID  ( IJDIM            )  !! pi velocity
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
      LOGICAL    OANM
      NAMELIST  /NMCHCK/ OCHECK, OCKALL
      NAMELIST  /NMRFRC/ RSFRC 
      NAMELIST  /NMSFRC/ FSFRC 
      NAMELIST  /NMANM/  OANM
      DATA       OCHECK, OCKALL / .FALSE., .FALSE. /
      DATA       RSFRC          / '              ' /
      DATA       FSFRC          / '              ' /
      DATA       OANM           / .FALSE.          /
      DATA       ISTEP          / 0 /
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
      CALL AHSTIN               !! standard data output
     I         ( GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     I           GFU   , GFV   , GFT   , GFPS  , GFQ   ,
     I           OANM                                     )
*
#ifndef OPT_NODYNAMICS
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
     M         ( GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     M           GBU   , GBV   , GBT   , GBPS  , GBQ   ,
     O           GSIGD , GPID  ,
     I           TIME  , DELT  , OINIT , OANM  , GDZS    )
      CALL MASFIX
     M         ( GAPS  , GAQ   , 
     I           DELT            )
      CALL CLCEND ( 'DYNMCS' )
#else
      CALL RESET( GSIGD, IJDIM*(KMAX+1) )
      CALL RESET( GPID , IJDIM )
#endif
*
#ifdef OPT_RWRIT
         TIME = TEND
         RETURN
#endif
*
      IF ( .NOT. OINIT ) THEN
*
#ifndef OPT_NOPHYSICS
*
*     << PHYSCS : physics >>
*
         IF ( OCHECK ) THEN
            CALL ACHKV 
     I         ( GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     I           'before PHYSCS', OCKALL                 )
         ENDIF
*
         CALL CLCSTR ( 'PHYSCS' )
         CALL PHYSCS            !! physics
     M         ( GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     I           GSIGD , GPID  , GDZS  ,
     I           TIME  , DELT*2, DELT                    )
         CALL CLCEND ( 'PHYSCS' )
#endif
*
#ifndef OPT_NOUSER
         CALL DOUSER            !! user defined routine
     M         ( GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     I           GSIGD , GPID  , GDZS  ,
     I           TIME  , DELT*2                         )
#endif
*
         TIME = TIMEZ
         CALL SETTIM ( TIME )
*
         CALL HISTOU( .FALSE. ) !! output data
*
         CALL TFILT             !! time filter
     M         ( GBU   , GBV   , GBT   , GBPS  , GBQ   ,
     I           GAU   , GAV   , GAT   , GAPS  , GAQ    )
         CALL FIXMAS
     M         ( GBPS  ,
     I           GBQ      )
*
         CALL WRRSTR            !! write to restart file
     I         ( GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     I           GBU   , GBV   , GBT   , GBPS  , GBQ   ,
     I           TIME  , TSTART, .FALSE.                 )
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
      WRITE ( 6,* ) ' @@@ AADMN: LBM2.2 DYNAMICAL-CORE MAIN 05/08/28'
*
      CALL   REWNML ( IFPAR , JFPAR )
      READ   ( IFPAR, NMCHCK, END=180 )
  180 WRITE  ( JFPAR, NMCHCK )
      CALL   REWNML ( IFPAR , JFPAR )
      READ   ( IFPAR, NMRFRC, END=190 )
  190 WRITE  ( JFPAR, NMRFRC )
      CALL   REWNML ( IFPAR , JFPAR )
      READ   ( IFPAR, NMSFRC, END=200 )
  200 WRITE  ( JFPAR, NMSFRC )
      CALL   REWNML ( IFPAR , JFPAR )
      READ   ( IFPAR, NMANM , END=210 )
  210 WRITE  ( JFPAR, NMANM  )
*
      CALL ASETUP                      !! initial setup of atmos.
     O         ( TSTART, TEND )
*
      CALL RDSTRT                      !! read initial data
     O         ( GAU   , GAV   , GAT   , GAPS  , GAQ   , 
     O           GBU   , GBV   , GBT   , GBPS  , GBQ   , 
     O           OINIT ,
     M           TSTART                                 )
      CALL COPY ( GFU , GAU , IJDIM*KMAX       )
      CALL COPY ( GFV , GAV , IJDIM*KMAX       )
      CALL COPY ( GFT , GAT , IJDIM*KMAX       )
      CALL COPY ( GFPS, GAPS, IJDIM            )
      CALL COPY ( GFQ , GAQ , IJDIM*KMAX*NTR   )
      CALL FIXMAS
     I         ( GBPS  ,
     I           GBQ     )
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
      CALL RDFRC                !! read residual forcing
     I         ( RSFRC  , 'RESIDUAL' , .FALSE.  )
      CALL RDFRC                !! read steady forcing
     I         ( FSFRC  , 'STEADY  ' , .TRUE.  )
*
      CALL ACHKV
     I         ( GAU   , GAV   , GAT   , GAPS  , GAQ   , 
     I           'start GA'    , .TRUE.                  )
      CALL ACHKV
     I         ( GBU   , GBV   , GBT   , GBPS  , GBQ   , 
     I           'start GB'    , .TRUE.                  )
*
      ISTEP = 0
*
      RETURN
*=====================================================================
*     Finalize
*=====================================================================
      ENTRY      ATMFIN        !! AGCM finalize
     I         ( TIME   )
*
      CALL WRRSTR               !! write to restart file
     I         ( GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     I           GBU   , GBV   , GBT   , GBPS  , GBQ   ,
     I           TIME  , TSTART, .TRUE.                  )
*
      RETURN
      END
