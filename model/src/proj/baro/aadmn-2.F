*  PACKAGE AADMIN !! admin: main routine
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/04/30(numaguti) TSTART, TEND
*         96/06/06(numaguti) change FIXMAS for accurate restart
*         96/06/19(numaguti) AGCM5.4f
*         96/09/18(numaguti) GSIGD,GPID
*         01/06/18(hiro)     barotropic vorticity eq.
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
      REAL*8     GFPSI ( IJDIM, KMAX      )  !! BS stream function
      REAL*8     GAU   ( IJDIM, KMAX      )  !! westerly
      REAL*8     GAV   ( IJDIM, KMAX      )  !! southern wind
      REAL*8     GDZS  ( IJDIM            )  !! topography
      INTEGER    ISTEP                 !! serial No. of step
      REAL*8     DELT                  !! time step delta(t)
      LOGICAL    OINIT                 !! initializing time or not
*        : time etc.
*
      REAL * 8   TSTART                !! start time
      INTEGER    IEOD
*
      SAVE       GAU, GAV
      SAVE       GFPSI
      SAVE       ISTEP, DELT, OINIT
      SAVE       TSTART
*
*   [INTERNAL WORK] 
      INTEGER    IFPAR, JFPAR
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
      LOGICAL    OCHECK, OCKALL
      NAMELIST  /NMCHCK/ OCHECK, OCKALL
      DATA       OCHECK, OCKALL / .FALSE., .FALSE. /
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
      CALL BHSTIN               !! standard data output
     I         ( GAU, GAV   )
*
*     << DYNMCS : dynamics >>
*     
      CALL CLCSTR ( 'DYNMCS' )
      CALL DYNMCS               !! dynamics
     M         ( GAU   , GAV   , 
     I           GFPSI , GDZS  ,
     I           TIME  , DELT  , OINIT )
      CALL CLCEND ( 'DYNMCS' )
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
      WRITE ( 6,* ) ' @@@ AADMN: LBM2.2 BAROTROPIC MAIN 05/08/16'
*
      CALL   REWNML ( IFPAR , JFPAR )
      READ   ( IFPAR, NMCHCK, END=190 )
  190 WRITE  ( JFPAR, NMCHCK )
*
      CALL ASETUP                      !! initial setup of atmos.
     O         ( TSTART, TEND )
*
      OINIT = .TRUE.

      CALL RESET( GFPSI,  IJDIM*KMAX )
      CALL BRDGDST              !! read initial data
     O         ( GFPSI , IEOD ,
     M           TSTART,
     I           'GF'                                           )
      IF ( IEOD .GE. 1 ) THEN
         WRITE (6,*) 
     &          ' ### ATMINI: No Initial Data'
         CALL XABORT( 1 )
         RETURN
      ENDIF
*
      CALL TIMADJ                      !! adjust time for experiment 
     M         ( TSTART, TEND  )
      TIME = TSTART
*
      CALL RESET( GDZS, IJDIM )
      CALL RDDAT
     O         ( GDZS  , IEOD  ,
     I           TIME  , 'GRZ' , 'ASFC', 0.D0  , .TRUE.  )
*
      CALL BCHKV
     I         ( GFPSI         ,
     I           'start GF'    , .TRUE.                  )
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
      WRITE ( 6,* ) ' @@@ FINALIZE A COLUMN CALC.'
cc      CALL WRRSTR               !! write to restart file
cc     I         ( GAU   , GAV   , GAT   , GAPS  , GFQ   ,
cc     I           GBU   , GBV   , GBT   , GBPS  , GBQ   ,
cc     I           TIME  , TSTART, .TRUE.                  )
*
      RETURN
      END
**********************************************************************
      SUBROUTINE BRDGDST    !! read initial data
     O         ( GDPSI , IEOD ,
     M           TSTART, 
     I           HCLAS                                          )
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc. */
#include        "zhdim.F"                /* # of char. in string */
*
*   [OUTPUT]
      REAL*8     GDPSI ( IJDIM, KMAX      )  !! psi
      INTEGER    IEOD                        !! end of data flag
*
*   [MODIFY] 
      REAL * 8   TSTART                      !! initial time
*
*   [INPUT] 
      CHARACTER  HCLAS *(*)                  !! GA or GB
*
*   [INTERNAL WORK]
      REAL * 8   TINIT0                !! time of initial data
      REAL * 8   TINIT1                !! time of initial data
      REAL * 8   TIMER
      INTEGER    IFLSTR, IEODX, KLEVS
      CHARACTER  HDFMT *(NCC)
      CHARACTER  HITEM *(NCC)
      CHARACTER  HITEMD*(NCC)
      CHARACTER  HX *2
*
*   [INTERNAL PARAM]
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
      CALL OPNINI         !! open initial data
     O         ( IFLSTR, TINIT0, TINIT1, HDFMT ,
     I           TSTART, HCLAS                  )
      HX = HCLAS
*
      IEOD = 1
 1100 CONTINUE 
      HITEM = HX//'PSI'
      CALL GDRDTS
     O         ( GDPSI , IEODX , 
     O           HITEMD, TIMER , KLEVS ,
     I           IFLSTR, HITEM , HDFMT ,
     I           IMAX  , JMAX  , TINIT0, TINIT1 ,
     I           IDIM  , JDIM  , KMAX             )
      IF ( IEODX  .GT. 0 ) RETURN
      IF ( TINIT0 .LT. 0 ) TSTART = TIMER
*
      IEOD = 0
      IF ( TINIT0 .LT. 0 ) THEN
         GOTO 1100
      ENDIF
*
      RETURN
      END
***********************************************************************
      SUBROUTINE BHSTIN    !! output standard history 
     I         ( GDU   , GDV   )
*
*   [PARAM] 
#include        "zcdim.F"
#include        "zpdim.F"                /* physics etc. */
#include        "zhdim.F"
*      
*   [INPUT] 
      REAL*8     GDU   ( IJKDIM )       !! westerly u
      REAL*8     GDV   ( IJKDIM )       !! southern wind v
*
*   [INTERNAL WORK] 
*
      CALL     HISTIN
     I       ( GDU, 'U ', 'u-velocity       ' ,'m/s  ', 'ALEV'   )
      CALL     HISTIN
     I       ( GDV, 'V ', 'v-velocity       ' ,'m/s  ', 'ALEV'   )
*
      RETURN
      END
*********************************************************************
      SUBROUTINE BCHKV     !! valid range monitor
     I         ( GDPSI , 
     I           HLAB  , OALL                            )
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc. */
*
*   [INPUT] 
      REAL*8     GDPSI ( IJKDIM )       !! psi
      CHARACTER  HLAB *(*)
      LOGICAL    OALL
*
      CALL CHKVAL(GDPSI,   IDIM,JDIM,KMAX, -2.D8, 2.D8,'PSI',HLAB,OALL)
      IF ( OALL ) THEN
         CALL DIAGP ( GDPSI,    'GDPSI',  KMAX,'(G13.5)'  )
      ENDIF
*
      RETURN
      END
