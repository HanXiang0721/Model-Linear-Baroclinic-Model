*  PACKAGE AADMIN !! admin: main routine
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/04/30(numaguti) TSTART, TEND
*         96/06/06(numaguti) change FIXMAS for accurate restart
*         96/06/19(numaguti) AGCM5.4f
*         96/09/18(numaguti) GSIGD,GPID
*         01/07/10(hiro)     linear MCA after Neelin&Yu (1994,JAS)
*         02/08/15(hiro)     linear LSC
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
      REAL*8     GAQ   ( IJDIM, KMAX      )  !! humidity etc.
*        : grid value(t) <DYNMCS>  grid value(t+dt)
*
      REAL*8     GBU   ( IJDIM, KMAX      )  !! westerly
      REAL*8     GBV   ( IJDIM, KMAX      )  !! southern wind
      REAL*8     GBT   ( IJDIM, KMAX      )  !! temperature
      REAL*8     GBPS  ( IJDIM            )  !! surface pressure
      REAL*8     GBQ   ( IJDIM, KMAX      )  !! humidity etc.
      REAL*8     GFU   ( IJDIM, KMAX      )  !! BS westerly u
      REAL*8     GFV   ( IJDIM, KMAX      )  !! BS southery wind v
      REAL*8     GFT   ( IJDIM, KMAX      )  !! BS temperature
      REAL*8     GFPS  ( IJDIM            )  !! BS surface pressure
      REAL*8     GFQ   ( IJDIM, KMAX      )  !! BS humidity
      SAVE       GFU, GFV, GFT, GFPS, GFQ
*        : grid value(t-dt) <DYNMCS>  grid value(t)
*
      INTEGER    ISTEP                 !! serial No. of step
      REAL*8     DELT                  !! time step delta(t)
      LOGICAL    OINIT                 !! initializing time or not
*        : time etc.
*
      REAL * 8   TSTART                !! start time
      REAL*8     GDZS  ( IJDIM )       !! surface topography
      REAL * 8   TFSEND                !! finish t of steady forcing
*
      SAVE       GAU, GAV, GAT, GAPS, GAQ
      SAVE       GBU, GBV, GBT, GBPS, GBQ
      SAVE       ISTEP, DELT, OINIT
      SAVE       TSTART, GDZS, TFSEND
*
*   [INTERNAL WORK] 
      REAL*8     GSIGD ( IJDIM, KMAX+1    )  !! sigma velocity
      REAL*8     GPID  ( IJDIM            )  !! pi velocity
#ifndef OPT_CLASSIC
      REAL*8     GFSST ( IJDIM            )  !! mean SST
      REAL*8     GFTV  ( IJDIM, KMAX      )  !! BS virtual temperature
      REAL*8     GIDX  ( IJDIM            )  !! surface index
      REAL*8     GFWG  ( IJDIM            )  !! soil wetness
#endif
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
      LOGICAL    OCHECK, OCKALL
      INTEGER    FSEND   ( 6 )  !! finish t of steady forcing
      CHARACTER*70 FFRC         !! filename for initial pert.
      CHARACTER*70 FSFRC        !! filename for steady forcing
      LOGICAL      OPER         !! use initial pert?
      LOGICAL      OFRC         !! use steady forcing?
      LOGICAL      OSSTF        !! is forcing SST?
      LOGICAL      OVD          !! initial pert. = vor,div
      INTEGER      NFCS         !! no. case for init. pert.
      INTEGER      NSFCS        !! no. case for steady forcing

      NAMELIST  /NMCHCK/ OCHECK, OCKALL
      NAMELIST  /NMFRC/  FFRC , OPER, NFCS, OVD
      NAMELIST  /NMSFRC/ FSFRC , OFRC, OSSTF, NSFCS, FSEND

      DATA         OCHECK, OCKALL / .FALSE., .FALSE. /
      DATA         ISTEP          / 0 /
      DATA         FSEND          / -1, 1, 1, 0, 0, 0 /
      DATA         OPER   / .FALSE. /
      DATA         OFRC   / .TRUE.  /
      DATA         OSSTF  / .TRUE.  /
      DATA         OVD    / .TRUE.  /
      DATA         NFCS   / 1       /
      DATA         NSFCS  / 1       /
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
     I         ( GAU   , GAV   , GAT   , GAPS  , GAQ   )
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
     I           GFU   , GFV   , GFT   , GFPS  , GFQ   ,
     I           TIME  , TFSEND, DELT  , OINIT , GDZS  ,           
     I           FFRC  , OPER  , NFCS  , OVD   ,
     I           FSFRC , OFRC  , OSSTF , NSFCS )
ccc      CALL MASFIX
ccc     M         ( GFPS  , GAQ   , 
ccc     I           DELT            )
      CALL CLCEND ( 'DYNMCS' )
#else
      CALL RESET( GSIGD, IJDIM*(KMAX+1) )
      CALL RESET( GPID , IJDIM )
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
         CALL DOUSER2           !! user defined routine
     M         ( GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     I           GFU   , GFV   , GFT   , GFPS  ,
     I           GSIGD , GPID  , GDZS  ,
     I           TIME  , DELT*2, DELT                     )
#endif
*
         TIME = TIMEZ
         CALL SETTIM ( TIME )
*
         CALL HISTOU( .FALSE. ) !! output data
*
ccc         CALL TFILT             !! time filter
ccc     M         ( GBU   , GBV   , GBT   , GBPS  ,
ccc     I           GAU   , GAV   , GAT   , GAPS   )
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
      WRITE ( 6,* ) ' @@@ AADMN: LBM2.2 TINTGR MAIN 05/08/16'
*
      CALL   REWNML ( IFPAR , JFPAR )
      READ   ( IFPAR, NMCHCK, END=190 )
  190 WRITE  ( JFPAR, NMCHCK )
*
      CALL   REWNML ( IFPAR , JFPAR )
      READ   ( IFPAR, NMFRC, END=290 )
  290 WRITE  ( JFPAR, NMFRC )
*
      CALL   REWNML ( IFPAR , JFPAR )
      READ   ( IFPAR, NMSFRC, END=390 )
  390 WRITE  ( JFPAR, NMSFRC )
*
      IF( FSEND(1) .LT. 0 ) THEN
         TFSEND = -999
      ELSE
         CALL  CYH2SS
     O        ( TFSEND  ,
     I          FSEND    )
      ENDIF
*
      CALL ASETUP                      !! initial setup of atmos.
     O         ( TSTART, TEND )
      WRITE(6,*) ' @@@ TSTART:',TSTART
      WRITE(6,*) ' @@@ TEND:  ',TEND
      WRITE(6,*) ' @@@ TFSEND:',TFSEND
*
      CALL RDSTRT                      !! read initial data
     O         ( GFU   , GFV   , GFT   , GFPS  , GFQ   , 
     O           GBU   , GBV   , GBT   , GBPS  , GBQ   , 
     O           OINIT ,
     M           TSTART                                 )
*
      CALL TIMADJ                      !! adjust time for experiment 
     M         ( TSTART, TEND  )
      TIME = TSTART
*
      CALL RESET( GDZS , IJDIM )
      CALL RDDAT
     O         ( GDZS  , ISET  ,
     I           TIME  , 'GRZ' , 'ASFC', 0.D0  , .TRUE.  )
*      
#ifndef OPT_CLASSIC
      CALL FIXMAS
     M         ( GFPS  ,
     I           GFQ   , TEND  )
      CALL MASFIX
     M         ( GFPS  , GFQ  ,
     I           TEND          )

      CALL RESET( GIDX , IJDIM )
      CALL RDDAT
     O         ( GIDX  , ISET  ,
     I           TIME  , 'GRIDX', 'ASFC', 0.D0  , .TRUE.  )
      CALL RESET( GFWG , IJDIM )
      CALL RDDAT
     O         ( GFWG  , ISET  ,
     I           TIME  , 'GRWG' , 'ASFC', 1.D0  , .TRUE.  )
      CALL RESET( GFSST, IJDIM )
      CALL RDDAT
     O         ( GFSST , ISET   ,
     I           TIME  , 'GRSST', 'ASFC', 300.D0  , .TRUE.  )
*      
      CALL VIRTMB
     O         ( GFTV  ,
     I           GFT   , GFQ     )
      CALL SETMCA
     I         ( GFT   , GFQ   , GFTV  , GFPS  , GDZS  , GIDX   )
      CALL SETLSC
     I         ( GFT   , GFQ   , GFPS  , GIDX   )
      CALL SETBULK
     I         ( GFU   , GFV   , GFT   , GFPS  , GFQ   ,
     I           GFTV  , GFSST , GFWG  , GDZS  , GIDX     )
      CALL SETLDMP ( GIDX )
#endif
*
      CALL ACHKV
     I         ( GFU   , GFV   , GFT   , GFPS  , GFQ   , 
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
      CALL WRRSTR               !! write to restart file
     I         ( GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     I           GBU   , GBV   , GBT   , GBPS  , GBQ   ,
     I           TIME  , TSTART, .TRUE.                  )
*
      CLOSE( 77 )
*
      RETURN
      END
