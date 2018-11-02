*  PACKAGE AADMIN !! admin: main routine
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/04/30(numaguti) TSTART, TEND
*         96/06/06(numaguti) change FIXMAS for accurate restart
*         96/06/19(numaguti) AGCM5.4f
*         96/09/18(numaguti) GSIGD,GPID
*         01/06/15(hiro)     PWM incl. moisture
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
      REAL*8     GFU   ( IJDIM, KMAX      )  !! westerly
      REAL*8     GFV   ( IJDIM, KMAX      )  !! southern wind
      REAL*8     GFT   ( IJDIM, KMAX      )  !! temperature
      REAL*8     GFPS  ( IJDIM            )  !! surface pressure
      REAL*8     GFPSM ( IJDIM            )  !! surface pressure (zonal mean)
      REAL*8     GFQ   ( IJDIM, KMAX      )  !! humidity etc.
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
*        : grid value(t-dt) <DYNMCS>  grid value(t)
*
      INTEGER    ISTEP                 !! serial No. of step
      REAL*8     DELT                  !! time step delta(t)
      LOGICAL    OINIT                 !! initializing time or not
*        : time etc.
*
      REAL * 8   TSTART                !! start time
      REAL*8     GDZS  ( IJDIM )       !! surface topography
      INTEGER    IEOD
*
      SAVE       GFU, GFV, GFT, GFPS, GFQ
      SAVE       GAU, GAV, GAT, GAPS, GAQ
      SAVE       GBU, GBV, GBT, GBPS, GBQ
      SAVE       ISTEP, DELT, OINIT
      SAVE       TSTART, GDZS
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
      CALL SETPSF               !! set output Ps
     I         ( GAPS   )     
      CALL AHSTIN               !! standard data output
     I         ( GAU   , GAV   , GAT   , GAPS  , GFQ   )
*
#ifndef OPT_NODYNAMICS
*
*     << DYNMCS : dynamics >>
*     
c      IF ( OCHECK ) THEN
c         CALL ACHKV 
c     I         ( GAU   , GAV   , GAT   , GAPS  , GFQ   ,
c     I           'before DYNMCS', OCKALL                 )
c      ENDIF
*
      CALL CLCSTR ( 'DYNMCS' )
      CALL DYNMCS               !! dynamics
     M         ( GAU   , GAV   , GAT   , GAPS  , GAQ   ,
     I           GFU   , GFV   , GFT   , GFPS  , GFQ   ,
     I           GFPSM ,
     O           GSIGD , GPID  ,
     I           TIME  , DELT  , GDZS  )
ccc      CALL MASFIX
ccc     M         ( GAPS  , GFQ   , 
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
     I         ( GAU   , GAV   , GAT   , GAPS  , GFQ   ,
     I           'before PHYSCS', OCKALL                 )
         ENDIF
*
         CALL CLCSTR ( 'PHYSCS' )
         CALL PHYSCS            !! physics
     M         ( GAU   , GAV   , GAT   , GAPS  , GFQ   ,
     I           GSIGD , GPID  , GDZS  ,
     I           TIME  , DELT*2, DELT                    )
         CALL CLCEND ( 'PHYSCS' )
#endif
*
#ifndef OPT_NOUSER
         CALL DOUSER            !! user defined routine
     M         ( GAU   , GAV   , GAT   , GAPS  , GFQ   ,
     I           GSIGD , GPID  , GDZS  ,
     I           TIME  , DELT*2                         )
#endif
*
         TIME = TIMEZ
         CALL SETTIM ( TIME )
*
         CALL HISTOU( .FALSE. ) !! output data
*
ccc         CALL TFILT             !! time filter
ccc     M         ( GBU   , GBV   , GBT   , GBPS  , GBQ   ,
ccc     I           GAU   , GAV   , GAT   , GAPS  , GFQ    )
ccc         CALL FIXMAS
ccc     M         ( GBPS  ,
ccc     I           GBQ   , DELT  )
*
ccc         CALL WRRSTR            !! write to restart file
ccc     I         ( GAU   , GAV   , GAT   , GAPS  , GFQ   ,
ccc     I           GBU   , GBV   , GBT   , GBPS  , GBQ   ,
ccc     I           TIME  , TSTART, .FALSE.                 )
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
      WRITE ( 6,* ) ' @@@ AADMN: LBM2.2 MKAMAT MAIN 05/08/18'
*
      CALL   REWNML ( IFPAR , JFPAR )
      READ   ( IFPAR, NMCHCK, END=190 )
  190 WRITE  ( JFPAR, NMCHCK )
*
      CALL ASETUP                      !! initial setup of atmos.
     O         ( TSTART, TEND )
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
*#ifdef OPT_WVFRC
      CALL RESET( GFPSM, IJDIM )
      CALL RDPSZM
     O         ( GFPSM , IEOD  ,
     I           'GA'  , TIME   )
      IF( IEOD .GT. 0 ) THEN
         WRITE ( 6,* ) ' ### ATMINI: ZM BASIC STATE WAS NOT READ'
*         CALL XABORT( 1 )
      ENDIF
*#endif
*
      CALL RESET( GDZS, IJDIM )
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
     I         ( GFT   , GFQ   , GFTV  , GFPS  , GDZS     )
      CALL SETBULK
     I         ( GFU   , GFV   , GFT   , GFPS  , GFQ   ,
     I           GFTV  , GFSST , GFWG  , GDZS  , GIDX     )

#ifdef OPT_DYNZM
      CALL MTRUN0( GFU  , KMAX    )
      CALL MTRUN0( GFV  , KMAX    )
      CALL MTRUN0( GFT  , KMAX    )
      CALL MTRUN0( GFPS ,    1    )
      CALL MTRUN0( GFQ  , KMAX    )
#else if OPT_DYNSW
      CALL MTRUNW( GFU   , KMAX    )
      CALL MTRUNW( GFV   , KMAX    )
      CALL MTRUNW( GFT   , KMAX    )
      CALL MTRUNW( GFQ   , KMAX    )
      CALL COPY  ( GFPSM , GFPS , IJDIM )
      CALL MTRUNW( GFPS  ,    1    ) 
      CALL MTRUN0( GFPSM ,    1    ) 
#endif
*
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
     I         ( GAU   , GAV   , GAT   , GAPS  , GFQ   ,
     I           GBU   , GBV   , GBT   , GBPS  , GBQ   ,
     I           TIME  , TSTART, .TRUE.                  )
*
      RETURN
      END
**********************************************************************
      SUBROUTINE RDPSZM
     O         ( GDPS  , IEOD  ,
     I           HCLAS , TSTART                          )
*
*   [PARAM]
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc. */
#include        "zhdim.F"                /* # of char. in string */
*
*   [OUTPUT]
      REAL*8     GDPS  ( IJDIM            )  !! surface pressure
      REAL*8     GDX   ( IJDIM , KMAX     )  !! dummy
      INTEGER    IEOD                        !! end of data flag
*
*   [INPUT] 
      CHARACTER  HCLAS *(*)                  !! GA or GB
      REAL*8     TSTART
*
*   [INTERNAL WORK]
      REAL * 8   TINIT0                !! time of initial data
      REAL * 8   TINIT1                !! time of initial data
      REAL * 8   TIME, TIMER
      INTEGER    IFPAR, JFPAR
      INTEGER    IFLSTR, IEODX, KLEVS
      CHARACTER  HDFMT *(NCC)
      CHARACTER  HITEM *(NCC)
      CHARACTER  HITEMD*(NCC)
      CHARACTER  HX *2
      DATA HDFMT / 'UR4' /
*
*   [INTERNAL PARAM]
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
      CHARACTER  FILE *(NFILN)  !! zonal mean basic state
      NAMELIST  /NMBSZM/ FILE
      DATA       FILE / '      '/

      IF( OFIRST ) THEN
         OFIRST = .FALSE.
         CALL   REWNML ( IFPAR , JFPAR )
         READ   ( IFPAR, NMBSZM, END=190 )
  190    WRITE  ( JFPAR, NMBSZM)
         IF( FILE(1:1) .NE. ' ' ) THEN
            IFLSTR = 76
            OPEN( IFLSTR, FILE = FILE, FORM='UNFORMATTED',
     $           STATUS = 'UNKNOWN' )
            WRITE( 6, *) '    $$$ OPEN ZONAL STATE 00/04/19 $$$'
         ENDIF
         HX = HCLAS
         TINIT0 = TSTART
         TINIT1 = TINIT0 + 1.D3
      ENDIF
*
      IEOD = 1
      IF( FILE(1:1) .EQ. ' ' ) RETURN
*
      HITEM = HX//'U'
      CALL GDRDTS
     O     ( GDX   , IEODX , 
     O       HITEMD, TIMER , KLEVS ,
     I       IFLSTR, HITEM , HDFMT ,
     I       IMAX  , JMAX  , TINIT0, TINIT1 ,
     I       IDIM  , JDIM  , KMAX             )
      IF ( IEODX  .GT. 0 ) RETURN
*
      HITEM = HX//'V'
      CALL GDRDTS
     O     ( GDX   , IEODX , 
     O       HITEMD, TIMER , KLEVS ,
     I       IFLSTR, HITEM , HDFMT ,
     I       IMAX  , JMAX  , TINIT0, TINIT1 ,
     I       IDIM  , JDIM  , KMAX             )
      IF ( IEODX  .GT. 0 ) RETURN
*
      HITEM = HX//'T'
      CALL GDRDTS
     O     ( GDX   , IEODX , 
     O       HITEMD, TIMER , KLEVS ,
     I       IFLSTR, HITEM , HDFMT ,
     I       IMAX  , JMAX  , TINIT0, TINIT1 ,
     I       IDIM  , JDIM  , KMAX             )
      IF ( IEODX  .GT. 0 ) RETURN
*
      HITEM = HX//'PS'
      CALL GDRDTS
     O     ( GDPS  , IEODX ,
     O       HITEMD, TIME  , KLEVS ,
     I       IFLSTR, HITEM , HDFMT , 
     I       IMAX  , JMAX  , TINIT0, TINIT1 ,
     I       IDIM  , JDIM  , 1               )
      IF ( IEODX .GT. 0 ) RETURN
      IEOD = IEODX
*
      RETURN
      END
