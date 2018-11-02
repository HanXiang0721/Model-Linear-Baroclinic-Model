*  PACKAGE AADMIN !! admin: main routine
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/04/30(numaguti) TSTART, TEND
*         96/06/06(numaguti) change FIXMAS for accurate restart
*         96/06/19(numaguti) AGCM5.4f
*         96/09/18(numaguti) GSIGD,GPID
*         01/06/15(hiro)     PWM incl. moisture
*         05/05/26(hiro)     AIM for baroclinic model
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
      REAL * 8   TIME                        !! time
      REAL * 8   TEND                        !! end time
*
*   [INTERNAL SAVE] 
      REAL*8     GFU   ( IJDIM, KMAX      )  !! BS zonal wind
      REAL*8     GFV   ( IJDIM, KMAX      )  !! BS meridional wind
      REAL*8     GFT   ( IJDIM, KMAX      )  !! BS temperature
      REAL*8     GFPS  ( IJDIM            )  !! BS surface pressure
      REAL*8     GFQ   ( IJDIM, KMAX      )  !! BS specific humidity
*        : grid value(t) <DYNMCS>  grid value(t+dt)
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
      SAVE       ISTEP, DELT, OINIT
      SAVE       TSTART, GDZS
*
*   [INTERNAL WORK] 
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
      LOGICAL    OMKMAT         !! use model to make matrix or iteration?
      NAMELIST  /NMUSE/ OMKMAT
      DATA       OMKMAT / .TRUE. /
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
     I           TIME  , GFU   , GFV   , ISTEP  )
*
      TIMEZ = ANINT( TIME + DELT )
      IF ( TIMEZ .GT. TEND ) THEN
         TIMEZ = TEND
         DELT  = TIMEZ - TIME
      ENDIF
*
*     << DYNMCS : dynamics >>
*     
      IF( OMKMAT ) THEN         !! make linear PWM matrix
         CALL DYNMCS_MAT              
     I         ( GFU   , GFV   , GFT   , GFPS  , GFQ   ,
     I           TIME  , DELT  , GDZS  )
      ELSE
         CALL DYNMCS_AIM        !! AIM with inverse PWM matrix 
     I         ( GFU   , GFV   , GFT   , GFPS  , GFQ   ,
     I           TIME  , DELT  , GDZS  )
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
      CALL   REWNML ( IFPAR , JFPAR )
      READ   ( IFPAR, NMUSE, END=190 )
  190 WRITE  ( JFPAR, NMUSE )
*
      IF( OMKMAT ) THEN         !! make linear PWM matrix
         WRITE ( 6,* ) ' @@@ AADMN: LBM2.2 AIM MAKING MATRIX 05/05/26'
      ELSE
         WRITE ( 6,* ) ' @@@ AADMN: LBM2.2 AIM MAIN 05/05/26'
      ENDIF
*
      CALL ASETUP                      !! initial setup of atmos.
     O         ( TSTART, TEND )
*
      CALL RDSTRT                      !! read initial data
     O         ( GFU   , GFV   , GFT   , GFPS  , GFQ   , 
     O           OINIT ,
     M           TSTART                                 )
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
      CALL MTRUNW( GFPS  ,    1    ) 
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
*
      RETURN
      END
