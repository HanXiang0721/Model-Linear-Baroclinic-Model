* PACKAGE DOUSER  !! user defined routine (DUMMY)
*
*   [HIS] 96/06/06(numaguti)
*         96/09/18(numaguti) GSIGD,GPID
***********************************************************************
      SUBROUTINE DOUSER    !! user defined function
     M         ( GDU   , GDV   , GDT   , GDPS  , GDQ   ,
     I           GDSIGD, GDPID , GDZS  ,
     I           TIME  , DELTX                          )
*     
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zpdim.F"                /* physics etc.           */
#include        "zhdim.F"                /* # of char. in string   */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
*
*   [MODIFY] 
      REAL*8     GDU   ( IJDIM, KMAX      )  !! westerly
      REAL*8     GDV   ( IJDIM, KMAX      )  !! southern wind
      REAL*8     GDT   ( IJDIM, KMAX      )  !! temperature
      REAL*8     GDPS  ( IJDIM            )  !! surface pressure
      REAL*8     GDQ   ( IJDIM, KMAX, NTR )  !! humidity etc.
*
*   [INPUT] 
      REAL*8     GDSIGD( IJDIM, KMAX+1    )  !! sigma velocity
      REAL*8     GDPID ( IJDIM            )  !! pi velocity
      REAL*8     GDZS  ( IJDIM            )  !! surface topography
      REAL * 8   TIME                        !! time
      REAL*8     DELTX                       !! time step 2*delta(t)
*
*   [INTERNAL SAVE] 
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         OFIRST = .FALSE. 
         WRITE ( 6,* ) ' @@@ DOUSER: DO NOTHING 96/06/06'
      ENDIF
*
      RETURN
      END
