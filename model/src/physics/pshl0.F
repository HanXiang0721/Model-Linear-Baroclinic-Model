* PACKAGE PSHL0   !! physics: shallow cumulus (dummy)
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*
*
***********************************************************************
      SUBROUTINE SHLCOF    !! shallow cumulus: diffusion type, dummy
     M         ( DFM   , DFH   , DFE   ,
     I           GDT   , GDQ   , GDP   , GDZ   ,
     W           GDTC  , GDQC  , ADIAF , DFS     )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zccom.F"                /* stand. physical const. */
*
*   [MODIFY] 
      REAL*8     DFM   ( IJSDIM, KMAX ) !! trans. coef. :u
      REAL*8     DFH   ( IJSDIM, KMAX ) !! trans. coef. :T
      REAL*8     DFE   ( IJSDIM, KMAX ) !! trans. coef. :q
*
*   [INPUT] 
      REAL*8     GDT   ( IJSDIM, KMAX )   !! temperature T
      REAL*8     GDQ   ( IJSDIM, KMAX )   !! humidity q
      REAL*8     GDP   ( IJSDIM, KMAX )   !! pressure P
      REAL*8     GDZ   ( IJSDIM, KMAX )   !! altitude Z
*
*   [WORK] 
      REAL*8     GDTC  ( IJSDIM, KMAX )   !! cloud temperature
      REAL*8     GDQC  ( IJSDIM, KMAX )   !! specific humidity of cloud
      REAL*8     ADIAF ( IJSDIM, KMAX )   !! dry adiabat
      REAL*8     DFS   ( IJSDIM, KMAX )   !! trans. coef. (monitor)
*
*   [INTERNAL ONCE] 
      LOGICAL    OFIRST
      DATA       OFIRST / .TRUE. /
*
*   [ONCE] 
      IF ( OFIRST ) THEN
         WRITE ( 6,* ) ' @@@ PSHL0: SHALLOW CONV. REMOVED 90/12/26'
         OFIRST = .FALSE.
      ENDIF
*
      RETURN
      END
