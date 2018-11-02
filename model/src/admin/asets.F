* PACKAGE ASETS  !! admin: constants (sigma-level)
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
***********************************************************************
      SUBROUTINE SETSIG    !!  set sigma level
     O         ( SIG   , DSIG  , HASIG ,
     O           SIGM  , DSIGM , HASIGM  )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zhdim.F"
*
*   [OUTPUT] 
      REAL*8     SIG   ( KMAX )              !! sigma-level (integer)
      REAL*8     SIGM  (KMAX+1)              !! sigma-level (half lev)
      REAL*8     DSIG  ( KMAX )              !! delta(sigma) (integer)
      REAL*8     DSIGM (KMAX+1)              !! delta(sigma) (half lev)
      CHARACTER  HASIG   *(NCC)
      CHARACTER  HASIGM  *(NCC)
*
*   [INTERNAL WORK] 
      INTEGER    IFPAR, JFPAR
*
*   [INTERNAL PARM] 
#include        "zlsig.F"            /* def. of SIGML,HSIG,HSIGM */
*
      CALL SETSI    !!  sigma-level SIGM -> SIG
     O         ( SIG   , DSIG  ,
     O           SIGM  , DSIGM ,
     I           SIGML , KMAX   )
*
      HASIG  = HSIG
      HASIGM = HSIGM
*
      RETURN
      END
***********************************************************************
      SUBROUTINE SETSI    !!  sigma-level SIGM -> SIG
     O         ( SIG   , DSIG  ,
     O           SIGM  , DSIGM ,
     I           SIGML , KMAX   )
*
*                (*  Arakawa & Suarez vert.diffr in sigma *)
*
*   [PARAM] 
      INTEGER    KMAX
#include        "zccom.F"                /* stand. physical const. */
*
*   [OUTPUT] 
      REAL*8     SIG   ( KMAX )              !! sigma-level (integer)
      REAL*8     DSIG  ( KMAX )              !! delta(sigma) (integer)
      REAL*8     SIGM  (KMAX+1)              !! sigma-level (half lev)
      REAL*8     DSIGM (KMAX+1)              !! delta(sigma) (half lev)
*
*   [INPUT] 
      REAL*8     SIGML (KMAX+1)              !! sigma-level (half lev)
*
*    [INTERNAL WORK]    
      REAL*8     AK                        !! kappa = R/Cp
      REAL*8     AK1                       !! 1+kappa
      REAL*8     AKIV                       !! 1/kappa
      INTEGER    K
*
      AK   = RAIR / CP
      AK1  = 1.D0 + AK
      AKIV = 1.D0 / AK
*
      CALL COPY( SIGM, SIGML, KMAX+1 )
*
      DO 3100 K = 1 , KMAX
        DSIG( K ) =   SIGM( K ) - SIGM( K+1 )
        SIG ( K ) = (  ( SIGM( K )**AK1 - SIGM( K+1 )**AK1 )
     &                /( DSIG( K )*AK1                     ) )**AKIV
 3100 CONTINUE
*
      DO 3200 K = 2, KMAX
        DSIGM( K ) = SIG ( K-1 ) - SIG ( K )
 3200 CONTINUE
      DSIGM( 1      ) = SIGM( 1    ) - SIG ( 1      )
      DSIGM( KMAX+1 ) = SIG ( KMAX ) - SIGM( KMAX+1 )
*
      RETURN
      END
