* PACKAGE APCON  !! admin: set standard physical constants
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*   [HIS] 95/04/29(numaguti) EMELT
*         97/06/03(numaguti) TQICE
*
*********************************************************************
      SUBROUTINE PCONST    !! stand. physical const.
     I          ( ONML )
*
#include        "zccom.F"                /* stand. physical const. */
*
*     COMMON /COMCON/
*     REAL*8     ER                          !! radius of the earth
*     REAL*8     GRAV                        !! acceleration of gravity
*     REAL*8     CP                          !! specific heat of air
*     REAL*8     RAIR                        !! gas constant of air
*     REAL*8     EL                          !! latent heat of cond.
*     REAL*8     CPVAP                       !! specific heat of vapor
*     REAL*8     RVAP                        !! gas constant of q
*     REAL*8     DWATR                       !! density of water
*     REAL*8     EPSV                        !! molec. weight water/air
*     REAL*8     ES0                         !! saturate e 0deg C(Pa)
*     REAL*8     STB                         !! Stefan-Boltzman
*     REAL*8     FKARM                       !! Karman constant
*     REAL*8     EPSVT                       !! 1/epsilon-1
*     REAL*8     EMELT                       !! latent heat of melt.
*     REAL*8     TMELT                       !! melting point of water
*     REAL*8     CPWATR                      !! specific heat of water
*     REAL*8     CPICE                       !! specific heat of ice
*     REAL*8     TFRZS                       !! freeze point of sea
*     REAL*8     TQICE                       !! T thres. for ice qsat
*     COMMON /COMCON/ end
*
*    [INPUT]
      LOGICAL    ONML           !! use namelist
*
*    [INTERNAL WORK]
      INTEGER    IFPAR, JFPAR
*
      NAMELIST /NMPCON/
     &          ER    , GRAV  ,
     &          CP    , RAIR  ,
     &          EL    , CPVAP , RVAP  , DWATR , EPSV  , ES0   ,
     &          STB   , FKARM , EPSVT , EMELT , TMELT , CPWATR,
     &          CPICE , TFRZS , TQICE
*
      ER     = 6370.E+3
      GRAV   = 9.8  
      CP     = 1004.6  
      RAIR   = 287.04  
      EL     = 2.5E+6
      CPVAP  = 1810.  
      RVAP   = 461.  
      DWATR  = 1000.  
      ES0    = 611.  
      STB    = 5.67E-8
      FKARM  = 0.4  
      EMELT  = 3.4E+5
      TMELT  = 273.15
      CPWATR = 4200.
      CPICE  = 2000.
      TFRZS  = 271.35
      TQICE  = 273.15
*
      IF( ONML ) THEN
         CALL   REWNML ( IFPAR , JFPAR )
         WRITE  ( JFPAR, * ) ' NAMELIST NMPCON: ' 
         READ   ( IFPAR, NMPCON, END=5190 )
 5190    CONTINUE 
      ENDIF
*
      EPSV   = RAIR / RVAP
      EPSVT  = 1.0/EPSV - 1.0
*
      WRITE  ( JFPAR, NMPCON )
*
      RETURN
      END
