* PACKAGE ZCCOM  !! standard physical constants (common)
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*
      REAL*8     ER                          !! radius of the earth
      REAL*8     GRAV                        !! acceleration of gravity
      REAL*8     CP                          !! specific heat of air
      REAL*8     RAIR                        !! gas constant of air
      REAL*8     EL                          !! latent heat of cond.
      REAL*8     CPVAP                       !! specific heat of vapor
      REAL*8     RVAP                        !! gas constant of q
      REAL*8     DWATR                       !! density of water
      REAL*8     EPSV                        !! molec. weight water/air
      REAL*8     ES0                         !! saturate e 0deg C(Pa)
      REAL*8     STB                         !! Stefan-Boltzman
      REAL*8     FKARM                       !! Karman constant
      REAL*8     EPSVT                       !! 1/epsilon-1
      REAL*8     EMELT                       !! latent heat of melt.
      REAL*8     TMELT                       !! melting point of water
      REAL*8     CPWATR                      !! specific heat of water
      REAL*8     CPICE                       !! specific heat of ice
      REAL*8     TFRZS                       !! freeze point of sea
      REAL*8     TQICE                       !! T thres. for ice qsat
*
      COMMON   /COMCON/
     &          ER    , GRAV  ,
     &          CP    , RAIR  ,
     &          EL    , CPVAP , RVAP  , DWATR , EPSV  , ES0   ,
     &          STB   , FKARM , EPSVT , EMELT , TMELT , CPWATR,
     &          CPICE , TFRZS , TQICE
