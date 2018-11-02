* PACKAGE ZHDIM  !! standard physical constants (common)
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
*
      PARAMETER ( ER     = 6370.E+3 )
      PARAMETER ( GRAV   = 9.8  )
      PARAMETER ( CP     = 1004.6  )
      PARAMETER ( RAIR   = 287.04  )
      PARAMETER ( EL     = 2.5E+6 )
      PARAMETER ( CPVAP  = 1810.  )
      PARAMETER ( RVAP   = 461.  )
      PARAMETER ( DWATR  = 1000.  )
      PARAMETER ( ES0    = 611.  )
      PARAMETER ( STB    = 5.67E-8 )
      PARAMETER ( FKARM  = 0.4  )
      PARAMETER ( EMELT  = 3.4E+5 )
      PARAMETER ( TMELT  = 273.15 )
      PARAMETER ( CPWATR = 4200. )
      PARAMETER ( CPICE  = 2000. )
      PARAMETER ( TFRZS  = 271.35 )
      PARAMETER ( EPSV   = RAIR / RVAP )
      PARAMETER ( EPSVT  = 1.0/EPSV - 1.0 )
