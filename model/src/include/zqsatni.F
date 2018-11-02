* PACKAGE ZQSAT     !! Clausis-Clapeyron
*
*     QSAT:  saturation water vapour mixing ratio
*     DQSAT: d(QSAT)/d(T)
*
      REAL*8     FQSAT
      REAL*8     FDQSAT
      REAL*8     T, P, QS
*
      FQSAT ( T,P )    = EPSV * ES0
     &                   * EXP( EL/RVAP *( 1./273. - 1./T ) )
     &                   / P
      FDQSAT( T,QS )   = EL * QS / ( RVAP * T*T )
