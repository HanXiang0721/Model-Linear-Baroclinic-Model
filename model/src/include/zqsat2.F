* PACKAGE ZQSAT     !! Clausis-Clapeyron
*
*     QSAT:  saturation water vapour mixing ratio
*     DQSAT: d(QSAT)/d(T)
*
      REAL*8     FQSATL
      REAL*8     FQSATI
      REAL*8     FQSATR
*
      FQSATL( T,P )  = EPSV * ES0 / P
     &               * EXP( EL/RVAP *( 1.D0/TMELT - 1.D0/T ) )
*
      FQSATI( T,P )  = EPSV * ES0 / P
     &               * EXP( (EL+EMELT)/RVAP *( 1.D0/TMELT - 1.D0/T ) )
*
      FQSATR( T   )  = EXP( -EMELT/RVAP *( 1.D0/TMELT - 1.D0/T ) )
