* PACKAGE ZQSAT     !! Clausis-Clapeyron
*
*     QSAT:  saturation water vapour mixing ratio
*     DQSAT: d(QSAT)/d(T)
*
      REAL*8     FQSAT
      REAL*8     FDQSAT
#ifdef OPT_FQSATL
      REAL*8     FQSATL
      REAL*8     FQSATI
      REAL*8     FQSATR
#endif
      REAL*8     T, P, QS
*
      FQSAT ( T,P )  = EPSV * ES0 / P
     &               * EXP( (EL+EMELT/2.D0*(1.D0-SIGN(1.D0,T-TQICE)))
     &                      /RVAP *( 1.D0/TMELT - 1.D0/T )           )
*
      FDQSAT( T,QS ) = (EL+EMELT/2.D0*(1.D0-SIGN(1.D0,T-TMELT)))
     &               * QS / ( RVAP * T*T )
*
#ifdef OPT_FQSATL
      FQSATL( T,P )  = EPSV * ES0 / P
     &               * EXP( EL/RVAP *( 1.D0/TMELT - 1.D0/T ) )
*
      FQSATI( T,P )  = EPSV * ES0 / P
     &               * EXP( (EL+EMELT)/RVAP *( 1.D0/TMELT - 1.D0/T ) )
*
      FQSATR( T   )  = EXP( -EMELT/RVAP *( 1.D0/TMELT - 1.D0/T ) )
#endif

