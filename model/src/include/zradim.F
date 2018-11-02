* PACKAGE ZRADIM  !! parameter for radiation code
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/04/16(numaguti) Itamar Aerosol
*         95/03/15(numaguti) AGCM5.4f
*
* ------------------------
*  for Nakajima radiation
* ------------------------
*
      INTEGER    NRBFLX
      PARAMETER ( NRBFLX = 3 )
*
      INTEGER    KMOL, KPCL
      PARAMETER ( KMOL   = 5 )
      PARAMETER ( KPCL   = 4 )
*
      INTEGER    KPLK, KDA, KCLD
      PARAMETER ( KPLK   = 2 )
      PARAMETER ( KDA    = 1 )
      PARAMETER ( KCLD   = 3 )
*
      INTEGER    KSFC, KWNB, KFLG, KCH
      INTEGER    KAO3, KPG , KTG,  KDMAX
      PARAMETER ( KSFC  =   7 )
      PARAMETER ( KWNB  =  18 )
      PARAMETER ( KFLG  =   9 )
      PARAMETER ( KCH   =  13 )
      PARAMETER ( KAO3  =   3 )
      PARAMETER ( KPG   =  10 )
      PARAMETER ( KTG   =   5 )
      PARAMETER ( KDMAX =   6 )
*
      INTEGER    KPLNK, KH2O
      PARAMETER ( KPLNK =   5 )
      PARAMETER ( KH2O  =   3 )
      INTEGER    KKDT, KKDP
      PARAMETER ( KKDT  =   3 )
      PARAMETER ( KKDP  =   8 )
      INTEGER    KCFC
      PARAMETER ( KCFC  =  16 )
*
      INTEGER   KCPCL, KAPC2, KAPC3, KRHG, KKPCL, KAPCL, KNPCL
      PARAMETER ( KCPCL =     2 )    !! cloud particle
      PARAMETER ( KAPC2 =     2 )    !! aerosol # of 2-dim distribution
      PARAMETER ( KAPC3 =     1 )    !! aerosol # of 3-dim distribution
      PARAMETER ( KRHG  =     8 )    !! RH grid
      PARAMETER ( KKPCL =     8 )    !! particle types in PARA
#ifdef OPT_ITAMAR
      PARAMETER ( KAPCL =    12 )    !! aerosol types
      PARAMETER ( KNPCL = KCPCL )    !! particle types in Q
#else
      PARAMETER ( KAPCL =     6 )    !! aerosol types
      PARAMETER ( KNPCL = KKPCL )    !! particle types in Q
#endif
      INTEGER    KAPCD               !! for compatibility
      PARAMETER ( KAPCD =     2 )    !! aerosol data coexist
