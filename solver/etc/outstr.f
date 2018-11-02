      PROGRAM OUTSTR
*
*     derived from util/gt2gr.f on 1999/05/02
*     This routine transforms Gt3 output from a storm track model
*     to GrADS file same as in the response.grd.
*
*     variables: PSI, VOR, U, V, T, Z, Ps
*
      include 'dim.f'
*
      REAL*4       XMISS
      PARAMETER (  XMISS = -999. )
      INTEGER      NV
      PARAMETER ( NV = 10 )
*
      REAL*4       DAT1( IMAX*JMAX, KMAX)
      REAL*4       DAT2( IMAX, JMAX     )
      REAL*4       G   ( IMAX, JMAX     )  
      REAL*4       GPS ( IMAX*JMAX      )
      CHARACTER    HEAD( 64 ) * 16
*
      REAL*4       COV( IMAX, JMAX, KMAX, NV )  
      REAL*4       PSI( IMAX*JMAX, KMAX)
      REAL*4       U( IMAX*JMAX, KMAX)
      REAL*4       V( IMAX*JMAX, KMAX)
      REAL*4       T( IMAX*JMAX, KMAX)
      REAL*4       Z( IMAX*JMAX, KMAX)
      REAL*4       R( IMAX*JMAX, KMAX)
*
*     [work]
      REAL*4      P( KMAX )          !! pressure level
      REAL*4      S( KMAX )          !! sigma level
      REAL*8      SIG( KMAX )        !! sigma level
      REAL*8      SIGM( KMAX+1 )     !! sigma level increment
      REAL*8      DSIG( KMAX )       !! unused
      REAL*8      DSIGM( KMAX+1 )    !! unused
      REAL*8      FACT( 7 )          !! factor
      CHARACTER   HSIG *(16)         !! unused
      CHARACTER   HSIGM *(16)        !! unused
      INTEGER     IFS, IFR, IFU, IFV, IFT, IFZ, IFP, IFO, IFO2, IFB
      INTEGER     IJ, I, J, K, IREC, JREC
      INTEGER     IDAY, INDAY, NNDAY, NMDAY
      INTEGER     IV
      INTEGER     NOUT
*
*     namelist
*
      INTEGER      NFCS         !! no. of integration
      INTEGER      NFDAY        !! first day for use
      INTEGER      NLDAY        !! length of integration (day)
      REAL*4       TINT         !! output interval
      CHARACTER*10 TUNIT        !! time unit for output
      CHARACTER*90 CFS          !! input file name (gtool3)
      CHARACTER*90 CFR          !! input file name (gtool3)
      CHARACTER*90 CFU          !! input file name (gtool3)
      CHARACTER*90 CFV          !! input file name (gtool3)
      CHARACTER*90 CFT          !! input file name (gtool3)
      CHARACTER*90 CFZ          !! input file name (gtool3)
      CHARACTER*90 CFP          !! input file name (gtool3)
      CHARACTER*90 CFO          !! output file name (grads)
      CHARACTER*90 CFO2         !! output file name (grads)
      CHARACTER*90 CBS          !! basic state
      LOGICAL      OZINC        !! include height?
      LOGICAL      OPL          !! convert to p-level?
      LOGICAL      OFO          !! write file CFO?
*
      NAMELIST    /NMREC/   NFCS, NFDAY, NLDAY, TINT, TUNIT
      NAMELIST    /NMFILI/   CFS, CFR, CFU, CFV, CFT, CFZ, CFP
      NAMELIST    /NMFILO/   CFO, CFO2, CBS, OFO
*
      DATA        FACT / 1.D0, 1.D0, 1.D0, 1.D0, 1.D0, 1.D0, 1.D0 /
      DATA        NOUT / 6 /
      DATA        OZINC / .TRUE. /
      DATA        OPL   / .FALSE. /
      DATA        OFO   / .FALSE. /
*
*     open the NAMELIST file
*
      REWIND( 1 )
      OPEN ( 0, FILE='IEEE_ERROR')
      OPEN ( 1, FILE = 'SETPAR', STATUS = 'OLD')
*
      READ( 1, NMREC  ) 
      REWIND( 1 )
      READ( 1, NMFILI ) 
      REWIND( 1 )
      READ( 1, NMFILO ) 
      CLOSE( 1 )
*
      WRITE( NOUT, * ) NLDAY-NFDAY+1,' day from',NLDAY,' day exp. No.'
     $     ,NFCS
cc      WRITE( NOUT, * ) '### start Gtool3 ---> GrADS ###'
cc      WRITE( NOUT, * )
*
*     read basic state Ps
*
      IFB = 55
      OPEN( IFB, FILE = CBS, FORM = 'UNFORMATTED', STATUS = 'OLD' )
      DO 100 K = 1, KMAX * 3
         READ( IFB ) G          ! dummy
 100  CONTINUE
      READ( IFB ) G
      DO 110 J = 1, JMAX
         DO 120 I = 1, IMAX
            IJ = IJ + 1
            GPS( IJ  ) = DBLE( G( I,J ) )
 120     CONTINUE
 110  CONTINUE
      CLOSE( IFB )
*
*     sigma coodinate
*
      CALL SETCONS
      CALL SETSIG
     O     ( SIG   , DSIG , HSIG  , 
     O       SIGM  , DSIGM, HSIGM  )
      DO 130 K = 1, KMAX
         S( K ) = SNGL( SIG( K ) )
         P( K ) = SNGL( PLEV( K ) )
 130  CONTINUE
*
*     open files
*
      IFS = 10
      IFR = 20
      IFU = 30
      IFV = 40
      IFT = 50
      IFZ = 60
      IFP = 70
      IFO = 80
      IFO2= 81
      OPEN ( IFS, FILE = CFS, FORM = 'UNFORMATTED',
     $     STATUS = 'OLD' )
      OPEN ( IFR, FILE = CFR, FORM = 'UNFORMATTED',
     $     STATUS = 'OLD' )
      OPEN ( IFU, FILE = CFU, FORM = 'UNFORMATTED',
     $     STATUS = 'OLD' )
      OPEN ( IFV, FILE = CFV, FORM = 'UNFORMATTED',
     $     STATUS = 'OLD' )
      OPEN ( IFT, FILE = CFT, FORM = 'UNFORMATTED',
     $     STATUS = 'OLD' )
      OPEN ( IFZ, FILE = CFZ, FORM = 'UNFORMATTED',
     $     STATUS = 'OLD' )
      OPEN ( IFP, FILE = CFP, FORM = 'UNFORMATTED',
     $     STATUS = 'OLD' )
      IF( OFO ) CALL FOPNEOF ( IFO, CFO )      
      OPEN ( IFO2, FILE = CFO2, FORM = 'UNFORMATTED', 
     $     STATUS = 'UNKNOWN' )
cc      WRITE( NOUT, * ) 'Input data :', CFS
cc      WRITE( NOUT, * ) 'Input data :', CFR
cc      WRITE( NOUT, * ) 'Input data :', CFU
cc      WRITE( NOUT, * ) 'Input data :', CFV
cc      WRITE( NOUT, * ) 'Input data :', CFT
cc      WRITE( NOUT, * ) 'Input data :', CFZ
cc      WRITE( NOUT, * ) 'Input data :', CFP
cc      WRITE( NOUT, * ) '................'
cc      WRITE( NOUT, * ) 'Output data:', CFO
cc      WRITE( NOUT, * ) '................'
cc      WRITE( NOUT, * ) 
*
*     read GrADS data
*
cc      WRITE( NOUT, * ) 'Read data'
cc      WRITE( NOUT, * ) '................'
cc      WRITE( NOUT, * ) 
*
*     read covariances at previous integ.
*
      CALL RESETM( G, IMAX*JMAX )
      JREC = 0
      DO 200 IV = 1, NV
         DO 210 K = 1, KMAX
            JREC = JREC + 1
            IF( NFCS .GT. 1 ) READ( IFO2 ) G
            DO 220 J = 1, JMAX
               DO 230 I = 1, IMAX
                  G( I, J) = G( I, J) * FLOAT( NFCS-1 )
                  COV( I, J, K, IV) = G( I, J)
  230          CONTINUE
  220       CONTINUE
  210    CONTINUE
  200 CONTINUE
*
      IF( TUNIT(1:3) .EQ. 'DAY' ) THEN
         NNDAY = 1
         IDAY = 0
      ELSEIF( TUNIT(1:4) .EQ. 'HOUR' ) THEN
         NNDAY = 24 / IFIX( TINT )
         IDAY = 1
      ELSE
         WRITE( 6, *) ' 24 / TINT not integer'
         CALL XABORT( 1 )
      ENDIF
*
      IREC = NNDAY * ( NLDAY-NFDAY+1 ) * ( NFCS-1 ) * ( 6*KMAX+1 )
      INDAY = 0
      NMDAY = NNDAY * ( NLDAY-NFDAY+1 )
 1000 INDAY = INDAY + 1
      IF( INDAY .EQ. NNDAY ) THEN
         IDAY = IDAY + 1
         INDAY = 0
      ENDIF
      READ( IFS , END = 99 ) HEAD
      READ( IFS ) DAT1          !! psi
*      WRITE( NOUT, *) 'DATE:',HEAD( 27 )
*
*     sigma to pressure
* 
      IF( OPL ) THEN
         CALL S2P
     O        ( PSI,
     I        DAT1 , GPS, P, S, IMAX*JMAX, KMAX, XMISS )
      ELSE
         CALL COPYM 
     O        ( PSI,
     I        DAT1, IMAX*JMAX*KMAX )
      ENDIF
*
      DO 300 K = 1, KMAX
         IJ = 0
         DO 310 J = 1, JMAX
            DO 320 I = 1, IMAX
               IJ = IJ + 1
               G( I, J) = FACT( 1 ) * PSI( IJ, K)
               IF( PSI( IJ, K) .EQ. XMISS ) G( I, J) = XMISS
 320        CONTINUE
 310     CONTINUE
         IF( IDAY .LT. NFDAY .OR. IDAY .GT. NLDAY ) GOTO 300
         IREC = IREC + 1
         IF( OFO ) WRITE( IFO ) G
 300  CONTINUE
*
      READ( IFR ) HEAD
      READ( IFR ) DAT1          !! vor
*
*     sigma to pressure
* 
      IF( OPL ) THEN
         CALL S2P
     O        ( R,
     I        DAT1 , GPS, P, S, IMAX*JMAX, KMAX, XMISS )
      ELSE
         CALL COPYM 
     O        ( R,
     I        DAT1, IMAX*JMAX*KMAX )
      ENDIF
*
      DO 400 K = 1, KMAX
         IJ = 0
         DO 410 J = 1, JMAX
            DO 420 I = 1, IMAX
               IJ = IJ + 1
               G( I, J) = FACT( 2 ) * R( IJ, K)
               IF( R( IJ, K) .EQ. XMISS ) G( I, J) = XMISS
 420        CONTINUE
 410      CONTINUE
         IF( IDAY .LT. NFDAY .OR. IDAY .GT. NLDAY ) GOTO 400
         IREC = IREC + 1
         IF( OFO ) WRITE( IFO ) G
 400  CONTINUE
*
      READ( IFU ) HEAD
      READ( IFU ) DAT1          !! u
*
*     sigma to pressure
* 
      IF( OPL ) THEN
         CALL S2P
     O        ( U,
     I        DAT1 , GPS, P, S, IMAX*JMAX, KMAX, XMISS )
      ELSE
         CALL COPYM 
     O        ( U,
     I        DAT1, IMAX*JMAX*KMAX )
      ENDIF
*
      DO 500 K = 1, KMAX
         IJ = 0
         DO 510 J = 1, JMAX
            DO 520 I = 1, IMAX
               IJ = IJ + 1
               G( I, J) = FACT( 3 ) * U( IJ, K)
               IF( U( IJ, K) .EQ. XMISS ) G( I, J) = XMISS
 520        CONTINUE
 510     CONTINUE
         IF( IDAY .LT. NFDAY .OR. IDAY .GT. NLDAY ) GOTO 500
         IREC = IREC + 1
         IF( OFO ) WRITE( IFO ) G
 500  CONTINUE
*
      READ( IFV ) HEAD
      READ( IFV ) DAT1          !! v
*
*     sigma to pressure
* 
      IF( OPL ) THEN
         CALL S2P
     O        ( V,
     I        DAT1 , GPS, P, S, IMAX*JMAX, KMAX, XMISS )
      ELSE
         CALL COPYM 
     O        ( V,
     I        DAT1, IMAX*JMAX*KMAX )
      ENDIF
*
      DO 600 K = 1, KMAX
         IJ = 0
         DO 610 J = 1, JMAX
            DO 620 I = 1, IMAX
               IJ = IJ + 1
               G( I, J) = FACT( 4 ) * V( IJ, K)
               IF( V( IJ, K) .EQ. XMISS ) G( I, J) = XMISS
 620        CONTINUE
 610     CONTINUE
         IF( IDAY .LT. NFDAY .OR. IDAY .GT. NLDAY ) GOTO 600
         IREC = IREC + 1
         IF( OFO ) WRITE( IFO ) G
 600  CONTINUE
*
      READ( IFT ) HEAD
      READ( IFT ) DAT1          !! T
*     
*     sigma to pressure
* 
      IF( OPL ) THEN
         CALL S2P
     O        ( T,
     I        DAT1 , GPS, P, S, IMAX*JMAX, KMAX, XMISS )
      ELSE
         CALL COPYM 
     O        ( T,
     I        DAT1, IMAX*JMAX*KMAX )
      ENDIF
*
      DO 700 K = 1, KMAX
         IJ = 0
         DO 710 J = 1, JMAX
            DO 720 I = 1, IMAX
               IJ = IJ + 1
               G( I, J) = FACT( 5 ) * T( IJ, K)
               IF( T( IJ, K) .EQ. XMISS ) G( I, J) = XMISS
 720        CONTINUE
 710     CONTINUE
         IF( IDAY .LT. NFDAY .OR. IDAY .GT. NLDAY ) GOTO 700
         IREC = IREC + 1
         IF( OFO ) WRITE( IFO ) G
 700  CONTINUE
*
      READ( IFZ ) HEAD
      READ( IFZ ) DAT1          !! z 
*
*     sigma to pressure
* 
      IF( OPL ) THEN
         CALL S2P
     O        ( Z,
     I        DAT1 , GPS, P, S, IMAX*JMAX, KMAX, XMISS )
      ELSE
         CALL COPYM 
     O        ( Z,
     I        DAT1, IMAX*JMAX*KMAX )
      ENDIF
*
      DO 800 K = 1, KMAX
         IJ = 0
         DO 810 J = 1, JMAX
            DO 820 I = 1, IMAX
               IJ = IJ + 1
               G( I, J) = FACT( 6 ) * Z( IJ, K)
               IF( Z( IJ, K) .EQ. XMISS ) G( I, J) = XMISS
 820        CONTINUE
 810     CONTINUE
         IF( IDAY .LT. NFDAY .OR. IDAY .GT. NLDAY ) GOTO 800
         IF( OZINC ) THEN
            IREC = IREC + 1
            IF( OFO ) WRITE( IFO ) G
         ENDIF
 800  CONTINUE
*
      READ( IFP ) HEAD
      READ( IFP ) DAT2          !! Ps
      DO 830 J = 1, JMAX
         DO 840 I = 1, IMAX
            G( I, J) = FACT( 7 ) * DAT2( I,J )
            IF( DAT2( I,J ) .EQ. XMISS ) G( I, J) = XMISS
  840    CONTINUE
  830 CONTINUE
      IF( IDAY .LT. NFDAY .OR. IDAY .GT. NLDAY ) GOTO 850
      IREC = IREC + 1
      IF( OFO ) WRITE( IFO ) G
  850 CONTINUE
*
*     covariances
*
      IF( IDAY .LT. NFDAY .OR. IDAY .GT. NLDAY ) GOTO 1999
      DO 900 IV = 1, NV
         DO 910 K = 1, KMAX
            IJ = 0
            DO 920 J = 1, JMAX
               DO 930 I = 1, IMAX
                  IJ = IJ + 1
                  IF( IV .EQ. 1 ) THEN
                     IF( U( IJ, K) .NE. XMISS )
     $                    COV( I, J, K, IV) = COV( I, J, K, IV) +
     $                    U( IJ, K)**2/FLOAT(NMDAY)
                  ENDIF
                  IF( IV .EQ. 2 ) THEN
                     IF( V( IJ, K) .NE. XMISS )
     $                    COV( I, J, K, IV) = COV( I, J, K, IV) +
     $                    V( IJ, K)**2/FLOAT(NMDAY)
                  ENDIF
                  IF( IV .EQ. 3 ) THEN
                     IF( T( IJ, K) .NE. XMISS )
     $                    COV( I, J, K, IV) = COV( I, J, K, IV) +
     $                    T( IJ, K)**2/FLOAT(NMDAY)
                  ENDIF
                  IF( IV .EQ. 4 ) THEN
                     IF( Z( IJ, K) .NE. XMISS )
     $                    COV( I, J, K, IV) = COV( I, J, K, IV) +
     $                    Z( IJ, K)**2/FLOAT(NMDAY)
                  ENDIF
                  IF( IV .EQ. 5 ) THEN
                     IF( U(IJ,K).NE.XMISS .AND. V(IJ,K).NE.XMISS )
     $                    COV( I, J, K, IV) = COV( I, J, K, IV) +
     $                    (U( IJ, K)**2 + V( IJ, K)**2)/FLOAT(NMDAY)
                  ENDIF
                  IF( IV .EQ. 6 ) THEN
                     IF( U(IJ,K).NE.XMISS .AND. V(IJ,K).NE.XMISS )
     $                    COV( I, J, K, IV) = COV( I, J, K, IV) +
     $                    (U( IJ, K)*V( IJ, K))/FLOAT(NMDAY)
                  ENDIF
                  IF( IV .EQ. 7 ) THEN
                     IF( U(IJ,K).NE.XMISS .AND. T(IJ,K).NE.XMISS )
     $                    COV( I, J, K, IV) = COV( I, J, K, IV) +
     $                    (U( IJ, K)*T( IJ, K))/FLOAT(NMDAY)
                  ENDIF
                  IF( IV .EQ. 8 ) THEN
                     IF( V(IJ,K).NE.XMISS .AND. T(IJ,K).NE.XMISS )
     $                    COV( I, J, K, IV) = COV( I, J, K, IV) +
     $                    (V( IJ, K)*T( IJ, K))/FLOAT(NMDAY)
                  ENDIF
                  IF( IV .EQ. 9 ) THEN
                     IF( U(IJ,K).NE.XMISS .AND. R(IJ,K).NE.XMISS )
     $                    COV( I, J, K, IV) = COV( I, J, K, IV) +
     $                    (U( IJ, K)*R( IJ, K))/FLOAT(NMDAY)
                  ENDIF
                  IF( IV .EQ. 10 ) THEN
                     IF( V(IJ,K).NE.XMISS .AND. R(IJ,K).NE.XMISS )
     $                    COV( I, J, K, IV) = COV( I, J, K, IV) +
     $                    (V( IJ, K)*R( IJ, K))/FLOAT(NMDAY)
                  ENDIF
  930          CONTINUE
  920       CONTINUE
  910    CONTINUE
  900 CONTINUE
 1999 CONTINUE
*
      GOTO 1000
*     
 99   CLOSE( IFS )
      CLOSE( IFR )
      CLOSE( IFU )
      CLOSE( IFV )
      CLOSE( IFT )
      CLOSE( IFZ )
      CLOSE( IFP )
      IF( OFO ) CLOSE( IFO )
*
*     overwrite covariances
*
      REWIND( IFO2 )
      JREC = 0
      DO 1100 IV = 1, NV
         DO 1110 K = 1, KMAX
            DO 1120 J = 1, JMAX
               DO 1130 I = 1, IMAX
                  G( I, J) = COV( I, J, K, IV) 
                  IF( G( I, J) .NE. XMISS ) 
     $                 G( I, J) = G( I, J)/FLOAT(NFCS) 
 1130          CONTINUE
 1120       CONTINUE
            JREC = JREC + 1
            WRITE( IFO2 ) G
 1110    CONTINUE
 1100 CONTINUE
*
      CLOSE( IFO2)
cc      WRITE( NOUT, * )
cc      WRITE( NOUT, * ) '### finish Gtool3 ---> GrADS ###'
*
*      
      STOP
      END
*##############################################
      SUBROUTINE S2P
     O         ( GDO  ,
     I           GDI, GDPS, PLI, SLI, IJDIM, KMAX, XMISS )
*
*   [PARAM] 
      INTEGER    IJDIM
      INTEGER    KMAX
*   [OUTPUT] 
      REAL       GDO ( IJDIM, KMAX )
*   [INPUT] 
      REAL       GDPS( IJDIM )
      REAL       GDI ( IJDIM, KMAX )
      REAL       PLI ( KMAX )
      REAL       SLI ( KMAX )
      LOGICAL    OMISB
      LOGICAL    OMIST
      REAL       XMISS
      DATA       OMISB / .FALSE. /
      DATA       OMIST / .FALSE. /
*
*   [INTERNAL WORK] 
      INTEGER    KMAXD
      PARAMETER  ( KMAXD = 100 )
      REAL       PILN ( KMAXD )
      REAL       POLN ( KMAXD )
      REAL       GDIZ ( KMAXD )
      REAL       GDOZ ( KMAXD )
      INTEGER    IJ, KI, KO
      REAL       TMP
*
      DO 2000 IJ = 1, IJDIM
         DO 2010 KI = 1, KMAX
            GDIZ(KI) = GDI(IJ,KI)
            PILN(KI) = LOG( GDPS(IJ)*SLI(KI) )
 2010    CONTINUE 
         IF(PILN(1).LT.PILN(2))THEN
            DO 2020 KI = 1, KMAX/2
               TMP              = GDIZ(KMAX+1-KI)
               GDIZ(KMAX+1-KI) = GDIZ(KI)
               GDIZ(KI)         = TMP
               TMP              = PILN(KMAX+1-KI)
               PILN(KMAX+1-KI) = PILN(KI)
               PILN(KI)         = TMP
 2020       CONTINUE 
         ENDIF
         DO 2030 KO = 1, KMAX
            POLN(KO) = LOG( PLI(KO) )
 2030    CONTINUE
         CALL SPLINE
     O        ( GDOZ,
     I                 POLN,  KMAX,
     I          GDIZ,  PILN,  KMAX,
     I          OMISB, OMIST, XMISS  )
         DO 2100 KO = 1, KMAX
            GDO(IJ,KO) = GDOZ(KO)
            IF( GDPS(IJ) .LT. PLI(KO) ) GDO(IJ,KO) = XMISS
 2100    CONTINUE 
 2000 CONTINUE
*
      RETURN
      END
*********************************************************************
      SUBROUTINE SPLINE
     O         ( ZI   ,
     I                  XI   , LMAX ,
     I           Z    , X    , KMAX , 
     I           OMISB, OMIST, XMISS  )
*
      INTEGER    LMAX, KMAX
      REAL       ZI  ( LMAX )
      REAL       XI  ( LMAX )
      REAL       Z   ( KMAX )
      REAL       X   ( KMAX )
      LOGICAL    OMISB
      LOGICAL    OMIST
      REAL       XMISS

*   [INTERNAL WORK] 
      INTEGER    KMAXD
      PARAMETER  ( KMAXD = 1024 )
      REAL       Y2  ( KMAXD )
      REAL       U   ( KMAXD )
      REAL       SIG, P, QN, UN, H, A, B 
      INTEGER    K, L, KU
*
      IF ( KMAX .GT. KMAXD ) THEN
         WRITE (6,*) ' ### KMAXD IS TOO SMALL ', KMAXD, KMAX
         STOP
      ENDIF
*
      Y2(1)=0.
      U (1)=0.
      DO 120 K=2, KMAX-1
         SIG   = (X(K)-X(K-1))/(X(K+1)-X(K-1))
         P     = SIG*Y2(K-1)+2.
         Y2(K) = (SIG-1.)/P
         U (K) = (6.*( (Z(K+1)-Z(K))/(X(K+1)-X(K))
     &                -(Z(K)-Z(K-1))/(X(K)-X(K-1)))
     &              /(X(K+1)-X(K-1))
     &             - SIG*U(K-1)                     )/P
  120 CONTINUE
      QN = 0.
      UN = 0.
      Y2(KMAX) = (UN-QN*U(KMAX-1))/(QN*Y2(KMAX-1)+1.)
      DO 130 K= KMAX-1, 1, -1
         Y2(K) = Y2(K)*Y2(K+1)+U(K)
  130 CONTINUE
*
      DO 500 L = 1, LMAX
         KU = 1
         DO 300 K = 1, KMAX
            IF( X(K) .LT. XI(L) ) THEN
               KU = K
               GOTO 310
            ENDIF
  300    CONTINUE
         KU = KMAX+1
  310    CONTINUE
*
         IF      ( KU .EQ. 1 ) THEN
            IF ( OMISB ) THEN
               ZI(L) = XMISS
            ELSE
               ZI(L) = Z(1)
            ENDIF
         ELSE IF ( KU .EQ. KMAX+1 ) THEN
            IF ( OMIST ) THEN
               ZI(L) = XMISS
            ELSE
               ZI(L) = Z(KMAX)
            ENDIF            
         ELSE
            KU   = MAX(KU,2)
            H    = X(KU)-X(KU-1)
            A    = (X(KU)-XI(L))/H
            B    = (XI(L)-X(KU-1))/H
            ZI(L)= A*Z(KU-1)+B*Z(KU)
     &           + (A*(A*A-1)*Y2(KU-1)+B*(B*B-1)*Y2(KU))*(H*H)/6.
         ENDIF
  500 CONTINUE
*
      RETURN
      END
***********************************************************************
      SUBROUTINE COPYM          !! copy matrix
     O         ( DATAO  ,
     I           DATAI  ,
     D           IDIM     )
*
*   [PARAM]
      INTEGER    IDIM
*
*   [OUTPUT]       
      REAL*4     DATAO ( IDIM )                !! output data
*
*   [INPUT] 
      REAL*4     DATAI ( IDIM )                !! input data
*
*   [INTERNAL WORK] 
      INTEGER    I
*
      DO 1100 I = 1, IDIM
        DATAO ( I ) = DATAI ( I )
 1100 CONTINUE
*
      RETURN
      END
***********************************************************************
      SUBROUTINE RESETM          !! reset matrix
     M         ( DAT   ,
     D           IDIM     )
*
*   [PARAM]
      INTEGER    IDIM
*
*   [MODIFY]
      REAL*4     DAT ( IDIM )                !! output data
*
*   [INTERNAL WORK] 
      INTEGER    I
*
      DO 1100 I = 1, IDIM
        DAT ( I ) = 0.0
 1100 CONTINUE
*
      RETURN
      END
