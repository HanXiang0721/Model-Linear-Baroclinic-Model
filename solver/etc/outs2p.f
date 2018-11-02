      PROGRAM OUTS2P
*
*     derived from gt2gr.f on 2001/11/06
*     This routine transforms sigma level to pressure level
*     for GrADS file containing eddy statistics.
*
*     variables: UU, VV, TT, ZZ, TKE, UV, UT, VT, UR, VR
*
      include 'dim.f'
*
*
      INTEGER      NQ
      REAL*4       XMISS
      PARAMETER (  NQ = 10, XMISS = -999. )
*
      REAL*4       DAT1( IMAX*JMAX )
      REAL*4       DAT2( IMAX*JMAX, KMAX)
      REAL*4       DAT3( IMAX*JMAX, KMAX)
      REAL*4       G   ( IMAX, JMAX     )  
      REAL*4       GPS ( IMAX*JMAX      )
*
*     [work]
      REAL*4      P( KMAX )          !! pressure level
      REAL*4      S( KMAX )          !! sigma level
      REAL*8      SIG( KMAX )        !! sigma level
      REAL*8      SIGM( KMAX+1 )     !! sigma level increment
      REAL*8      DSIG( KMAX )       !! unused
      REAL*8      DSIGM( KMAX+1 )    !! unused
      INTEGER     IFI, IFO, IFB
      INTEGER     IJ, I, J, K, N
      INTEGER     NOUT
      CHARACTER   HSIG *(16)         !! unused
      CHARACTER   HSIGM *(16)        !! unused
      CHARACTER   CV( NQ )*3
*
*     [intrinsic]
*
      CHARACTER*90 CFI          !! input file name (grads)
      CHARACTER*90 CFO          !! output file name (grads)
      CHARACTER*90 CBS          !! basic state
*
      NAMELIST    /NMFIN/   CFI, CFO
      NAMELIST    /NMBS/    CBS

      DATA        NOUT / 6 /
      DATA        CV /'UU ','VV ','TT ','ZZ ','TKE',
     &                'UV ','UT ','VT ','UR ','VR '/
*
*     open the NAMELIST file
*
      REWIND( 1 )
      OPEN ( 0, FILE='IEEE_ERROR')
      OPEN ( 1, FILE = 'SETPAR', STATUS = 'OLD')
*
      READ( 1, NMFIN) 
      REWIND( 1 )
      READ( 1, NMBS) 
*
*
      WRITE( NOUT, * ) '### start Gtool3 ---> GrADS ###'
      WRITE( NOUT, * )
*
*     read basic state Ps
*
      IFB = 55
      OPEN( IFB, FILE = CBS, FORM = 'UNFORMATTED', STATUS = 'UNKNOWN' )
      DO 500 I = 1, KMAX*3
         READ( IFB ) G
  500 CONTINUE
      READ( IFB ) G
      DO 510 J = 1, JMAX
         DO 520 I = 1, IMAX
            IJ = IJ + 1
            GPS( IJ ) = DBLE( G( I, J) )
  520    CONTINUE
  510 CONTINUE
      CLOSE( IFB )
*
*     sigma coodinate
*
      CALL SETCONS
      CALL SETSIG
     O     ( SIG   , DSIG , HSIG  , 
     O       SIGM  , DSIGM, HSIGM  )
      DO 530 K = 1, KMAX
         S( K ) = SNGL( SIG( K ) )
         P( K ) = SNGL( PLEV( K ) )
  530 CONTINUE
*
*     open files
*
      IFI = 10
      IFO = 98
      OPEN ( IFI, FILE = CFI, FORM = 'UNFORMATTED', STATUS = 'OLD' )
      OPEN ( IFO, FILE = CFO, FORM = 'UNFORMATTED', STATUS = 'UNKNOWN' )
      
      WRITE( NOUT, * ) 'Input data :', CFI
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 'Output data:', CFO
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 
*
*     read GrADS data
*
      WRITE( NOUT, * ) 'Read data'
      WRITE( NOUT, * ) '................'
      WRITE( NOUT, * ) 
*
      DO 1000 N = 1, NQ
         WRITE( NOUT, *) ' VAR:',CV( N )
         DO 1100 K = 1, KMAX
            READ( IFI, END = 99 ) DAT1  
            DO 1200 IJ = 1, IMAX*JMAX
               DAT2( IJ,K ) = DAT1( IJ )
 1200       CONTINUE
 1100    CONTINUE
*     
*     sigma to pressure
* 
         CALL S2P
     O        ( DAT3,
     I          DAT2, GPS, P, S, IMAX*JMAX, KMAX, XMISS )
*
         DO 1300 K = 1, KMAX
            WRITE( IFO ) ( DAT3( IJ,K ), IJ = 1, IMAX*JMAX )
 1300    CONTINUE
 1000 CONTINUE
*     
   99 CLOSE( IFI )
      CLOSE( IFO )
      WRITE( NOUT, * )
      WRITE( NOUT, * ) '### finish sigma ---> pressure ###'
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
