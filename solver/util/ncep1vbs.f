      PROGRAM NCEP1VBS
*
*     writtedn on 2001/10/16
*     This routine makes a basic state for barotropic or moist model
*
      include 'dim.f'
      include 'hdim.f'
*
*     [work]
      REAL*4    DAT0( IMAX, JMAX )
      REAL*8    PSI( IMAX*JMAX, KMAX)
      REAL*8    WT( NMDIM, KMAX)
      REAL*8    ZT( IMAX+1, JMAX, KMAX)
      REAL*4    GT( IMAX*JMAX, KMAX)

      INTEGER*4 IREC, ISSN, IAVG, I, J, K, N, IJ

      CHARACTER HEAD( 64 )*16
*
*     [param]
      REAL*4     UNDEF1, XMISS
      INTEGER    NMO
      INTEGER    NOUT
*
*     [intrinsic]
*
      CHARACTER*90 CNCEP         !! NCEP psi data            (grads)
      CHARACTER*90 CNCEP2        !! NCEP surface pressure    (dummy)
      CHARACTER*90 CALT          !! topography               (dummy)
      CHARACTER*90 CBS0          !! basic state              (gtool)
      CHARACTER*90 CBS           !! basic state              (grads)
      CHARACTER*3  CVAR          !! name of variable
      INTEGER      KMO           !! first month 
      INTEGER      NAVG          !! no. of month averaged
      LOGICAL      OZM           !! zonal mean
      LOGICAL      OSW           !! zonal asymmetry
      LOGICAL      OUSEZ         !! use Z to derive Ps
*
      NAMELIST    /NMNCP/   CNCEP, CNCEP2, CALT, 
     &                      KMO, NAVG, OZM, OSW, OUSEZ, CVAR
      NAMELIST    /NMBS/    CBS0, CBS

      DATA       UNDEF1  / -9.999E20  /
      DATA       XMISS   / -999.      /
      DATA       NMO     / 12         /
      DATA       NOUT    / 6          /

      DATA KMO       / 12      /
      DATA NAVG      / 3       /
      DATA OZM       / .FALSE. /
      DATA OSW       / .FALSE. /
      DATA OUSEZ     / .TRUE.  /
      DATA CVAR      / 'PSI'   /
*
*     open the NAMELIST file
*
      REWIND( 1 )
      OPEN ( 0, FILE='IEEE_ERROR')
      OPEN ( 1, FILE = 'SETPAR', STATUS = 'OLD')
*
      READ( 1, NMNCP) 
      REWIND( 1 )
      READ( 1, NMBS) 
*
*
      WRITE( NOUT, * )
      WRITE( NOUT, * ) '### start making basic state ###'
      WRITE( NOUT, * )
*
*     selected parameters
*
      WRITE( NOUT, *) '... selected month from:', KMO,' ...'
      WRITE( NOUT, *) '... number of month averaged:', NAVG,' ...'
*
      OPEN (70, FILE=CBS0,   FORM='UNFORMATTED', STATUS='UNKNOWN' )
      OPEN (80, FILE=CBS,    FORM='UNFORMATTED', STATUS='UNKNOWN' )
      WRITE( NOUT, *) 
      WRITE( NOUT, *) ' @@ Input :',CNCEP
      WRITE( NOUT, *) ' @@ Output:',CBS0
      WRITE( NOUT, *) ' @@ Output:',CBS
      WRITE( NOUT, *) 
*
*     read input
*
      ISSN = 0
      IAVG = 0
      WRITE( NOUT, *) '... data read start ...'
      ISSN = KMO
 222  OPEN (10, FILE=CNCEP, FORM='UNFORMATTED', STATUS='OLD' )
      WRITE( NOUT, *) '   ... month = ', ISSN
      IREC = ISSN - 1
      DO 210 N = 1, IREC
         READ( 10, END=111 ) DAT0
 210  CONTINUE

      READ( 10, END=111) DAT0

      IJ = 0
      DO 20 J = 1, JMAX
         DO 20 I = 1, IMAX
            IJ = IJ + 1
            PSI( IJ,1 ) = 
     &           PSI( IJ,1 ) + DBLE( DAT0( I, JMAX+1-J ) / FLOAT(NAVG) )
            IF( DAT0( I, JMAX+1-J ) .EQ. UNDEF1 ) PSI( IJ,1 ) = XMISS
 20   CONTINUE

  111 CLOSE( 10 )

      ISSN = ISSN + 1
      IAVG = IAVG + 1
      IF( ISSN .GT. NMO  ) ISSN = ISSN - NMO
      IF( IAVG .EQ. NAVG ) GOTO 333
      GOTO 222
*
  333 WRITE( NOUT, *) '... data read end ...'
*
*     read basic/add/write
*
      WRITE( NOUT, *) '... write basic state ...'
      DO 600 N = 1, 64
         IF( CVAR .EQ. 'PSI' ) THEN
            HEAD( N ) = CHEADPSI( N )
         ELSEIF( CVAR .EQ. 'SST' ) THEN
            HEAD( N ) = CHEADSST( N )
         ELSEIF( CVAR .EQ. 'WG'  ) THEN
            HEAD( N ) = CHEADWG( N )
         ELSE
            WRITE( NOUT,* ) ' ### HEADER UNSPECIFIED ### '
            HEAD( N ) = CHEADPSI( N )
         ENDIF
 600  CONTINUE
      CALL COPY8T4( GT, PSI, IMAX*JMAX*KMAX )
      IF( CVAR .EQ. 'PSI' ) THEN
         CALL SPECSM
     I        ( IMAX, JMAX, KMAX, CVAR, 
     M          GT,
     W          WT, ZT  )
      ENDIF
      CALL ZMSW
     M     ( GT  , 
     I       IMAX, JMAX, KMAX, OZM, OSW )
      WRITE( 70 ) HEAD
      WRITE( 70 ) GT
      DO 610 K = 1, KMAX
         WRITE( 80 ) (GT(IJ,K),IJ=1,IMAX*JMAX)
 610  CONTINUE
      WRITE( NOUT, *) '    ... ',CVAR,' ... '
*
      CLOSE( 70 )
      CLOSE( 80 )
      WRITE( NOUT, *) '... end execution ...'
*
*
  999 STOP
      END
************************************
      SUBROUTINE COPY4T8( 
     O     GD8, 
     I     GD4, IDIM )

      INTEGER   IDIM, I
      REAL*8    GD8( IDIM )
      REAL*4    GD4( IDIM )

      DO 10 I = 1, IDIM
         GD8( I ) = DBLE( GD4( I ) )
   10 CONTINUE

      RETURN
*===================================
      ENTRY COPY8T4( 
     O     GD4, 
     I     GD8, IDIM )

      DO 20 I = 1, IDIM
         GD4( I ) = SNGL( GD8( I ) )
   20 CONTINUE

      RETURN
      END
************************************
      SUBROUTINE ACOPY4T8( 
     O     GD8, 
     I     GD4, IDIM, JDIM )

      INTEGER   IDIM, JDIM, I, J
      REAL*8    GD8( IDIM+1, JDIM)
      REAL*4    GD4( IDIM, JDIM)

      DO 10 J = 1, JDIM
         DO 20 I = 1, IDIM
         GD8( I, J) = DBLE( GD4( I, J) )
   20 CONTINUE
      GD8( IDIM+1, J) = GD8( 1, J)
   10 CONTINUE

      RETURN
*===================================
      ENTRY ACOPY8T4( 
     O     GD4, 
     I     GD8, IDIM, JDIM )

      DO 30 J = 1, JDIM
         DO 40 I = 1, IDIM
            GD4( I, J) = SNGL( GD8( I, J) )
   40    CONTINUE
   30 CONTINUE

      RETURN
      END
************************************
      SUBROUTINE SPECSM
     I     ( JX, JY, JZ, CV, 
     M       G4,
     W       W8, G8  )
*
      INTEGER NMAX, NMDIM
      PARAMETER ( NMAX = 21, NMDIM = 506 )
*
      INTEGER JX, JY, JZ
      CHARACTER CV*2
      REAL*4  G4( JX, JY, JZ)
      REAL*8  G8( JX+1, JY, JZ)      
      REAL*8  W8( NMDIM, JZ)

      REAL*8    ER, PI
      INTEGER   NMO   ( 2, 0:NMAX, 0:NMAX ) !! order of spect. suffix
      REAL*8    EDEL  ( NMDIM  ) !! vor.,D -> U,V
      LOGICAL OFIRST
      DATA    OFIRST / .TRUE. /
*
*     spectral smoothing
*
      IF( OFIRST ) THEN
         ER = 6370.D+3
         PI = ATAN( 1.D0 ) * 4.D0
         CALL SETCONS
         CALL SPSTUP            !! spherical harmonic functions
         CALL SETNMO2
     O        ( NMO   ,
     D        NMAX  , NMAX  , NMAX  , 1    )
         CALL DSETED2
     O        ( EDEL, 
     I        NMDIM, NMAX, NMAX, NMAX, 1, NMO, ER )
         OFIRST = .FALSE.
      ENDIF
*
      WRITE( 6, *) '... spectral smoothing for ',CV,' ...'
      CALL ACOPY4T8( G8, G4, JX, JY )
      CALL G2W
     M     ( W8  ,
     I       G8  , '    ', 'POS ', JZ )
      CALL W2G
     O     ( G8  ,
     I       W8  , '    ', 'POS ', JZ )
      CALL ACOPY8T4( G4, G8, JX, JY )

      RETURN
      END
***************************************************************
      SUBROUTINE Z2PS
     O         ( GDPS  , 
     I           GDZ   , GDZS  , PLEV0 , IDIM , NLEV  )
*
      INTEGER    IDIM, NLEV
*
*   [OUTPUT] 
      REAL*4     GDPS ( IDIM )
*   [INPUT] 
      REAL*4     GDZ  ( IDIM, NLEV )
      REAL*4     GDZS ( IDIM )
      REAL*8     PLEV0 ( NLEV )
*
      REAL*4     AL, BL, CL, DL
      INTEGER    KA, IJ, K
*
      KA = NLEV
      DO 10 IJ = 1, IDIM
         DO 20 K = 1, NLEV
            IF ( GDZ(IJ,K) .GT. GDZS(IJ) ) THEN
               KA = K
               GOTO 30
            ENDIF
   20    CONTINUE
   30    CONTINUE
         KA = MIN( MAX( KA,2 ), NLEV )
         AL = LOG( SNGL(PLEV0(KA-1)) )
         BL = ( LOG( SNGL(PLEV0(KA)) ) - LOG( SNGL(PLEV0(KA-1)) ) )
         CL = ( GDZS(IJ)    - GDZ(IJ,KA-1) )
         DL = ( GDZ (IJ,KA) - GDZ(IJ,KA-1) )
         GDPS(IJ) = EXP( AL + BL * CL / DL )
   10 CONTINUE
*
      RETURN
      END
***************************************************************
      SUBROUTINE ZMSW
     M         ( GDX  , 
     I           IDIM, JDIM, KDIM, OZM, OSW )

      INTEGER  IDIM, JDIM, KDIM
      REAL*4   GDX( IDIM,JDIM,KDIM )
      LOGICAL  OZM, OSW

      REAL*4   ZM
      INTEGER  I, J, K

      DO K = 1, KDIM
         DO J = 1, JDIM
            ZM = 0.
            DO I = 1, IDIM
               ZM = ZM + GDX( I,J,K ) / FLOAT( IDIM )
            ENDDO
            IF( OZM ) THEN
               DO I = 1, IDIM
                  GDX( I,J,K ) = ZM
               ENDDO
            ELSEIF( OSW ) THEN
               DO I = 1, IDIM
                  GDX( I,J,K ) = GDX( I,J,K ) - ZM
               ENDDO
            ENDIF
         ENDDO
      ENDDO

      RETURN
      END
***************************************************************
      SUBROUTINE P2S
     O         ( GDO  ,
     I           GDI, GDPS, PLI, SLI, IJDIM, KMAXP, KMAXS, XMISS )
*
*   [PARAM] 
      INTEGER    IJDIM
      INTEGER    KMAXP, KMAXS
*   [OUTPUT] 
      REAL*8       GDO ( IJDIM, KMAXS ) !! sigma level
*   [INPUT] 
      REAL*8       GDPS( IJDIM )
      REAL*8       GDI ( IJDIM, KMAXP ) !! p level
      REAL*8       PLI ( KMAXP )
      REAL*8       SLI ( KMAXS )
      LOGICAL    OMISB
      LOGICAL    OMIST
      REAL*8       XMISS
      DATA       OMISB / .FALSE. /
      DATA       OMIST / .FALSE. /
*
*   [INTERNAL WORK] 
      INTEGER    KMAXD
      PARAMETER  ( KMAXD = 100 )
      REAL*8       PILN ( KMAXD )
      REAL*8       POLN ( KMAXD )
      REAL*8       GDIZ ( KMAXD )
      REAL*8       GDOZ ( KMAXD )
      INTEGER    IJ, KI, KO
*
      DO 1000 KI = 1, KMAXP
         PILN( KI ) = LOG( PLI( KI ) )
 1000 CONTINUE 

      DO 2000 IJ = 1, IJDIM
         DO 2010 KI = 1, KMAXP
            GDIZ(KI) = GDI(IJ,KI)
 2010    CONTINUE 
         DO 2020 KO = 1, KMAXS
            POLN(KO) = LOG( GDPS(IJ)*SLI(KO) )
 2020    CONTINUE
         CALL SPLINE
     O        ( GDOZ,
     I                 POLN,  KMAXS,
     I          GDIZ,  PILN,  KMAXP,
     I          OMISB, OMIST, XMISS  )
         DO 2100 KO = 1, KMAXS
            GDO(IJ,KO) = GDOZ(KO)
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
      REAL*8       ZI  ( LMAX )
      REAL*8       XI  ( LMAX )
      REAL*8       Z   ( KMAX )
      REAL*8       X   ( KMAX )
      LOGICAL    OMISB
      LOGICAL    OMIST
      REAL*8       XMISS

*   [INTERNAL WORK] 
      INTEGER    KMAXD
      PARAMETER  ( KMAXD = 1024 )
      REAL*8       Y2  ( KMAXD )
      REAL*8       U   ( KMAXD )
      REAL*8       SIG, P, QN, UN, H, A, B 
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
*********************************************************************
      SUBROUTINE SETNMO2    !! order of matrix
     O         ( NMO   ,
     D           MMAX  , LMAX  , NMAX  , MINT    )
*
*   [PARAM] 
      INTEGER    MMAX
      INTEGER    LMAX
      INTEGER    NMAX
      INTEGER    MINT
*
*   [OUTPUT]
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
*
*   [INTERNAL WORK] 
      INTEGER    L, M, MEND, NMH
*
      NMH  = 0
      DO 2200 L = 0, LMAX
         MEND = MIN( MMAX, NMAX-L )
         DO 2100 M = 0, MEND, MINT
            NMH = NMH + 1
            IF ( MMAX .EQ. 0 ) THEN
               NMO ( 1, M, L ) = NMH
               NMO ( 2, M, L ) = NMH
            ELSE
               NMO ( 1, M, L ) = 2* NMH - 1
               NMO ( 2, M, L ) = 2* NMH
            ENDIF
 2100    CONTINUE
 2200 CONTINUE
*
      RETURN
      END
*********************************************************************
      SUBROUTINE    DSETED2          !! factors for spectral calculation
     O     ( EDEL,
     I     NMDIM, NMAX, LMAX, MMAX, MINT, NMO, ER )
*
      INTEGER NMDIM, NMAX, LMAX, MMAX, MINT
      INTEGER NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      REAL*8  EDEL  ( NMDIM  )  !! vor.,D -> U,V
      REAL*8  ER
*
      INTEGER L, M, N, LEND
*
      IF ( LMAX .NE. 0 ) THEN      
         DO 3110 M = 0 , MMAX, MINT
            LEND = MIN( LMAX, NMAX-M )
            DO 3100 L = 0 , LEND
               N = L + M
               IF ( N .GT. 0 ) THEN
                  EDEL  ( NMO(1,M,L) )= - ER / DBLE( N*(N+1) )
                  EDEL  ( NMO(2,M,L) )= - ER / DBLE( N*(N+1) )
               ENDIF
 3100       CONTINUE
 3110    CONTINUE
         EDEL  ( NMO(1,0,0) ) =  0.  
         EDEL  ( NMO(2,0,0) ) =  0.  
      ELSE
         DO 3200 M = 0 , MMAX, MINT
            IF ( M .GT. 0 ) THEN
               EDEL  ( NMO(1,M,0) )= - ER / DBLE( M**2 )
               EDEL  ( NMO(2,M,0) )= - ER / DBLE( M**2 )
            ENDIF
 3200    CONTINUE
         EDEL  ( NMO(1,0,0) ) =  ER
         EDEL  ( NMO(2,0,0) ) =  0.  
      ENDIF
*
      RETURN
      END
