* PACKAGE USPHE   !!  spherical trans.
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*
*
**********************************************************************
      SUBROUTINE SPW2G     !! spectral -> grid transform
     M         ( GDATA ,
     I           WDATA ,
     C           PNM   , NMO   , TRIGS , IFAX ,
     F           HGRAD , HFUNC ,
     D           IMAX  , JMAX  , KMAX  , IDIM  , JDIM  ,
     D           LMAX  , MMAX  , NMAX  , MINT  , NMDIM , JMXHF, 
     W           ZDATA , WORK                                   )
*
*   [PARAM] 
      INTEGER    IMAX
      INTEGER    JMAX
      INTEGER    KMAX
      INTEGER    IDIM
      INTEGER    JDIM
      INTEGER    LMAX
      INTEGER    MMAX
      INTEGER    NMAX
      INTEGER    MINT
      INTEGER    NMDIM
      INTEGER    JMXHF
*
*   [MODIFY]       
      REAL*8     GDATA ( IDIM*JDIM, KMAX )    !! grid point data
*
*   [INPUT]
      REAL*8     WDATA ( NMDIM, KMAX     )    !! spectral data
*
      REAL*8     PNM   ( NMDIM, JMXHF )       !! Legendre function
      INTEGER    NMO   ( 2, 0:MMAX , 0:LMAX ) !! order of spect. suffix
      REAL*8     TRIGS ( * )                  !! triangle function table
      INTEGER    IFAX  ( * )                  !! factorziation of IMAX
*
      CHARACTER  HGRAD*4                      !! flag of differential
      CHARACTER  HFUNC*4                      !! flag of sign
*
*   [WORK] 
      REAL*8     ZDATA ( IDIM*JDIM, KMAX )    !! zonal spectral
      REAL*8     WORK  ( IDIM*JDIM, KMAX )    !! work
*
*   [INTERNAL WORK] 
      LOGICAL    LDPNM                        !! y differentail flag
      LOGICAL    LOFFS                        !! offset flag
      INTEGER    KMAXD
      PARAMETER (KMAXD=100)
      REAL*8     DOFFS ( KMAXD )              !! offset value
      INTEGER    IJ, K, I
      REAL*8     WORKZ
*
      IF ( IMAX .EQ. 1 .OR. JMAX .EQ. 1 ) THEN
         WRITE (6,*) ' ### SPW2G: THIS ROUTINE IS FOR 3 DIM.'
         CALL XABORT( 2 )
         RETURN
      ENDIF
*
*          < 1. LOFFS, LDPNM : flag >
*
      IF ( HFUNC(4:4) .EQ. 'O' ) THEN
         LOFFS = .TRUE.
         IF ( KMAXD .LT. KMAX ) THEN
            WRITE (6,*) ' ### SPW2G: WORK AREA(KMAXD) TOO SMALL < ',
     &                  KMAX
            CALL XABORT( 1 )
            RETURN
         ENDIF
      ELSE
         LOFFS = .FALSE.
      ENDIF
*
      IF ( HGRAD(1:1) .EQ. 'Y' ) THEN
         LDPNM = .TRUE.
         LOFFS = .FALSE.
      ELSE
         LDPNM = .FALSE.
      ENDIF
*
*          < 2. spectral -> zonal wave >
*
      IF ( LOFFS ) THEN
         DO 2000 K = 1, KMAX
            DOFFS( K ) = WDATA( NMO(1,0,0), K )
            WDATA( NMO(1,0,0), K ) = 0.  
 2000    CONTINUE
      ENDIF
*
      CALL SPW2Z
     O         ( ZDATA ,
     I           WDATA ,
     C           PNM   , NMO   ,
     F           LDPNM ,
     D           JMAX  , KMAX  , IDIM  , JDIM  ,
     D           LMAX  , MMAX  , NMAX  , MINT  , NMDIM , JMXHF ,
     W           WORK                                           )
*
      IF ( LOFFS ) THEN
         DO 2100 K = 1, KMAX
            WDATA( NMO(1,0,0), K ) = DOFFS( K )
 2100    CONTINUE
      ENDIF
*
      IF ( HGRAD(1:1) .EQ. 'X' ) THEN
*
*          < 3. x deriv. >
*
         CALL GRADX
     M         ( ZDATA,
     D           IDIM , JDIM , KMAX  , MMAX , MINT ,
     W           WORK                                 )
         LOFFS = .FALSE.
      ENDIF
*
*          < 4. zonal wvae -> grid >
*
      CALL FFT99X
     O         ( WORK  ,
     M           ZDATA ,
     C           TRIGS , IFAX  ,
     C           1     , IDIM  , IMAX  , JDIM*KMAX , 1    )
*

      DO 4000 K = 1, KMAX
         WORKZ = WORK( 1,K )
         DO 4010 IJ = IDIM*JMAX+1, IDIM*JDIM
            WORK( IJ,K ) = WORKZ
 4010    CONTINUE 
         DO 4020 I = IMAX+1, IDIM
            DO 4020 IJ = 1, IDIM*JDIM, IDIM
               WORK( IJ+I-1,K ) = WORKZ
 4020    CONTINUE 
 4000 CONTINUE 
*
      IF ( LOFFS ) THEN
         DO 4100 K = 1, KMAX
#ifdef SYS_SX3
*vdir noloopchg
#endif
            DO 4100 IJ = 1, IDIM*JDIM
               WORK ( IJ,K ) = WORK ( IJ,K ) + DOFFS( K )
 4100    CONTINUE
      ENDIF
*
*          < 5. output data >
*
      IF      ( HFUNC(1:1) .EQ. 'A' ) THEN
*
*                                ( add )
        DO 5000 K = 1, KMAX
           DO 5000 IJ = 1, IDIM*JDIM
              GDATA( IJ,K ) = GDATA( IJ,K ) + WORK( IJ,K )
 5000   CONTINUE
*
      ELSE IF ( HFUNC(1:1) .EQ. 'S' ) THEN
*
*                                ( sub )
        DO 5100 K = 1, KMAX
           DO 5100 IJ = 1, IDIM*JDIM
              GDATA( IJ,K ) = GDATA( IJ,K ) - WORK( IJ,K )
 5100   CONTINUE
*
      ELSE IF ( HFUNC(1:1) .EQ. 'N' ) THEN
*
*                                ( negative )
        DO 5200 K = 1, KMAX
           DO 5200 IJ = 1, IDIM*JDIM
              GDATA( IJ,K ) = - WORK( IJ,K )
 5200   CONTINUE
*
      ELSE
*                                ( positive )
        DO 5300 K = 1, KMAX
           DO 5300 IJ = 1, IDIM*JDIM
              GDATA( IJ,K ) = WORK( IJ,K )
 5300   CONTINUE
      ENDIF
*
      RETURN
      END
**********************************************************************
      SUBROUTINE SPG2W     !! grid -> spectral
     M         ( WDATA ,
     I           GDATA ,
     C           PNM   , NMO   , TRIGS , IFAX , GW   ,
     F           HGRAD , HFUNC ,
     D           IMAX  , JMAX  , KMAX  , IDIM  , JDIM  ,
     D           LMAX  , MMAX  , NMAX  , MINT  , NMDIM , JMXHF ,
     W           ZDATA , WORK                                    )
*
*   [PARAM] 
      INTEGER    IMAX
      INTEGER    JMAX
      INTEGER    KMAX
      INTEGER    IDIM
      INTEGER    JDIM
      INTEGER    LMAX
      INTEGER    MMAX
      INTEGER    NMAX
      INTEGER    MINT
      INTEGER    NMDIM
      INTEGER    JMXHF
*
*   [MODIFY] 
      REAL*8     WDATA ( NMDIM, KMAX     )    !! spectral data
*
*   [INPUT] 
      REAL*8     GDATA ( IDIM*JDIM, KMAX )    !! grid point data
*
      REAL*8     PNM   ( NMDIM, JMXHF )       !! Legendre function
      INTEGER    NMO   ( 2, 0:MMAX , 0:LMAX ) !! order of spect. suffix
      REAL*8     GW    ( JDIM )               !! Gaussian weight
      REAL*8     TRIGS ( * )                  !! triangle function table
      INTEGER    IFAX  ( * )                  !! factorziation of IMAX
*
      CHARACTER  HGRAD*4                      !! flag of differential
      CHARACTER  HFUNC*4                      !! flag of sign
*
*   [WORK] 
      REAL*8     ZDATA ( IDIM*JDIM, KMAX )    !! zonal spectral
      REAL*8     WORK  ( IDIM*JDIM, KMAX )    !! work
*
*   [INTERNAL WORK] 
      LOGICAL    LDPNM                        !! y differentail flag
      LOGICAL    LOFFS                        !! offset flag
      INTEGER    KMAXD
      PARAMETER (KMAXD=100)
      REAL*8     DOFFS ( KMAXD )              !! offset value
      INTEGER    IJ, K
*
      IF ( IMAX .EQ. 1 .OR. JMAX .EQ. 1 ) THEN
         WRITE (6,*) ' ### SPG2W: THIS ROUTINE IS FOR 3 DIM.'
         CALL XABORT( 2 )
         RETURN
      ENDIF
*
*          < 1. LOFFS, LDPNM : flag >
*
      IF ( HFUNC(4:4) .EQ. 'O' ) THEN
         LOFFS = .TRUE.
         IF ( KMAXD .LT. KMAX ) THEN
            WRITE (6,*) ' ### SPG2W: WORK AREA(KMAXD) TOO SMALL < ',
     &                  KMAX
            CALL XABORT( 1 )
            RETURN
         ENDIF
      ELSE
         LOFFS = .FALSE.
      ENDIF
*
      IF ( HGRAD(1:1) .EQ. 'Y' ) THEN
         LDPNM = .TRUE.
         LOFFS = .FALSE.
      ELSE
         LDPNM = .FALSE.
      ENDIF
*
*          < 2. duplicate input >
*
      IF ( LOFFS ) THEN
         DO 2000 K = 1, KMAX
            DOFFS ( K ) = GDATA( 1,K )
#ifdef SYS_SX3
*vdir noloopchg
#endif
            DO 2010 IJ = 1, IDIM*JDIM
               WORK ( IJ,K ) = GDATA( IJ,K ) - DOFFS( K )
 2010       CONTINUE
 2000    CONTINUE
      ELSE
         CALL COPY  ( WORK , GDATA , IDIM*JDIM*KMAX )
      ENDIF
*
*          < 3. grid -> zonal wave >
*
      CALL FFT99X
     M         ( WORK  ,
     O           ZDATA ,
     C           TRIGS , IFAX  ,
     C           1     , IDIM  , IMAX  , JDIM*KMAX , 0    )
*
      IF ( HGRAD(1:1) .EQ. 'X' ) THEN
*
*                  < 4. x deriv. >
*
         CALL GRADX
     M         ( ZDATA,
     D           IDIM , JDIM , KMAX , MMAX , MINT  ,
     W           WORK )
*
         LOFFS = .FALSE.
      ENDIF
*
*          < 5. zonal wave -> spectral >
*
      CALL SPZ2W
     M         ( WDATA ,
     I           ZDATA ,
     C           PNM   , NMO   , GW    ,
     F           LDPNM , HFUNC ,
     D           JMAX  , KMAX  , IDIM  , JDIM  ,
     D           LMAX  , MMAX  , NMAX  , MINT  , NMDIM , JMXHF ,
     W           WORK                                           )
*
      IF ( LOFFS ) THEN
         IF (      ( HFUNC(1:1) .EQ. 'N' )
     &        .OR. ( HFUNC(1:1) .EQ. 'S' )    ) THEN
            DO 5100 K = 1, KMAX
               WDATA( NMO(1,0,0),K ) = WDATA( NMO(1,0,0),K )
     &                               - DOFFS( K )
 5100       CONTINUE
         ELSE
            DO 5200 K = 1, KMAX
               WDATA( NMO(1,0,0),K ) = WDATA( NMO(1,0,0),K )
     &                               + DOFFS( K )
 5200       CONTINUE
         ENDIF
      ENDIF
      DO 5300 K = 1, KMAX
         WDATA( NMO(2,0,0), K ) = 0.D0
 5300 CONTINUE
*
      RETURN
      END
**********************************************************************
      SUBROUTINE SPW2Z     !! Legendre trans. (spect.->grid)
     O         ( ZDATA   ,
     I           WDATA ,
     C           PNM   , NMO   ,
     F           LDPNM ,
     D           JMAX  , KMAX  , IDIM  , JDIM  ,
     D           LMAX  , MMAX  , NMAX  , MINT  , NMDIM , JMXHF ,
     W           ZDW                                            )
*
*   [PARAM] 
      INTEGER    JMAX
      INTEGER    KMAX
      INTEGER    IDIM
      INTEGER    JDIM
      INTEGER    LMAX
      INTEGER    MMAX
      INTEGER    NMAX
      INTEGER    MINT
      INTEGER    NMDIM
      INTEGER    JMXHF
*
*   [OUTPUT] 
      REAL*8     ZDATA ( IDIM, JDIM, KMAX )   !! zonal spectral
*
*   [INPUT] 
      REAL*8     WDATA ( NMDIM, KMAX      )   !! spectral
*
      REAL*8     PNM   ( NMDIM, JMXHF )       !! Legendre function
      INTEGER    NMO   ( 2, 0:MMAX , 0:LMAX ) !! order of spect. suffix
*
      LOGICAL    LDPNM                        !! y differentail flag
*
*   [WORK] 
      REAL*8     ZDW   ( IDIM, JDIM, KMAX )   !! work
*
*   [INTERNAL WORK] 
      INTEGER    L, M, NM, K
      INTEGER    LEND, MM, IM
      INTEGER    J, JP, JE, JO, JN, JS
*
*
*   [INTERNAL ONCE] 
      INTEGER    NMDIMD
      PARAMETER (NMDIMD=12000)
      INTEGER    MLIST ( NMDIMD )             !! list vector
      INTEGER    JLIST ( NMDIMD )             !! list vector
      SAVE       MLIST, JLIST, OLSET
      LOGICAL    OLSET
      DATA       OLSET / .FALSE. /
*
*          < 0. make list vector >
*
*   [ONCE] 
      IF ( .NOT. OLSET ) THEN
         OLSET = .TRUE.
*
         IF ( NMDIMD .LT. NMDIM ) THEN
            WRITE (6,*) ' ### SPW2Z: WORK AREA(NMDIMD) TOO SMALL < ',
     &                  NMDIM
            CALL XABORT( 1 )
            RETURN
         ENDIF
*
         DO 200 M = 0, MMAX, MINT
            LEND = MIN ( LMAX, NMAX-M )
            MM = M / MINT
*
            DO 100 L = 0, LEND
               MLIST (  NMO ( 1,M,L )  ) = 2 * MM + 1
               MLIST (  NMO ( 2,M,L )  ) = 2 * MM + 2
*
               IF ( MOD( L,2 ) .EQ. 0 ) THEN
                  JLIST (  NMO ( 1,M,L )  ) = 0
                  JLIST (  NMO ( 2,M,L )  ) = 0
               ELSE
                  JLIST (  NMO ( 1,M,L )  ) = (JMAX+1)/2
                  JLIST (  NMO ( 2,M,L )  ) = (JMAX+1)/2
               ENDIF
*
  100       CONTINUE
  200    CONTINUE
*
      ENDIF
*
*          < 1. reset work area >
*
      CALL RESET ( ZDW  , IDIM*JDIM*KMAX )
      CALL RESET ( ZDATA, IDIM*JDIM*KMAX )
*
*          < 2.  Legendre transform >
*
      IF ( KMAX .LT. (JMAX+1)/2 ) THEN
         DO 2300 NM = 1 , NMDIM
            IM = MLIST( NM )
            DO 2200 K = 1 , KMAX
#ifdef SYS_HITAC
*VOPTION INDEP(ZDW)
#endif
#ifdef SYS_SX3
*vdir nodep(ZDW)
#endif
               DO 2100 J = 1 , (JMAX+1)/2
                  JP = JLIST( NM ) + J
                  ZDW( IM,JP,K ) = ZDW( IM,JP,K   )
     &                           +  PNM  ( NM,J ) * WDATA( NM,K )
 2100          CONTINUE
 2200       CONTINUE
 2300    CONTINUE
      ELSE
         DO 2600 NM = 1 , NMDIM
            IM = MLIST( NM )
#ifdef SYS_SX3
*vdir noloopchg
#endif
            DO 2500 J = 1 , (JMAX+1)/2
               JP = JLIST( NM ) + J
#ifdef SYS_HITAC
*VOPTION INDEP(ZDW)
#endif
#ifdef SYS_SX3
*vdir nodep(ZDW)
#endif
               DO 2400 K = 1 , KMAX
                  ZDW( IM,JP,K ) = ZDW( IM,JP,K   )
     &                           +  PNM  ( NM,J ) * WDATA( NM,K )
 2400          CONTINUE
 2500       CONTINUE
 2600    CONTINUE
      ENDIF
*
*          < 3. sym, assym -> normal coder >
*
      DO 3200 J = 1 , (JMAX+1)/2
         JN = J
         JS = JMAX+1 - J
         IF ( .NOT. LDPNM ) THEN
*                                 (  PNM using case )
            JE = J
            JO = (JMAX+1)/2 + J
         ELSE
*                                 ( DPNM using case )
            JE = (JMAX+1)/2 + J
            JO = JN
         ENDIF
*
         DO 3100 K = 1 , KMAX
            DO 3100 IM = 1 , IDIM
               ZDATA( IM,JS,K ) = ZDW( IM,JE,K ) - ZDW( IM,JO,K )
               ZDATA( IM,JN,K ) = ZDW( IM,JE,K ) + ZDW( IM,JO,K )
 3100    CONTINUE
*
 3200 CONTINUE
*
      DO 3300 K = 1, KMAX
         DO 3300 J = 1, JDIM
            ZDATA( 2,J,K ) = 0.D0
 3300 CONTINUE 
*
      RETURN
      END
**********************************************************************
      SUBROUTINE SPZ2W     !! Legendre trans. (grid->spect.)
     M         ( WDATA ,
     I           ZDATA   ,
     C           PNM   , NMO   , GW    ,
     F           LDPNM , HFUNC ,
     D           JMAX  , KMAX  , IDIM  , JDIM  ,
     D           LMAX  , MMAX  , NMAX  , MINT  , NMDIM , JMXHF ,
     W           ZDW                                            )
*
*   [PARAM] 
      INTEGER    JMAX
      INTEGER    KMAX
      INTEGER    IDIM
      INTEGER    JDIM
      INTEGER    LMAX
      INTEGER    MMAX
      INTEGER    NMAX
      INTEGER    MINT
      INTEGER    NMDIM
      INTEGER    JMXHF
*
*   [MODIFY] 
      REAL*8     WDATA ( NMDIM, KMAX      )   !! spectral
*
*   [INPUT] 
      REAL*8     ZDATA ( IDIM, JDIM, KMAX )   !! zonal spectral
*
      REAL*8     PNM   ( NMDIM, JMXHF )       !! Legendre function
      INTEGER    NMO   ( 2, 0:MMAX , 0:LMAX ) !! order of spect. suffix
      REAL*8     GW    ( JDIM )               !! Gaussian weight
*
      LOGICAL    LDPNM                        !! y differentail flag
      CHARACTER  HFUNC*4                      !! flag of sign
*
*   [WORK] 
      REAL*8     ZDW   ( IDIM, JDIM, KMAX )   !! work
*
*   [INTERNAL WORK] 
      INTEGER    L, M, NM, K
      INTEGER    LEND, MM, IM
      INTEGER    J, JP, JE, JO, JN, JS
*
*   [INTERNAL ONCE] 
      INTEGER    NMDIMD
      PARAMETER (NMDIMD=12000)
      INTEGER    MLIST ( NMDIMD )             !! list vector
      INTEGER    JLIST ( NMDIMD )             !! list vector
      SAVE       MLIST, JLIST, OLSET
      LOGICAL    OLSET
      DATA       OLSET / .FALSE. /
*
*          < 0. make list vector >
*
*   [ONCE] 
      IF ( .NOT. OLSET ) THEN
         OLSET = .TRUE.
*
         IF ( NMDIMD .LT. NMDIM ) THEN
            WRITE (6,*) ' ### SPZ2W: WORK AREA(NMDIMD) TOO SMALL < ',
     &                  NMDIM
            CALL XABORT( 1 )
            RETURN
         ENDIF
*
         DO 200 M = 0, MMAX, MINT
            LEND = MIN ( LMAX, NMAX-M )
            MM   = M / MINT
            DO 100 L = 0, LEND
               MLIST (  NMO ( 1,M,L )  ) = 2* MM + 1
               MLIST (  NMO ( 2,M,L )  ) = 2* MM + 2
*
               IF ( MOD( L,2 ) .EQ. 0 ) THEN
                  JLIST (  NMO ( 1,M,L )  ) = 0
                  JLIST (  NMO ( 2,M,L )  ) = 0
               ELSE
                  JLIST (  NMO ( 1,M,L )  ) = (JMAX+1)/2
                  JLIST (  NMO ( 2,M,L )  ) = (JMAX+1)/2
               ENDIF
*
  100       CONTINUE
  200    CONTINUE
*
      ENDIF
*
*
      IF (      ( HFUNC(1:1) .NE. 'A' )
     &     .AND.( HFUNC(1:1) .NE. 'S' )    ) THEN
*
*                  < 1. reset unless add/sub >
*
         CALL RESET ( WDATA, NMDIM*KMAX )
      ENDIF
*
*          < 2. multip Gauss weight >
*               ( normal order -> sym.\&assym. comp. )
*
      DO 2200 J = 1 , (JMAX+1)/2
*
         JN = J
         JS = JMAX+1 - J
         IF ( .NOT. LDPNM ) THEN
*                                 (  PNM using case )
            JE = J
            JO = (JMAX+1)/2 + J
         ELSE
*                                 ( DPNM using case )
            JE = (JMAX+1)/2 + J
            JO = JN
         ENDIF
*
         DO 2100 K = 1 , KMAX
            DO 2100 IM = 1 , IDIM
               ZDW( IM,JE,K ) = GW ( J ) *
     &                      ( ZDATA( IM,JN,K ) + ZDATA( IM,JS,K ) )
               ZDW( IM,JO,K ) = GW ( J ) *
     &                      ( ZDATA( IM,JN,K ) - ZDATA( IM,JS,K ) )
 2100    CONTINUE
*
 2200 CONTINUE
*
*
      IF (      ( HFUNC(1:1) .EQ. 'N' )
     &     .OR. ( HFUNC(1:1) .EQ. 'S' )    ) THEN
*
*          < 3. Legendre transform negative   >
*
         IF ( KMAX .LT. NMDIM ) THEN
            DO 3300 J = 1 , (JMAX+1)/2
               DO 3200 K = 1 , KMAX
                  DO 3100 NM = 1 , NMDIM
                     IM = MLIST( NM )
                     JP = JLIST( NM ) + J
                     WDATA( NM,K   )  = WDATA( NM,K   )
     &                                -  PNM ( NM,J ) * ZDW( IM,JP,K   )
 3100             CONTINUE
 3200          CONTINUE
 3300       CONTINUE
         ELSE
            DO 3600 J = 1 , (JMAX+1)/2
               DO 3500 NM = 1 , NMDIM
                  DO 3400 K = 1 , KMAX
                     IM = MLIST( NM )
                     JP = JLIST( NM ) + J
                     WDATA( NM,K   )  = WDATA( NM,K   )
     &                                -  PNM ( NM,J ) * ZDW( IM,JP,K   )
 3400             CONTINUE
 3500          CONTINUE
 3600       CONTINUE
         ENDIF
*
      ELSE
*
*          < 4. Legendre transform positive   >
*
         IF ( KMAX .LT. NMDIM ) THEN
            DO 4300 J = 1 , (JMAX+1)/2
               DO 4200 K = 1 , KMAX
                  DO 4100 NM = 1 , NMDIM
                     IM = MLIST( NM )
                     JP = JLIST( NM ) + J
                     WDATA( NM,K   )  = WDATA( NM,K   )
     &                                +  PNM ( NM,J ) * ZDW( IM,JP,K   )
 4100             CONTINUE
 4200          CONTINUE
 4300       CONTINUE
         ELSE
            DO 4600 J = 1 , (JMAX+1)/2
               DO 4500 NM = 1 , NMDIM
                  DO 4400 K = 1 , KMAX
                     IM = MLIST( NM )
                     JP = JLIST( NM ) + J
                     WDATA( NM,K   )  = WDATA( NM,K   )
     &                                +  PNM ( NM,J ) * ZDW( IM,JP,K   )
 4400             CONTINUE
 4500          CONTINUE
 4600       CONTINUE
         ENDIF
*
      ENDIF
*
      RETURN
      END
************************************************************************
      SUBROUTINE GRADX     !! spectral component of x-differential
     M         ( ZDATA ,
     D           IDIM  , JDIM  , KMAX  , MMAX  , MINT ,
     W           ZDW    )
*
*   [PARAM] 
      INTEGER    IDIM
      INTEGER    JDIM
      INTEGER    KMAX
      INTEGER    MMAX
      INTEGER    MINT
*
*   [MODIFY] 
      REAL*8     ZDATA ( IDIM, JDIM, KMAX ) !! data
*
*   [WORK] 
      REAL*8     ZDW   ( IDIM, JDIM, KMAX ) !! work
*
*   [INTERNAL WORK] 
      INTEGER    M, MR, MI, MM
      INTEGER    J, K
*
*
*          < 1. work area >
*
      CALL COPY ( ZDW , ZDATA , IDIM*JDIM*KMAX )
*
*          < 2. multiply im >
*
      DO 2100 M = 0, MMAX, MINT
         MM = M / MINT
         MR = 2*MM + 1
         MI = 2*MM + 2
         DO 2110 K = 1, KMAX
            DO 2110 J = 1, JDIM
               ZDATA( MR, J, K ) = - DBLE( M ) * ZDW  ( MI, J, K )
               ZDATA( MI, J, K ) =   DBLE( M ) * ZDW  ( MR, J, K )
 2110    CONTINUE      
 2100 CONTINUE
*
      RETURN
      END
