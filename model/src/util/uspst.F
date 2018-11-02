* PACKAGE USPST   !! setup for spherical trans.
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*         96/10/21(numaguti) separate LGNDRD 
*
**********************************************************************
      SUBROUTINE SETPNM    !! calculation of Legendre function
     O         ( PNM   , DPNM  , NMO   ,
     I           QSINLA,
     D           JMAX  , MMAX  , LMAX  , NMAX  , MINT  , 
     D           NMDIM , JMXHF ,
     W           QPNM  , QDPNM                          )
*
*   [PARAM] 
      INTEGER    JMAX
      INTEGER    MMAX
      INTEGER    LMAX
      INTEGER    NMAX
      INTEGER    MINT
      INTEGER    NMDIM
      INTEGER    JMXHF
*
*   [OUTPUT]       
      REAL*8     PNM   ( NMDIM, JMXHF )      !! Legendre function
      REAL*8     DPNM  ( NMDIM, JMXHF )      !! mu differential of Pnm
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
*
*   [INPUT] 
      REAL * 8   QSINLA( JMAX )              !! sin(lat.):double
*
*   [WORK] 
      REAL * 8   QPNM  ( 0:NMAX+1, 0:MMAX )  !! Pnm Legendre
      REAL * 8   QDPNM ( 0:NMAX+1, 0:MMAX )  !! mu differential of Pnm
*
*   [INTERNAL WORK] 
      INTEGER    J, L, M, LEND
*
*          < 1. order of matrix >
*
      CALL SETNMO
     O         ( NMO   ,
     D           MMAX  , LMAX  , NMAX  , MINT    )
*
      DO 3300 J = 1, (JMAX+1)/2
*
*          < 2. calculation of Legendre function >
*
         CALL LGNDRE
     I         ( QSINLA(J), MMAX  , NMAX,
     O           QPNM                     )
         CALL LGNDRD 
     I         ( QPNM   , MMAX  , NMAX,
     O           QDPNM                    )
*
*          < 3. repack Legendre func. >
*
         DO 3200 M = 0, MMAX, MINT
            LEND = MIN( LMAX, NMAX-M )
            DO 3100 L = 0, LEND
               PNM  (  NMO( 1,M,L ),J  ) =   QPNM ( L+M,M )
               PNM  (  NMO( 2,M,L ),J  ) =   QPNM ( L+M,M )
               DPNM (  NMO( 1,M,L ),J  ) = - QDPNM( L+M,M )
               DPNM (  NMO( 2,M,L ),J  ) = - QDPNM( L+M,M )
 3100       CONTINUE
 3200    CONTINUE
*
 3300 CONTINUE
*
      RETURN
*==================================================================
      ENTRY      SETPNX         !! calculation of Legendre function
     O         ( PNM   , NMO   ,
     I           QSINLA,
     D           JMAX  , MMAX  , LMAX  , NMAX  , MINT  , 
     D           NMDIM , JMXHF ,
     W           QPNM                                   )
*
*          < 1. order of matrix >
*
      CALL SETNMO
     O         ( NMO   ,
     D           MMAX  , LMAX  , NMAX  , MINT    )
*
      DO 4300 J = 1, (JMAX+1)/2
*
*          < 2. calculation of Legendre function >
*
         CALL LGNDRE
     I         ( QSINLA(J), MMAX  , NMAX,
     O           QPNM                     )
*
*          < 4. repack Legendre func. >
*
         DO 4200 M = 0, MMAX, MINT
            LEND = MIN( LMAX, NMAX-M )
            DO 4100 L = 0, LEND
               PNM  (  NMO( 1,M,L ),J  ) =   QPNM ( L+M,M )
               PNM  (  NMO( 2,M,L ),J  ) =   QPNM ( L+M,M )
 4100       CONTINUE
 4200    CONTINUE
*
 4300 CONTINUE
*
      RETURN
      END
**********************************************************************
      SUBROUTINE SETNMO    !! order of matrix
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
**********************************************************************
      SUBROUTINE LGNDRE    !! calculation of Legendre function
     I         ( CTHETA , MMAX  , NMAX,
     O           QPNM                   )
*
*   [PARAM] 
      INTEGER    NMAX
      INTEGER    MMAX
*
*   [INPUT] 
      REAL * 8   CTHETA                      !! mu = sin(latitude)
*
*   [OUTPUT] 
      REAL * 8   QPNM  ( 0:NMAX+1, 0:MMAX )  !! Pnm Legendre
*
*   [INTERNAL WORK] 
      INTEGER    M, N
      REAL * 8   STHETA, EPS, EPSM
*      
*
*          < 1. (1-mu^2)^1/2  >
*
      STHETA = SQRT ( 1.d0 - CTHETA **2 )
*
*          < 2. calc. P_m^m P_m+1^m >
*
      DO 2100 M = 0, MMAX
*
          IF ( M .EQ. 0 ) THEN
             QPNM ( 0, 0 ) = 1.  
          ELSE
             QPNM ( M ,M ) = QPNM ( M-1,M-1 )
     &                        * SQRT(  DBLE ( 2*M+1 )/ DBLE ( 2*M )  )
     &                        * STHETA
          ENDIF
*
          QPNM ( M+1,M ) = QPNM ( M,M )
     &                     * SQRT( DBLE ( 2*M+3 ) ) * CTHETA
*
 2100 CONTINUE
*
*          < 3. recursive calc. P_n^m >
*
      DO 3100 M = 0, MMAX
         DO 3100 N = M+2, NMAX+1
*
            EPS       = SQRT (   ( DBLE ( N   )**2 - DBLE ( M )**2 )
     &                         / ( 4.d0 * DBLE ( N   )**2 - 1.d0   )  )
            EPSM      = SQRT (   ( DBLE ( N-1 )**2 - DBLE ( M )**2 )
     &                         / ( 4.d0 * DBLE ( N-1 )**2 - 1.d0   )  )
*
            QPNM( N,M ) = (   QPNM ( N-1,M ) * CTHETA
     &                      - QPNM ( N-2,M ) * EPSM    ) / EPS
*
 3100 CONTINUE
*
      RETURN
      END
**********************************************************************
      SUBROUTINE LGNDRD    !! calculation of Legendre function
     I         ( QPNM   , MMAX  , NMAX,
     O           QDPNM                  )
*
*   [PARAM] 
      INTEGER    NMAX
      INTEGER    MMAX
*
*   [INPUT] 
      REAL * 8   QPNM  ( 0:NMAX+1, 0:MMAX )  !! Pnm Legendre
*
*   [OUTPUT] 
      REAL * 8   QDPNM ( 0:NMAX+1, 0:MMAX )  !! mu differential of Pnm
*
*   [INTERNAL WORK] 
      INTEGER    M, N
      REAL * 8   EPS, EPSP
*      
*
*          < 4. recursive calc. d/dy P_n^m >
*
      DO 4100 M = 0, MMAX
*
         EPS         = SQRT (   ( DBLE ( M+1 )**2 - DBLE ( M )**2 )
     &                        / ( 4.d0 * DBLE ( M+1 )**2 - 1.d0   )  )
         QDPNM ( M,M ) = QPNM ( M+1,M ) * DBLE ( M ) * EPS
*
         DO 4200 N = M+1, NMAX
            EPS        = SQRT (   ( DBLE ( N   )**2 - DBLE ( M )**2 )
     &                          / ( 4.d0 * DBLE ( N   )**2 - 1.d0   ) )
            EPSP       = SQRT (   ( DBLE ( N+1 )**2 - DBLE ( M )**2 )
     &                          / ( 4.d0 * DBLE ( N+1 )**2 - 1.d0   ) )
*
            QDPNM( N,M ) =   QPNM ( N+1,M ) * DBLE ( N   ) * EPSP
     &                     - QPNM ( N-1,M ) * DBLE ( N+1 ) * EPS
*
 4200    CONTINUE
 4100 CONTINUE
*
      RETURN
      END
**********************************************************************
      SUBROUTINE GAUSS     !!  calculate Gaussian latitude & weight
     O         ( CTHETA , GW     ,
     I           JMAX             )
*
*   [PARAM] 
      INTEGER    JMAX
*
*   [OUTPUT] 
      REAL * 8   CTHETA( JMAX )              !! sin(lat.)=mu
      REAL * 8   GW    ( JMAX )              !! Gaussian weight
*
*   [INTERNAL WORK] 
      INTEGER    JMAXD
      PARAMETER (JMAXD=360)
      REAL * 8   QPN( 0:JMAXD+1 )            !! Pn (double precision)
      REAL * 8   DELTP
      REAL * 8   PI, X0, DELTX, R, RP
      REAL * 8   EPS, EPSM, EPSP, QDPN
      INTEGER    N, J, ITER, I
*
*   [INTERNAL PARM] 
      INTEGER    ITRMAX
      DATA       ITRMAX / 50 /               !! max. of iteration
*
      IF ( JMAX .GT. JMAXD ) THEN
            WRITE (6,*) ' ### GAUSS: WORK AREA(JMAXD) TOO SMALL < ',
     &                  JMAX
            CALL XABORT( 1 )
            RETURN
      ENDIF
*
*          < 0. check precision >
*
      DELTP = 1.
      DO 1100 I = 1, 30
         DELTP = DELTP / 10.
         R   = 1. 
         RP  = R + DELTP
         WRITE ( 6,* ) ' *** precision check .. ', RP
         IF ( RP .LE. R ) GOTO 1200
 1100 CONTINUE 
 1200 CONTINUE 
      DELTP = DELTP *100.
*
      PI = ATAN( 1.d0 ) * 4.d0
*
      DO 5100 J = 1 , (JMAX+1)/2
*
*          < 1. initial guess >
*
         X0 = COS (  ( DBLE ( J ) - 0.5d0  )/ DBLE ( JMAX ) * PI   )
*
         DO 4100 ITER = 1, ITRMAX
*
*          < 2. calc. Pn >
*
            QPN   ( 0 ) = 1.d0
            QPN   ( 1 ) = SQRT( 3.d0  ) * X0
*
            DO 2100 N = 2, JMAX+1
               EPS      = SQRT (   ( DBLE ( N   )**2                )
     &                           / ( 4.d0 * DBLE ( N   )**2 - 1.d0  ) )
               EPSM     = SQRT (   ( DBLE ( N-1 )**2                )
     &                           / ( 4.d0 * DBLE ( N-1 )**2 - 1.d0  ) )
*
               QPN ( N ) = (   QPN  ( N-1 ) * X0
     &                        - QPN  ( N-2 ) * EPSM    ) / EPS
 2100       CONTINUE
*
*          < 3. calc. d/dmu Pn >
*
            EPS      = SQRT (   ( DBLE ( JMAX   )**2                  )
     &                        / ( 4.d0 * DBLE ( JMAX   )**2 - 1.d0 )  )
            EPSP     = SQRT (   ( DBLE ( JMAX+1 )**2                  )
     &                        / ( 4.d0 * DBLE ( JMAX+1 )**2 - 1.d0 )  )
*
            QDPN      =   QPN  ( JMAX+1 ) * DBLE ( JMAX   ) * EPSP
     &                  - QPN  ( JMAX-1 ) * DBLE ( JMAX+1 ) * EPS
*
*          < 4. solve by Newton method >
*
            DELTX = QPN ( JMAX ) / QDPN * ( 1.d0 - X0 **2 )
            X0    = X0 + DELTX
*
            IF ( ABS( DELTX ) .LT. DELTP ) GOTO 4200
*                                              >---- exit ----->
 4100    CONTINUE
         WRITE ( 6,* ) ' error in GAUSS(4) ', DELTX
*
*                                              <---- exit -----<
 4200    CONTINUE
*
         CTHETA ( J ) = X0
*
*          < 5. Gaussian weight >
*
         GW     ( J ) =    ( DBLE ( 2*JMAX ) - 1.d0  )
     &                   * ( 1.d0  - X0 **2 )
     &                   / (   DBLE ( JMAX ) * QPN ( JMAX-1 )  )**2
*
 5100 CONTINUE
*
      DO 6100 J = 1, JMAX/2
         GW     ( JMAX-J+1 ) =   GW     ( J )
         CTHETA ( JMAX-J+1 ) = - CTHETA ( J )
 6100 CONTINUE
*
      RETURN
      END
