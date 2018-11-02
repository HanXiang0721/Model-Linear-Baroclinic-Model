*"表題 球面調和関数の準備 GTOOL3(GPASPS)
*
*"履歴 91/01/17 沼口  敦
*
*
**********************************************************************
*"      << 球面調和関数の準備 >>
**********************************************************************
      SUBROUTINE GPASPS
     I         ( IMAX  , JMAX  , KMAX  , ER    ,
     O           PNM   , DPNM  , TRIGS , IFAX  ,
     O           NMO   , GW    , COSLAT,
     O           LMAX  , MMAX  , NMAX  , MINT  ,
     O           NMDIM , JMXHF ,
     W           QPNM  , QDPNM , QGW   , QSINLA          )
*
      INTEGER    IMAX
      INTEGER    JMAX
      REAL       ER                          !" 地球半径
*
      REAL       PNM   ( * )                 !" Ｐnm ルジャンドル
      REAL       DPNM  ( * )                 !" Ｐnm μ微分
      REAL       TRIGS ( * )                 !" 三角関数表
      INTEGER    IFAX  ( * )                 !" IMAX の因数分解
*
      INTEGER    NMO   ( * )                 !" スペクトルの添字順番
      REAL       GW    ( * )                 !" ガウス荷重
      REAL       COSLAT( * )                 !" cos(緯度)
*
      REAL*8     QPNM  ( * )                 !" Ｐnm ルジャンドル
      REAL*8     QDPNM ( * )                 !" Ｐnm μ微分
      REAL*8     QGW   ( * )                 !" ガウス荷重 ：倍精度
      REAL*8     QSINLA( * )                 !" sin(緯度) (倍精度
*
      INTEGER    IMAX0 , JMAX0               !" セット値
      INTEGER    IMAXD , JMAXD, KMAXD        !" 領域の大きさ
      CHARACTER  HMSG *100
*
      DATA       IMAX0 , JMAX0 / 0, 0 /
      DATA       IMAXD , JMAXD, KMAXD / 0, 0, 0 /
      SAVE
*
      IF ( ( IMAXD*JMAXD*KMAXD .NE. 0 )
     &             .AND.
     &     ( ( IMAX .GT. IMAXD ) .OR. ( JMAX .GT. JMAXD )
     &                           .OR. ( KMAX .GT. KMAXD ) ) ) THEN
*
         HMSG = 'INSUFFICIENT SPACE: IMAX= '
         NH = LENC( HMSG )
         WRITE ( HMSG(NH+1:NH+9), '(I4,1X,I4)' ) IMAX,IMAXD
         HMSG(NH+10:100) = ', JMAX= '
         NH = LENC( HMSG )
         WRITE ( HMSG(NH+1:NH+9), '(I4,1X,I4)' ) JMAX,JMAXD
         HMSG(NH+10:100) = ', KMAX= '
         NH = LENC( HMSG )
         WRITE ( HMSG(NH+1:NH+9), '(I4,1X,I4)' ) KMAX,KMAXD
*
         CALL MSGDMP('E', 'GPASPS', HMSG )
      ENDIF
*
*"         < 1. 波数：三角形切断の場合のみ！ >
*
      MMAX  = IMAX     / 3
      LMAX  = (JMAX*2) / 3
      NMAX  = MAX( MMAX, LMAX )
      MINT  = 1
      NML   = NMAX-LMAX
      IF ( MMAX .EQ. 0 ) THEN
         NMDIM = LMAX+1
      ELSE
         NMDIM = (MMAX/MINT+1)*(2*(NMAX+1)-LMAX)-NML*(NML+1)
      ENDIF
      JMXHF = JMAX/2+1
*
      IF ( ( IMAX .NE. IMAX0 ) .OR. ( JMAX .NE. JMAX0 ) ) THEN
*
         IMAX0 = IMAX
         JMAX0 = JMAX
*
*"         < 2. FFTの準備 >
*
         IF ( IMAX .GE. 2 ) THEN
            CALL RFFTIM
     I            ( IMAX  ,
     O              TRIGS , IFAX )
         ENDIF
*
*"         < 3. ルジャンドル関数の計算 >
*
         IF ( JMAX .GE. 2 ) THEN
*
            CALL GAUSS
     O         ( QSINLA , QGW   ,
     I           JMAX            )
*
            CALL SETPNM
     O         ( PNM   , DPNM  , NMO   ,
     I           QSINLA,
     D           JMAX  , MMAX  , LMAX  , NMAX  , MINT  ,
     D           NMDIM , JMXHF ,
     W           QPNM  , QDPNM                                  )
*
*"         < 4. ガウス荷重 >
*
            DO 4100 J = 1, JMAX
               GW    ( J ) = QGW( J )
               COSLAT( J ) = SQRT( 1.D0 - QSINLA(J)**2 )
 4100       CONTINUE
*
         ENDIF
*
      ENDIF
*
      RETURN
*=====================================================================
*"    << 球面調和関数領域の大きさ指定 >>
*=====================================================================
      ENTRY      GPASPW
     I         ( IMAXDW, JMAXDW, KMAXDW )
*
      IMAXD = IMAXDW
      JMAXD = JMAXDW
      KMAXD = KMAXDW
*
      END
