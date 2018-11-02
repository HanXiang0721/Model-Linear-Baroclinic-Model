* SUPPLEMENT ROUTINE IN LBM2.0 on 2001/10/22
*=============================================================
      SUBROUTINE INTP
     I(IMAX,JMAX,KMAX,IDIM,JDIM,ALONI,COLRAI,ALONO,COLRAO,DMS,
     I DIN,
     O DOUT)
*
*     spatial interporation
*
C     USAGE:
C        IMAX: x-dimension for input data
C        JMAX: y-dimension for input data
C        KMAX: z-dimension for input data
C        IDIM: x-dimension for output data
C        JDIM: y-dimension for output data
C        ALONI : array of longitude (0 - 360  deg.) for input
C        COLRAI: array of latitude  (-90 - 90 deg.) for input
C        ALONO : array of longitude (0 - 360  deg.) for output
C        COLRAO: array of latitude  (-90 - 90 deg.) for output
C        DMS   : value for missing grid
C        DIN   : input array, DIN=DIN(IMAX,JMAX,KMAX)
C        DOUT  : output array, DOUT=DOUT(IDIM,JDIM,KMAX)
C
*
      INTEGER   IMAX, JMAX, KMAX
      INTEGER   IDIM, JDIM
      REAL*4    DMS
      REAL*4    DIN(IMAX,JMAX,KMAX),DOUT(IDIM,JDIM,KMAX)
      REAL*4    ALONI(IMAX),ALONO(IDIM)
      REAL*4    COLRAO(JDIM),COLRAI(JMAX)
*
      INTEGER   I, J, K
*
      CALL LINTPMS
     I(IMAX,JMAX,KMAX,IDIM,JDIM,ALONI,COLRAI,ALONO,COLRAO,DMS,
     I DIN,
     O DOUT)
*
      DO K = 1, KMAX
         DO J = 1, JDIM
            IF( COLRAO( J ) .LT. COLRAI( 1    ) .OR.
     &          COLRAO( J ) .GT. COLRAI( JMAX )      ) THEN
               DO I = 1, IDIM
                  DOUT( I,J,K ) = DMS
               ENDDO    
            ENDIF
         ENDDO
      ENDDO
*
      RETURN
      END
*=============================================================
      SUBROUTINE LINTPMS
     I(IMAX,JMAX,KMAX,IDIM,JDIM,ALONI,COLRAI,ALONO,COLRAO,DMS,
     I DIN,
     O DOUT)
*
      INTEGER   MAXDM
      PARAMETER ( MAXDM = 1000 )
*
      INTEGER   IMAX, JMAX, KMAX
      INTEGER   IDIM, JDIM
      REAL*4    DMS
      REAL*4    DIN(IMAX,JMAX,KMAX),DOUT(IDIM,JDIM,KMAX)
      REAL*4    DX(MAXDM),DY(MAXDM),LX(MAXDM),LY(MAXDM),LXP(MAXDM)
      REAL*4    ALONI(IMAX),ALONO(IDIM)
      REAL*4    COLRAO(JDIM),COLRAI(JMAX),WEI(4)

      REAL*4    DLON, FLON, FX, FY, WSUM
      INTEGER   ID, JD, IP, JP, IDP, K
      LOGICAL   FIRST/.TRUE./
      DATA      LY/MAXDM*0/                 ! 1/10/95
*
      IF(FIRST) THEN
         FIRST=.FALSE.
         DLON=ALONI(2)-ALONI(1)
         DO 100 JP=1,JDIM
            DO 120 JD=2,JMAX
               IF(COLRAI(JD).GE.COLRAO(JP)) THEN
                  LY(JP)=JD
                  DY(JP)=(COLRAO(JP)-COLRAI(JD-1))
     &                  /(COLRAI(JD)-COLRAI(JD-1))
                  GO TO 100
               ENDIF
  120       CONTINUE
  100    CONTINUE
         DO 140 IP=1,IDIM
            FLON=ALONO(IP)
            IF(ALONO(IP) .GT. 360.) FLON=ALONO(IP)-360.
            DO 141 ID=1,IMAX-1
               IF(ALONI(ID).LE.FLON .AND. ALONI(ID+1).GT.FLON) GO TO 142
  141       CONTINUE
            ID=IMAX
  142       LX(IP)=ID
            IF(LX(IP) .GT. IMAX) LX(IP)=LX(IP)-IMAX
            IDP=MOD(ID,IMAX)+1
            LXP(IP)=IDP
            DX(IP)=(FLON-ALONI(ID))/DLON
            IF(DX(IP) .LT. 0.0) DX(IP)=DX(IP)+360./DLON 
  140    CONTINUE
      ENDIF
*
      DO 200  K=1,KMAX
         DO 220 JP=1,JDIM
         JD=LY(JP)
         FY=DY(JP)
         DO 220 IP=1,IDIM
         ID=LX(IP)
         IDP=LXP(IP)
         FX=DX(IP)
         WEI(1)=FX*FY
         WEI(2)=(1.0-FX)*FY
         WEI(3)=FX*(1.0-FY)
         WEI(4)=(1.0-FX)*(1.0-FY)
         IF(DIN(IDP ,JD  ,K) .EQ. DMS) WEI(1)=0.0
         IF(DIN(ID  ,JD  ,K) .EQ. DMS) WEI(2)=0.0
         IF(DIN(IDP ,JD-1,K) .EQ. DMS) WEI(3)=0.0
         IF(DIN(ID  ,JD-1,K) .EQ. DMS) WEI(4)=0.0
         WSUM=WEI(1)+WEI(2)+WEI(3)+WEI(4)
         IF(WSUM .LE. 0.0 .OR. JD .LE. 1) THEN
            DOUT(IP,JP,K)=DMS
         ELSE
            DOUT(IP,JP,K)=WEI(1)*DIN(IDP ,JD  ,K)
     &                   +WEI(2)*DIN(ID  ,JD  ,K)
     &                   +WEI(3)*DIN(IDP ,JD-1,K)
     &                   +WEI(4)*DIN(ID  ,JD-1,K)
            DOUT(IP,JP,K)=DOUT(IP,JP,K)/WSUM
         ENDIF
  220    CONTINUE
  200 CONTINUE
*
      RETURN
      END
