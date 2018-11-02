* PACKAGE UFFTP  !! FFT (NAKAMURA)
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*
**********************************************************************
*            << FFT --call >>
**********************************************************************
      SUBROUTINE FFT99X    !! call FFT
     M         ( G     ,
     M           Z     ,
     C           TRIGS , IFAX  ,
     C           INC   , JUMP  , N     , LOT  , ISIGN  )
*
      REAL*8     G ( * )                     !! grid point data
      REAL*8     Z ( * )                     !! spectral
*
      REAL*8     TRIGS ( * )                 !! triangle function table
      INTEGER    IFAX  ( * )                 !! factorization (dummy)
      INTEGER    INC                         !! how to fill in matrix
      INTEGER    JUMP
      INTEGER    N
      INTEGER    LOT
      INTEGER    ISIGN                       !! 0:forward 1:inverse
*
*   [INTERNAL WORK] 
      INTEGER    INCN, LB, LA
      INTEGER    I, K
*
      IF ( N .EQ. 1 ) THEN
         IF ( ISIGN .EQ. 0 ) THEN  
            DO 100 K = 1, LOT
               Z ( K ) = G ( K )
  100       CONTINUE
         ELSE
            DO 200 K = 1, LOT
               G ( K ) = Z ( K )
  200       CONTINUE
         ENDIF
         RETURN
      ENDIF
*
      IF ( JUMP .GT. INC ) THEN
         INCN = ( LOT*JUMP ) / N
      ELSE
         INCN = INC
      ENDIF
      IF ( MOD( INCN,16 ) .EQ. 0 ) INCN = INCN-1
      INCN = MAX ( INCN, LOT )
*
      IF ( ISIGN .EQ. 0 ) THEN  !!  grid -> spectral
*
         DO 1100 I = 1, N
            LA = (I-1)*INC  + 1
            LB = (I-1)*INCN
            DO 1110 K = 1, LOT
               Z ( LB+K ) = G ( LA )
               LA = LA + JUMP
 1110       CONTINUE
 1100    CONTINUE
*
         CALL   RFFTFM
     I         ( N     , INCN  , 1     , LOT   ,
     M           Z     , 
     I           TRIGS , IFAX  ,
     W           G                               )
*
         DO 1200 K = 1, LOT
            G( K      ) = Z( K )
            G( K+INCN ) = Z( K+INCN*(N-1) )
 1200    CONTINUE 
         DO 1300 I = 3, N
            LB = (I-1)*INCN
            DO 1310 K = 1, LOT
               G( LB+K ) = Z( LB+K-INCN ) 
 1310       CONTINUE 
 1300    CONTINUE 
         DO 1400 I = 1, N
            LA = (I-1)*INC  + 1
            LB = (I-1)*INCN
            DO 1410 K = 1, LOT
               Z ( LA ) = G ( LB+K )
               LA = LA + JUMP
 1410       CONTINUE
 1400    CONTINUE
*
      ELSE     !! spectral -> grid
*
         DO 2100 I = 1, N
            LA = (I-1)*INC  + 1
            LB = (I-1)*INCN
            DO 2110 K = 1, LOT
               G ( LB+K ) = Z ( LA )
               LA = LA + JUMP
 2110       CONTINUE
 2100    CONTINUE
         DO 2200 K = 1, LOT
            Z ( K            ) = G ( K )
            Z ( K+INCN*(N-1) ) = G ( K+INCN )
 2200    CONTINUE
         DO 2310 I = 3, N
            LB = (I-1)*INCN
            DO 2310 K = 1, LOT
               Z( LB+K-INCN ) = G( LB+K ) 
 2310       CONTINUE 
 2300    CONTINUE 
*
         CALL   RFFTBM
     I         ( N     , INCN  , 1     , LOT   ,
     M           Z     , 
     I           TRIGS , IFAX  ,
     W           G                               )
*
         DO 2400 I = 1, N
            LA = (I-1)*INC  + 1
            LB = (I-1)*INCN
            DO 2410 K = 1, LOT
               G ( LA ) = Z ( LB+K )
               LA = LA + JUMP
 2410       CONTINUE
 2400    CONTINUE
*
      ENDIF
*
      RETURN
      END
************************************************************************
*            << FFT create table >>
************************************************************************
      SUBROUTINE RFFTIM    !!  make function table for FFT
     I         ( N     ,
     O           TRIGS , IFAX  )
*
      INTEGER    N
      REAL*8     TRIGS ( * )                 !! triangle function table
      INTEGER    IFAX  ( * )                 !! factorization
*
      IF (N .EQ. 1) RETURN
      CALL RFTI1M ( N, TRIGS, IFAX )
*
      RETURN
      END
************************************************************************
      SUBROUTINE RFTI1M    !! FFT
     &          (N, WA,IFAC)
*
      INTEGER    N
      REAL*8     WA(N)
      INTEGER    IFAC(*)
*
      INTEGER    NTRYH(4)
      REAL*8     ARG, ARGLD, TPI
      INTEGER    NL, NF, NTRY, NR, NQ
      INTEGER    IS, L1, K, IP
      INTEGER    IDO, IFI
      INTEGER    I, J
*
      DATA NTRYH(1),NTRYH(2),NTRYH(3),NTRYH(4) /4,2,3,5/
*
      TPI = 8.D0* ATAN(1.D0)
*
      NL = N
      NF = 0
      J  = 0
  101 CONTINUE 
         J = J + 1
         IF (J .LE. 4) THEN
            NTRY = NTRYH(J)
         ELSE
            NTRY = NTRY+2
         ENDIF
  104    CONTINUE 
            NQ = NL/NTRY
            NR = NL-NTRY*NQ
            IF (NR .EQ. 0) THEN
               NF = NF+1
               IFAC(NF+2) = NTRY
               NL = NQ
               IF ( NL .GT. 1 ) GOTO 104
            ELSE
               GOTO 101
            ENDIF
*
      DO 105 I = 2, NF
         IF ( IFAC(I+2) .EQ. 2 ) THEN
            IFAC(I+2) = 4
            IFAC(3)   = 2
         ENDIF
  105 CONTINUE 
*
      IFAC(1) = N
      IFAC(2) = NF
      IF (NF .EQ. 1) RETURN
*
      L1 = 1
      IS = 0
      DO 110 K = 1, NF-1
         IP = IFAC(K+2)
         IDO = N/(L1*IP)
         DO 109 J = 1, IP-1
            ARGLD = (J*L1)*(TPI/N)
*VOPTION NOFVAL
            DO 108 IFI = 1, (IDO-1)/2
               ARG = IFI*ARGLD
               WA(2*IFI+IS-1) = COS(ARG)
               WA(2*IFI+IS  ) = SIN(ARG)
  108       CONTINUE
            IS = IS+IDO
  109    CONTINUE
         L1 = L1*IP
  110 CONTINUE
*
      RETURN
      END
C***********************************************************************
      SUBROUTINE RFFTBM    !! FFT
     &          (N,INC,JUMP,LOT, R, WA,IFAC, WSAVE)
*
      INTEGER    N
      INTEGER    INC
      INTEGER    JUMP
      INTEGER    LOT
      REAL*8     R(*)
      REAL*8     WA(N)
      INTEGER    IFAC(*)
      REAL*8     WSAVE(*)
C
      IF (N .EQ. 1) RETURN
C
      IF (JUMP .NE. 1) THEN
         CALL RFTB2M (N,INC,JUMP,LOT, R, WA,IFAC, WSAVE)
      ELSE
         CALL RFTB1M (N,INC,     LOT, R, WA,IFAC, WSAVE)
      ENDIF
      RETURN
      END
C***********************************************************************
      SUBROUTINE RFTB2M    !! FFT
     &          (N,INC,JUMP,LOT, R, WA,IFAC, WSAVE)
*
      INTEGER    N
      INTEGER    INC
      INTEGER    JUMP
      INTEGER    LOT
      REAL*8     R(*)
      REAL*8     WA(N)
      INTEGER    IFAC(*)
      REAL*8     WSAVE(*)
*
      INTEGER    INCN, N4, IABASE, JABASE, K
      INTEGER    IA, IB, IBBASE, IC, ICBASE, ID, IDBASE
      INTEGER    JA, JB, JBBASE, JC, JCBASE, JD, JDBASE, L
      INTEGER    INQ, INQN
C
      IF (JUMP .GT. INC) THEN
         INCN = (LOT*JUMP)/N
      ELSE
         INCN = INC
      ENDIF
      IF(MOD(INCN,16) .EQ. 0) INCN = INCN-1
      INCN = MAX(INCN,LOT)
C
      N4 = (N/4)*4
      IF (N4 .GE. 4) THEN
         IABASE = 1
         IBBASE = 1+INC
         ICBASE = 1+INC+INC
         IDBASE = 1+INC+INC+INC
         JABASE = 1
         JBBASE = 1+INCN
         JCBASE = 1+INCN+INCN
         JDBASE = 1+INCN+INCN+INCN
         INQ  = 4*INC
         INQN = 4*INCN
*VOPTION NOFVAL
         DO 102 K=1,N4,4
            IA = IABASE
            IB = IBBASE
            IC = ICBASE
            ID = IDBASE
            JA = JABASE
            JB = JBBASE
            JC = JCBASE
            JD = JDBASE
*VOPTION VEC,NOFVAL
            DO 101 L=1,LOT
               WSAVE(JA) = R(IA)
               WSAVE(JB) = R(IB)
               WSAVE(JC) = R(IC)
               WSAVE(JD) = R(ID)
               IA = IA+JUMP
               IB = IB+JUMP
               IC = IC+JUMP
               ID = ID+JUMP
               JA = JA+1
               JB = JB+1
               JC = JC+1
               JD = JD+1
  101       CONTINUE
            IABASE = IABASE+INQ
            IBBASE = IBBASE+INQ
            ICBASE = ICBASE+INQ
            IDBASE = IDBASE+INQ
            JABASE = JABASE+INQN
            JBBASE = JBBASE+INQN
            JCBASE = JCBASE+INQN
            JDBASE = JDBASE+INQN
  102    CONTINUE
      ENDIF
      IF (N4 .NE. N) THEN
         IABASE = 1+N4*INC
         JABASE = 1+N4*INCN
*VOPTION NOFVAL
         DO 104 K=N4+1,N
            IA = IABASE
            JA = JABASE
*VOPTION VEC,NOFVAL
            DO 103 L=1,LOT
               WSAVE(JA) = R(IA)
               IA = IA+JUMP
               JA = JA+1
  103       CONTINUE
            IABASE = IABASE+INC
            JABASE = JABASE+INCN
  104    CONTINUE
      ENDIF
C
      CALL RFTB1M (N,INCN,LOT, WSAVE, WA,IFAC, R)
C
      IF (N4 .GE. 4) THEN
         IABASE = 1
         IBBASE = 1+INC
         ICBASE = 1+INC+INC
         IDBASE = 1+INC+INC+INC
         JABASE = 1
         JBBASE = 1+INCN
         JCBASE = 1+INCN+INCN
         JDBASE = 1+INCN+INCN+INCN
         INQ  = 4*INC
         INQN = 4*INCN
*VOPTION NOFVAL
         DO 112 K=1,N4,4
            IA = IABASE
            IB = IBBASE
            IC = ICBASE
            ID = IDBASE
            JA = JABASE
            JB = JBBASE
            JC = JCBASE
            JD = JDBASE
*VOPTION VEC,NOFVAL
            DO 111 L=1,LOT
               R(IA) = WSAVE(JA)
               R(IB) = WSAVE(JB)
               R(IC) = WSAVE(JC)
               R(ID) = WSAVE(JD)
               IA = IA+JUMP
               IB = IB+JUMP
               IC = IC+JUMP
               ID = ID+JUMP
               JA = JA+1
               JB = JB+1
               JC = JC+1
               JD = JD+1
  111       CONTINUE
            IABASE = IABASE+INQ
            IBBASE = IBBASE+INQ
            ICBASE = ICBASE+INQ
            IDBASE = IDBASE+INQ
            JABASE = JABASE+INQN
            JBBASE = JBBASE+INQN
            JCBASE = JCBASE+INQN
            JDBASE = JDBASE+INQN
  112    CONTINUE
      ENDIF
      IF (N4 .NE. N) THEN
         IABASE = 1+N4*INC
         JABASE = 1+N4*INCN
*VOPTION NOFVAL
         DO 114 K=N4+1,N
            IA = IABASE
            JA = JABASE
*VOPTION VEC,NOFVAL
            DO 113 L=1,LOT
               R(IA) = WSAVE(JA)
               IA = IA+JUMP
               JA = JA+1
  113       CONTINUE
            IABASE = IABASE+INC
            JABASE = JABASE+INCN
  114    CONTINUE
      ENDIF
*
      RETURN
      END
C***********************************************************************
      SUBROUTINE RFTB1M    !! FFT
     &          (N,INC,LOT, C, WA,IFAC, CH)
*
      INTEGER     N
      INTEGER     INC
      INTEGER     LOT
      REAL*8      C(INC,N)
      REAL*8      WA(N)
      INTEGER     IFAC(*)
      REAL*8      CH(INC,N)
*
      INTEGER     NA, N4, K, L
*
      CALL RFTB9M (N,INC,LOT, NA,C, WA, IFAC, CH)
      IF (NA .EQ. 1) RETURN
      N4 = (N/4)*4
      IF (N4 .GE. 4) THEN
         DO 117 K=1,N4,4
            DO 117 L=1,LOT
               C(L,K  ) = CH(L,K  )
               C(L,K+1) = CH(L,K+1)
               C(L,K+2) = CH(L,K+2)
               C(L,K+3) = CH(L,K+3)
  117    CONTINUE
      ENDIF
      IF (N4 .NE. N) THEN
         DO 118 K=N4+1,N
            DO 118 L=1,LOT
               C(L,K) = CH(L,K)
  118    CONTINUE
      ENDIF
*
      RETURN
      END
C***********************************************************************
      SUBROUTINE RFTB9M    !! FFT
     &          (N,INC,LOT, NA,C, WA,IFAC, CH)
*
      INTEGER     N
      INTEGER     INC
      INTEGER     LOT
      INTEGER     NA
      REAL*8      C(INC,N)
      REAL*8      WA(N)
      INTEGER     IFAC(*)
      REAL*8      CH(INC,N)
*
      INTEGER     NF, L1, IW, K1, IP, IX3, IX2, IDO, IX4, IDL1, L2
*
      NF = IFAC(2)
      NA = 1
      L1 = 1
      IW = 1
      DO 116 K1=1,NF
         IP = IFAC(K1+2)
         L2 = IP*L1
         IDO = N/L2
         IDL1 = IDO*L1
         NA = 1-NA
         IF (IP .EQ. 4) THEN
            IX2 = IW +IDO
            IX3 = IX2+IDO
            IF (NA .EQ. 0) THEN
               CALL RADB4M (INC,LOT,IDO,L1,C,CH,WA(IW),WA(IX2),WA(IX3))
            ELSE
               CALL RADB4M (INC,LOT,IDO,L1,CH,C,WA(IW),WA(IX2),WA(IX3))
            ENDIF
         ELSE IF (IP .EQ. 2) THEN
            IF (NA .EQ. 0) THEN
               CALL RADB2M (INC,LOT,IDO,L1,C,CH,WA(IW))
            ELSE
               CALL RADB2M (INC,LOT,IDO,L1,CH,C,WA(IW))
            ENDIF
         ELSE IF (IP .EQ. 3) THEN
            IX2 = IW +IDO
            IF (NA .EQ. 0) THEN
               CALL RADB3M (INC,LOT,IDO,L1,C,CH,WA(IW),WA(IX2))
            ELSE
               CALL RADB3M (INC,LOT,IDO,L1,CH,C,WA(IW),WA(IX2))
            ENDIF
         ELSE IF (IP .EQ. 5) THEN
            IX2 = IW +IDO
            IX3 = IX2+IDO
            IX4 = IX3+IDO
            IF (NA .EQ. 0) THEN
               CALL RADB5M (INC,LOT,IDO,L1,C,CH
     &                     ,WA(IW),WA(IX2),WA(IX3),WA(IX4))
            ELSE
               CALL RADB5M (INC,LOT,IDO,L1,CH,C
     &                     ,WA(IW),WA(IX2),WA(IX3),WA(IX4))
            ENDIF
         ELSE
            IF (NA .EQ. 0) THEN
               CALL RADBGM (INC,LOT,IDO,IP,L1,IDL1,C,C,C,CH,CH,WA(IW))
               NA = 1
            ELSE
               CALL RADBGM (INC,LOT,IDO,IP,L1,IDL1,CH,CH,CH,C,C,WA(IW))
               NA = 0
            ENDIF
            IF (IDO .EQ. 1) NA = 1-NA
         ENDIF
         L1 = L2
         IW = IW+(IP-1)*IDO
  116 CONTINUE
*
      RETURN
      END
C***********************************************************************
      SUBROUTINE RFFTFM    !! FFT
     &          (N,INC,JUMP,LOT, R, WA,IFAC, WSAVE)
*
      INTEGER     N
      INTEGER     INC
      INTEGER     JUMP
      INTEGER     LOT
      REAL*8      R(*)
      REAL*8      WA(N)
      INTEGER     IFAC(*)
      REAL*8      WSAVE(*)
*
C
      IF (N .EQ. 1) RETURN
C
      IF (JUMP .NE. 1) THEN
         CALL RFTF2M (N,INC,JUMP,LOT, R, WA,IFAC, WSAVE)
      ELSE
         CALL RFTF3M (N,INC,     LOT, R, WA,IFAC, WSAVE)
      ENDIF
*
      RETURN
      END
C***********************************************************************
      SUBROUTINE RFTF2M    !! FFT
     &          (N,INC,JUMP,LOT, R, WA,IFAC, WSAVE)
*
      INTEGER     N
      INTEGER     INC
      INTEGER     JUMP
      INTEGER     LOT
      REAL*8      R(*)
      REAL*8      WA(N)
      INTEGER     IFAC(*)
      REAL*8      WSAVE(*)
*
      INTEGER     INCN, N4, IABASE, JABASE, K
      INTEGER     IA, IB, IBBASE, IC, ICBASE, ID, IDBASE
      INTEGER     JA, JB, JBBASE, JC, JCBASE, JD, JDBASE, L
      REAL*8      CF
      INTEGER     INQ, INQN
C
      IF (JUMP .GT. INC) THEN
         INCN = (LOT*JUMP)/N
      ELSE
         INCN = INC
      ENDIF
      IF(MOD(INCN,16) .EQ. 0) INCN = INCN-1
      INCN = MAX(INCN,LOT)
C
      N4 = (N/4)*4
      IF (N4 .GE. 4) THEN
         IABASE = 1
         IBBASE = 1+INC
         ICBASE = 1+INC+INC
         IDBASE = 1+INC+INC+INC
         JABASE = 1
         JBBASE = 1+INCN
         JCBASE = 1+INCN+INCN
         JDBASE = 1+INCN+INCN+INCN
         INQ  = 4*INC
         INQN = 4*INCN
*VOPTIONNOFVAL
         DO 102 K=1,N4,4
            IA = IABASE
            IB = IBBASE
            IC = ICBASE
            ID = IDBASE
            JA = JABASE
            JB = JBBASE
            JC = JCBASE
            JD = JDBASE
*VOPTION VEC,NOFVAL
            DO 101 L=1,LOT
               WSAVE(JA) = R(IA)
               WSAVE(JB) = R(IB)
               WSAVE(JC) = R(IC)
               WSAVE(JD) = R(ID)
               IA = IA+JUMP
               IB = IB+JUMP
               IC = IC+JUMP
               ID = ID+JUMP
               JA = JA+1
               JB = JB+1
               JC = JC+1
               JD = JD+1
  101       CONTINUE
            IABASE = IABASE+INQ
            IBBASE = IBBASE+INQ
            ICBASE = ICBASE+INQ
            IDBASE = IDBASE+INQ
            JABASE = JABASE+INQN
            JBBASE = JBBASE+INQN
            JCBASE = JCBASE+INQN
            JDBASE = JDBASE+INQN
  102    CONTINUE
      ENDIF
      IF (N4 .NE. N) THEN
         IABASE = 1+N4*INC
         JABASE = 1+N4*INCN
*VOPTION NOFVAL
         DO 104 K=N4+1,N
            IA = IABASE
            JA = JABASE
*VOPTION VEC,NOFVAL
            DO 103 L=1,LOT
               WSAVE(JA) = R(IA)
               IA = IA+JUMP
               JA = JA+1
  103       CONTINUE
            IABASE = IABASE+INC
            JABASE = JABASE+INCN
  104    CONTINUE
      ENDIF
C
      CALL RFTF1M (N,INCN,LOT, WSAVE, WA,IFAC, R)
C
      CF = 1.D0/DBLE(N)
C
      IF (N4 .GE. 4) THEN
         IABASE = 1
         IBBASE = 1+INC
         ICBASE = 1+INC+INC
         IDBASE = 1+INC+INC+INC
         JABASE = 1
         JBBASE = 1+INCN
         JCBASE = 1+INCN+INCN
         JDBASE = 1+INCN+INCN+INCN
         INQ  = 4*INC
         INQN = 4*INCN
*VOPTION NOFVAL
         DO 112 K=1,N4,4
            IA = IABASE
            IB = IBBASE
            IC = ICBASE
            ID = IDBASE
            JA = JABASE
            JB = JBBASE
            JC = JCBASE
            JD = JDBASE
*VOPTION VEC,NOFVAL
            DO 111 L=1,LOT
               R(IA) = CF*WSAVE(JA)
               R(IB) = CF*WSAVE(JB)
               R(IC) = CF*WSAVE(JC)
               R(ID) = CF*WSAVE(JD)
               IA = IA+JUMP
               IB = IB+JUMP
               IC = IC+JUMP
               ID = ID+JUMP
               JA = JA+1
               JB = JB+1
               JC = JC+1
               JD = JD+1
  111       CONTINUE
            IABASE = IABASE+INQ
            IBBASE = IBBASE+INQ
            ICBASE = ICBASE+INQ
            IDBASE = IDBASE+INQ
            JABASE = JABASE+INQN
            JBBASE = JBBASE+INQN
            JCBASE = JCBASE+INQN
            JDBASE = JDBASE+INQN
  112    CONTINUE
      ENDIF
      IF (N4 .NE. N) THEN
         IABASE = 1+N4*INC
         JABASE = 1+N4*INCN
*VOPTION NOFVAL
         DO 114 K=N4+1,N
            IA = IABASE
            JA = JABASE
*VOPTION VEC,NOFVAL
            DO 113 L=1,LOT
               R(IA) = CF*WSAVE(JA)
               IA = IA+JUMP
               JA = JA+1
  113       CONTINUE
            IABASE = IABASE+INC
            JABASE = JABASE+INCN
  114    CONTINUE
      ENDIF
*
      RETURN
      END
C***********************************************************************
      SUBROUTINE RFTF3M    !! FFT
     &          (N,INC,LOT, C, WA,IFAC, CH)
*
      INTEGER     N
      INTEGER     INC
      INTEGER     LOT
      REAL*8      C(INC,N)
      REAL*8      WA(N)
      INTEGER     IFAC(*)
      REAL*8      CH(INC,N)
*
      INTEGER     NA, N4, K, L
      REAL*8      CF
*
      CALL RFTF9M (N,INC,LOT, NA,C, WA, IFAC, CH)
C
      CF = 1.D0/DBLE(N)
      N4=(N/4)*4
      IF (NA .EQ. 1) THEN
         IF (N4 .GE. 4) THEN
            DO 117 K=1,N4,4
               DO 117 L=1,LOT
                  C(L,K  ) = CF*C(L,K  )
                  C(L,K+1) = CF*C(L,K+1)
                  C(L,K+2) = CF*C(L,K+2)
                  C(L,K+3) = CF*C(L,K+3)
  117       CONTINUE
         ENDIF
         IF (N4 .NE. N) THEN
            DO 118 K=N4+1,N
               DO 118 L=1,LOT
                  C(L,K) = CF*C(L,K)
  118       CONTINUE
         ENDIF
      ELSE
         IF (N4 .GE. 4) THEN
            DO 119 K=1,N4,4
               DO 119 L=1,LOT
                  C(L,K  ) = CF*CH(L,K  )
                  C(L,K+1) = CF*CH(L,K+1)
                  C(L,K+2) = CF*CH(L,K+2)
                  C(L,K+3) = CF*CH(L,K+3)
  119       CONTINUE
         ENDIF
         IF (N4 .NE. N) THEN
            DO 120 K=N4+1,N
               DO 120 L=1,LOT
                  C(L,K) = CF*CH(L,K)
  120       CONTINUE
         ENDIF
      ENDIF
*
      RETURN
      END
C***********************************************************************
      SUBROUTINE RFTF1M    !! FFT
     &          (N,INC,LOT, C, WA,IFAC, CH)
*
      INTEGER     N
      INTEGER     INC
      INTEGER     LOT
      REAL*8      C(INC,N)
      REAL*8      WA(N)
      INTEGER     IFAC(*)
      REAL*8      CH(INC,N)
*
      INTEGER     NA, N4, K, L
*
      CALL RFTF9M (N,INC,LOT, NA,C, WA, IFAC, CH)
      IF (NA .EQ. 1) RETURN
      N4=(N/4)*4
      IF (N4 .GE. 4) THEN
         DO 117 K=1,N4,4
            DO 117 L=1,LOT
               C(L,K  ) = CH(L,K  )
               C(L,K+1) = CH(L,K+1)
               C(L,K+2) = CH(L,K+2)
               C(L,K+3) = CH(L,K+3)
  117    CONTINUE
      ENDIF
      IF (N4 .NE. N) THEN
         DO 118 K=N4+1,N
            DO 118 L=1,LOT
               C(L,K) = CH(L,K)
  118    CONTINUE
      ENDIF
*
      RETURN
      END
C***********************************************************************
      SUBROUTINE RFTF9M    !! FFT
     &          (N,INC,LOT, NA,C, WA,IFAC, CH)
*
      INTEGER     N
      INTEGER     INC
      INTEGER     LOT
      INTEGER     NA
      REAL*8      C(INC,N)
      REAL*8      WA(N)
      INTEGER     IFAC(*)
      REAL*8      CH(INC,N)
*
      INTEGER     NF, L2, IW, K1
      INTEGER     IP, KH, IDL1, IDO, L1, IX2, IX3, IX4
*
      NF = IFAC(2)
      NA = 1
      L2 = N
      IW = N
      DO 111 K1=1,NF
         KH = NF-K1
         IP = IFAC(KH+3)
         L1 = L2/IP
         IDO = N/L2
         IDL1 = IDO*L1
         IW = IW-(IP-1)*IDO
         NA = 1-NA
         IF (IP .EQ. 4) THEN
            IX2 = IW +IDO
            IX3 = IX2+IDO
            IF (NA .EQ. 0) THEN
               CALL RADF4M (INC,LOT,IDO,L1,C,CH,WA(IW),WA(IX2),WA(IX3))
            ELSE
               CALL RADF4M (INC,LOT,IDO,L1,CH,C,WA(IW),WA(IX2),WA(IX3))
            ENDIF
         ELSE IF (IP .EQ. 2) THEN
            IF (NA .EQ. 0) THEN
               CALL RADF2M (INC,LOT,IDO,L1,C,CH,WA(IW))
            ELSE
               CALL RADF2M (INC,LOT,IDO,L1,CH,C,WA(IW))
            ENDIF
         ELSE IF (IP .EQ. 3) THEN
            IX2 = IW +IDO
            IF (NA .EQ. 0) THEN
               CALL RADF3M (INC,LOT,IDO,L1,C,CH,WA(IW),WA(IX2))
            ELSE
               CALL RADF3M (INC,LOT,IDO,L1,CH,C,WA(IW),WA(IX2))
            ENDIF
         ELSE IF (IP .EQ. 5) THEN
            IX2 = IW +IDO
            IX3 = IX2+IDO
            IX4 = IX3+IDO
            IF (NA .EQ. 0) THEN
               CALL RADF5M (INC,LOT,IDO,L1,C,CH
     &                      ,WA(IW),WA(IX2),WA(IX3),WA(IX4))
            ELSE
               CALL RADF5M (INC,LOT,IDO,L1,CH,C
     &                      ,WA(IW),WA(IX2),WA(IX3),WA(IX4))
            ENDIF
         ELSE
            IF (IDO .EQ. 1) NA = 1-NA
            IF (NA .EQ. 0) THEN
               CALL RADFGM (INC,LOT,IDO,IP,L1,IDL1,C,C,C,CH,CH,WA(IW))
               NA = 1
            ELSE
               CALL RADFGM (INC,LOT,IDO,IP,L1,IDL1,CH,CH,CH,C,C,WA(IW))
               NA = 0
            ENDIF
         ENDIF
         L2 = L1
  111 CONTINUE
*
      RETURN
      END
C***********************************************************************
      SUBROUTINE RADB2M    !! FFT
     &          (INC,LOT,IDO,L1,CC,CH,WA1)
*
      INTEGER     INC
      INTEGER     LOT
      INTEGER     IDO
      INTEGER     L1
      REAL*8      CC(INC,IDO,2,L1)
      REAL*8      CH(INC,IDO,L1,2)
      REAL*8      WA1(*)
*
      INTEGER     IDP2, IC
      REAL*8      TR2, TI2
      INTEGER     K, L, I
C
      DO 101 K=1,L1
         DO 101 L=1,LOT
            CH(L,1,K,1) = CC(L,1,1,K)+CC(L,IDO,2,K)
            CH(L,1,K,2) = CC(L,1,1,K)-CC(L,IDO,2,K)
  101 CONTINUE
      IF (MOD(IDO,2) .EQ. 0) THEN
         DO 102 K=1,L1
            DO 102 L=1,LOT
               CH(L,IDO,K,1) =   CC(L,IDO,1,K)+CC(L,IDO,1,K)
               CH(L,IDO,K,2) = -(CC(L,  1,2,K)+CC(L,  1,2,K))
  102    CONTINUE
      END IF
      IF (IDO .GT. 2) THEN
         IDP2 = IDO+2
         DO 104 K=1,L1
*VOPTION NOFVAL
            DO 104 I=3,IDO,2
               IC = IDP2-I
*VOPTION NOFVAL
               DO 103 L=1,LOT
                  CH(L,I-1,K,1) = CC(L,I-1,1,K)+CC(L,IC-1,2,K)
                  CH(L,I  ,K,1) = CC(L,I  ,1,K)-CC(L,IC  ,2,K)
                  TR2           = CC(L,I-1,1,K)-CC(L,IC-1,2,K)
                  TI2           = CC(L,I  ,1,K)+CC(L,IC  ,2,K)
                  CH(L,I-1,K,2) = WA1(I-2)*TR2 -WA1(I-1)*TI2
                  CH(L,I  ,K,2) = WA1(I-2)*TI2 +WA1(I-1)*TR2
  103          CONTINUE
  104    CONTINUE
      END IF
*
      RETURN
      END
C***********************************************************************
      SUBROUTINE RADB3M    !! FFT
     &          (INC,LOT,IDO,L1,CC,CH,WA1,WA2)
*
      INTEGER     INC
      INTEGER     LOT
      INTEGER     IDO
      INTEGER     L1
      REAL*8      CC(INC,IDO,3,L1)
      REAL*8      CH(INC,IDO,L1,3)
      REAL*8      WA1(*)
      REAL*8      WA2(*)
*
      REAL*8      TAUR, TAUI
      REAL*8      TR2, TI2
      REAL*8      CR2, CI2
      REAL*8      CR3, CI3
      REAL*8      DR2, DI2
      REAL*8      DR3, DI3
      INTEGER     IDP2, IC
      INTEGER     K, L, I
*
C     DATA TAUR,TAUI / -.5D0,.866025403784439D0  /
      TAUR = - 1/2.D0
      TAUI = SQRT(3.D0)/2.D0
C
      DO 101 K=1,L1
*VOPTION NOFVAL
         DO 101 L=1,LOT
            TR2         = CC(L,IDO,2,K)+CC(L,IDO,2,K)
            CR2         = CC(L,  1,1,K)+TAUR*TR2
            CI3         = TAUI*(CC(L,1,3,K)+CC(L,1,3,K))
            CH(L,1,K,1) = CC(L,  1,1,K)+TR2
            CH(L,1,K,2) = CR2-CI3
            CH(L,1,K,3) = CR2+CI3
  101 CONTINUE
      IF (IDO .GT. 1) THEN
         IDP2 = IDO+2
         DO 103 K=1,L1
*VOPTION NOFVAL
            DO 103 I=3,IDO,2
               IC = IDP2-I
*VOPTION NOFVAL
               DO 102 L=1,LOT
                  TR2           = CC(L,I-1,3,K)+CC(L,IC-1,2,K)
                  TI2           = CC(L,I  ,3,K)-CC(L,IC  ,2,K)
                  CR2           = CC(L,I-1,1,K)+TAUR*TR2
                  CI2           = CC(L,I  ,1,K)+TAUR*TI2
                  CR3           = TAUI*(CC(L,I-1,3,K)-CC(L,IC-1,2,K))
                  CI3           = TAUI*(CC(L,I  ,3,K)+CC(L,IC  ,2,K))
                  CH(L,I-1,K,1) = CC(L,I-1,1,K)+TR2
                  CH(L,I  ,K,1) = CC(L,I  ,1,K)+TI2
                  DR2           = CR2-CI3
                  DI2           = CI2+CR3
                  DR3           = CR2+CI3
                  DI3           = CI2-CR3
                  CH(L,I-1,K,2) = WA1(I-2)*DR2-WA1(I-1)*DI2
                  CH(L,I  ,K,2) = WA1(I-2)*DI2+WA1(I-1)*DR2
                  CH(L,I-1,K,3) = WA2(I-2)*DR3-WA2(I-1)*DI3
                  CH(L,I  ,K,3) = WA2(I-2)*DI3+WA2(I-1)*DR3
  102          CONTINUE
  103    CONTINUE
      END IF
*
      RETURN
      END
C***********************************************************************
      SUBROUTINE RADB4M    !! FFT
     &          (INC,LOT,IDO,L1,CC,CH,WA1,WA2,WA3)
*
      INTEGER     INC
      INTEGER     LOT
      INTEGER     IDO
      INTEGER     L1
      REAL*8      CC(INC,IDO,4,L1)
      REAL*8      CH(INC,IDO,L1,4)
      REAL*8      WA1(*)
      REAL*8      WA2(*)
      REAL*8      WA3(*)
*
      REAL*8      SQRT2
      REAL*8      TR1, TR2, TR3, TR4
      REAL*8      TI1, TI2, TI3, TI4
      REAL*8      CR2, CR3, CR4
      REAL*8      CI2, CI3, CI4
      INTEGER     IDP2, IC
      INTEGER     K, L, I
*
C     DATA SQRT2 / 1.414213562373095D0  /
      SQRT2 = SQRT(2.D0)
C
      DO 101 K=1,L1
*VOPTION NOFVAL
         DO 101 L=1,LOT
            TR1         = CC(L,  1,1,K)-CC(L,IDO,4,K)
            TR2         = CC(L,  1,1,K)+CC(L,IDO,4,K)
            TR3         = CC(L,IDO,2,K)+CC(L,IDO,2,K)
            TR4         = CC(L,  1,3,K)+CC(L,  1,3,K)
            CH(L,1,K,1) = TR2+TR3
            CH(L,1,K,2) = TR1-TR4
            CH(L,1,K,3) = TR2-TR3
            CH(L,1,K,4) = TR1+TR4
  101 CONTINUE
      IF (MOD(IDO,2) .EQ. 0) THEN
         DO 102 K=1,L1
*VOPTION NOFVAL
            DO 102 L=1,LOT
               TR1           = CC(L,IDO,1,K)-CC(L,IDO,3,K)
               TI1           = CC(L,  1,2,K)+CC(L,  1,4,K)
               TR2           = CC(L,IDO,1,K)+CC(L,IDO,3,K)
               TI2           = CC(L,  1,4,K)-CC(L,  1,2,K)
               CH(L,IDO,K,1) =         TR2+TR2
               CH(L,IDO,K,2) =  SQRT2*(TR1-TI1)
               CH(L,IDO,K,3) =         TI2+TI2
               CH(L,IDO,K,4) = -SQRT2*(TR1+TI1)
  102    CONTINUE
      END IF
      IF (IDO .GT. 2) THEN
         IDP2 = IDO+2
         DO 104 K=1,L1
*VOPTION NOFVAL
            DO 104 I=3,IDO,2
               IC = IDP2-I
*VOPTION NOFVAL
               DO 103 L=1,LOT
                  TR1           = CC(L,I-1,1,K)-CC(L,IC-1,4,K)
                  TI1           = CC(L,I  ,1,K)+CC(L,IC  ,4,K)
                  TR2           = CC(L,I-1,1,K)+CC(L,IC-1,4,K)
                  TI2           = CC(L,I  ,1,K)-CC(L,IC  ,4,K)
                  TR3           = CC(L,I-1,3,K)+CC(L,IC-1,2,K)
                  TI3           = CC(L,I  ,3,K)-CC(L,IC  ,2,K)
                  TI4           = CC(L,I-1,3,K)-CC(L,IC-1,2,K)
                  TR4           = CC(L,I  ,3,K)+CC(L,IC  ,2,K)
                  CH(L,I-1,K,1) = TR2+TR3
                  CH(L,I  ,K,1) = TI2+TI3
                  CR3           = TR2-TR3
                  CI3           = TI2-TI3
                  CR2           = TR1-TR4
                  CI2           = TI1+TI4
                  CR4           = TR1+TR4
                  CI4           = TI1-TI4
                  CH(L,I-1,K,2) = WA1(I-2)*CR2-WA1(I-1)*CI2
                  CH(L,I  ,K,2) = WA1(I-2)*CI2+WA1(I-1)*CR2
                  CH(L,I-1,K,3) = WA2(I-2)*CR3-WA2(I-1)*CI3
                  CH(L,I  ,K,3) = WA2(I-2)*CI3+WA2(I-1)*CR3
                  CH(L,I-1,K,4) = WA3(I-2)*CR4-WA3(I-1)*CI4
                  CH(L,I  ,K,4) = WA3(I-2)*CI4+WA3(I-1)*CR4
  103          CONTINUE
  104    CONTINUE
      END IF
*
      RETURN
      END
C***********************************************************************
      SUBROUTINE RADB5M    !! FFT
     &          (INC,LOT,IDO,L1,CC,CH,WA1,WA2,WA3,WA4)
*
      INTEGER     INC
      INTEGER     LOT
      INTEGER     IDO
      INTEGER     L1
      REAL*8      CC(INC,IDO,5,L1)
      REAL*8      CH(INC,IDO,L1,5)
      REAL*8      WA1(*)
      REAL*8      WA2(*)
      REAL*8      WA3(*)
      REAL*8      WA4(*)
*
      REAL*8      TR2, TR3, TR4, TR5
      REAL*8      TI2, TI3, TI4, TI5
      REAL*8      TR11, TI11, TR12, TI12
      REAL*8      CI2, CI3, CI4, CI5
      REAL*8      CR2, CR3, CR4, CR5
      REAL*8      DR2, DR3, DR4, DR5
      REAL*8      DI2, DI3, DI4, DI5
      INTEGER     IDP2, IC
      INTEGER     K, L, I
*
C     DATA TR11,TI11,TR12,TI12 /.309016994374947D0,.951056516295154D0,
C    1                         -.809016994374947D0,.587785252292473D0/
      TR11 = (-1.D0+SQRT(5.D0))/4.D0
      TI11 = SQRT(1.D0-TR11*TR11)
      TR12 = (-1.D0-SQRT(5.D0))/4.D0
      TI12 = SQRT(1.D0-TR12*TR12)
*
      DO 101 K=1,L1
*VOPTION NOFVAL
         DO 101 L=1,LOT
            TR2         = CC(L,IDO,2,K)+CC(L,IDO,2,K)
            TR3         = CC(L,IDO,4,K)+CC(L,IDO,4,K)
            TI5         = CC(L,  1,3,K)+CC(L,  1,3,K)
            TI4         = CC(L,  1,5,K)+CC(L,  1,5,K)
            CR2         = CC(L,  1,1,K)+TR11*TR2+TR12*TR3
            CI5         =               TI11*TI5+TI12*TI4
            CR3         = CC(L,  1,1,K)+TR12*TR2+TR11*TR3
            CI4         =               TI12*TI5-TI11*TI4
            CH(L,1,K,1) = CC(L,  1,1,K)+TR2+TR3
            CH(L,1,K,2) = CR2-CI5
            CH(L,1,K,3) = CR3-CI4
            CH(L,1,K,4) = CR3+CI4
            CH(L,1,K,5) = CR2+CI5
  101 CONTINUE
      IF (IDO .GT. 1) THEN
         IDP2 = IDO+2
         DO 103 K=1,L1
*VOPTION NOFVAL
            DO 103 I=3,IDO,2
               IC = IDP2-I
*VOPTION NOFVAL
               DO 102 L=1,LOT
                  TR2           = CC(L,I-1,3,K)+CC(L,IC-1,2,K)
                  TI2           = CC(L,I  ,3,K)-CC(L,IC  ,2,K)
                  TR3           = CC(L,I-1,5,K)+CC(L,IC-1,4,K)
                  TI3           = CC(L,I  ,5,K)-CC(L,IC  ,4,K)
                  TR4           = CC(L,I-1,5,K)-CC(L,IC-1,4,K)
                  TI4           = CC(L,I  ,5,K)+CC(L,IC  ,4,K)
                  TR5           = CC(L,I-1,3,K)-CC(L,IC-1,2,K)
                  TI5           = CC(L,I  ,3,K)+CC(L,IC  ,2,K)
                  CR2           = CC(L,I-1,1,K)+TR11*TR2+TR12*TR3
                  CI2           = CC(L,I  ,1,K)+TR11*TI2+TR12*TI3
                  CR4           =               TI12*TR5-TI11*TR4
                  CI4           =               TI12*TI5-TI11*TI4
                  CR3           = CC(L,I-1,1,K)+TR12*TR2+TR11*TR3
                  CI3           = CC(L,I  ,1,K)+TR12*TI2+TR11*TI3
                  CR5           =               TI11*TR5+TI12*TR4
                  CI5           =               TI11*TI5+TI12*TI4
                  CH(L,I-1,K,1) = CC(L,I-1,1,K)+     TR2+     TR3
                  CH(L,I  ,K,1) = CC(L,I  ,1,K)+     TI2+     TI3
                  DR2           = CR2-CI5
                  DI2           = CI2+CR5
                  DR3           = CR3-CI4
                  DI3           = CI3+CR4
                  DR4           = CR3+CI4
                  DI4           = CI3-CR4
                  DR5           = CR2+CI5
                  DI5           = CI2-CR5
                  CH(L,I-1,K,2) = WA1(I-2)*DR2-WA1(I-1)*DI2
                  CH(L,I  ,K,2) = WA1(I-2)*DI2+WA1(I-1)*DR2
                  CH(L,I-1,K,3) = WA2(I-2)*DR3-WA2(I-1)*DI3
                  CH(L,I  ,K,3) = WA2(I-2)*DI3+WA2(I-1)*DR3
                  CH(L,I-1,K,4) = WA3(I-2)*DR4-WA3(I-1)*DI4
                  CH(L,I  ,K,4) = WA3(I-2)*DI4+WA3(I-1)*DR4
                  CH(L,I-1,K,5) = WA4(I-2)*DR5-WA4(I-1)*DI5
                  CH(L,I  ,K,5) = WA4(I-2)*DI5+WA4(I-1)*DR5
  102          CONTINUE
  103    CONTINUE
      END IF
*
      RETURN
      END
C***********************************************************************
      SUBROUTINE RADF2M    !! FFT
     &          (INC,LOT,IDO,L1,CC,CH,WA1)
*
      INTEGER     INC
      INTEGER     LOT
      INTEGER     IDO
      INTEGER     L1
      REAL*8      CC(INC,IDO,L1,2)
      REAL*8      CH(INC,IDO,2,L1)
      REAL*8      WA1(*)
*
      INTEGER     IDP2, IC
      REAL*8      TR2, TI2
      INTEGER     K, L, I
*
      DO 101 K=1,L1
         DO 101 L=1,LOT
            CH(L,  1,1,K) = CC(L,1,K,1)+CC(L,1,K,2)
            CH(L,IDO,2,K) = CC(L,1,K,1)-CC(L,1,K,2)
  101 CONTINUE
      IF (MOD(IDO,2) .EQ. 0) THEN
         DO 102 K=1,L1
            DO 102 L=1,LOT
               CH(L,  1,2,K) = -CC(L,IDO,K,2)
               CH(L,IDO,1,K) =  CC(L,IDO,K,1)
  102    CONTINUE
      END IF
      IF (IDO .GT. 2) THEN
         IDP2 = IDO+2
         DO 104 K=1,L1
*VOPTION NOFVAL
            DO 104 I=3,IDO,2
               IC = IDP2-I
*VOPTION NOFVAL
               DO 103 L=1,LOT
                  TR2 = WA1(I-2)*CC(L,I-1,K,2)+WA1(I-1)*CC(L,I  ,K,2)
                  TI2 = WA1(I-2)*CC(L,I  ,K,2)-WA1(I-1)*CC(L,I-1,K,2)
                  CH(L,I -1,1,K) = CC(L,I-1,K,1)+TR2
                  CH(L,I   ,1,K) = TI2          +CC(L,I,K,1)
                  CH(L,IC-1,2,K) = CC(L,I-1,K,1)-TR2
                  CH(L,IC  ,2,K) = TI2          -CC(L,I,K,1)
  103          CONTINUE
  104    CONTINUE
      END IF
*
      RETURN
      END
C***********************************************************************
      SUBROUTINE RADF3M    !! FFT
     &          (INC,LOT,IDO,L1,CC,CH,WA1,WA2)
*
      INTEGER     INC
      INTEGER     LOT
      INTEGER     IDO
      INTEGER     L1
      REAL*8      CC(INC,IDO,L1,3)
      REAL*8      CH(INC,IDO,3,L1)
      REAL*8      WA1(*)
      REAL*8      WA2(*)
*
      REAL*8      TAUR, TAUI
      REAL*8      CR2, DR2, DR3, CI2, DI2, DI3
      INTEGER     IDP2, IC
      REAL*8      TR2, TR3, TI3, TI2
      INTEGER     K, L, I
*
C     DATA TAUR,TAUI /-.5D0,.866025403784439D0  /
      TAUR = - 1/2.D0
      TAUI = SQRT(3.D0)/2.D0
*
      DO 101 K=1,L1
*VOPTION NOFVAL
         DO 101 L=1,LOT
            CR2           =       CC(L,1,K,2)+CC(L,1,K,3)
            CH(L,  1,1,K) =       CC(L,1,K,1)+     CR2
            CH(L,  1,3,K) = TAUI*(CC(L,1,K,3)-CC(L,1,K,2))
            CH(L,IDO,2,K) =       CC(L,1,K,1)+TAUR*CR2
  101 CONTINUE
      IF (IDO .GT. 1) THEN
         IDP2 = IDO+2
         DO 103 K=1,L1
*VOPTION NOFVAL
            DO 103 I=3,IDO,2
               IC = IDP2-I
*VOPTION NOFVAL
               DO 102 L=1,LOT
                  DR2 = WA1(I-2)*CC(L,I-1,K,2)+WA1(I-1)*CC(L,I  ,K,2)
                  DI2 = WA1(I-2)*CC(L,I  ,K,2)-WA1(I-1)*CC(L,I-1,K,2)
                  DR3 = WA2(I-2)*CC(L,I-1,K,3)+WA2(I-1)*CC(L,I  ,K,3)
                  DI3 = WA2(I-2)*CC(L,I  ,K,3)-WA2(I-1)*CC(L,I-1,K,3)
                  CR2            = DR2+DR3
                  CI2            = DI2+DI3
                  TR2            = CC(L,I-1,K,1)+TAUR*CR2
                  TI2            = CC(L,I  ,K,1)+TAUR*CI2
                  TR3            = TAUI*(DI2-DI3)
                  TI3            = TAUI*(DR3-DR2)
                  CH(L,I -1,1,K) = CC(L,I-1,K,1)+     CR2
                  CH(L,I   ,1,K) = CC(L,I  ,K,1)+     CI2
                  CH(L,IC-1,2,K) = TR2-TR3
                  CH(L,IC  ,2,K) = TI3-TI2
                  CH(L,I -1,3,K) = TR2+TR3
                  CH(L,I   ,3,K) = TI2+TI3
  102          CONTINUE
  103    CONTINUE
      END IF
      RETURN
      END
C***********************************************************************
      SUBROUTINE RADF4M    !! FFT
     &          (INC,LOT,IDO,L1,CC,CH,WA1,WA2,WA3)
*
      INTEGER     INC
      INTEGER     LOT
      INTEGER     IDO
      INTEGER     L1
      REAL*8      CC(INC,IDO,L1,4)
      REAL*8      CH(INC,IDO,4,L1)
      REAL*8      WA1(*)
      REAL*8      WA2(*)
      REAL*8      WA3(*)
*
      REAL*8      HSQT2, TR1, TR2, TI1, TI2
      REAL*8      CR2, CR3, CR4
      REAL*8      CI2, CI3, CI4
      INTEGER     IDP2, IC
      REAL*8      TR3, TI4, TR4, TI3
      INTEGER     K, L, I
*
C     DATA HSQT2 /.7071067811865475D0  /
      HSQT2 = SQRT(2.D0)/2.D0
*
      DO 101 K=1,L1
*VOPTION NOFVAL
         DO 101 L=1,LOT
            TR1           = CC(L,1,K,2)+CC(L,1,K,4)
            TR2           = CC(L,1,K,1)+CC(L,1,K,3)
            CH(L,  1,3,K) = CC(L,1,K,4)-CC(L,1,K,2)
            CH(L,  1,1,K) = TR1+TR2
            CH(L,IDO,2,K) = CC(L,1,K,1)-CC(L,1,K,3)
            CH(L,IDO,4,K) = TR2-TR1
  101 CONTINUE
      IF (MOD(IDO,2) .EQ. 0) THEN
         DO 102 K=1,L1
*VOPTION NOFVAL
            DO 102 L=1,LOT
               TR1           =  HSQT2*(CC(L,IDO,K,2)-CC(L,IDO,K,4))
               TI1           = -HSQT2*(CC(L,IDO,K,2)+CC(L,IDO,K,4))
               CH(L,IDO,1,K) = CC(L,IDO,K,1)+TR1
               CH(L,  1,2,K) = TI1          -CC(L,IDO,K,3)
               CH(L,IDO,3,K) = CC(L,IDO,K,1)-TR1
               CH(L,  1,4,K) = TI1          +CC(L,IDO,K,3)
  102    CONTINUE
      END IF
      IF (IDO .GT. 2) THEN
         IDP2 = IDO+2
         DO 104 K=1,L1
*VOPTION NOFVAL
            DO 104 I=3,IDO,2
               IC = IDP2-I
*VOPTION NOFVAL
               DO 103 L=1,LOT
                  CR2 = WA1(I-2)*CC(L,I-1,K,2)+WA1(I-1)*CC(L,I  ,K,2)
                  CI2 = WA1(I-2)*CC(L,I  ,K,2)-WA1(I-1)*CC(L,I-1,K,2)
                  CR3 = WA2(I-2)*CC(L,I-1,K,3)+WA2(I-1)*CC(L,I  ,K,3)
                  CI3 = WA2(I-2)*CC(L,I  ,K,3)-WA2(I-1)*CC(L,I-1,K,3)
                  CR4 = WA3(I-2)*CC(L,I-1,K,4)+WA3(I-1)*CC(L,I  ,K,4)
                  CI4 = WA3(I-2)*CC(L,I  ,K,4)-WA3(I-1)*CC(L,I-1,K,4)
                  TR2            = CC(L,I-1,K,1)+CR3
                  TI2            = CC(L,I  ,K,1)+CI3
                  TR1            = CR2+CR4
                  TI1            = CI2+CI4
                  TR3            = CC(L,I-1,K,1)-CR3
                  TI3            = CC(L,I  ,K,1)-CI3
                  TR4            = CR4-CR2
                  TI4            = CI2-CI4
                  CH(L,I -1,1,K) = TR1+TR2
                  CH(L,I   ,1,K) = TI1+TI2
                  CH(L,IC-1,2,K) = TR3-TI4
                  CH(L,IC  ,2,K) = TR4-TI3
                  CH(L,I -1,3,K) = TI4+TR3
                  CH(L,I   ,3,K) = TR4+TI3
                  CH(L,IC-1,4,K) = TR2-TR1
                  CH(L,IC  ,4,K) = TI1-TI2
  103          CONTINUE
  104    CONTINUE
      END IF
*
      RETURN
      END
C***********************************************************************
      SUBROUTINE RADF5M    !! FFT
     &          (INC,LOT,IDO,L1,CC,CH,WA1,WA2,WA3,WA4)
*
      INTEGER     INC
      INTEGER     LOT
      INTEGER     IDO
      INTEGER     L1
      REAL*8      CC(INC,IDO,L1,5)
      REAL*8      CH(INC,IDO,5,L1)
      REAL*8      WA1(*)
      REAL*8      WA2(*)
      REAL*8      WA3(*)
      REAL*8      WA4(*)
*
      REAL*8      TR11, TI11, TR12, TI12
      REAL*8      CR2, CR3, CI4, CI5
      REAL*8      CA2, CV2, CU2, CB2, CA3, CV3, CU3, CB3
      REAL*8      CA4, CV4, CU4, CB4, CA5, CV5, CU5, CB5
      REAL*8      TR2, TR5, TI5, TI2, TR3, TR4, TI4, TI3
      REAL*8      CTR2, CTI2, CTR3, CTI3, SR23, SI23
      INTEGER     IDP2, IC
      INTEGER     K, L, I
*
C     DATA TR11,TI11,TR12,TI12 /.309016994374947D0,.951056516295154D0,
C    1                         -.809016994374947D0,.587785252292473D0/
      TR11 = (-1.D0+SQRT(5.D0))/4.D0
      TI11 = SQRT(1.D0-TR11*TR11)
      TR12 = (-1.D0-SQRT(5.D0))/4.D0
      TI12 = SQRT(1.D0-TR12*TR12)
*
      DO 101 K=1,L1
*VOPTION NOFVAL
         DO 101 L=1,LOT
            CR2           = CC(L,1,K,5)+CC(L,1,K,2)
            CR3           = CC(L,1,K,4)+CC(L,1,K,3)
            CI5           = CC(L,1,K,5)-CC(L,1,K,2)
            CI4           = CC(L,1,K,4)-CC(L,1,K,3)
            CH(L,  1,1,K) = CC(L,1,K,1)+     CR2+     CR3
            CH(L,  1,3,K) =             TI11*CI5+TI12*CI4
            CH(L,IDO,2,K) = CC(L,1,K,1)+TR11*CR2+TR12*CR3
            CH(L,  1,5,K) =             TI12*CI5-TI11*CI4
            CH(L,IDO,4,K) = CC(L,1,K,1)+TR12*CR2+TR11*CR3
  101 CONTINUE
      IF (IDO .GT. 1) THEN
         IDP2 = IDO+2
         DO 103 K=1,L1
*VOPTION NOFVAL
            DO 103 I=3,IDO,2
               IC = IDP2-I
*VOPTION NOFVAL
               DO 102 L=1,LOT
                  CA2            = WA1(I-2)*CC(L,I-1,K,2)
                  CU2            = WA1(I-2)*CC(L,I  ,K,2)
                  CA3            = WA2(I-2)*CC(L,I-1,K,3)
                  CU3            = WA2(I-2)*CC(L,I  ,K,3)
                  CA4            = WA3(I-2)*CC(L,I-1,K,4)
                  CU4            = WA3(I-2)*CC(L,I  ,K,4)
                  CA5            = WA4(I-2)*CC(L,I-1,K,5)
                  CU5            = WA4(I-2)*CC(L,I  ,K,5)
                  CB2            = WA1(I-1)*CC(L,I-1,K,2)
                  CV2            = WA1(I-1)*CC(L,I  ,K,2)
                  CB3            = WA2(I-1)*CC(L,I-1,K,3)
                  CV3            = WA2(I-1)*CC(L,I  ,K,3)
                  CB4            = WA3(I-1)*CC(L,I-1,K,4)
                  CV4            = WA3(I-1)*CC(L,I  ,K,4)
                  CB5            = WA4(I-1)*CC(L,I-1,K,5)
                  CV5            = WA4(I-1)*CC(L,I  ,K,5)
                  CH(L,IC-1,2,K) = CA2+CV2
                  CH(L,IC  ,2,K) = CU2-CB2
                  CH(L,I -1,3,K) = CA3+CV3
                  CH(L,I   ,3,K) = CU3-CB3
                  CH(L,IC-1,4,K) = CA4+CV4
                  CH(L,IC  ,4,K) = CU4-CB4
                  CH(L,I -1,5,K) = CA5+CV5
                  CH(L,I   ,5,K) = CU5-CB5
  102          CONTINUE
  103    CONTINUE
         DO 105 K=1,L1
*VOPTION NOFVAL
            DO 105 I=3,IDO,2
               IC = IDP2-I
               DO 104 L=1,LOT
                  CC(L,I-1,K,2) = CH(L,IC-1,2,K)+CH(L,I -1,5,K)
                  CC(L,I  ,K,2) = CH(L,IC  ,2,K)+CH(L,I   ,5,K)
                  CC(L,I-1,K,3) = CH(L,I -1,3,K)+CH(L,IC-1,4,K)
                  CC(L,I  ,K,3) = CH(L,I   ,3,K)+CH(L,IC  ,4,K)
                  CC(L,I-1,K,4) = CH(L,I   ,3,K)-CH(L,IC  ,4,K)
                  CC(L,I  ,K,4) = CH(L,IC-1,4,K)-CH(L,I -1,3,K)
                  CC(L,I-1,K,5) = CH(L,IC  ,2,K)-CH(L,I   ,5,K)
                  CC(L,I  ,K,5) = CH(L,I -1,5,K)-CH(L,IC-1,2,K)
  104          CONTINUE
  105    CONTINUE
         DO 107 K=1,L1
            DO 107 I=3,IDO,2
               IC = IDP2-I
*VOPTION NOFVAL
               DO 106 L=1,LOT
                  CTR2          = TR11*CC(L,I-1,K,2)+TR12*CC(L,I-1,K,3)
                  CTI2          = TR11*CC(L,I  ,K,2)+TR12*CC(L,I  ,K,3)
                  CTR3          = TR12*CC(L,I-1,K,2)+TR11*CC(L,I-1,K,3)
                  CTI3          = TR12*CC(L,I  ,K,2)+TR11*CC(L,I  ,K,3)
                  TR2           = CTR2+CC(L,I-1,K,1)
                  TI2           = CTI2+CC(L,I  ,K,1)
                  TR3           = CTR3+CC(L,I-1,K,1)
                  TI3           = CTI3+CC(L,I  ,K,1)
                  TR4           = TI12*CC(L,I-1,K,5)-TI11*CC(L,I-1,K,4)
                  TI4           = TI12*CC(L,I  ,K,5)-TI11*CC(L,I  ,K,4)
                  SR23          = CC(L,I-1,K,2)+CC(L,I-1,K,3)
                  SI23          = CC(L,I  ,K,2)+CC(L,I  ,K,3)
                  TR5           = TI11*CC(L,I-1,K,5)+TI12*CC(L,I-1,K,4)
                  TI5           = TI11*CC(L,I  ,K,5)+TI12*CC(L,I  ,K,4)
                  CH(L,I -1,1,K)= CC(L,I-1,K,1)+SR23
                  CH(L,I   ,1,K)= CC(L,I  ,K,1)+SI23
                  CH(L,IC-1,2,K)= TR2-TR5
                  CH(L,IC  ,2,K)= TI5-TI2
                  CH(L,I -1,3,K)= TR2+TR5
                  CH(L,I   ,3,K)= TI2+TI5
                  CH(L,IC-1,4,K)= TR3-TR4
                  CH(L,IC  ,4,K)= TI4-TI3
                  CH(L,I -1,5,K)= TR3+TR4
                  CH(L,I   ,5,K)= TI3+TI4
  106          CONTINUE
  107    CONTINUE
      END IF
      RETURN
      END
C***********************************************************************
      SUBROUTINE RADBGM    !! FFT
     &          (INC,LOT,IDO,IP,L1,IDL1,CC,C1,C2,CH,CH2,WA)
*
      INTEGER    INC
      INTEGER    LOT
      INTEGER    IDO
      INTEGER    IP
      INTEGER    L1
      INTEGER    IDL1
      REAL*8     CC(INC,IDO,IP,L1)
      REAL*8     C1(INC,IDO,L1,IP)
      REAL*8     C2(INC,IDL1,IP)
      REAL*8     CH(INC,IDO,L1,IP)
      REAL*8     CH2(INC,IDL1,IP)
      REAL*8     WA(*)
*
      INTEGER    K, I, L, J, IPPH, M, IK
      INTEGER    IDP2, IPP2, J2, JC, IC, MC, IS, IDIJ
*  
      REAL*8 ARG,DCP,DSP,AR1,AI1,AR1H,DC2,DS2,AR2,AI2,AR2H,TPI
C
      TPI = 8.D0* ATAN(1.D0)
      ARG = TPI/IP
      DCP = COS(ARG)
      DSP = SIN(ARG)
      IDP2 = IDO+2
      IPP2 = IP+2
      IPPH = (IP+1)/2
C     IPPHC = IPP2-IPPH
C     IPPH2 = 2*IPPH
C
      DO 102 K=1,L1
         DO 102 I=1,IDO
            DO 101 L=1,LOT
               CH(L,I,K,1) = CC(L,I,1,K)
  101       CONTINUE 
  102 CONTINUE
*VOPTION NOFVAL
      DO 113 J=2,IPPH
         JC = IPP2-J
         J2 = J+J
         DO 113 K=1,L1
            DO 112 L=1,LOT
               CH(L,1,K,J ) = CC(L,IDO,J2-2,K)+CC(L,IDO,J2-2,K)
               CH(L,1,K,JC) = CC(L,  1,J2-1,K)+CC(L,  1,J2-1,K)
  112       CONTINUE
  113 CONTINUE
      IF (IDO .NE. 1) THEN
*VOPTION NOFVAL
         DO 121 J=2,IPPH
            JC = IPP2-J
            DO 120 K=1,L1
*VOPTION NOFVAL
               DO 119 I=3,IDO,2
                  IC = IDP2-I
                  DO 118 L=1,LOT
                    CH(L,I-1,K,J )=CC(L,I-1,2*J-1,K)+CC(L,IC-1,2*J-2,K)
                    CH(L,I  ,K,J )=CC(L,I  ,2*J-1,K)-CC(L,IC  ,2*J-2,K)
                    CH(L,I-1,K,JC)=CC(L,I-1,2*J-1,K)-CC(L,IC-1,2*J-2,K)
                    CH(L,I  ,K,JC)=CC(L,I  ,2*J-1,K)+CC(L,IC  ,2*J-2,K)
  118             CONTINUE
  119          CONTINUE
  120       CONTINUE
  121    CONTINUE
      ENDIF
      AR1 = 1.D0
      AI1 = 0.D0
*VOPTION NOFVAL
      DO 126 M=2,IPPH
         MC = IPP2-M
         AR1H = DCP*AR1-DSP*AI1
         AI1  = DCP*AI1+DSP*AR1
         AR1  = AR1H
         DO 123 IK=1,IDL1
            DO 123 L=1,LOT
               C2(L,IK,M ) = CH2(L,IK,1)+AR1*CH2(L,IK, 2)
               C2(L,IK,MC) =             AI1*CH2(L,IK,IP)
  123    CONTINUE
         DC2 = AR1
         DS2 = AI1
         AR2 = AR1
         AI2 = AI1
*VOPTION NOFVAL
         DO 125 J=3,IPPH
            JC = IPP2-J
            AR2H = DC2*AR2-DS2*AI2
            AI2  = DC2*AI2+DS2*AR2
            AR2  = AR2H
            DO 124 IK=1,IDL1
               DO 124 L=1,LOT
                  C2(L,IK,M ) = C2(L,IK,M )+AR2*CH2(L,IK,J )
                  C2(L,IK,MC) = C2(L,IK,MC)+AI2*CH2(L,IK,JC)
  124       CONTINUE
  125    CONTINUE
  126 CONTINUE
      DO 129 J=2,IPPH
         DO 129 IK=1,IDL1
            DO 128 L=1,LOT
               CH2(L,IK,1) = CH2(L,IK,1)+CH2(L,IK,J)
  128       CONTINUE 
  129 CONTINUE
*VOPTION NOFVAL
      DO 142 J=2,IPPH
         JC = IPP2-J
         DO 141 K=1,L1
            DO 140 L=1,LOT
               CH(L,1,K,J ) = C1(L,1,K,J)-C1(L,1,K,JC)
               CH(L,1,K,JC) = C1(L,1,K,J)+C1(L,1,K,JC)
  140       CONTINUE
  141    CONTINUE
  142 CONTINUE
      IF (IDO .EQ. 1) RETURN
*VOPTION NOFVAL
      DO 150 J=2,IPPH
         JC = IPP2-J
         DO 149 K=1,L1
            DO 149 I=3,IDO,2
               DO 148 L=1,LOT
                  CH(L,I-1,K,J ) = C1(L,I-1,K,J)-C1(L,I  ,K,JC)
                  CH(L,I  ,K,J ) = C1(L,I  ,K,J)+C1(L,I-1,K,JC)
                  CH(L,I-1,K,JC) = C1(L,I-1,K,J)+C1(L,I  ,K,JC)
                  CH(L,I  ,K,JC) = C1(L,I  ,K,J)-C1(L,I-1,K,JC)
  148          CONTINUE
  149    CONTINUE
  150 CONTINUE
      DO 152 IK=1,IDL1
         DO 152 L=1,LOT
            C2(L,IK,1) = CH2(L,IK,1)
  152 CONTINUE 
      DO 155 J=2,IP
         DO 155 K=1,L1
            DO 154 L=1,LOT
               C1(L,1,K,J) = CH(L,1,K,J)
  154       CONTINUE 
  155 CONTINUE
      IS = -IDO
*VOPTION NOFVAL
      DO 169 J=2,IP
         IS = IS+IDO
         DO 168 K=1,L1
*VOPTION NOFVAL
            DO 167 I=3,IDO,2
               IDIJ = IS+I-1
               DO 166 L=1,LOT
                  C1(L,I-1,K,J) =
     *               WA(IDIJ-1)*CH(L,I-1,K,J)-WA(IDIJ)*CH(L,I  ,K,J)
                  C1(L,I  ,K,J) =
     *               WA(IDIJ-1)*CH(L,I  ,K,J)+WA(IDIJ)*CH(L,I-1,K,J)
  166          CONTINUE
  167       CONTINUE
  168    CONTINUE
  169 CONTINUE
*
      RETURN
      END
C***********************************************************************
      SUBROUTINE RADFGM    !! FFT
     &          (INC,LOT,IDO,IP,L1,IDL1,CC,C1,C2,CH,CH2,WA)
*
      INTEGER     INC
      INTEGER     LOT
      INTEGER     IDO
      INTEGER     IP
      INTEGER     L1
      INTEGER     IDL1
      REAL*8      CC(INC,IDO,IP,L1)
      REAL*8      C1(INC,IDO,L1,IP)
      REAL*8      C2(INC,IDL1,IP)
      REAL*8      CH(INC,IDO,L1,IP)
      REAL*8      CH2(INC,IDL1,IP)
      REAL*8      WA(*)
*
      INTEGER     IK, L, J, K, I, IPPH, M, J2, JC
      INTEGER     IPP2, IDP2, IS, IDIJ, MC, IC
*
      REAL*8 ARG,DCP,DSP,AR1,AI1,AR1H,DC2,DS2,AR2,AI2,AR2H,TPI
C
      TPI = 8.D0* ATAN(1.D0)
      ARG = TPI/IP
      DCP = COS(ARG)
      DSP = SIN(ARG)
      IPPH = (IP+1)/2
      IPP2 = IP+2
      IDP2 = IDO+2
C
      IF (IDO .EQ. 1) GO TO 1000
      DO 101 IK=1,IDL1
         DO 101 L=1,LOT
            CH2(L,IK,1) = C2(L,IK,1)
  101 CONTINUE 
      DO 104 J=2,IP
         DO 104 K=1,L1
            DO 103 L=1,LOT
               CH(L,1,K,J) = C1(L,1,K,J)
  103       CONTINUE 
  104 CONTINUE
      IS = -IDO
*VOPTION NOFVAL
      DO 116 J=2,IP
         IS = IS+IDO
         DO 115 K=1,L1
*VOPTION NOFVAL
            DO 114 I=3,IDO,2
               IDIJ = IS+I-1
               DO 113 L=1,LOT
                  CH(L,I-1,K,J) =
     *                WA(IDIJ-1)*C1(L,I-1,K,J)+WA(IDIJ)*C1(L,I  ,K,J)
                  CH(L,I  ,K,J) =
     *                WA(IDIJ-1)*C1(L,I  ,K,J)-WA(IDIJ)*C1(L,I-1,K,J)
  113          CONTINUE
  114       CONTINUE
  115    CONTINUE
  116 CONTINUE
*VOPTION NOFVAL
      DO 120 J=2,IPPH
         JC = IPP2-J
         DO 119 K=1,L1
            DO 119 I=3,IDO,2
               DO 118 L=1,LOT
                  C1(L,I-1,K,J ) = CH(L,I-1,K,J )+CH(L,I-1,K,JC)
                  C1(L,I  ,K,J ) = CH(L,I  ,K,J )+CH(L,I  ,K,JC)
                  C1(L,I-1,K,JC) = CH(L,I  ,K,J )-CH(L,I  ,K,JC)
                  C1(L,I  ,K,JC) = CH(L,I-1,K,JC)-CH(L,I-1,K,J )
  118          CONTINUE
  119    CONTINUE
  120 CONTINUE
      GO TO 2000
C
 1000 CONTINUE
      DO 122 IK=1,IDL1
         DO 122 L=1,LOT
            C2(L,IK,1) = CH2(L,IK,1)
  122 CONTINUE 
*
 2000 CONTINUE
*VOPTION NOFVAL
      DO 125 J=2,IPPH
         JC = IPP2-J
         DO 124 K=1,L1
            DO 124 L=1,LOT
               C1(L,1,K,J ) = CH(L,1,K,J )+CH(L,1,K,JC)
               C1(L,1,K,JC) = CH(L,1,K,JC)-CH(L,1,K,J )
  124    CONTINUE
  125 CONTINUE
      AR1 = 1.D0
      AI1 = 0.D0  
*VOPTION NOFVAL
      DO 134 M=2,IPPH
         MC = IPP2-M
         AR1H = DCP*AR1-DSP*AI1
         AI1  = DCP*AI1+DSP*AR1
         AR1  = AR1H
         DO 131 IK=1,IDL1
            DO 131 L=1,LOT
               CH2(L,IK,M ) = C2(L,IK,1)+AR1*C2(L,IK, 2)
               CH2(L,IK,MC) =            AI1*C2(L,IK,IP)
  131    CONTINUE
         DC2 = AR1
         DS2 = AI1
         AR2 = AR1
         AI2 = AI1
*VOPTION NOFVAL
         DO 133 J=3,IPPH
            JC = IPP2-J
            AR2H = DC2*AR2-DS2*AI2
            AI2  = DC2*AI2+DS2*AR2
            AR2  = AR2H
            DO 132 IK=1,IDL1
               DO 132 L=1,LOT
                  CH2(L,IK,M ) = CH2(L,IK,M )+AR2*C2(L,IK,J )
                  CH2(L,IK,MC) = CH2(L,IK,MC)+AI2*C2(L,IK,JC)
  132       CONTINUE
  133    CONTINUE
  134 CONTINUE
      DO 137 J=2,IPPH
         DO 137 IK=1,IDL1
            DO 136 L=1,LOT
               CH2(L,IK,1) = CH2(L,IK,1)+C2(L,IK,J)
  136       CONTINUE 
  137 CONTINUE
      DO 148 K=1,L1
         DO 148 I=1,IDO
            DO 147 L=1,LOT
               CC(L,I,1,K) = CH(L,I,K,1)
  147       CONTINUE 
  148 CONTINUE
*VOPTION NOFVAL
      DO 159 J=2,IPPH
         JC = IPP2-J
         J2 = J+J
         DO 158 K=1,L1
            DO 158 L=1,LOT
               CC(L,IDO,J2-2,K) = CH(L,1,K,J )
               CC(L,  1,J2-1,K) = CH(L,1,K,JC)
  158    CONTINUE
  159 CONTINUE
      IF (IDO .EQ. 1) RETURN
*VOPTION NOFVAL
      DO 167 J=2,IPPH
         JC = IPP2-J
         J2 = J+J
         DO 166 K=1,L1
*VOPTION NOFVAL
            DO 165 I=3,IDO,2
               IC = IDP2-I
               DO 164 L=1,LOT
                  CC(L,I -1,J2-1,K) = CH(L,I-1,K,J )+CH(L,I-1,K,JC)
                  CC(L,I   ,J2-1,K) = CH(L,I  ,K,J )+CH(L,I  ,K,JC)
                  CC(L,IC-1,J2-2,K) = CH(L,I-1,K,J )-CH(L,I-1,K,JC)
                  CC(L,IC  ,J2-2,K) = CH(L,I  ,K,JC)-CH(L,I  ,K,J )
  164          CONTINUE
  165       CONTINUE
  166    CONTINUE
  167 CONTINUE
*
      RETURN
      END
