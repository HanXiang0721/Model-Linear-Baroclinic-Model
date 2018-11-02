* PACKAGE UMTLU   !! solution of linear eqs. by LU decomposition
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*
**********************************************************************
      SUBROUTINE LUMAKE    !! LU decomposition (partial pivot select)
     M         ( ALU   ,
     O           KP    ,
     D           JDIM  , NDIM   )
*
*     overwrite input matrix by LU matrix
*
*   [PARAM] 
      INTEGER    JDIM
      INTEGER    NDIM
*
*   [MODIFY] 
      REAL*8     ALU ( JDIM, NDIM, NDIM )     !! input / LU matrix
*
*   [OUTPUT] 
      INTEGER    KP  ( JDIM, NDIM )           !! pivot
*
*   [INTERNAL WORK] 
      INTEGER    J, K, M, N
      REAL*8     TEMP
*
*
      DO 3000 K = 1, NDIM-1
*
*          < 1. partial pivot selection >
*
         DO 1000  J = 1, JDIM
            KP ( J,K ) = K
 1000    CONTINUE 
*
         DO 1100  M = K+1, NDIM
            DO 1110 J = 1, JDIM
               IF (      ABS( ALU ( J,M      ,K ) ) 
     &              .GT. ABS( ALU ( J,KP(J,K),K ) )   ) THEN
                  KP ( J,K ) = M
               ENDIF
 1110       CONTINUE 
 1100    CONTINUE
*
         DO 1200 N = 1, NDIM
#ifdef SYS_SX3
*vdir nodep(ALU)
#endif
            DO 1210 J = 1, JDIM
               IF ( KP(J,K).NE.K ) THEN
                  TEMP                = ALU ( J,K,N )
                  ALU ( J,K,N )       = ALU ( J,KP(J,K),N )
                  ALU ( J,KP(J,K),N ) = TEMP
               ENDIF
 1210       CONTINUE 
 1200    CONTINUE
*
*          < 2. LU decompo. >
*
         DO 2100  N = K+1, NDIM
            DO 2110 J = 1, JDIM
               ALU ( J,K,N ) = ALU( J,K,N ) / ALU( J,K,K )
 2110       CONTINUE 
*
            DO 2200 M = K+1, NDIM
               DO 2210 J = 1, JDIM                  
                  ALU ( J,M,N ) = ALU( J,M,N )
     &                          - ALU( J,M,K ) * ALU( J,K,N )
*
 2210          CONTINUE 
 2200       CONTINUE
 2100    CONTINUE
*
 3000 CONTINUE
*
      RETURN
      END
***********************************************************************
      SUBROUTINE LUSOLV    !! solution by LU decomposition
     M         ( XV    ,
     I           ALU   , KP    ,
     D           JDIM  , NDIM   )
*
*    overwrite input  by solution
*
*   [PARAM] 
      INTEGER    JDIM
      INTEGER    NDIM
*
*   [MODIFY] 
      REAL*8     XV  ( JDIM, NDIM )     !! right side vector = solution
*
*   [INPUT] 
      REAL*8     ALU ( JDIM, NDIM, NDIM )     !! LU matrix
      INTEGER    KP  ( JDIM, NDIM )           !! pivot
*
*   [INTERNAL WORK] 
      INTEGER    J, K, N, NN
      REAL*8     TEMP
*
*
*          < 1. pivot selection >
*
      DO 1100 K = 1, NDIM-1
#ifdef SYS_SX3
*vdir nodep(XV)
#endif
         DO 1110 J = 1, JDIM
            IF ( KP ( J,K ) .NE. K ) THEN
               TEMP             = XV ( J,K )
               XV ( J,K )       = XV ( J,KP(J,K) )
               XV ( J,KP(J,K) ) = TEMP
            ENDIF
 1110    CONTINUE
 1100 CONTINUE
*
*          < 2. forward substitute >
*
      DO 2100 N = 1, NDIM
         DO 2110 J = 1, JDIM
            XV ( J,N ) = XV ( J,N ) / ALU ( J,N,N )
 2110    CONTINUE
*
         DO 2130 NN = N+1, NDIM
            DO 2120 J = 1, JDIM
               XV ( J,NN ) = XV ( J,NN )
     &                     - XV ( J,N ) * ALU ( J,NN,N )
 2120       CONTINUE
 2130    CONTINUE
 2100 CONTINUE
*
*          < 3. backward substitute >
*
      DO 3100 K = NDIM-1, 1, -1
         DO 3100 N = K+1, NDIM
            DO 3110 J = 1, JDIM
               XV ( J,K ) = XV ( J,K )
     &                    - XV ( J,N ) * ALU ( J,K,N )
 3110       CONTINUE
*
 3100 CONTINUE
*
      RETURN
      END
**********************************************************************
      SUBROUTINE LUMAK3    !! LU decomposition [3-diagonal]
     M         ( ALU   ,
     D           JDIM  , NDIM )
*
*       overwrite input matrix by LU matrix
*
*   [PARAM] 
      INTEGER    JDIM
      INTEGER    NDIM
*
*   [MODIFY] 
      REAL*8     ALU ( JDIM, NDIM, -1:1 )     !! input / LU matrix
*
*   [INTERNAL WORK] 
      INTEGER    J, K
*
      DO 1100 K = NDIM-1, 1, -1
         DO 1100 J = 1, JDIM
            ALU ( J,K,0 ) =  ALU ( J,K,0 )
     &                     - ALU ( J,K+1,-1 ) / ALU ( J,K+1,0 )
     &                     * ALU ( J,K,1 )
 1100 CONTINUE
*
      RETURN
      END
***********************************************************************
      SUBROUTINE LUSOL3    !! solution by LU decomp.[triple diagonal]
     M         ( XV    ,
     I           ALU   ,
     D           JDIM , NDIM )
*
*     overwrite input  by solution
*
*   [PARAM] 
      INTEGER    JDIM
      INTEGER    NDIM
*
*   [MODIFY] 
      REAL*8     XV  ( JDIM, NDIM )     !! right side vector = solution
*
*   [INPUT] 
      REAL*8     ALU ( JDIM, NDIM, -1:1 )     !! LU matrix
*
*   [INTERNAL WORK] 
      INTEGER    J, K
*
      DO 1100 K = NDIM-1, 1, -1
         DO 1110 J = 1, JDIM
            XV ( J,K ) = XV ( J,K )
     &                 - XV ( J,K+1 ) / ALU ( J,K+1,0 )
     &                                * ALU ( J,K  ,1 ) 
 1110    CONTINUE
 1100 CONTINUE
*
      DO 2000 J = 1, JDIM
         XV ( J,1 ) = XV ( J,1 ) / ALU ( J,1,0 )
 2000 CONTINUE
*
      DO 2100 K = 2, NDIM
         DO 2110 J =  1, JDIM
            XV ( J,K ) = (   XV ( J,K   )
     &                     - XV ( J,K-1 ) * ALU ( J,K,-1 )  )
     &                 / ALU ( J,K,0 )
 2110    CONTINUE
 2100 CONTINUE
*
      RETURN
      END
***********************************************************************
      SUBROUTINE LUFWD3    !! LU decomp. forward insert[3-diagonal]
     M         ( XV    ,
     I           ALU   ,
     D           JDIM , NDIM )
*
*     overwrite input  by solution
*
*   [PARAM] 
      INTEGER    JDIM
      INTEGER    NDIM
*
*   [MODIFY] 
      REAL*8     XV  ( JDIM, NDIM )     !! right side vector = solution
*
*   [INPUT] 
      REAL*8     ALU ( JDIM, NDIM, -1:1 )     !! LU matrix
*
*   [INTERNAL WORK] 
      INTEGER    J, K
*
      DO 1100 K = NDIM-1, 1, -1
         DO 1110 J = 1, JDIM
            XV ( J,K ) = XV ( J,K )
     &                 - XV ( J,K+1 ) / ALU ( J,K+1,0 )
     &                                * ALU ( J,K  ,1 ) 
 1110    CONTINUE
 1100 CONTINUE
*
      RETURN
      END
***********************************************************************
      SUBROUTINE LUBWD3    !! LU decomp. backward insert[3-diagonal]
     M         ( XV    ,
     I           ALU   ,
     D           JDIM , NDIM )
*
*     overwrite input  by solution
*
*   [PARAM] 
      INTEGER    JDIM
      INTEGER    NDIM
*
*   [MODIFY] 
      REAL*8     XV  ( JDIM, NDIM )     !! right side vector = solution
*
*   [INPUT] 
      REAL*8     ALU ( JDIM, NDIM, -1:1 )     !! LU matrix
*
*   [INTERNAL WORK] 
      INTEGER    J, K
*
      DO 2100 K = 2, NDIM
         DO 2110 J =  1, JDIM
            XV ( J,K ) = (   XV ( J,K   )
     &                     - XV ( J,K-1 ) * ALU ( J,K,-1 )  )
     &                 / ALU ( J,K,0 )
 2110    CONTINUE
 2100 CONTINUE
*
      RETURN
      END
