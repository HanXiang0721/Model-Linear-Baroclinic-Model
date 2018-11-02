* PACKAGE ZHDIM  !! # of char. in string
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*
      INTEGER     NCC
      INTEGER     NDC
      INTEGER     NFILN
*
#ifdef SYS_IBMS
      PARAMETER ( NCC   = 16, NDC   = 64 ,
     &            NFILN = 32              )
#else
      PARAMETER ( NCC   = 16, NDC   = 64 ,
     &            NFILN = 128             )
#endif
