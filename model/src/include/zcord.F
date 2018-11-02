* PACKAGE ZCORD  !! standard coordinate (common)
*
*   [HIS] 96/06/19(numaguti) AGCM5.4f
*
      REAL*8     ALON  ( IJDIM )               !! longitude
      REAL*8     ALAT  ( IJDIM )               !! latitude
      REAL*8     DLON  ( IJDIM )               !! longitude weight
      REAL*8     DLAT  ( IJDIM )               !! latitude weight
      REAL*8     SIG   ( KMAX   )              !! sigma-level (integer)
      REAL*8     SIGM  ( KMAX+1 )              !! sigma-level (half lev)
      REAL*8     DSIG  ( KMAX   )              !! delta(sigma)
      REAL*8     DSIGM ( KMAX+1 )              !! delta(sigma) (half lev)
      COMMON    /COMCOR/
     &           ALON, ALAT, DLON, DLAT ,
     &           SIG , SIGM, DSIG, DSIGM
