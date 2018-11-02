* PACKAGE ZCT63  !! No. of grid point & wave number (T63)
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*
*
      INTEGER     IMAX           !! No.of longitudinal grid
      INTEGER     JMAX           !! number of latitude grid
      INTEGER     NMAX           !! maximum total wave number
      INTEGER     MINT           !! interval of zonal wave number
      INTEGER     MMAX           !! maximum zonal wave number
      INTEGER     LMAX           !! maximum meridional wave number
      INTEGER     IDIM           !! size of longitude dimension
      INTEGER     JDIM           !! size of latitude dimension
      INTEGER     IJDIM          !! IDIM*JDIM
      INTEGER     IJSDIM         !! size of physical process
      INTEGER     KDIM           !! vertical dimension size
      INTEGER     IJKDIM         !! total size of matrix
*
      PARAMETER ( IMAX  = 192,       JMAX   = 96,  
     &            NMAX  = 63,        MINT   = 1,
     &            MMAX  = NMAX,      LMAX   = NMAX,
     &            IDIM  = IMAX+1,    JDIM   = JMAX,
     &            IJDIM = IDIM*JDIM, IJSDIM = IDIM,
     &            KDIM  = KMAX,      IJKDIM = IJDIM*KDIM  )
