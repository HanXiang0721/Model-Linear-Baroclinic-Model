* PACKAGE ZCX168s4  !! No. of grid point & wave number (X168s4)
*
*   [HIS] 95/07/30(numaguti)
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
      PARAMETER ( IMAX  = 128,       JMAX   = 1,  
     &            NMAX  = 168,       MINT   = 4,
     &            MMAX  = NMAX,      LMAX   = 0,
     &            IDIM  = IMAX+1,    JDIM   = JMAX,
     &            IJDIM = IDIM*JDIM, IJSDIM = IDIM,
     &            KDIM  = KMAX,      IJKDIM = IJDIM*KDIM  )
