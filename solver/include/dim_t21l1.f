*
*     T21L1 include file for dimension declaration
*
      INTEGER     IMAX           !! No.of longitudinal grid
      INTEGER     JMAX           !! number of latitude grid
      INTEGER     IDIM           !! size of longitude dimension
      INTEGER     JDIM           !! size of latitude dimension
      INTEGER     NMAX           !! maximum total wave number
      INTEGER     NTR            !! truncation wave number
      INTEGER     MINT           !! interval of zonal wave number
      INTEGER     MMAX           !! maximum zonal wave number
      INTEGER     LMAX           !! maximum meridional wave number
      INTEGER     IJDIM          !! IDIM*JDIM
      INTEGER     IJSDIM         !! size of physical process
      INTEGER     KMAX           !! vertical dimension size
      INTEGER     KDIM           !! vertical dimension size
      INTEGER     IJKDIM         !! total size of matrix
      INTEGER     NVAR
      INTEGER     NMDIM          !! # of horizontal wave
      INTEGER     JMXHF          !! JMAX/2+1
      INTEGER     MMXMI          
      INTEGER     MAXH, MAXV
      INTEGER     NOMIT          !! # omitted WN 
      INTEGER     MSIZ           !! matrix size
      INTEGER     MSIZ2          !! matrix size (1 variable)
      INTEGER     MSIZR          !! maximum size for NRHS

      PARAMETER ( IMAX  = 64,        JMAX   = 32,
     $            NMAX  = 21,        MINT   = 1,
     $            MMAX  = NMAX,      LMAX   = NMAX,
     $            IDIM  = IMAX+1,    JDIM   = JMAX,
     $            IJDIM = IDIM*JDIM, IJSDIM = IJDIM,
     $            KMAX  = 1,
     $            KDIM  = KMAX,      IJKDIM = IJDIM*KDIM,
     $            NTR   = NMAX    )

      PARAMETER ( NVAR = 0 )
*
      PARAMETER ( MMXMI =  MMAX/MINT,
     &            NMDIM = (MMXMI+1)*(2*(NMAX+1)-MMXMI*MINT)
     &                  - (NMAX-LMAX)/MINT*(NMAX-LMAX+1),
     &            JMXHF = JMAX/2+1                          )

      PARAMETER ( MAXH = 2, MAXV = 3 )
      PARAMETER ( NOMIT = NMAX+2 )
      PARAMETER ( MSIZ = (NMDIM-NOMIT)*(KMAX*NVAR+1) )
      PARAMETER ( MSIZ2 = (NMDIM-NOMIT)*KMAX )
      PARAMETER ( MSIZR = 1 )

      REAL*8      PLEV( KMAX )
      DATA PLEV / 1000. /
