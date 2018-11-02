* PACKAGE ZDDIM  !! spectral wave number(NMDIM, JMXHF)
*
*   [HIS] 95/03/15(numaguti) AGCM5.4.01
*
      INTEGER     NMDIM          !! # of horizontal wave
      INTEGER     JMXHF          !! JMAX/2+1
      INTEGER     MMXMI          
*
      PARAMETER ( MMXMI =  MMAX/MINT,
     &            NMDIM = (MMXMI+1)*(2*(NMAX+1)-MMXMI*MINT)
     &                  - (NMAX-LMAX)/MINT*(NMAX-LMAX+1),
     &            JMXHF = JMAX/2+1                          )
