* PACKAGE DSPHE  !! dynamics: spherical trans.
*
**********************************************************************
      SUBROUTINE W2G       !! spherical trans.(spect.->grid)
     M         ( GDATA ,
     I           WDATA ,
     I           HGRAD , HFUNC , KMAXD )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
#include        "zddim.F"                /* NMDIM                  */
#include        "zccom.F"                /* stand. physical const. */
#include        "zcord.F"                /* coordinate             */
*
      INTEGER    KMAXD
*
*   [MODIFY] 
      REAL*8     GDATA ( IDIM*JDIM, KMAXD )  !! grid point data
*
*   [INPUT] 
      REAL*8     WDATA ( NMDIM, KMAXD    )   !! spectral data
      CHARACTER  HGRAD*4                     !! flag of differential
      CHARACTER  HFUNC*4                     !! flag of sign
*
*   [INTERNAL WORK] 
      REAL*8     ZDATA ( IDIM*JDIM, KMAX )   !! zonal spectral
      REAL*8     WORK  ( IDIM*JDIM, KMAX )   !! work
*
      REAL * 8   QSINLA( JDIM )              !! sin(lat.):double
      REAL * 8   QGW   ( JDIM )              !! Gaussian weight:double
      REAL * 8   QPNM  ( 0:NMAX+1, 0:MMAX )  !! Pnm Legendre
      REAL * 8   QDPNM ( 0:NMAX+1, 0:MMAX )  !! mu differential of Pnm
*
      INTEGER    J
*
*   [INTERNAL SAVE] 
      REAL*8     PNM   ( JMXHF*NMDIM )       !! Pnm Legendre
      REAL*8     DPNM  ( JMXHF*NMDIM )       !! mu differential of Pnm
      REAL*8     TRIGS ( IDIM*2 )            !! triangle function table
      INTEGER    IFAX  ( 10 )                !! factorziation of IMAX
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
      REAL*8     GWX   ( JDIM )              !! Gaussian weight
      REAL*8     GWDEL ( JDIM )              !! Gaussian weight for diff
      SAVE       PNM, DPNM, TRIGS, IFAX, NMO, GWX, GWDEL
      LOGICAL    OSET                        !! flag of setting const.
      LOGICAL    OFIRST
      DATA       OSET   / .FALSE. /
      DATA       OFIRST / .TRUE. /
*
      IF ( OFIRST ) THEN
         WRITE (6,*) ' @@@ DSPHE: SPHERICAL TRANSFORM INTFC. 93/12/07'
         OFIRST= .FALSE.
      ENDIF
*
      IF ( .NOT. OSET ) THEN
         WRITE (6,*) ' ### W2G: SPSTUP MUST BE CALLED BEFORE'
         CALL XABORT( 1 )
         RETURN
      ENDIF
*
      IF ( HGRAD(1:1) .EQ. 'Y' ) THEN
         CALL    SPW2G
     M         ( GDATA ,
     I           WDATA ,
     C           DPNM  , NMO   , TRIGS , IFAX ,
     F           HGRAD , HFUNC ,
     D           IMAX  , JMAX  , KMAXD , IDIM  , JDIM  ,
     D           LMAX  , MMAX  , NMAX  , MINT  , NMDIM , JMXHF ,
     W           ZDATA , WORK                                   )
      ELSE
         CALL    SPW2G
     M         ( GDATA ,
     I           WDATA ,
     C           PNM   , NMO   , TRIGS , IFAX ,
     F           HGRAD , HFUNC ,
     D           IMAX  , JMAX  , KMAXD , IDIM  , JDIM  ,
     D           LMAX  , MMAX  , NMAX  , MINT  , NMDIM , JMXHF ,
     W           ZDATA , WORK                                   )
      ENDIF
*
      RETURN
*======================================================================
      ENTRY      G2W       !!  spherical trans.(grid->spect.)
     M         ( WDATA ,
     I           GDATA ,
     I           HGRAD , HFUNC , KMAXD )
*
      IF ( .NOT. OSET ) THEN
         WRITE (6,*) ' ### G2W: SPSTUP MUST BE CALLED BEFORE'
         CALL XABORT( 1 )
         RETURN
      ENDIF
*
      IF      ( HGRAD(1:1) .EQ. 'Y' ) THEN
         CALL    SPG2W
     M         ( WDATA ,
     I           GDATA ,
     C           DPNM  , NMO   , TRIGS , IFAX  , GWDEL ,
     F           HGRAD , HFUNC ,
     D           IMAX  , JMAX  , KMAXD , IDIM  , JDIM  ,
     D           LMAX  , MMAX  , NMAX  , MINT  , NMDIM , JMXHF ,
     W           ZDATA , WORK                                   )
      ELSE IF ( HGRAD(1:1) .EQ. 'X' ) THEN
         CALL    SPG2W
     M         ( WDATA ,
     I           GDATA ,
     C           PNM   , NMO   , TRIGS , IFAX  , GWDEL ,
     F           HGRAD , HFUNC ,
     D           IMAX  , JMAX  , KMAXD , IDIM  , JDIM  ,
     D           LMAX  , MMAX  , NMAX  , MINT  , NMDIM , JMXHF ,
     W           ZDATA , WORK                                   )
      ELSE
         CALL    SPG2W
     M         ( WDATA ,
     I           GDATA ,
     C           PNM   , NMO   , TRIGS , IFAX  , GWX   ,
     F           HGRAD , HFUNC ,
     D           IMAX  , JMAX  , KMAXD , IDIM  , JDIM  ,
     D           LMAX  , MMAX  , NMAX  , MINT  , NMDIM , JMXHF ,
     W           ZDATA , WORK                                   )
      ENDIF
*
      RETURN
*=====================================================================
      ENTRY      SPSTUP    !! setup spherical harmonic functions
*
      IF ( OSET ) RETURN
*
      CALL DSETNM
     O         ( NMO    )
*
*          < prepare FFT >
*
      IF ( IMAX .GE. 2 ) THEN
         CALL RFFTIM
     I         ( IMAX  ,
     O           TRIGS , IFAX )
      ENDIF
*
      IF ( JMAX .GE. 2 ) THEN
*
*          < Gaussian weight >
*
         CALL    GAUSS       !!  calculate Gaussian latitude & weight
     O         ( QSINLA, QGW   ,
     I           JMAX            )
*
         DO 3100 J = 1, JMAX
            GWX  ( J ) = QGW( J )
            GWDEL( J ) = QGW( J ) / ER / ( 1. - QSINLA(J)**2 )
 3100    CONTINUE
*
*          < Legendre function >
*
         CALL SETPNM
     O         ( PNM   , DPNM  , NMO   ,
     I           QSINLA,
     D           JMAX  , MMAX  , LMAX  , NMAX  , MINT  ,
     D           NMDIM , JMXHF ,
     W           QPNM  , QDPNM                          )
      ELSE
*
         DO 3200 J = 1, JMAX
            GWX  ( J ) = 1.
            GWDEL( J ) = 1. / ER / ( 1. - SIN(ALAT(J))**2 )
 3200    CONTINUE
*
         CALL    SETNMO      !! order of matrix
     O         ( NMO   ,
     D           MMAX  , LMAX  , NMAX  , MINT    )
      ENDIF
*
      OSET = .TRUE.
*
      RETURN
      END
*****************************************************************
      SUBROUTINE DSETNM    !! order of matrix
     O         ( NMO    )
*
*   [PARAM] 
#include        "zcdim.F"                /* # of grid point & wave */
*
*   [OUTPUT] 
      INTEGER    NMO   ( 2, 0:MMAX, 0:LMAX ) !! order of spect. suffix
*
      CALL       SETNMO      !! order of matrix
     O         ( NMO   ,
     D           MMAX  , LMAX  , NMAX  , MINT    )
*
      RETURN
      END
