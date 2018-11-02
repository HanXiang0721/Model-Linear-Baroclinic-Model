program main
implicit none

! this is the ncep grid settings
integer :: nx=144, ny=73, nt=12, nz=17,nz1=8,nz2=12

! this is the T42 grid settings
integer :: nxo=128, nyo=64

integer :: k,l

character*100 :: filepath
character*20 :: field
real,allocatable :: HGT(:,:,:,:),AIR(:,:,:,:),UWND(:,:,:,:),VWND(:,:,:,:), &
        RHUM(:,:,:,:),OMEGA(:,:,:,:),Q(:,:,:,:),PRES(:,:,:,:)
real,allocatable :: HGTO(:,:,:,:),AIRO(:,:,:,:),UWNDO(:,:,:,:),VWNDO(:,:,:,:), &
        RHUMO(:,:,:,:),OMEGAO(:,:,:,:),QO(:,:,:,:),PRESO(:,:,:,:)
real,allocatable :: level(:),lat(:),lon(:)
real,allocatable :: lat_out(:), lon_out(:), level1(:), level2(:)


allocate(HGT(nx,ny,nz,nt))
allocate(AIR(nx,ny,nz,nt))
allocate(UWND(nx,ny,nz,nt))
allocate(VWND(nx,ny,nz,nt))

allocate(Q(nx,ny,nz1,nt))
allocate(RHUM(nx,ny,nz1,nt))

allocate(OMEGA(nx,ny,nz2,nt))

allocate(PRES(nx,ny,1,nt))

allocate(HGTO(nxo,nyo,nz,nt))
allocate(AIRO(nxo,nyo,nz,nt))
allocate(UWNDO(nxo,nyo,nz,nt))
allocate(VWNDO(nxo,nyo,nz,nt))

allocate(RHUMO(nxo,nyo,nz1,nt))
allocate(QO(nxo,nyo,nz1,nt))

allocate(OMEGAO(nxo,nyo,nz2,nt))

allocate(PRESO(nxo,nyo,1,nt))

allocate(level(nz))
allocate(lat(ny))
allocate(lon(nx))

allocate(lat_out(nyo))
allocate(lon_out(nxo))

allocate(level1(nz1))
allocate(level2(nz2))

filepath="./ncep.nc"

field="hgt"
call readfield(filepath,field,HGT,nx,ny,nz,nt)

field="air"
call readfield(filepath,field,AIR,nx,ny,nz,nt)

field="rhum"
call readfield(filepath,field,RHUM,nx,ny,nz1,nt)

field="omega"
call readfield(filepath,field,OMEGA,nx,ny,nz2,nt)

field="uwnd"
call readfield(filepath,field,UWND,nx,ny,nz,nt)

field="vwnd"
call readfield(filepath,field,VWND,nx,ny,nz,nt)

field="q"
call readfield(filepath,field,Q,nx,ny,nz1,nt)

field="pres"
call readfield(filepath,field,PRES,nx,ny,1,nt)

field="level"
call readoned(filepath,field,level,nz)

field="lat"
call readoned(filepath,field,lat,ny)

field="lon"
call readoned(filepath,field,lon,nx)

field="level1"
call readoned(filepath,field,level1,nz1)

field="level2"
call readoned(filepath,field,level2,nz2)

lat_out = & 
(/-87.8638, -85.0965, -82.3129, -79.5256, -76.7369, -73.9475, -71.1577, &
-68.3678, -65.5776, -62.7873, -59.997, -57.2066, -54.4162, -51.6257, &
-48.8352, -46.0447, -43.2542, -40.4636, -37.6731, -34.8825, -32.0919, &
-29.3014, -26.5108, -23.7202, -20.9296, -18.139, -15.3484, -12.5578, &
-9.76715, -6.97653, -4.18592, -1.39531, 1.39531, 4.18592, 6.97653, &
9.76715, 12.5578, 15.3484, 18.139, 20.9296, 23.7202, 26.5108, 29.3014, &
32.0919, 34.8825, 37.6731, 40.4636, 43.2542, 46.0447, 48.8352, 51.6257, &
54.4162, 57.2066, 59.997, 62.7873, 65.5776, 68.3678, 71.1577, 73.9475, &
76.7369, 79.5256, 82.3129, 85.0965, 87.8638/)

lon_out = &
(/0.0, 2.8125, 5.625, 8.4375, 11.25, 14.0625, 16.875, 19.6875, 22.5, &
25.3125, 28.125, 30.9375, 33.75, 36.5625, 39.375, 42.1875, 45.0, 47.8125, &
50.625, 53.4375, 56.25, 59.0625, 61.875, 64.6875, 67.5, 70.3125, 73.125, &
75.9375, 78.75, 81.5625, 84.375, 87.1875, 90.0, 92.8125, 95.625, 98.4375, &
101.25, 104.0625, 106.875, 109.6875, 112.5, 115.3125, 118.125, 120.9375, &
123.75, 126.5625, 129.375, 132.1875, 135.0, 137.8125, 140.625, 143.4375, &
146.25, 149.0625, 151.875, 154.6875, 157.5, 160.3125, 163.125, 165.9375, & 
168.75, 171.5625, 174.375, 177.1875, 180.0, 182.8125, 185.625, 188.4375, &
191.25, 194.0625, 196.875, 199.6875, 202.5, 205.3125, 208.125, 210.9375, &
213.75, 216.5625, 219.375, 222.1875, 225.0, 227.8125, 230.625, 233.4375, &
236.25, 239.0625, 241.875, 244.6875, 247.5, 250.3125, 253.125, 255.9375, &
258.75, 261.5625, 264.375, 267.1875, 270.0, 272.8125, 275.625, 278.4375, &
281.25, 284.0625, 286.875, 289.6875, 292.5, 295.3125, 298.125, 300.9375, &
303.75, 306.5625, 309.375, 312.1875, 315.0, 317.8125, 320.625, 323.4375, &
326.25, 329.0625, 331.875, 334.6875, 337.5, 340.3125, 343.125, 345.9375, &
348.75, 351.5625, 354.375, 357.1875/)

open(unit=1,file='ncep.clim.y79-14.t42.grd',form='unformatted',convert='big_endian')
open(unit=2,file='ncep.clim.y79-14.ps.t42.grd',form='unformatted',convert='big_endian')


do k=1,nt
        call INTP(nx,ny,nz,nxo,nyo,lon,lat,lon_out,lat_out,-9.9e8,HGT(:,:,:,k),HGTO(:,:,:,k))
        call INTP(nx,ny,nz,nxo,nyo,lon,lat,lon_out,lat_out,-9.9e8,AIR(:,:,:,k),AIRO(:,:,:,k))
        call INTP(nx,ny,nz1,nxo,nyo,lon,lat,lon_out,lat_out,-9.9e8,RHUM(:,:,:,k),RHUMO(:,:,:,k))
        call INTP(nx,ny,nz,nxo,nyo,lon,lat,lon_out,lat_out,-9.9e8,UWND(:,:,:,k),UWNDO(:,:,:,k))
        call INTP(nx,ny,nz,nxo,nyo,lon,lat,lon_out,lat_out,-9.9e8,VWND(:,:,:,k),VWNDO(:,:,:,k))
        call INTP(nx,ny,nz1,nxo,nyo,lon,lat,lon_out,lat_out,-9.9e8,Q(:,:,:,k),QO(:,:,:,k))
        call INTP(nx,ny,nz2,nxo,nyo,lon,lat,lon_out,lat_out,-9.9e8,OMEGA(:,:,:,k),OMEGAO(:,:,:,k))
        call INTP(nx,ny,1,nxo,nyo,lon,lat,lon_out,lat_out,-9.9e8,PRES(:,:,:,k),PRESO(:,:,:,k))
end do

do k=1,nt
   do l=1,nz
      write(1) HGTO(:,:,l,k)
   end do
   do l=1,nz1
      write(1) RHUMO(:,:,l,k)
   end do
   do l=1,nz1
      write(1) QO(:,:,l,k)
   end do
   do l=1,nz
      write(1) AIRO(:,:,l,k)
   end do
   do l=1,nz
      write(1) UWNDO(:,:,l,k)
   end do
   do l=1,nz
      write(1) VWNDO(:,:,l,k)
   end do
   do l=1,nz2
      write(1) OMEGAO(:,:,l,k)
   end do
   write(2) PRESO(:,:,1,k)
end do
end program main


subroutine readoned(filepath,field,VAR,nd)
use netcdf
implicit none
integer :: nd
character*20 :: field
real :: VAR(nd)
integer::ncid,ncfile,dimidvar
integer::varidvar
character*100 :: filepath
ncfile=NF90_OPEN(filepath,nf90_nowrite,ncid)
ncfile=NF90_INQ_VARID(ncid,field,varidvar)
ncfile=NF90_GET_VAR(ncid,varidvar,VAR)
ncfile=NF90_CLOSE(ncid)
end subroutine readoned


subroutine readfield(filepath,field,VAR,nx,ny,nz,nt)
use netcdf
implicit none
integer :: nx,ny,nz,nt
character*20 :: field
real :: VAR(nx,ny,nz,nt)
integer::ncid,ncfile,dimidvar
integer::varidvar
character*100 :: filepath
ncfile=NF90_OPEN(filepath,nf90_nowrite,ncid)
ncfile=NF90_INQ_VARID(ncid,field,varidvar)
ncfile=NF90_GET_VAR(ncid,varidvar,VAR)
ncfile=NF90_CLOSE(ncid)
end subroutine readfield

!==============below is from intp.f of LBM, in ln_solver/solver/custom==============


!* SUPPLEMENT ROUTINE IN LBM2.0 on 2001/10/22
!*=============================================================
      SUBROUTINE INTP(IMAX,JMAX,KMAX,IDIM,JDIM,ALONI,COLRAI,ALONO,COLRAO,DMS,DIN,DOUT)
!*
!*     spatial interporation
!*
!C     USAGE:
!C        IMAX: x-dimension for input data
!C        JMAX: y-dimension for input data
!C        KMAX: z-dimension for input data
!C        IDIM: x-dimension for output data
!C        JDIM: y-dimension for output data
!C        ALONI : array of longitude (0 - 360  deg.) for input
!C        COLRAI: array of latitude  (-90 - 90 deg.) for input
!C        ALONO : array of longitude (0 - 360  deg.) for output
!C        COLRAO: array of latitude  (-90 - 90 deg.) for output
!C        DMS   : value for missing grid
!C        DIN   : input array, DIN=DIN(IMAX,JMAX,KMAX)
!C        DOUT  : output array, DOUT=DOUT(IDIM,JDIM,KMAX)
!C
!*
      INTEGER   IMAX, JMAX, KMAX
      INTEGER   IDIM, JDIM
      REAL*4    DMS
      REAL*4    DIN(IMAX,JMAX,KMAX),DOUT(IDIM,JDIM,KMAX)
      REAL*4    ALONI(IMAX),ALONO(IDIM)
      REAL*4    COLRAO(JDIM),COLRAI(JMAX)
!*
      INTEGER   I, J, K
!*
      CALL LINTPMS(IMAX,JMAX,KMAX,IDIM,JDIM,ALONI,COLRAI,ALONO,COLRAO,DMS,DIN,DOUT)
!*
      DO K = 1, KMAX
         DO J = 1, JDIM
            IF( COLRAO( J ) .LT. COLRAI( 1    ) .OR. COLRAO( J ) .GT. COLRAI( JMAX )      ) THEN
               DO I = 1, IDIM
                  DOUT( I,J,K ) = DMS
               ENDDO    
            ENDIF
         ENDDO
      ENDDO
!*
      RETURN
      END SUBROUTINE INTP
!*=============================================================
      SUBROUTINE LINTPMS(IMAX,JMAX,KMAX,IDIM,JDIM,ALONI,COLRAI,ALONO,COLRAO,DMS,DIN,DOUT)
!*
      INTEGER   MAXDM
      PARAMETER ( MAXDM = 1000 )
!*
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
!*
      IF(FIRST) THEN
         FIRST=.FALSE.
         DLON=ALONI(2)-ALONI(1)
         DO 100 JP=1,JDIM
            DO 120 JD=2,JMAX
               IF(COLRAI(JD).GE.COLRAO(JP)) THEN
                  LY(JP)=JD
                  DY(JP)=(COLRAO(JP)-COLRAI(JD-1))/(COLRAI(JD)-COLRAI(JD-1))
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
!*
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
            DOUT(IP,JP,K)=WEI(1)*DIN(IDP ,JD  ,K)+WEI(2)*DIN(ID  ,JD  ,K)+WEI(3)*DIN(IDP ,JD-1,K)+WEI(4)*DIN(ID  ,JD-1,K)
            DOUT(IP,JP,K)=DOUT(IP,JP,K)/WSUM
         ENDIF
  220    CONTINUE
  200 CONTINUE
!*
      RETURN
      END SUBROUTINE LINTPMS


