load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/calendar_decode2.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

datafile = addfile("./linear.t42l20.qqf.nc","r")

f_time = datafile->time
f_lat  = datafile->lat
f_lon  = datafile->lon
f_lev  = datafile->lev
f_z    = datafile->z
f_t    = datafile->t
f_u    = datafile->u
f_v    = datafile->v

f_lat = f_lat(::-1)
f_z = f_z(:,:,::-1,:)
f_u = f_u(:,:,::-1,:)
f_v = f_v(:,:,::-1,:)
f_t = f_t(:,:,::-1,:)

level = 200
mylev = ind(f_lev.eq.level)
mylat = ind(f_lat.ge.0.and.f_lat.le.90)

z = f_z(:,mylev,:,:)
u = f_u(:,mylev,:,:)
v = f_v(:,mylev,:,:)
t = f_t(:,mylev,:,:)

ntime = dimsizes(f_time)

clim_data = addfile("ncep.nc","r")

clim_uwnd = clim_data->uwnd
clim_vwnd = clim_data->vwnd
clim_lev = clim_data->level
clim_lat = clim_data->lat
clim_lon = clim_data->lon

climlev = ind(clim_lev.eq.level)

uwnd = (clim_uwnd(9,climlev,:,:) + clim_uwnd(10,climlev,:,:) + clim_uwnd(8,climlev,:,:)) / 3.0
vwnd = (clim_vwnd(9,climlev,:,:) + clim_vwnd(10,climlev,:,:) + clim_vwnd(8,climlev,:,:)) / 3.0

uwnd_clim = linint2_Wrap(clim_lon, clim_lat, uwnd, True, f_lon, f_lat, 0)
vwnd_clim = linint2_Wrap(clim_lon, clim_lat, vwnd, True, f_lon, f_lat, 0)

lat = f_lat
lon = f_lon

hgt_anom = dim_avg_n_Wrap(z(13:17,:,:),0)
;print(hgt_anom)

res = True
res@gsnDraw=False
res@gsnFrame=False
res@gsnSpreadColors=True
res@gsnAddCyclic=True


res@cnFillOn=True
res@cnLinesOn=False
res@cnLevelSelectionMode = "ExplicitLevels"
res@cnLevels=(/-40,-30,-20,-10,0,10,20,30,40/)
;res@cnLevels=(/-16,-8,-4,-2,0,2,4,8,16/)
;res@cnLevels=(/-2,-1.5,-1,-0.5,0,0.5,1,1.5,2/)
res@cnFillColors=(/2,4,6,8,0,0,13,15,17,19/)


res@lbOrientation="Vertical"

res@gsnMaximize=True
res@vpWidthF      = 0.4
res@vpHeightF     = 0.6

res@gsnLeftString=""
res@gsnRightString=""
res@tiMainString=""

res@mpCenterLonF=0
res@mpMinLatF=10


res@tmXBLabelFontHeightF=0.005
res@tmYLLabelFontHeightF=0.005
res@gsnRightStringFontHeightF=8


resp = True
resp@gsLineColor      = "gray" ; polylines color
resp@gsLineThicknessF = 2    ; polylines thickness
ftp=asciiread("./tibet_shape",(/2066,2/),"float")


resvc                  = True                      ; plot mods desired
resvc@gsnDraw=False
resvc@gsnFrame=False

resvc@gsnLeftString=""
resvc@gsnRightString=""

resvc@vcMinDistanceF=0.018

resvc@vcRefMagnitudeF  = 3
resvc@vcRefLengthF     = 0.015

resvc@vcGlyphStyle="LineArrow"
resvc@vcLineArrowThicknessF=2.6

resvc@vcRefAnnoOn               = True
resvc@vcRefAnnoString1          = "3"
resvc@vcRefAnnoSide             = "Top"
resvc@vcRefAnnoString2On        = False
resvc@vcRefAnnoPerimOn          = False
resvc@vcRefAnnoOrthogonalPosF   = -0.449
resvc@vcRefAnnoParallelPosF     = 0.999
resvc@vcRefAnnoBackgroundColor  = "green"
resvc@vcVectorDrawOrder         = "PostDraw"





;  Gas constant
gc=290
;  Gravitational acceleration
ga=9.80665

;  Radius of the earth
re=6378388

; pi
pi = atan(1.0)*4.

; Coriolis parameter
f =  2.*2.*pi/(60.*60.*24.)*sin(pi/180. * lat(:))
f!0 = "lat"
f&lat = lat
f@_FillValue = hgt_anom@_FillValue

; missing for 10S - 10N
do ilat = 0, dimsizes(lat)-1
if (abs(lat(ilat) ).lt. 20. ) then
f(ilat)= f@_FillValue
end if
end do

; cosine
coslat = cos(lat(:)*pi/180.)

coslattmp = conform_dims(dimsizes(hgt_anom),coslat,0)
ftmp = conform_dims(dimsizes(hgt_anom),f,0)

; magnitude of climatological wind
cumag = sqrt(uwnd_clim^2 + vwnd_clim^2)
cumag@_FillValue = uwnd_clim@_FillValue
cumag = where(cumag .gt. 0, cumag, cumag@_FillValue)

; QG steam function for anomaly
psidev = hgt_anom*ga /ftmp

;dpsidev/dlon
dpsidevdlon =  center_finite_diff_n(psidev,lon*pi/180.,True,0,1)

;ddpsidev/dlonlon
ddpsidevdlonlon =  center_finite_diff_n(dpsidevdlon,lon*pi/180.,True,0,1)

;dpsidev/dlat
dpsidevdlat = center_finite_diff_n(psidev, lat*pi/180., False,0,0)

;ddpsidev/dlonlat
ddpsidevdlonlat =  center_finite_diff_n(dpsidevdlon,lat*pi/180.,False,0,0)

;ddpsidev/dlatdlat
ddpsidevdlatlat = center_finite_diff_n(dpsidevdlat, lat*pi/180.,False,0,0)

xuterm = (dpsidevdlon*dpsidevdlon - psidev*ddpsidevdlonlon)
xvterm = (dpsidevdlon*dpsidevdlat - psidev*ddpsidevdlonlat)
;yuterm = xvterm
yvterm = (dpsidevdlat*dpsidevdlat - psidev*ddpsidevdlatlat)

; Mask out where westerlies is small or negative (less than 5 m/s).
;  by using mask

;x-component of (38)
Fx = mask( mylev/1000./(2.*cumag*re*re)*( uwnd_clim/coslattmp * xuterm + vwnd_clim * xvterm), uwnd_clim.lt.5,False)
;Fx =mylev/1000./(2.*cumag*re*re)*( uwnd_clim/coslattmp * xuterm + vwnd_clim * xvterm)

;y-component 
Fy = mask(mylev/1000./(2.*cumag*re*re)*( uwnd_clim*xvterm + coslattmp*vwnd_clim*yvterm),  uwnd_clim.lt.5,False)
;Fy = mylev/1000./(2.*cumag*re*re)*( uwnd_clim*xvterm + coslattmp*vwnd_clim*yvterm)

; for output
Fx!0 = "lat"
Fx&lat = lat
Fx!1 = "lon"
Fx&lon = lon

Fy!0 = "lat"
Fy&lat = lat
Fy!1 = "lon"
Fy&lon = lon

psidev!0 = "lat"
psidev&lat = lat
psidev!1 = "lon"
psidev&lon = lon

Fx = Fx * 100
Fy = Fy * 100

Fx@units = "m^2/s^2"
Fx@units = "m^2/s^2"

copy_VarMeta(hgt_anom, Fx)
copy_VarMeta(hgt_anom, Fy)
Fx@_FillValue = -32767
Fy@_FillValue = -32767




ref = 0.3
spd = wind_speed(Fx, Fy)
Fx = mask(Fx, spd.lt.ref, False)
Fy = mask(Fy, spd.lt.ref, False)

Fx = mask(Fx, Fx.lt.0, False)


;print(dim_avg_n_Wrap(t(9:15,:,:),0))


wks = gsn_open_wks("png", "./wave-"+level+"-10-16-days")

gsn_define_colormap(wks,"BlueDarkRed18")

res@gsnRightString="14-18 days"

map = gsn_csm_contour_map(wks, hgt_anom, res)

map1 = gsn_csm_vector(wks,Fx,Fy,resvc)

overlay(map,map1)

poli=gsn_add_polyline(wks,map,ftp(:,0),ftp(:,1),resp)

draw(map)
frame(wks)

