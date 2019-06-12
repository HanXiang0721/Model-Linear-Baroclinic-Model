load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/calendar_decode2.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

datafile = addfile("./VORTEX.nc","r")

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

level = 500
mylev = ind(f_lev.eq.level)
mylat = ind(f_lat.ge.0.and.f_lat.le.90)

z = f_z(:,mylev,:,:)
u = f_u(:,mylev,:,:)
v = f_v(:,mylev,:,:)
t = f_t(:,mylev,:,:)

ntime = dimsizes(f_time)


lat = f_lat
lon = f_lon

hgt_anom = dim_avg_n_Wrap(z(15:20,:,:),0)
;print(hgt_anom)

res = True
res@gsnDraw=False
res@gsnFrame=False
res@gsnSpreadColors=True
res@gsnAddCyclic=True


res@cnFillOn=True
res@cnLinesOn=False
res@cnLevelSelectionMode = "ExplicitLevels"
res@cnLevels=(/-4,-3,-2,-1,0,1,2,3,4/)
res@cnFillColors=(/2,4,6,8,0,0,13,15,17,19/)


res@lbOrientation="Vertical"

res@gsnMaximize=True
res@vpWidthF      = 0.4
res@vpHeightF     = 0.6

res@gsnLeftString=""
res@gsnRightString=""
res@tiMainString=""

res@mpCenterLonF=180
res@mpLimitMode = "LatLon" 
res@mpMinLatF = 10 
res@mpMaxLatF = 46.5 
res@mpMinLonF = 50 
res@mpMaxLonF = 120


res@tmXBLabelFontHeightF=0.005
res@tmYLLabelFontHeightF=0.005
res@gsnRightStringFontHeightF=8


resp = True
resp@gsLineColor      = "gray" ; polylines color
resp@gsLineThicknessF = 2    ; polylines thickness
ftp=asciiread("./tibet_shape",(/2066,2/),"float")




wks = gsn_open_wks("eps", "./wave-"+level+"-10-16-days")

gsn_define_colormap(wks,"BlueDarkRed18")

res@gsnRightString="14-18 days"

map = gsn_csm_contour_map(wks, hgt_anom, res)

poli=gsn_add_polyline(wks,map,ftp(:,0),ftp(:,1),resp)

draw(map)
frame(wks)

