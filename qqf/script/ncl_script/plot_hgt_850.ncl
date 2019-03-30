load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/calendar_decode2.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

datafile = addfile("../out/linear.t42l20.qqf.nc","r")

f_time = datafile->time
f_lat  = datafile->lat
f_lon  = datafile->lon
f_lev  = datafile->lev
f_z    = datafile->z
f_u    = datafile->u
f_v    = datafile->v

level = 850
mylev = ind(f_lev.eq.level)
mylat = ind(f_lat.ge.-30.and.f_lat.le.90)

z = f_z(:,mylev,mylat,:)
u = f_u(:,mylev,mylat,:)
v = f_v(:,mylev,mylat,:)

ntime = dimsizes(f_time)

res = True
res@gsnDraw=False
res@gsnFrame=False
res@gsnSpreadColors=True
res@gsnAddCyclic=True


res@cnFillOn=True
res@cnLinesOn=False
;res@cnLevelSelectionMode = "ExplicitLevels"
;res@cnLevels=(/-40,-30,-20,-10,0,10,20,30,40/)
;res@cnLevels=(/-16,-8,-4,-2,0,2,4,8,16/)
;;;res@cnLevels=(/-140,-105,-70,-35,0,35,70,105,140/)  ;(/-4,-3,-2,-1,0,1,2,3,4/)
;res@cnLevels=(/-20,-15,-10,-5,0,5,10,15,20/)
;res@cnLevels=(/-80,-60,-40,-20,0,20,40,60,80/)
;res@cnFillColors=(/2,4,6,8,0,0,13,15,17,19/)


res@lbOrientation="Vertical"

res@gsnMaximize=True
res@vpWidthF      = 0.4
res@vpHeightF     = 0.6

res@gsnLeftString=""
res@gsnRightString=""
res@tiMainString=""

res@mpCenterLonF=180
res@mpMinLatF=f_lat(mylat(dimsizes(mylat)-1))
res@mpMaxLatF=f_lat(mylat(0))

res@tmXBLabelFontHeightF=0.005
res@tmYLLabelFontHeightF=0.005
res@gsnRightStringFontHeightF=8


resp = True
resp@gsLineColor      = "gray" ; polylines color
resp@gsLineThicknessF = 2    ; polylines thickness
f=asciiread("ncl_script/tibet_shape",(/2066,2/),"float")


resvc                  = True                      ; plot mods desired
resvc@gsnDraw=False
resvc@gsnFrame=False

resvc@gsnLeftString=""
resvc@gsnRightString=""

resvc@vcMinDistanceF=0.018

resvc@vcRefMagnitudeF  = 2
resvc@vcRefLengthF     = 0.015

resvc@vcGlyphStyle="LineArrow"
resvc@vcLineArrowThicknessF=2.6

resvc@vcRefAnnoOn               = True
resvc@vcRefAnnoString1          = "2"
resvc@vcRefAnnoSide             = "Top"
resvc@vcRefAnnoString2On        = False
resvc@vcRefAnnoPerimOn          = False
resvc@vcRefAnnoOrthogonalPosF   = -0.44
resvc@vcRefAnnoParallelPosF     = 0.9999
resvc@vcRefAnnoBackgroundColor  = "green"
resvc@vcVectorDrawOrder         = "PostDraw"

;spd = wind_speed(u,v)
;u = mask(u, spd.le.0.1, False)
;v = mask(v, spd.le.0.1, False)

do ii = 0, ntime-2


if(ii.le.8) then
	wks = gsn_open_wks("png", "./pics/HGT/HGT-"+level+"-0"+(ii+1))
else
	wks = gsn_open_wks("png", "./pics/HGT/HGT-"+level+"-"+(ii+1))
end if

gsn_define_colormap(wks,"BlueDarkRed18")

res@gsnRightString=(ii+1) + " days"

map = gsn_csm_contour_map(wks, z(ii,:,:), res)

map1 = gsn_csm_vector(wks,u(ii,:,:),v(ii,:,:),resvc)

overlay(map,map1)

poli=gsn_add_polyline(wks,map,f(:,0),f(:,1),resp)

draw(map)
frame(wks)

end do
