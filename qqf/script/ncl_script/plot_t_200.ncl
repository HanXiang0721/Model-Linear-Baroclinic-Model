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
f_t    = datafile->t
f_u    = datafile->u
f_v    = datafile->v

level = 200
mylev = ind(f_lev.eq.level)
mylat = ind(f_lat.ge.0.and.f_lat.le.90)

z = f_z(:,mylev,mylat,:)
u = f_u(:,mylev,mylat,:)
v = f_v(:,mylev,mylat,:)
t = f_t(:,mylev,mylat,:)

ntime = dimsizes(f_time)

res = True
res@gsnDraw=False
res@gsnFrame=False
res@gsnSpreadColors=True
res@gsnAddCyclic=True


res@cnFillOn=True
res@cnLinesOn=False
res@cnLevelSelectionMode = "ExplicitLevels"
res@cnLevels=(/-2,-1.5,-1,-0.5,0,0.5,1,1.5,2/)
res@cnFillColors=(/2,4,6,8,0,0,13,15,17,19/)


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

resvc@vcRefMagnitudeF  = 0.2 
resvc@vcRefLengthF     = 0.015

resvc@vcGlyphStyle="LineArrow"
resvc@vcLineArrowThicknessF=2.6

resvc@vcRefAnnoOn               = True
resvc@vcRefAnnoString1          = "0.2"
resvc@vcRefAnnoSide             = "Top"
resvc@vcRefAnnoString2On        = False
resvc@vcRefAnnoPerimOn          = False
resvc@vcRefAnnoOrthogonalPosF   = -0.44
resvc@vcRefAnnoParallelPosF     = 0.9999
resvc@vcRefAnnoBackgroundColor  = "green"
resvc@vcVectorDrawOrder         = "PostDraw"




resT = True
resT@gsnDraw=False
resT@gsnFrame=False
resT@gsnSpreadColors=True
resT@gsnAddCyclic=True


resT@cnFillOn=False
resT@cnLinesOn=True
resT@cnLevelSelectionMode = "ExplicitLevels"
resT@cnLevels=(/-0.08,-0.06,-0.04,-0.02,0.02,0.04,0.06,0.08/)

resT@cnLineLabelFontHeightF=0.007

resT@gsnMaximize=True

resT@gsnLeftString=""
resT@gsnRightString=""
resT@tiMainString=""

resT@cnInfoLabelOn=False

resT@cnLineThicknesses=8

;print(dim_avg_n_Wrap(t(9:15,:,:),0))


wks = gsn_open_wks("png", "./pics/T-"+level+"-10-16-days")

gsn_define_colormap(wks,"BlueDarkRed18")

res@gsnRightString="10-16 days"

map = gsn_csm_contour_map(wks, dim_avg_n_Wrap(z(9:15,:,:),0), res)

map1 = gsn_csm_vector(wks,dim_avg_n_Wrap(u(9:15,:,:),0),dim_avg_n_Wrap(v(9:15,:,:),0),resvc)

map2 = gsn_csm_contour(wks,dim_avg_n_Wrap(t(9:15,:,:),0),resT)

map2 = ColorNegDashZeroPosContour(map2,"red","red","red")

overlay(map,map1)
overlay(map,map2)

poli=gsn_add_polyline(wks,map,f(:,0),f(:,1),resp)

draw(map)
frame(wks)

