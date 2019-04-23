load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/calendar_decode2.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

res = True
res@gsnMaximize=True
res@gsnDraw = False
res@gsnFrame = False
res@vpWidthF      = 0.4
res@vpHeightF     = 0.6

res@gsnLeftString=""
res@gsnRightString=""
res@tiMainString=""

res@mpCenterLonF=180
res@mpMinLatF=0
;res@mpMaxLatF=46

;res@mpMinLonF=62
;res@mpMaxLonF=108

res@mpFillOn = False


res@tmXBLabelFontHeightF=0.005
res@tmYLLabelFontHeightF=0.005


resp = True
resp@gsLineColor      = "gray" ; polylines color
resp@gsLineThicknessF = 2    ; polylines thickness
f=asciiread("ncl_script/tibet_shape",(/2066,2/),"float")


wks = gsn_open_wks("png", "frc_horizontal")


map = gsn_csm_map(wks, res)

poli=gsn_add_polyline(wks,map,f(:,0),f(:,1),resp)


resc=True
resc@gsLineColor      = "black"
resc@gsLineDashPattern = 16
resc@gsLineThicknessF = 3.5
resc@Rotation         = -90

XCNT=133
YCNT=42
XDIL=10
YDIL=10

resc@Scale            = XDIL/YDIL

circle_ll(wks,map,XCNT,YCNT,YDIL,resc)
circle_ll(wks,map,XCNT,YCNT,YDIL/1.5,resc)
circle_ll(wks,map,XCNT,YCNT,YDIL/4.,resc)

draw(map)
frame(wks)

