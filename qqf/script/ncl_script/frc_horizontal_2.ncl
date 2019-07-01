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
;res@mpMaxLatF=30

;res@mpMinLonF=40
;res@mpMaxLonF=180

res@mpFillOn = False


res@tmXBLabelFontHeightF=0.005
res@tmYLLabelFontHeightF=0.005


resp = True
resp@gsLineColor      = "gray" ; polylines color
resp@gsLineThicknessF = 2    ; polylines thickness
f=asciiread("ncl_script/tibet_shape",(/2066,2/),"float")


wks = gsn_open_wks("png", "frc_horizontal2")


map = gsn_csm_map(wks, res)

poli=gsn_add_polyline(wks,map,f(:,0),f(:,1),resp)


resc=True
resc@gsLineColor      = "black"
resc@gsLineDashPattern = 16
resc@gsLineThicknessF = 3.5
resc@Rotation         = -90

XCNT=296.5
YCNT=28.5
XDIL=15.0
YDIL=7.0

resc@Scale            = XDIL/YDIL

circle_ll(wks,map,XCNT,YCNT,YDIL,resc)
circle_ll(wks,map,XCNT,YCNT,YDIL/1.5,resc)
circle_ll(wks,map,XCNT,YCNT,YDIL/4.,resc)


XCNT2=320.
YCNT2=41.
XDIL2=10.
YDIL2=9.
delete(resc@Scale)
resc@Scale            = XDIL2/YDIL2
resc@gsLineDashPattern = 0

circle_ll(wks,map,XCNT2,YCNT2,YDIL2,resc)
circle_ll(wks,map,XCNT2,YCNT2,YDIL2/1.5,resc)
circle_ll(wks,map,XCNT2,YCNT2,YDIL2/4.,resc)


draw(map)
frame(wks)

