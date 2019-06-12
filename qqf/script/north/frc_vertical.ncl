load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/calendar_decode2.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"


lev = (/0.995, 0.97999, 0.94995, 0.89988, 0.82977, 0.74468, 0.64954, 0.54946,0.45447, 0.36948, 0.2945, 0.22953, 0.17457, 0.1244, 0.084683, 0.0598005,0.0449337, 0.0349146, 0.02488, 0.00829901/)

VAMP=2.0
VDIL=9.
VCNT=0.25

profile = VAMP * exp( -VDIL * (lev - VCNT)^2 ) + 0.34



wks = gsn_open_wks("png","vertical")

res                        = True                        ; plot mods desired
res@tiYAxisString          = ""   ; add units to title
res@tiMainString          = ""      ; title
res@xyLineThicknessF = 4
res@vpHeightF          = 0.35
res@vpWidthF           = 0.20
res@gsnMaximize=True
res@trYReverse = True
res@xyMarker = 4
res@xyMarkLineMode="MarkLines"
res@xyMarkerColor = "black"
res@xyMarkerSizes = 5
res@trXMinF=0

plot=gsn_csm_xy(wks,profile,lev,res)

asciiwrite("ideal.txt", profile)
