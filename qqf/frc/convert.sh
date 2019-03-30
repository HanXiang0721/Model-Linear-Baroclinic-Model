#!/bin/bash

rm -rf frc.nc
cdo -f nc import_binary frc.t42l20.CNP.ctl frc.nc
