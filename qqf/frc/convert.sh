#!/bin/bash

rm -rf out.nc
cdo -f nc import_binary frc.t42l20.CNP.ctl out.nc
