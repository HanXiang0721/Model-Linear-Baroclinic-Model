#!/bin/bash

rm -rf bs.nc
cdo -f nc import_binary qqf.t42l20.ctl bs.nc
