#!/bin/bash

./clean.sh

./compile.csh

./mk_2frc_bs.sh

./t42-run.csh

./plot_frc_2.sh

./plot_hgt.sh
