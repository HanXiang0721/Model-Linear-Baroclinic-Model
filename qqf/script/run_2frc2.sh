#!/bin/bash

./clean.sh

./compile.csh

./mk_2frc_bs2.sh

./t42-run.csh

./plot_frc_222.sh

./plot_hgt.sh
