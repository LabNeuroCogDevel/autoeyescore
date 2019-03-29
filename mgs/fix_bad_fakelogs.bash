#!/usr/bin/env bash

# fakeLog.pl failed if eye data started with 0s. created a bad first line with no expected xdat. this remove sthe bad

grep -lP "^1.925\tMGSTarget\t\t0" /Volumes/L/bea_res//Data/Tasks/MGS/Basic/1*/2*/Raw/EyeData/txt/*.MGS.EPxdat.log|xargs  rm
./00_eyd.bash
