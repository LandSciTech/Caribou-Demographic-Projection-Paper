#!/bin/bash
#############################################
#####Run R script##################
echo "updating repo code"
cd /Caribou-Demographic-Projection-Paper
git pull https://<PAT>@github.com/LandSciTech/Caribou-Demographic-Projection-Paper.git BbouIntegration

apt install libcurl4-openssl-dev
apt install libfreetype6-dev
apt install libpng-dev
apt install libtiff5-dev
apt install libjpeg-dev

echo "Running the scripts"
nohup Rscript --vanilla "make.R" <PAT>
nohup Rscript --vanilla "analysis/scripts/sensitivityMinimalTest.R" <batch> <setName> > $AZ_BATCH_TASK_WORKING_DIR/nohup_<batch>.out 2>&1
