#!/bin/bash
#############################################
#####Run R script##################
echo "updating repo code"
cd /Caribou-Demographic-Projection-Paper
git pull https://<PAT>@github.com/LandSciTech/Caribou-Demographic-Projection-Paper.git BbouIntegration

echo "Running the scripts"
nohup Rscript --vanilla "make.R" <PAT>
nohup Rscript --vanilla "analysis/scripts/sensitivityMinimalTest.R" <batch> <setName> > $AZ_BATCH_TASK_WORKING_DIR/nohup_<batch>.out 2>&1
