#!/usr/bin/env Rscript
#nohup Rscript --vanilla "analysis/scripts/sensitivityMinimalTest.R" 1 &
n_reps <- 1

# Run batches from Rscript that uses parallel backend and new caribouMetrics functions
args <- commandArgs(trailingOnly = TRUE)
cpageId <- args[1]

cDir = getwd()

setName = args[2]
niters = 1000

library(caribouMetrics)
# TO DO: remove dependency on CaribouDemographyBasicApp - make caribouMetrics sufficient.
# devtools::load_all(path = "../caribouMetrics/")
# devtools::load_all(path = "../CaribouDemographyBasicApp/")
# setName="s1"; cpageId <- 1;n_reps <- "all";niters<-10; cDir = "C:/Users/HughesJo/Documents/gitprojects/Caribou-Demographic-Projection-Paper"

#######################
dir.create(paste0(cDir,"/figs/",setName),recursive=T)
dir.create(paste0(cDir,"/tabs/",setName),recursive=T)
dir.create(paste0(cDir,"/results/",setName),recursive=T)

simBig<-getSimsInitial(replicates=500) #If called with default parameters, use saved object to speed things up.

allScns = read.csv(paste0(cDir,"/tabs/",setName,".csv"))

allScns

unique(allScns$pageId)
####################
eParsIn = list()
eParsIn$collarOnTime=4
eParsIn$collarOffTime=4
eParsIn$collarNumYears=1

scns = subset(allScns, pageId==cpageId)

#scns$zMin= 0; scns$zMax = 0
#scns$uMin = 0; scns$uMax = 0
#scns$qMin = 0; scns$qMax = 0
message("batch ", cpageId, " started")

if(n_reps=="all"){
  #devtools::load_all(path = "../caribouMetrics/")
  scResults = caribouMetrics:::runScnSet(scns,eParsIn,simBig,omitInterannual=T,printProgress=T,niters=niters)
}else{
  #devtools::load_all(path = "../caribouMetrics/")
  scResults = caribouMetrics:::runScnSet(scns[1:n_reps,],eParsIn,simBig,omitInterannual=T,printProgress=T,niters=niters)
}

unique(scResults$rr.summary.all$Parameter)
saveRDS(scResults,paste0(cDir,"/results/",setName,"/rTest",cpageId,n_reps,cpageId,".Rds"))

message("batch ", cpageId, " complete")
