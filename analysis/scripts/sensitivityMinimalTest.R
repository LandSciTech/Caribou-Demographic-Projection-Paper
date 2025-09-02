#!/usr/bin/env Rscript
#nohup Rscript --vanilla "analysis/scripts/sensitivityMinimalTest.R" 1 &
n_reps <- 1

# Run batches from Rscript that uses parallel backend and new caribouMetrics functions
args <- commandArgs(trailingOnly = TRUE)
cpageId <- args[1]

cDir = getwd()

setName = args[2]

library(caribouMetrics)
# devtools::load_all(path = "../caribouMetrics/")
# setName="s2"; cpageId <- 1;n_reps <- "all";niters<-10; cDir = "C:/Users/HughesJo/Documents/gitprojects/Caribou-Demographic-Projection-Paper"

niters = 1000

#######################
dir.create(paste0(cDir,"/figs/",setName),recursive=T)
dir.create(paste0(cDir,"/tabs/",setName),recursive=T)
dir.create(paste0(cDir,"/results/",setName),recursive=T)

allScns = read.csv(paste0(cDir,"/tabs/",setName,".csv"))

simBig<-getSimsInitial(replicates=500,cPars=allScns,forceUpdate = T) #If called with default parameters, use saved object to speed things up.

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
  scResults = caribouMetrics:::runScnSet(scns,simBig,eParsIn,printProgress=T,niters=niters)
}else{
  #devtools::load_all(path = "../caribouMetrics/")
  #n_reps=37
  scResults = caribouMetrics:::runScnSet(scns[n_reps:n_reps,],simBig,eParsIn,printProgress=T,niters=niters)
}

saveRDS(scResults,paste0(cDir,"/results/",setName,"/rTest",cpageId,n_reps,cpageId,".Rds"))

message("batch ", cpageId, " complete")
