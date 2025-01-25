#!/usr/bin/env Rscript
#nohup Rscript --vanilla "analysis/scripts/sensitivityMinimalTest.R" 1 &
n_reps <- 1

# Run batches from Rscript that uses parallel backend and new caribouMetrics functions
args <- commandArgs(trailingOnly = TRUE)
cpageId <- args[1]

cDir = getwd()

setName = args[2]

library(caribouMetrics)
# setName="s10"; cpageId <- 1;n_reps <- 1;cDir = "C:/Users/HughesJo/Documents/gitprojects/Caribou-Demographic-Projection-Paper"


#######################
dir.create(paste0(cDir,"/figs/",setName),recursive=T)
dir.create(paste0(cDir,"/tabs/",setName),recursive=T)
dir.create(paste0(cDir,"/results/",setName),recursive=T)


simBig<-getSimsNational() #If called with default parameters, use saved object to speed things up.

allScns = read.csv(paste0(cDir,"/tabs/",setName,".csv"))
unique(allScns$pageId)
####################
eParsIn = list()
eParsIn$cowCounts <- data.frame(Year = 1981:2023,
                                Count = 100,
                                Class = "cow")
eParsIn$freqStartsByYear <- data.frame(Year = 1981:2023,
                                       numStarts = 30)
eParsIn$collarOnTime=1
eParsIn$collarOffTime=12
eParsIn$collarNumYears=6

scns = subset(allScns, pageId==cpageId)

str(scns)

#scns$zMin= 0; scns$zMax = 0
#scns$uMin = 0; scns$uMax = 0
#scns$qMin = 0; scns$qMax = 0
message("batch ", cpageId, " started")

str(scns)
#scns=subset(scns,obsYears==2)

if(n_reps=="all"){
  scResults = caribouMetrics:::runScnSet(scns,eParsIn,simBig,getKSDists=F,printProgress=F)
}else{
  scResults = caribouMetrics:::runScnSet(scns[1:n_reps,],eParsIn,simBig,getKSDists=F,printProgress=F)
  # scResults = runScnSet(scns[10:10,],eParsIn,simBig,getKSDists=F,printProgress=F)
}

unique(scResults$rr.summary.all$Parameter)
saveRDS(scResults,paste0("results/",setName,"/rTest",cpageId,n_reps,cpageId,".Rds"))

message("batch ", cpageId, " complete")
