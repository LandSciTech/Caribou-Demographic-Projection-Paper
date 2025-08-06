#!/usr/bin/env Rscript

# Run batches from Rscript that uses parallel backend and new caribouMetrics functions
args <- commandArgs(trailingOnly = TRUE)
cpageId <- args[1]

cDir = getwd()

library(caribouMetrics)

# cpageId <- 31;n_reps <- 1;cDir = "C:/Users/HughesJo/Documents/gitprojects/Caribou-Demographic-Projection-Paper"

setName = args[2]

#######################
dir.create(paste0("figs/",setName),recursive=T)
dir.create(paste0("tabs/",setName),recursive=T)
dir.create(paste0("results/",setName),recursive=T)

allScns = read.csv(paste0("tabs/",setName,".csv"))

simBig<-getSimsInitial(cPars=allScns,forceUpdate = T) #If called with default parameters, use saved object to speed things up.

####################
eParsIn = list()
eParsIn$collarOnTime=4
eParsIn$collarOffTime=4
eParsIn$collarNumYears=1

scns = subset(allScns, pageId==cpageId)

rm(allScns)

message("batch ", cpageId, " started")

scResults = caribouMetrics:::runScnSet(scns,simBig,eParsIn,printProgress=T)

saveRDS(scResults,paste0("results/",setName,"/rTest",cpageId,".Rds"))

message("batch ", cpageId, " complete")

