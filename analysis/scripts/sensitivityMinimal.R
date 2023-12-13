#!/usr/bin/env Rscript

# Run batches from Rscript that uses parallel backend and new caribouMetrics functions
cpageId <- commandArgs(trailingOnly = TRUE)
# cpageId <- 1
#setwd("C:/Users/HughesJo/Documents/gitprojects/Caribou-Demographic-Projection-Paper")
library(caribouMetrics)

setName = "s7"

#######################
dir.create(paste0("figs/",setName),recursive=T)
dir.create(paste0("tabs/",setName),recursive=T)
dir.create(paste0("results/",setName),recursive=T)


simBig<-getSimsNational() #If called with default parameters, use saved object to speed things up.

allScns = read.csv(paste0("tabs/",setName,".csv"))

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

rm(allScns)

message("batch ", cpageId, " started")

scResults = caribouMetrics:::runScnSet(scns,eParsIn,simBig,getKSDists=F,printProgress=T)

saveRDS(scResults,paste0("results/",setName,"/rTest",cpageId,".Rds"))

message("batch ", cpageId, " complete")

