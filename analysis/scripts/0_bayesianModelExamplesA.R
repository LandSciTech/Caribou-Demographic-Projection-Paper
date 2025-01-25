# Prior and posterior predictions from Bayesian model
library(caribouMetrics)
library(tidyverse)
library(ggpubr)
theme_set(theme_bw())
library(RColorBrewer)

baseDir <- "."

monitoringScns = data.frame(obsYears=c(1,16),collarCount=c(0,30),cowMult=c(6),collarInterval=c(1),
                            assessmentYrs=c(1))
stateScns = data.frame(obsAnthroSlope=c(2),projAnthroSlope=c(2))
stateScns = merge(stateScns,data.frame(rep=seq(1:1)))
#stateScns = merge(stateScns,data.frame(interannualVar=c("list(R_CV=0.46,S_CV=0.087)")))
stateScns = merge(stateScns,data.frame(interannualVar=c("list(R_CV=0.23,S_CV=0.0435)")))
#stateScns = merge(stateScns,data.frame(interannualVar=c(NA)))
stateScns = merge(stateScns,data.frame(survivalModelNumber = c("M1","M0"), recruitmentModelNumber=c("M4","M0"),
                                       rAnthroSlopeSE = c(0.006,0.06),rFireSlopeSE = c(0.002,0.02),
                                       sAnthroSlopeSE = c(0.0005,0.005),sIntSE = c(0.06,0.6),
                                       rIntSE = c(0.35,2.5),sQuantile=c(0.9,0.9),rQuantile=c(0.9,0.9)))

scns=merge(monitoringScns,stateScns)

scns = subset(scns,!(grepl("2.5",rIntSE,fixed=T)&(collarCount==0)))
scns$iAnthro = 0
scns$tA = scns$iAnthro+(scns$obsYears)*scns$obsAnthroSlope
scns$projYears = 50-scns$obsYears
scns$N0 = 2000

scns

#scns$assessmentYrs=3

bounds = list(recHigh=1,survLow=0,lamLow=0.4)

####################
eParsIn = list()
eParsIn$cowCounts <- data.frame(Year = 1981:2023,
                                Count = 100,
                                Class = "cow")
eParsIn$freqStartsByYear <- data.frame(Year = 1981:2023,
                                       numStarts = 30)
eParsIn$collarOnTime=1
eParsIn$collarOffTime=12
eParsIn$collarNumYears=1
labFontSize = 10; breakInterval=5

###############################
#With bias adjustment
simBig<-getSimsNational(replicates=3000,forceUpdate=T)

priorResult = caribouMetrics:::runScnSet(scns[1,],eParsIn,simBig,getKSDists=F,printProgress=F)
priorResult$obs.all=NULL
yr_scale1 <- scale_x_continuous(breaks = 2023:2072 %>% .[0:9*5+1],
                                labels = 2009:2058 %>% .[0:9*5+1] )
yr_scale2 <- scale_x_continuous(breaks = 2009:2058 %>% .[0:9*5+1],
                                labels = 2009:2058 %>% .[0:9*5+1] )
recPrior =  plotRes(priorResult, "Recruitment", lowBound=0, highBound = bounds$recHigh,
                    legendPosition="none",breakInterval=breakInterval,
                    labFontSize=labFontSize) +
  yr_scale1 +
  labs(tag = "a")
plot(recPrior)
survPrior =  plotRes(priorResult, "Adult female survival", lowBound=bounds$survLow,
                     legendPosition="none",breakInterval=breakInterval,
                     labFontSize=labFontSize)+
  yr_scale1
plot(survPrior)
lambdaPrior =  plotRes(priorResult, "Population growth rate", lowBound=bounds$lamLow,
                       legendPosition="none",breakInterval=breakInterval,
                       labFontSize=labFontSize)+
  yr_scale1 +
  ylim(c(0, 1.8))
plot(lambdaPrior)

posteriorResult = caribouMetrics:::runScnSet(scns[3,],eParsIn,simBig,getKSDists=F,printProgress=F)
recPosterior =  plotRes(posteriorResult, "Recruitment", lowBound=0,highBound = bounds$recHigh,
                        legendPosition="none",breakInterval=breakInterval,
                        labFontSize=labFontSize)+
  yr_scale2 +
  labs(tag = "c")

plot(recPosterior)

survPosterior =  plotRes(posteriorResult, "Adult female survival", lowBound=bounds$survLow,
                         legendPosition="none",breakInterval=breakInterval,
                         labFontSize=labFontSize)+
  yr_scale2
plot(survPosterior)
lambdaPosterior =  plotRes(posteriorResult, "Population growth rate", lowBound=bounds$lamLow,
                           legendPosition="none",breakInterval=breakInterval,
                           labFontSize=labFontSize)+
  yr_scale2 +
  ylim(c(0, 1.8))
plot(lambdaPosterior)

posteriorResultB = caribouMetrics:::runScnSet(scns[2,],eParsIn,simBig,getKSDists=F,printProgress=F)
recPosteriorB =  plotRes(posteriorResultB, "Recruitment", lowBound=0,highBound = bounds$recHigh,
                        legendPosition="none",breakInterval=breakInterval,
                        labFontSize=labFontSize)+
  yr_scale2 +
  labs(tag = "b")
plot(recPosteriorB)

ggsave(paste0(baseDir,"/analysis/paper/figs/bayesianModelExamplesRecBOnly.png"),
       width = 3, height = 3, units = "in",
       dpi = 1200)

survPosteriorB =  plotRes(posteriorResultB, "Adult female survival", lowBound=bounds$survLow,
                         legendPosition="none",breakInterval=breakInterval,
                         labFontSize=labFontSize)+
  yr_scale2
plot(survPosteriorB)
lambdaPosteriorB =  plotRes(posteriorResultB, "Population growth rate", lowBound=bounds$lamLow,
                           legendPosition="none",breakInterval=breakInterval,
                           labFontSize=labFontSize)+
  yr_scale2 +
  ylim(c(0, 1.8))
plot(lambdaPosteriorB)


leg <- plotRes(posteriorResult, "Recruitment", lowBound=0,highBound = 0.85,
               legendPosition="left",breakInterval=breakInterval,labFontSize=labFontSize)
leg <- ggpubr::get_legend(leg)

# combine ggplots to one figure
plts <- ggpubr::ggarrange(recPrior, survPrior, lambdaPrior,
                          recPosteriorB, survPosteriorB,lambdaPosteriorB,
                          recPosterior, survPosterior, lambdaPosterior,
                          labels = "",
                          ncol = 3, nrow = 3, vjust = 1)
ggpubr::ggarrange(plts, leg, ncol = 2, widths = c(6,1),heights=0.75)+bgcolor("white")

ggsave(paste0(baseDir,"/analysis/paper/figs/bayesianModelExamples.png"),
       width = 9.6*0.779, height = 9.2, units = "in",
       dpi = 1200)

ggsave(paste0(baseDir,"/analysis/paper/figs_submit/bayesianModelExamples.pdf"),
       width = 9.6*0.779, height = 9.2, units = "in",
       dpi = 1200)
###############################
#Without bias adjustment

biasPars = data.frame(qMax=0,uMax=0,zMax=0)
simBig<-getSimsNational(replicates=3000,forceUpdate=T,cPars=getScenarioDefaults(paramTable=biasPars))

scns=merge(subset(scns,select=setdiff(names(scns),names(biasPars))),biasPars)
priorResult = caribouMetrics:::runScnSet(scns[1,],eParsIn,simBig,getKSDists=F,printProgress=F)
head(priorResult$obs.all)

priorResult$obs.all=NULL

yr_scale1 <- scale_x_continuous(breaks = 2023:2072 %>% .[0:9*5+1],
                                labels = 2009:2058 %>% .[0:9*5+1] )
yr_scale2 <- scale_x_continuous(breaks = 2009:2058 %>% .[0:9*5+1],
                                labels = 2009:2058 %>% .[0:9*5+1] )
recPrior =  plotRes(priorResult, "Recruitment", lowBound=0, highBound = bounds$recHigh,
                    legendPosition="none",breakInterval=breakInterval,
                    labFontSize=labFontSize) +
  yr_scale1 +
  labs(tag = "a")
plot(recPrior)
survPrior =  plotRes(priorResult, "Adult female survival", lowBound=bounds$survLow,
                     legendPosition="none",breakInterval=breakInterval,
                     labFontSize=labFontSize)+
  yr_scale1
plot(survPrior)
lambdaPrior =  plotRes(priorResult, "Population growth rate", lowBound=bounds$lamLow,
                       legendPosition="none",breakInterval=breakInterval,
                       labFontSize=labFontSize)+
  yr_scale1 +
  ylim(c(0, 1.8))
plot(lambdaPrior)

posteriorResult = caribouMetrics:::runScnSet(scns[3,],eParsIn,simBig,getKSDists=F,printProgress=F)
recPosterior =  plotRes(posteriorResult, "Recruitment", lowBound=0,highBound = bounds$recHigh,
                        legendPosition="none",breakInterval=breakInterval,
                        labFontSize=labFontSize)+
  yr_scale2 +
  labs(tag = "c")
plot(recPosterior)
survPosterior =  plotRes(posteriorResult, "Adult female survival", lowBound=bounds$survLow,
                         legendPosition="none",breakInterval=breakInterval,
                         labFontSize=labFontSize)+
  yr_scale2
plot(survPosterior)
lambdaPosterior =  plotRes(posteriorResult, "Population growth rate", lowBound=bounds$lamLow,
                           legendPosition="none",breakInterval=breakInterval,
                           labFontSize=labFontSize)+
  yr_scale2 +
  ylim(c(0, 1.8))
plot(lambdaPosterior)

posteriorResultB = caribouMetrics:::runScnSet(scns[2,],eParsIn,simBig,getKSDists=F,printProgress=F)
recPosteriorB =  plotRes(posteriorResultB, "Recruitment", lowBound=0,highBound = bounds$recHigh,
                        legendPosition="none",breakInterval=breakInterval,
                        labFontSize=labFontSize)+
  yr_scale2 +
  labs(tag = "b")
plot(recPosteriorB)
survPosteriorB =  plotRes(posteriorResultB, "Adult female survival", lowBound=bounds$survLow,
                         legendPosition="none",breakInterval=breakInterval,
                         labFontSize=labFontSize)+
  yr_scale2
plot(survPosteriorB)
lambdaPosteriorB =  plotRes(posteriorResultB, "Population growth rate", lowBound=bounds$lamLow,
                           legendPosition="none",breakInterval=breakInterval,
                           labFontSize=labFontSize)+
  yr_scale2 +
  ylim(c(0, 1.8))
plot(lambdaPosteriorB)

leg <- plotRes(posteriorResult, "Recruitment", lowBound=0,highBound = bounds$recHigh,
               legendPosition="left",breakInterval=breakInterval,labFontSize=labFontSize)
leg <- ggpubr::get_legend(leg)

# combine ggplots to one figure
plts <- ggpubr::ggarrange(recPrior, survPrior, lambdaPrior,
                          recPosteriorB, survPosteriorB, lambdaPosteriorB,
                          recPosterior, survPosterior, lambdaPosterior,
                          labels = "",
                          ncol = 3, nrow = 3, vjust = 1)
ggpubr::ggarrange(plts, leg, ncol = 2, widths = c(6,1),heights=0.75)+bgcolor("white")

ggsave(paste0(baseDir,"/analysis/paper/figs/bayesianModelExamplesNoBias.png"),
       width = 9.6*0.779, height = 9.2, units = "in",
       dpi = 1200)

###############################
#Sensitivity of lambda to bias par adjustment

#Interpretation - uncertainty is so high that the additional uncertainty associated with composition survey biases has little noticable effect on overall uncertainty.
#However, impacts on mean are noticeable.
biasPars = expand.grid(uMax=c(0,0.2,0.4),zMax=c(0,0.2,0.4))
#biasPars$uMin= biasPars$uMax; biasPars$zMin=biasPars$zMax
#biasPars = expand.grid(zMax=c(0.2,0.4),uMax=c(0))
#biasPars$zMin[biasPars$zMax==0.2]= 0.2
#biasPars$zMin[biasPars$zMax==0.4]= 0

scnsN=merge(subset(scns[1,],select=setdiff(names(scns),names(biasPars))),biasPars)
scnsN$projYears = 30
priorResult = caribouMetrics:::runScnSet(scnsN,eParsIn,simBig,getKSDists=F,printProgress=F)

priorResult$obs.all=NULL; priorResult$sim.all=NULL


yr_scale1 <- scale_x_continuous(breaks = 2023:2072 %>% .[0:9*5+1],
                                labels = 2009:2058 %>% .[0:9*5+1] )
yr_scale2 <- scale_x_continuous(breaks = 2009:2058 %>% .[0:9*5+1],
                                labels = 2009:2058 %>% .[0:9*5+1] )
lambdaPrior1 =  plotRes(priorResult, "Population growth rate", lowBound=0,
                       legendPosition="none",breakInterval=breakInterval,
                       labFontSize=labFontSize,facetVars=c("uMax","zMax"))+
  yr_scale1 +
  ylim(c(0.75, 1.5))

plot(lambdaPrior1)

ggsave(paste0(baseDir,"/analysis/paper/figs/bayesianModelBiasSensitivity.png"),
       width = 12*0.8, height = 3.6*2, units = "in",
       dpi = 1200)

###################################
#Low effort scenario
monitoringScns = data.frame(obsYears=c(1,4),collarCount=c(0,15),cowMult=c(3),collarInterval=c(1),
                            assessmentYrs=c(1))
stateScns = data.frame(obsAnthroSlope=c(2),projAnthroSlope=c(2))
stateScns = merge(stateScns,data.frame(rep=seq(1:1)))
stateScns = merge(stateScns,data.frame(interannualVar=c("list(R_CV=0.46,S_CV=0.087)")))
#stateScns = merge(stateScns,data.frame(interannualVar=c("list(R_CV=0.23,S_CV=0.0435)")))
stateScns = merge(stateScns,data.frame(survivalModelNumber = c("M1","M0"), recruitmentModelNumber=c("M4","M0"),
                                       rAnthroSlopeSE = c(0.006,0.06),rFireSlopeSE = c(0.002,0.02),
                                       sAnthroSlopeSE = c(0.0005,0.005),sIntSE = c(0.04,0.4),
                                       rIntSE = c(0.25,2.5),sQuantile=c(0.3,0.3),rQuantile=c(0.1,0.1)))

scns=merge(monitoringScns,stateScns)

scns = subset(scns,!(grepl("2.5",rIntSE,fixed=T)&(collarCount==0)))
scns$iAnthro = 0
scns$tA = scns$iAnthro+(scns$obsYears)*scns$obsAnthroSlope
scns$projYears = 50-scns$obsYears
scns$N0 = 2000

scns
posteriorResult = caribouMetrics:::runScnSet(scns[3,],eParsIn,simBig,getKSDists=F,printProgress=F)
recPosterior =  plotRes(posteriorResult, "Recruitment", lowBound=0,highBound = bounds$recHigh,
                        legendPosition="none",breakInterval=breakInterval,
                        labFontSize=labFontSize)+
  yr_scale2 +
  labs(tag = "b")

plot(recPosterior)

survPosterior =  plotRes(posteriorResult, "Adult female survival", lowBound=bounds$survLow,
                         legendPosition="none",breakInterval=breakInterval,
                         labFontSize=labFontSize)+
  yr_scale2
plot(survPosterior)
lambdaPosterior =  plotRes(posteriorResult, "Population growth rate", lowBound=bounds$lamLow,
                           legendPosition="none",breakInterval=breakInterval,
                           labFontSize=labFontSize)+
  yr_scale2 +
  ylim(c(0, 1.8))
plot(lambdaPosterior)

posteriorResultB = caribouMetrics:::runScnSet(scns[2,],eParsIn,simBig,getKSDists=F,printProgress=F)
recPosteriorB =  plotRes(posteriorResultB, "Recruitment", lowBound=0,highBound = bounds$recHigh,
                         legendPosition="none",breakInterval=breakInterval,
                         labFontSize=labFontSize)+
  yr_scale2 +
  labs(tag = "a")
plot(recPosteriorB)

survPosteriorB =  plotRes(posteriorResultB, "Adult female survival", lowBound=bounds$survLow,
                          legendPosition="none",breakInterval=breakInterval,
                          labFontSize=labFontSize)+
  yr_scale2
plot(survPosteriorB)
lambdaPosteriorB =  plotRes(posteriorResultB, "Population growth rate", lowBound=bounds$lamLow,
                            legendPosition="none",breakInterval=breakInterval,
                            labFontSize=labFontSize)+
  yr_scale2 +
  ylim(c(0, 1.8))
plot(lambdaPosteriorB)


leg <- plotRes(posteriorResult, "Recruitment", lowBound=0,highBound = 0.85,
               legendPosition="left",breakInterval=breakInterval,labFontSize=labFontSize)
leg <- ggpubr::get_legend(leg)

# combine ggplots to one figure
plts <- ggpubr::ggarrange(
                          recPosteriorB, survPosteriorB,lambdaPosteriorB,
                          recPosterior, survPosterior, lambdaPosterior,
                          labels = "",
                          ncol = 3, nrow = 2, vjust = 1)
ggpubr::ggarrange(plts, leg, ncol = 2, widths = c(6,1),heights=0.75)+bgcolor("white")

ggsave(paste0(baseDir,"/analysis/paper/figs/bayesianModelExamplesLowEffort.png"),
       width = 9.6*0.779, height = 9.2*2/3, units = "in",
       dpi = 1200)

ggsave(paste0(baseDir,"/analysis/paper/figs_submit/bayesianModelExamplesLowEffort.pdf"),
       width = 9.6*0.779, height = 9.2, units = "in",
       dpi = 1200)
