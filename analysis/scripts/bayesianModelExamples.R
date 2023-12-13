# Prior and posterior predictions from Bayesian model
library(caribouMetrics)
library(tidyverse)
library(ggpubr)
theme_set(theme_bw())
library(RColorBrewer)

baseDir <- "."

simBig<-getSimsNational(replicates=3000,forceUpdate=T)

monitoringScns = data.frame(obsYears=c(1,15),collarCount=c(0,30),cowMult=c(6),collarInterval=c(1),assessmentYrs=c(3))
stateScns = data.frame(obsAnthroSlope=c(2),projAnthroSlope=c(2))
stateScns = merge(stateScns,data.frame(rep=seq(1:1)))

stateScns$sQuantile=0.8
stateScns$rQuantile = 0.8
scns=merge(monitoringScns,stateScns)

scns$iAnthro = 0
scns$tA = scns$iAnthro+(scns$obsYears)*scns$obsAnthroSlope
scns$projYears = 50-scns$obsYears
scns$N0 = 2000
scns$adjustR = TRUE

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

priorResult = caribouMetrics:::runScnSet(scns[1,],eParsIn,simBig,getKSDists=F,printProgress=F)
priorResult$obs.all=NULL
yr_scale1 <- scale_x_continuous(breaks = 2023:2072 %>% .[0:9*5+1],
                                labels = 2009:2058 %>% .[0:9*5+1] )
yr_scale2 <- scale_x_continuous(breaks = 2009:2058 %>% .[0:9*5+1],
                                labels = 2009:2058 %>% .[0:9*5+1] )
recPrior =  plotRes(priorResult, "Recruitment", lowBound=0, highBound = 0.75,
                    legendPosition="none",breakInterval=breakInterval,
                    labFontSize=labFontSize) +
  yr_scale1 +
  labs(tag = "a")
plot(recPrior)
survPrior =  plotRes(priorResult, "Adult female survival", lowBound=0.6,
                     legendPosition="none",breakInterval=breakInterval,
                     labFontSize=labFontSize)+
  yr_scale1
plot(survPrior)
lambdaPrior =  plotRes(priorResult, "Population growth rate", lowBound=0.6,
                       legendPosition="none",breakInterval=breakInterval,
                       labFontSize=labFontSize)+
  yr_scale1 +
  ylim(c(0, 1.2))
plot(lambdaPrior)

posteriorResult = caribouMetrics:::runScnSet(scns[2,],eParsIn,simBig,getKSDists=F,printProgress=F)
recPosterior =  plotRes(posteriorResult, "Recruitment", lowBound=0,highBound = 0.75,
                        legendPosition="none",breakInterval=breakInterval,
                        labFontSize=labFontSize)+
  yr_scale2 +
  labs(tag = "b")
plot(recPosterior)
survPosterior =  plotRes(posteriorResult, "Adult female survival", lowBound=0.6,
                         legendPosition="none",breakInterval=breakInterval,
                         labFontSize=labFontSize)+
  yr_scale2
plot(survPosterior)
lambdaPosterior =  plotRes(posteriorResult, "Population growth rate", lowBound=0.6,
                           legendPosition="none",breakInterval=breakInterval,
                           labFontSize=labFontSize)+
  yr_scale2 +
  ylim(c(0, 1.2))
plot(lambdaPosterior)

leg <- plotRes(posteriorResult, "Recruitment", lowBound=0,highBound = 0.75,
               legendPosition="left",breakInterval=breakInterval,labFontSize=labFontSize)
leg <- ggpubr::get_legend(leg)

# combine ggplots to one figure
plts <- ggpubr::ggarrange(recPrior, survPrior, lambdaPrior,
                          recPosterior, survPosterior, lambdaPosterior, labels = "",
                          ncol = 3, nrow = 2, vjust = 1)
ggpubr::ggarrange(plts, leg, ncol = 2, widths = c(6,1))+bgcolor("white")

ggsave(paste0(baseDir,"/analysis/paper/figs/bayesianModelExamples.png"),
       width = 12*0.8, height = 3.6*2, units = "in",
       dpi = 1200)

