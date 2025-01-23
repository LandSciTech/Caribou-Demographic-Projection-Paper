#devtools::load_all(here::here())

# Get some priors from PIs of Johnson et al regression
library(caribouMetrics)
library(tidyverse)
library(ggpubr)
theme_set(theme_bw())
library(RColorBrewer)

dia_shp <- 23

err_col <- "grey50"

baseDir <- "."

#numbers from Johnson et al for validation
johnsonCompare <- read.csv(paste0(baseDir,"/data/Johnson et al. figures5_6.csv"))



getAnswers <-function(scns,par,johnsonCompare,vName){
  scResults = runScenario(scns,survAnalysisMethod = "Exponential")
  rr = subset(scResults$rr.summary.all,Parameter == par)
  rr$ylow = rr[["Lower 95% CRI"]]
  rr$yhigh = rr[["Upper 95% CRI"]]
  d = unique(rr$iAnthro)
  if(par=="Recruitment"){
    rr$lowLine =johnsonCompare$Rlow_95_pre[johnsonCompare$anthro==d]/100
    rr$highLine = johnsonCompare$Rhigh_95_pred[johnsonCompare$anthro==d]/100
    rr$meanLine = johnsonCompare$Rresp[johnsonCompare$anthro==d]/100
  }else{
    rr$lowLine =johnsonCompare$Slow_95_pred[johnsonCompare$anthro==d]
    rr$highLine = johnsonCompare$Shigh_95_pred[johnsonCompare$anthro==d]
    rr$meanLine = johnsonCompare$Sresp[johnsonCompare$anthro==d]
  }
  rr$vv = rr[[vName]]
  pp1 = ggplot(rr,aes(x=vv,y=Mean,ymin=ylow,ymax=yhigh))+
    geom_line(col="#EF8A62")+geom_ribbon(fill="#EF8A62",alpha=0.2)+
    geom_hline(yintercept=unique(rr$lowLine),colour="black")+
    geom_hline(yintercept=unique(rr$highLine),colour="black")+
    geom_hline(yintercept=unique(rr$meanLine),colour="black",linetype="dashed")+
    labs(title=paste0(d,"% anthropogenic disturbance"),y=par,x=vName)
  #plot(pp1)
  return(list(data=rr,plot=pp1))
}

rrAll=list()
#Johnson analysis does not include interannual variation. So omit that to set the survival and reproduction priors.
scnsApplyAll = data.frame(qMin=0,qMax=0,uMin=0,uMax=0,zMin=0,zMax=0,iFire=4.27)
scnsNoData = data.frame(obsYears=1,collarCount=0,cowMult=0,projYears=1)
scnsLow = data.frame(obsYears=15,collarCount=60,cowMult=9,projYears=1,rQuantile=0.025,sQuantile=0.025)
scnsHigh = scnsLow;scnsHigh$rQuantile =0.975;scnsHigh$sQuantile=0.975
########################
#Survival

#vary sIntSE to match Johnson predictive interval when anthropogenic disturbance is low.
sIntSE=seq(0.02,0.1,by=0.01);sNuMin=0;sNuMax=0
scnsSInt=expand.grid(obsAnthroSlope = 0, projAnthroSlope = 0,sIntSE=sIntSE,
                     sNuMin=sNuMin,sNuMax=sNuMax)
scnsSInt$NuMin = scnsSInt$sNuMin
scns=merge(scnsSInt,scnsApplyAll);scns=merge(scns,scnsNoData)
par = "Adult female survival";vName="sIntSE"
rrAll[[vName]] = getAnswers(scns,par,johnsonCompare,vName)

plot(rrAll[[vName]]$plot)
#set sIntSE = 0.04. Set to match high bound.

#Now vary sAnthroSlopeSE to match Johnson predictive interval when anthropogenic disturbance is high.
sIntSE=0.06; sNuMin=0;sNuMax=0
sAnthroSlopeSE=seq(0.0001,0.001,by=0.0001)

scnsSSlope=expand.grid(obsAnthroSlope = 0, projAnthroSlope = 0,iAnthro=90,
                       sIntSE=sIntSE,sAnthroSlopeSE=sAnthroSlopeSE,
                     sNuMin=sNuMin,sNuMax=sNuMax)
scnsSSlope$NuMin = scnsSSlope$sNuMin
scns=merge(scnsSSlope,scnsApplyAll);scns=merge(scns,scnsNoData)
par = "Adult female survival";vName="sAnthroSlopeSE"
rrAll[[vName]] = getAnswers(scns,par,johnsonCompare,vName)
plot(rrAll[[vName]]$plot)
#set sAnthroSlopeSE = 0.0005 (set to include the full range of variability)

########################
#Recruitment

#vary rIntSE to match Johnson predictive interval when anthropogenic disturbance is low.
rIntSE=seq(0.1,0.5,by=0.05);rNuMin=0;rNuMax=0
scnsRInt=expand.grid(obsAnthroSlope = 0, projAnthroSlope = 0,rIntSE=rIntSE,
                     rNuMin=rNuMin,rNuMax=rNuMax)
scnsRInt$NuMin = scnsRInt$rNuMin
scns=merge(scnsRInt,scnsApplyAll);scns=merge(scns,scnsNoData)
par = "Recruitment";vName="rIntSE"
rrAll[[vName]] = getAnswers(scns,par,johnsonCompare,vName)
plot(rrAll[[vName]]$plot)
#set rIntSE = 0.25. Set to match upper bound of Johnson.

#Now vary rAnthroSlopeSE to match Johnson predictive interval when anthropogenic disturbance is high.
rIntSE=0.35;rNuMin=0;rNuMax=0;
rAnthroSlopeSE=seq(0.001,0.01,by=0.001)

scnsRSlope=expand.grid(obsAnthroSlope = 0, projAnthroSlope = 0,iAnthro=90,
                       rIntSE=rIntSE,rAnthroSlopeSE=rAnthroSlopeSE,
                       rNuMin=rNuMin,rNuMax=rNuMax)
scnsRSlope$NuMin = scnsRSlope$rNuMin
scns=merge(scnsRSlope,scnsApplyAll);scns=merge(scns,scnsNoData)
par = "Recruitment";vName="rAnthroSlopeSE"
rrAll[[vName]] = getAnswers(scns,par,johnsonCompare,vName)
plot(rrAll[[vName]]$plot)
#set rAnthroSlopeSE = 0.006

#TO DO: put pp 1,2,3,4 in one plot.

png(here::here("figs/InterceptsAndSlopes.png"),
    height = 7.48, width = 7.48, units = "in",res=600)

ggarrange(rrAll[["sIntSE"]]$plot,rrAll[["rIntSE"]]$plot,rrAll[["sAnthroSlopeSE"]]$plot,rrAll[["rAnthroSlopeSE"]]$plot,nrow=2,ncol=2)

dev.off()

###########################################
#Now - confirm that these parameters work.

