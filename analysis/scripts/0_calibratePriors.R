#devtools::load_all(here::here())
library(caribouMetrics)
scnsApplyAll = data.frame(qMin=0,qMax=0,uMin=0,uMax=0,zMin=0,zMax=0)
scnsNoData = data.frame(obsYears=1,collarCount=0,cowMult=0,projYears=4)
scnsLow = data.frame(obsYears=15,collarCount=60,cowMult=9,projYears=1,rQuantile=0.025,sQuantile=0.025)
scnsHigh = scnsLow;scnsHigh$rQuantile =0.975;scnsHigh$sQuantile=0.975
########################
#Survival random effects
#vary sIntSE, sNuMin, and sNuMax in factorial array.
sIntSE=getPriors()$l.Saf.Prior2;sNuMin=0.01;sNuMax=seq(0.07,0.15,by=0.01)

#Looking for low KS distance from full range of input sims when given almost no info
scnsSInt=expand.grid(obsAnthroSlope = 0, projAnthroSlope = 5,sIntSE=sIntSE,
                     sNuMin=sNuMin,sNuMax=sNuMax)
scnsSInt$NuMin = scnsSInt$sNuMin
scns=merge(scnsSInt,scnsApplyAll);scns=merge(scns,scnsNoData)
scns$projYears=20
scResults = runScenario(scns,survAnalysisMethod = "Binomial")
KSAll = subset(scResults$ksDists,(Year==2023+scnsNoData$projYears)&(Parameter=="Adult female survival"))
addBit = paste0("InterceptAll")
scResults$obs.all = NULL
makeInterceptPlots(scResults,addBit=addBit,facetVars=c("sNuMax"),loopVars="sIntSE",whichPlots=c("Adult female survival"))

########################
#Recruitment random effects
#vary rIntSE, rNuMin, and rNuMax in factorial array.
#devtools::load_all(here::here())
rIntSE=getPriors()$l.R.Prior2;rNuMin=0.01;rNuMax=seq(0.1,0.9,by=0.1)

#Looking for low KS distance from full range of input sims when given almost no info
scnsRInt=expand.grid(obsAnthroSlope = 0, projAnthroSlope = 5,rIntSE=rIntSE,
                     rNuMin=rNuMin,rNuMax=rNuMax)
scnsRInt$NuMin = scnsRInt$rNuMin
scns=merge(scnsRInt,scnsApplyAll);scns=merge(scns,scnsNoData)
scns$projYears=20
scResults = runScenario(scns,survAnalysisMethod = "Binomial")
KSAll = subset(scResults$ksDists,(Year==2023+scnsNoData$projYears)&(Parameter=="Recruitment"))
addBit = paste0("InterceptAll")
scResults$obs.all = NULL
makeInterceptPlots(scResults,addBit=addBit,facetVars=c("rNuMax"),loopVars="rIntSE",whichPlots=c("Recruitment"))

########################
#Survival data scenarios
sSlopeMod=c(0,2)
scnsSSlope=expand.grid(obsAnthroSlope = 0, projAnthroSlope = 0, iAnthro = 90)
scnsP=merge(scnsSSlope,scnsApplyAll);scnsP=merge(scnsP,data.frame(sSlopeMod=sSlopeMod))
scnsA=merge(scnsP,scnsLow)
scnsB=merge(scnsP,scnsHigh)

scns=rbind(scnsA,scnsB)
scResultsBoth = runScenario(scns,Anthro=90,survAnalysisMethod = "KaplanMeier")
addBit="Both"
makeInterceptPlots(scResultsBoth,addBit=addBit,facetVars=c("sSlopeMod","sQuantile"),whichPlots=c("Adult female survival"),
                   useNational=F)

########################
#Recruitment data scenarios
rSlopeMod=c(0,2)
scnsSSlope=expand.grid(obsAnthroSlope = 0, projAnthroSlope = 0, iAnthro = 90)
scnsP=merge(scnsSSlope,scnsApplyAll);scnsP=merge(scnsP,data.frame(rSlopeMod=rSlopeMod))
scnsA=merge(scnsP,scnsLow)
scnsB=merge(scnsP,scnsHigh)

scns=rbind(scnsA,scnsB)
scResultsBothR = runScenario(scns,Anthro=90,survAnalysisMethod = "KaplanMeier")
addBit="Both"
makeInterceptPlots(scResultsBothR,addBit=addBit,facetVars=c("rSlopeMod","rQuantile"),whichPlots=c("Recruitment"),
                   useNational=F)

