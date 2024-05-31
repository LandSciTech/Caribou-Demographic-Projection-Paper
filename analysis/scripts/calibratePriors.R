#devtools::load_all(here::here())
library(caribouMetrics)
scnsApplyAll = data.frame(qMin=0,qMax=0,uMin=0,uMax=0,zMin=0,zMax=0,adjustR=F)
scnsNoData = data.frame(obsYears=1,collarCount=0,cowMult=0,projYears=4)
scnsLow = data.frame(obsYears=15,collarCount=60,cowMult=9,projYears=1,rQuantile=0.025,sQuantile=0.025)
scnsHigh = scnsLow;scnsHigh$rQuantile =0.975;scnsHigh$sQuantile=0.975
########################
#Survival random effects
#vary sIntSE, sSigmaMin, and sSigmaMax in factorial array.
sIntSE=getPriors()$l.Saf.Prior2;sSigmaMin=0.00001;sSigmaMax=seq(0.02,0.12,by=0.02)

#Looking for low KS distance from full range of input sims when given almost no info
scnsSInt=expand.grid(obsAnthroSlope = 0, projAnthroSlope = 0,sIntSE=sIntSE,
                     sSigmaMin=sSigmaMin,sSigmaMax=sSigmaMax)
scnsSInt$SigmaMin = scnsSInt$sSigmaMin
scns=merge(scnsSInt,scnsApplyAll);scns=merge(scns,scnsNoData)
scResults = runScenario(scns,survAnalysisMethod = "Exponential")
KSAll = subset(scResults$ksDists,(Year==2023+scnsNoData$projYears)&(Parameter=="Adult female survival"))
addBit = paste0("InterceptAll")
scResults$obs.all = NULL
makeInterceptPlots(scResults,addBit=addBit,facetVars=c("sSigmaMax"),loopVars="sIntSE",whichPlots=c("Adult female survival"))

########################
#Recruitment random effects
#vary rIntSE, rSigmaMin, and rSigmaMax in factorial array.
rIntSE=getPriors()$l.R.Prior2;rSigmaMin=0.00001;rSigmaMax=seq(0.05,0.3,by=0.05)

#Looking for low KS distance from full range of input sims when given almost no info
scnsRInt=expand.grid(obsAnthroSlope = 0, projAnthroSlope = 0,rIntSE=rIntSE,
                     rSigmaMin=rSigmaMin,rSigmaMax=rSigmaMax)
scnsRInt$SigmaMin = scnsRInt$rSigmaMin
scns=merge(scnsRInt,scnsApplyAll);scns=merge(scns,scnsNoData)
scResults = runScenario(scns,survAnalysisMethod = "Exponential")
KSAll = subset(scResults$ksDists,(Year==2023+scnsNoData$projYears)&(Parameter=="Recruitment"))
addBit = paste0("InterceptAll")
scResults$obs.all = NULL
makeInterceptPlots(scResults,addBit=addBit,facetVars=c("rSigmaMax"),loopVars="rIntSE",whichPlots=c("Recruitment"))

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

