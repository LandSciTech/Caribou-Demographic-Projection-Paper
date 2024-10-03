#app_path <- "C:/Users/HughesJo/Documents/gitprojects/BayesianCaribouDemographicProjection"
#devtools::load_all(here::here())

########################
#sensitivity
setName = "s10"
monitoringScns = expand.grid(obsYears=c(1,2,4,8,16,24),collarCount=c(0,15,30,60),
                             cowMult=c(6),collarInterval=c(1),
                             assessmentYrs=c(1))
#TO DO - in next iteration, remove multiple years of one collar
monitoringScns = subset(monitoringScns, !((obsYears>1)&(collarCount==0))&!((collarCount==1)&(cowMult>3))&!((collarCount==0)&(collarInterval>1)))
stateScns = data.frame(tA=c(0,20,40,60),
                       obsAnthroSlope=c(0,1,1,1),
                       projAnthroSlope=c(1,1,1,1)
                       )
stateScns = merge(stateScns,data.frame(rep=seq(1:500)))
stateScns = merge(stateScns,data.frame(interannualVar=c("list(R_CV=0.46,S_CV=0.087)","list(R_CV=0.23,S_CV=0.0435)")))
stateScns$sQuantile=runif(nrow(stateScns),min=0.01,max=0.99)
stateScns$rQuantile = runif(nrow(stateScns),min=0.01,max=0.99)
#monitoringScns=rbind(monitoringScns,minimalScn)
scns=merge(monitoringScns,stateScns)

scns$preYears = max(scns$obsYears)-scns$obsYears

scns$iAnthro = scns$tA-(scns$obsYears+scns$preYears-1)*scns$obsAnthroSlope
scns$projYears = 20
unique(scns$iAnthro)
scns$repBatch = ceiling(scns$rep/50)
table(scns$repBatch)
scns$N0 = 2000

scns$pageLab = paste0("cmult",scns$cowMult,"ay",scns$assessmentYrs,"aSf",scns$projAnthroSlope,"repBatch",scns$repBatch)
scns$pageId = as.numeric(as.factor(scns$pageLab))
nrow(scns)
length(unique(scns$pageLab))

write.csv(scns,paste0("tabs/",setName,".csv"),row.names=F)
pages=unique(scns$pageLab)

###################
#Without bias
setName = "s12"
scns$qMax = 0;scns$uMax=0;scns$zMax=0
write.csv(scns,paste0("tabs/",setName,".csv"),row.names=F)
pages=unique(scns$pageLab)

###################
#Different anthropogenic disturbance scenarios, collar intervals. With bias and default interannual variability.
setName = "s11"
monitoringScns = expand.grid(obsYears=c(1,2,4,8,16,24),collarCount=c(0,15,30,60),
                             cowMult=c(3,6,9),collarInterval=c(1,4),
                             assessmentYrs=c(1))
#TO DO - in next iteration, remove multiple years of one collar
monitoringScns = subset(monitoringScns, !((obsYears>1)&(collarCount==0))&!((collarCount==1)&(cowMult>3))&!((collarCount==0)&(collarInterval>1)))
stateScns = data.frame(tA=c(0,20,40,60,0,20,40,60,60,40,20),
                       obsAnthroSlope=c(0,1,1,1,0,0,0,0,0,0,0),
                       projAnthroSlope=c(1,1,1,1,0,0,0,0,-1,-1,-1))
stateScns = merge(stateScns,data.frame(rep=seq(1:500)))

stateScns$sQuantile=runif(nrow(stateScns),min=0.01,max=0.99)
stateScns$rQuantile = runif(nrow(stateScns),min=0.01,max=0.99)
#monitoringScns=rbind(monitoringScns,minimalScn)
scns=merge(monitoringScns,stateScns)

scns$preYears = max(scns$obsYears)-scns$obsYears

scns$iAnthro = scns$tA-(scns$obsYears+scns$preYears-1)*scns$obsAnthroSlope
scns$projYears = 20
unique(scns$iAnthro)
scns$repBatch = ceiling(scns$rep/50)
table(scns$repBatch)
scns$N0 = 2000

scns$pageLab = paste0("cmult",scns$cowMult,"ay",scns$assessmentYrs,"aSf",scns$projAnthroSlope,"repBatch",scns$repBatch)
scns$pageId = as.numeric(as.factor(scns$pageLab))
nrow(scns)
length(unique(scns$pageLab))

write.csv(scns,paste0("tabs/",setName,".csv"),row.names=F)
pages=unique(scns$pageLab)

setName="s9"
scns = subset(scns,(rep==1)&(collarCount==30)
              &(cowMult==6)&(collarInterval==1)&(projAnthroSlope==1))
nrow(scns)
write.csv(scns,paste0("tabs/",setName,".csv"),row.names=F)
pages=unique(scns$pageLab)

