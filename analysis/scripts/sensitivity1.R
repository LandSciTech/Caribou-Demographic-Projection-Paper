#app_path <- "C:/Users/HughesJo/Documents/gitprojects/BayesianCaribouDemographicProjection"
#devtools::load_all(here::here())

########################
#sensitivity
#setName = "s1"
#minimalScn = expand.grid(P=1,st=1,cmult=0,ri=c(1,4),assessmentYrs=3)
#monitoringScns = expand.grid(P=c(1,2,4,8,16),st=c(30,60),cmult=c(2,3,4),ri=c(1,2,4),assessmentYrs=c(1,3))
#stateScns = data.frame(tA=c(0,20,40,60,0,20,40,60),aS=c(0,1,1,1,0,0,0,0),aSf=c(1,1,1,1,0,0,0,0))
#stateScns = merge(stateScns,data.frame(rep=seq(1:100)))

#monitoringScns = expand.grid(P=c(1,4,16),st=c(60),cmult=c(3),ri=c(1),assessmentYrs=c(3))
#stateScns = data.frame(tA=c(0,20,40,60),aS=c(0,1,1,1),aSf=c(1,1,1,1))
#stateScns = merge(stateScns,data.frame(rep=seq(1:10)))

#monitoringScns = expand.grid(P=c(1,4,8,16),st=c(30,60),cmult=c(3),ri=c(1,2,4),assessmentYrs=1)
#stateScns = expand.grid(iA=c(0),rep=seq(1:30))

setName = "s7"
monitoringScns = expand.grid(obsYears=c(1,2,4,8,16,24),collarCount=c(0,15,30,60),
                             cowMult=c(3,6,9),collarInterval=c(1,4),
                             assessmentYrs=c(3))
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

scns$iAnthro = scns$tA-(scns$obsYears-1)*scns$obsAnthroSlope
scns$projYears = 20
unique(scns$iAnthro)
scns$repBatch = ceiling(scns$rep/50)
table(scns$repBatch)
scns$N0 = 2000
scns$adjustR = TRUE
scns$preYears = max(scns$obsYears)-scns$obsYears
#scns=scns[10,]
#scResults = readRDS("temp.Rds")
head(scns)

scns$pageLab = paste0("cmult",scns$cowMult,"ay",scns$assessmentYrs,"aSf",scns$projAnthroSlope,"repBatch",scns$repBatch)
scns$pageId = as.numeric(as.factor(scns$pageLab))
unique(scns$pageId)

nrow(scns)
write.csv(scns,paste0("tabs/",setName,".csv"),row.names=F)
pages=unique(scns$pageLab)
