#devtools::load_all(here::here())
library(tidyverse)
library(ggplot2)
library(caribouMetrics)
library(RColorBrewer)

pal2 = brewer.pal(7,"RdBu")[c(2,6)]
pal4 = brewer.pal(5,"RdBu")[c(1,2,4,5)]

pal4b = brewer.pal(5,"Purples")[c(2,3,4,5)]

scn_defaults <- eval(formals(getScenarioDefaults))

########################
#sensitivity
setName = "s11" #need to do s10, s11, and s12
dir.create(paste0("figs/",setName),recursive=T)
scns = read.csv(here::here(paste0("tabs/",setName,".csv")))


table(scns$pageId)

nrow(scns)
pagesa=unique(scns$pageId)

pages = pagesa

str(scns)

setLTYVar <-function(scns){
  if(is.element("interannualVar",names(scns))&&(length(unique(scns$interannualVar))>1)){
    scns$ltyVariable = "low"
    scns$ltyVariable[grepl("0.46",scns$interannualVar,fixed=T)]="high"
  }else{
    scns$ltyVariable=scns$collarInterval
  }
  return(scns)
}

scns = setLTYVar(scns)
if(is.element("interannualVar",names(scns))){
  ltyLabel = "Interannual\nvariation"
}else{
  ltyLabel = "Collar\nrenewal\ninterval"
}


for (p in pagesa){
  #p=pagesa[1]
  if(!file.exists(paste0("results/",setName,"/rTest",p,".Rds"))){pages=pages[pages!=p]}
}

batchStrip<-function(l,batches=c(10,seq(1:9))){
  for(b in batches){
    l=gsub(paste0("repBatch",b),"",l,fixed=T)
  }
  return(l)
}

pages=sort(pages)

combine=T
addEV = F
addProbs = F

for(i in 1:length(pages)){
  #combine=F;i=10
  cpageId=pages[i]

  if(i==length(pages)){
    nextP = "TrepBatch1"
  }else{
    nextP = unique(scns$pageLab[scns$pageId==pages[i+1]])
  }

  p = unique(scns$pageLab[scns$pageId==cpageId])

  print(paste(i,p))

  if(combine&(cpageId>1)){
    scNew = readRDS(paste0("results/",setName,"/rTest",cpageId,".Rds"))
    for(n in names(scNew)){
      if(n=="errorLog"){
        scResults[[n]]=c(scResults[[n]],scNew[[n]])
      }else{
        scResults[[n]]=rbind(scResults[[n]],scNew[[n]])
      }
    }
  }else{
    scResults = readRDS(paste0("results/",setName,"/rTest",cpageId,".Rds"))
  }

  if((as.numeric(strsplit(p,"repBatch")[[1]][2])!=10)&(as.numeric(strsplit(nextP,"repBatch")[[1]][2])<=as.numeric(strsplit(p,"repBatch")[[1]][2]))){
    combine=F
  }else{combine=T;next}

  scResults$rr.summary.all = setLTYVar(scResults$rr.summary.all)

  scResults$obs.all = setLTYVar(scResults$obs.all)
  #figure out how to count out errors.
  head(scResults$rr.summary.all)
  unique(scResults$rr.summary.all$collarInterval)
  #show examples projections
  exResults = subset(scResults$rr.summary.all,(collarCount==30)&(ltyVariable==scns$ltyVariable[1])&(Parameter=="Population growth rate"))

  exResults$startYear = exResults$startYear+exResults$preYears
  exResults$meanQ = (exResults$rQuantile+exResults$sQuantile)/2
  grpID = subset(exResults,select=c(tA,obsYears,meanQ)) %>% group_by(tA,obsYears) %>%
    summarise(minQ = min(meanQ),maxQ=max(meanQ))

  exResults=merge(exResults,grpID)
  exResults$quantile[exResults$meanQ == exResults$maxQ]="high"
  exResults$quantile[exResults$meanQ == exResults$minQ]="low"
  exResults = subset(exResults,!is.na(quantile))
  exResults$type[exResults$Year<=2023]="estimated"
  exResults$type[exResults$Year>2023]="projected"
  exResults$grp = paste(exResults$type,exResults$quantile, exResults$tA,exResults$obsYears)
  exResults$Anthro2023 = pmax(0,exResults$tA)#pmax(0,exResults$tA-2) #correcting for error - change back in round 4


  unique(scResults$obs.all$collarCount)
  unique(scResults$obs.all$ltyVariable)
  unique(scResults$obs.all$parameter)
  obs = subset(scResults$obs.all,(collarCount==30)&(ltyVariable==scns$ltyVariable[1])&(parameter=="Population growth rate"))
  obs$startYear = obs$startYear+obs$preYears
  obs = merge(obs,unique(subset(exResults,select=c(tA,obsYears,rQuantile,sQuantile,quantile,grp,Anthro2023))))
  obs$type = "true"

  png(here::here(paste0("figs/",setName,"/examples",batchStrip(p),".png")),
      height = 6, width = 10.56, units = "in",res=600)
  base=ggplot(exResults,aes(x=Year,y=Mean,col=quantile,group=grp,linetype=type))+geom_line(show.legend=T)+
    geom_ribbon(aes(ymin = `Lower 95% CRI`, ymax = `Upper 95% CRI`,fill=quantile),
                show.legend = FALSE, alpha = 0.25,col=NA)+
    geom_line(data=obs,aes(x=Year,y=Mean, col=quantile,group=grp,linetype=type),show.legend=T)+
    theme_bw()+facet_grid(obsYears~Anthro2023,labeller = "label_both")+ylab("Population growth rate")+
    theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1,size=8))+
    geom_hline(yintercept=1, color = "black",size=0.7)+scale_fill_discrete(type=pal2)+scale_color_discrete(type=pal2)
  print(base)
  dev.off()


  probs = subset(scResults$rr.summary.all,(Parameter=="Population growth rate"))
  probs$startYear = probs$startYear+probs$preYears
  probs$projectionTime = probs$Year-2023
  probs$Anthro2023 = pmax(0,probs$tA)#pmax(0,probs$tA-2) #correcting for error - change back in round 4
  probs$YearsOfProjection = probs$projectionTime

  names(probs)

  #See disturbance scenarios
  #distScns = subset(probs,is.element(projectionTime,c(0,5,20))|(Year==iYr))
  distScns = unique(subset(probs,is.element(projectionTime,c(0,5,20))|(Year<2023),select=c(startYear,projectionTime,Year,obsYears,Anthro2023,Anthro)))
  distScns$grp = paste0(distScns$Anthro2023,distScns$projAnthroSlope)

  distScns$Timeline[distScns$Year==2023]="Finish monitoring"
  distScns$Timeline[distScns$projectionTime==5]="End 5 yr projection (2028)"
  distScns$Timeline[distScns$projectionTime==20]="End 20 yr projection (2043)"

  Ps = unique(distScns$obsYears)
  for(s in Ps){
    distScns$Timeline[distScns$Year==(2023-s+1)]= paste("Start",s,"yrs monitoring")
  }

  startLevels = unique(distScns$Timeline[distScns$Year<2023])
  #distScns$Timeline = factor(distScns$Timeline,levels=c(startLevels[length(startLevels):1],"Finish monitoring","Assessment 2028","Assessment 2043"))
  distScns$Anthro2023=as.factor(distScns$Anthro2023)
  levels(distScns$Anthro2023) = c("high","med-high","low-med","low")

  distScns$DisturbanceScn = distScns$Anthro2023

  timelineLabs = unique(subset(distScns,(Year==startYear)|(Year>=2023),select=c(Year,Timeline,Anthro,DisturbanceScn,grp)))
  timelineLabs = timelineLabs[order(timelineLabs$Year),]

  base=ggplot(distScns,aes(x=Year,y=Anthro,col=DisturbanceScn,group=grp))+geom_line()+geom_point(data=timelineLabs)+
    theme_bw()+theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1,size=8)) + labs(color="Anthropogenic\nDisturbance\nScenario")+
    scale_x_continuous(name="Timeline", breaks=timelineLabs$Year, labels=timelineLabs$Timeline)+ylab("Anthropogenic Disturbance")+
    scale_color_discrete(type=(pal4b))

  png(here::here(paste0("figs/",setName,"/distScns",batchStrip(p),".png")),
      height = 4, width = 5.51, units = "in",res=600)
  print(base)
  dev.off()

  pdf(here::here(paste0("figs/",setName,"/distScns",batchStrip(p),".pdf")),
      height = 4, width = 5.51)
  print(base)
  dev.off()

  probs = subset(probs,is.element(projectionTime,c(5,20)))

  #plot probability of making an error about true population state
  statusTrue = subset(scResults$obs.all,(type=="true")&(parameter=="Population growth rate")&(Year>(startYear+obsYears-1)))
  statusTrue$startYear=statusTrue$startYear+statusTrue$preYears
  statusTrue$trueMean = statusTrue$Mean
  statusTrue$parameter=NULL;statusTrue$type=NULL;statusTrue$Mean=NULL

  sizeTrue = subset(scResults$obs.all,(type=="true")&(parameter=="Female population size")&(Year>(startYear+obsYears-1)))
  sizeTrue$startYear = sizeTrue$startYear+sizeTrue$preYears
  sizeTrue$trueSize = sizeTrue$Mean
  sizeTrue$parameter=NULL;sizeTrue$type=NULL;sizeTrue$Mean=NULL
  setdiff(names(statusTrue),names(probs))

  #sizeProj = subset(scResults$rr.summary.all,(Parameter=="Female population size"),select=c(label,Year,Mean))
  #sizeProj$projSize = sizeProj$Mean; sizeProj$Mean=NULL

  probs = merge(probs,statusTrue)
  probs = merge(probs,sizeTrue)
  #probs = merge(probs,sizeProj)
  probs$trueChange = probs$trueMean#probs$trueSize/probs$N0
  probs$predChange = probs$Mean#probs$projSize/probs$N0

  probs$viableTrue = (probs$trueChange>0.99)#&(probs$trueSize>10)

  probs$viablePred = (probs$predChange>0.99)#&(probs$projSize>10)

  probs$wrong = probs$viableTrue != probs$viablePred

  probs$CorrectStatus[probs$wrong]="no"
  probs$CorrectStatus[!probs$wrong]="yes"
  probs$LambdaDiff = probs$trueChange-probs$predChange
  #probs$LambdaDiff[(probs$projSize<=10)]=NA

  probs$pageLabB = paste0(batchStrip(probs$pageLab),"st",probs$collarCount,"ri",probs$ltyVariable)
  pagesB=unique(probs$pageLabB)

  probs$AnthroScn=as.factor(probs$Anthro2023)
  levels(probs$AnthroScn) = c("low","low-med","med-high","high")

  for(pp in pagesB){
    #pp=pagesB[3]
    png(here::here(paste0("figs/",setName,"/bands",pp,".png")),
        height = 4, width = 7.48, units = "in",res=600)
    base=ggplot(subset(probs,pageLabB==pp),aes(x=obsYears,y=Mean,col=CorrectStatus))+geom_point(shape="-",size=3,alpha=0.5)+
      facet_grid(YearsOfProjection~AnthroScn,labeller="label_both")+
      theme_bw()+xlab("Years of monitoring")+ylab("Estimated mean population growth rate")
    print(base)
    dev.off()

    png(here::here(paste0("figs/",setName,"/bandsTrue",pp,".png")),
        height = 4, width = 7.48, units = "in",res=600)
    base=ggplot(subset(probs,pageLabB==pp),aes(x=obsYears,y=trueMean,col=CorrectStatus))+geom_point(shape="-",size=3,alpha=0.5)+
      facet_grid(YearsOfProjection~AnthroScn,labeller="label_both")+
      theme_bw()+xlab("Years of monitoring")+ylab("True population growth rate")
    print(base)
    dev.off()

  }


  probs$pageLab = batchStrip(probs$pageLab)
  pagesCa = unique(probs$pageLab)
  probs$RenewalInterval=as.factor(probs$ltyVariable)
  probs$NumCollars = as.factor(probs$collarCount)
  probs$grp = paste(probs$obsYears,probs$NumCollars)

  probs$CollarYrs = as.numeric(as.character(probs$NumCollars))*probs$obsYears
  for(pp in pagesCa){
    #pp=pagesCa[1]
    base=ggplot(subset(probs,(pageLab==pp)&(ltyVariable==scns$ltyVariable[nrow(scns)])),aes(x=as.factor(obsYears),y=LambdaDiff,col=NumCollars,fill=NumCollars,group=grp))+
      geom_violin(alpha=0.5)+ylim(-0.15,0.15)+
      facet_grid(YearsOfProjection~AnthroScn,labeller="label_both")+labs(color="Number of\ncollars",fill="Number of\ncollars")+
      theme_bw()+xlab("Years of monitoring")+ylab("Difference between true growth rate and posterior mean")+
      scale_color_discrete(type=(pal4))+scale_fill_discrete(type=(pal4))

    png(here::here(paste0("figs/",setName,"/diffs",pp,".png")),
        height = 4, width = 7.48, units = "in",res=600)
    print(base)
    dev.off()

    pdf(here::here(paste0("figs/",setName,"/diffs",pp,".pdf")),
        height = 4, width = 7.48)
    print(base)
    dev.off()
  }

  #######################
  #EVSI calculations. See Dunham et al and references therein, and EVPIFromNationalSims.R

  groupVars = c("Anthro","AnthroScn","YearsOfProjection",setdiff(names(scns),c("rQuantile","sQuantile","rep","pageId","repBatch")))

  #Step 1: get posterior belief that true state is s p0_sd
  p1 = probs
  p1$p1_sd = p1$probViable
  p1$s = 1
  p1_s0 = p1; p1_s0$s = 0; p1_s0$p1_sd = 1-p1_s0$p1_sd
  p1 = rbind(p1,p1_s0)

  #step 2: value matrix v_as
  v = expand.grid(s = c(0,1),a=c(0,1))
  v$v_as = as.numeric(v$s==v$a)

  #step 3: merge
  vals = merge(v,p1)

  #step 4: calculate
  vals$innerProd = vals$p1_sd*vals$v_as

  Es = vals %>%
    group_by(across(c(groupVars,"rep","a"))) %>%
    summarise(Es = sum(innerProd))

  Ex = Es %>%
    group_by(across(c(groupVars,"rep"))) %>%
    summarise(Ex = max(Es))

  #Now get expected value of Ex across all x i.e. rep
  EVsample = Ex %>%
    group_by(across(groupVars)) %>%
    summarise(EVsample = mean(Ex),EVvar=sd(Ex))
  #write out sd as a check. For runs informed only by prior sd should be 0ish.

  #Write out results - need to combine with results informed only by priors to get EVSI
  if(addEV){
    EVout = rbind(EVout,EVsample)
  }else{
    EVout = EVsample;addEV=T
  }

  ################
  #summarize outcome - proportion wrong
  probsSum <- probs %>% group_by(across(groupVars)) %>%
    summarize(propWrong = mean(wrong),propViableTrue=mean(viableTrue),propViablePred = mean(viablePred),
              propViablePosterior=mean(probViable))



  probsSum = merge(probsSum,EVsample)
  probsSum$grp = paste(probsSum$collarCount,probsSum$ltyVariable)

  table(probsSum$grp)
  probsSum$pageLabC = batchStrip(probsSum$pageLab)
  pagesC=unique(probsSum$pageLabC)

  probsSum$RenewalInterval=as.factor(probsSum$ltyVariable)
  probsSum$NumCollars = as.factor(probsSum$collarCount)

  probsSum$CollarYrs = as.numeric(as.character(probsSum$NumCollars))*probsSum$obsYears

  if(addProbs){
    ProbsOut = rbind(ProbsOut,probsSum)
  }else{
    ProbsOut = probsSum;addProbs=T
  }

  probsSum <- subset(probsSum,collarCount>0)

  for(pp in pagesC){
    #pp=pagesC[1]
    base=ggplot(subset(probsSum,pageLabC==pp),aes(x=obsYears,y=1-propWrong,col=NumCollars,linetype=RenewalInterval,group=grp))+geom_line()+
      facet_grid(YearsOfProjection~AnthroScn,labeller="label_both")+labs(color="Number of\ncollars", linetype=ltyLabel)+
      theme_bw()+xlab("Years of monitoring")+ylab("Probability of correct status assessment")+
      scale_color_discrete(type=(pal4))

    png(here::here(paste0("figs/",setName,"/power",pp,".png")),
        height = 4, width = 7.48, units = "in",res=600)
    print(base)
    dev.off()

    pdf(here::here(paste0("figs/",setName,"/power",pp,".pdf")),
        height = 4, width = 7.48)
    print(base)
    dev.off()

    png(here::here(paste0("figs/",setName,"/EVsample",pp,".png")),
        height = 4, width = 7.48, units = "in",res=600)
    base=ggplot(subset(probsSum,pageLabC==pp),aes(x=obsYears,y=EVsample,col=NumCollars,linetype=RenewalInterval,group=grp))+geom_line()+
      facet_grid(YearsOfProjection~AnthroScn,labeller="label_both")+labs(color="Number of\ncollars", linetype=ltyLabel)+
      theme_bw()+xlab("Years of monitoring")+ylab("EVsample")+
      scale_color_discrete(type=(pal4))
    print(base)
    dev.off()

    png(here::here(paste0("figs/",setName,"/probChecks",pp,".png")),
        height = 4, width = 7.48, units = "in",res=600)
    base=ggplot(subset(probsSum,pageLabC==pp),aes(x=obsYears,y=propViableTrue,col=NumCollars,linetype=RenewalInterval,group=grp))+geom_line()+
      facet_grid(YearsOfProjection~AnthroScn,labeller="label_both")+labs(color="Number of\ncollars", linetype=ltyLabel)+
      theme_bw()+xlab("Years of monitoring")+ylab("propViableTrue")+
      scale_color_discrete(type=(pal4))
    print(base)
    dev.off()


    png(here::here(paste0("figs/",setName,"/powerEffort",pp,".png")),
        height = 4, width = 7.48, units = "in",res=600)
    base=ggplot(subset(probsSum,pageLabC==pp),aes(x=CollarYrs,y=1-propWrong,linetype=RenewalInterval,col=NumCollars,group=grp))+geom_line()+
      facet_grid(YearsOfProjection~AnthroScn,labeller="label_both")+labs(color="Number of\ncollars", linetype=ltyLabel)+
      theme_bw()+xlab("Years of monitoring * NumCollars")+ylab("Probability of correct status assessment")+
      scale_color_discrete(type=(pal4))
    print(base)
    dev.off()

  }
}

write.csv(EVout,here::here(paste0("tabs/EVsample",setName,".csv")),row.names=F)

write.csv(ProbsOut,here::here(paste0("tabs/ProbsOut",setName,".csv")),row.names=F)

str(subset(EVout,collarCount==0))
