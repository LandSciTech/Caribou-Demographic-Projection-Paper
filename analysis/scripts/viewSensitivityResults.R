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
setName = "s6"
dir.create(paste0("figs/",setName),recursive=T)
scns = read.csv(here::here("tabs/s6.csv"))
pagesa=unique(scns$pageId)

pages = pagesa
for (p in pagesa){
  #p=pagesa[1]
  if(!file.exists(paste0("results/",setName,"/rTest",p,".Rds"))){pages=pages[pages!=p]}
}

batchStrip<-function(l,batches=seq(1:4)){
  for(b in batches){
    l=gsub(paste0("repBatch",b),"",l,fixed=T)
  }
  return(l)
}

pages=sort(pages)

combine=T
for(i in 1:length(pages)){
  #combine=F;i=5
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

  if(as.numeric(strsplit(nextP,"repBatch")[[1]][2])<=as.numeric(strsplit(p,"repBatch")[[1]][2])){
    combine=F
  }else{combine=T;next}

  #figure out how to count out errors.
  head(scResults$rr.summary.all)
  unique(scResults$rr.summary.all$collarInterval)
  #show examples projections
  exResults = subset(scResults$rr.summary.all,(collarCount==30)&(collarInterval==1)&(Parameter=="Population growth rate"))
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

  obs = subset(scResults$obs.all,(collarCount==30)&(collarInterval==1)&(parameter=="Population growth rate"))
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
  levels(distScns$Anthro2023) = c("low","low-med","med-high","high")

  distScns$DisturbanceScn = distScns$Anthro2023


  timelineLabs = unique(subset(distScns,(Year==startYear)|(Year>=2023),select=c(Year,Timeline,Anthro,DisturbanceScn,grp)))
  timelineLabs = timelineLabs[order(timelineLabs$Year),]

  png(here::here(paste0("figs/",setName,"/distScns",batchStrip(p),".png")),
      height = 4, width = 5.51, units = "in",res=600)
  base=ggplot(distScns,aes(x=Year,y=Anthro,col=DisturbanceScn,group=grp))+geom_line()+geom_point(data=timelineLabs)+
    theme_bw()+theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1,size=8)) + labs(color="Anthropogenic\nDisturbance\nScenario")+
    scale_x_continuous(name="Timeline", breaks=timelineLabs$Year, labels=timelineLabs$Timeline)+ylab("Anthropogenic Disturbance")+
    scale_color_discrete(type=(pal4b))
  print(base)
  dev.off()

  probs = subset(probs,is.element(projectionTime,c(5,20)))

  #plot probability of making an error about true population state
  statusTrue = subset(scResults$obs.all,(type=="true")&(parameter=="Population growth rate")&(Year>(startYear+obsYears-1)))
  statusTrue$trueMean = statusTrue$Mean
  statusTrue$parameter=NULL;statusTrue$type=NULL;statusTrue$Mean=NULL

  sizeTrue = subset(scResults$obs.all,(type=="true")&(parameter=="Female population size")&(Year>(startYear+obsYears-1)))
  sizeTrue$trueSize = sizeTrue$Mean
  sizeTrue$parameter=NULL;sizeTrue$type=NULL;sizeTrue$Mean=NULL
  setdiff(names(statusTrue),names(probs))

  sizeProj = subset(scResults$rr.summary.all,(Parameter=="Female population size"),select=c(label,Year,Mean))
  sizeProj$projSize = sizeProj$Mean; sizeProj$Mean=NULL

  probs = merge(probs,statusTrue)
  probs = merge(probs,sizeTrue)
  probs = merge(probs,sizeProj)

  head(probs)
  probs$obsYears[probs$collarCount==1]=0
  probs$viableTrue = (probs$trueMean>0.99)&(probs$trueSize>10)

  probs$wrong = ((probs$Mean<=0.99)|(probs$projSize<=10))&probs$viableTrue
  probs$CorrectStatus[probs$wrong]="no"
  probs$CorrectStatus[!probs$wrong]="yes"
  probs$LambdaDiff = probs$trueMean-probs$Mean
  #probs$LambdaDiff[(probs$projSize<=10)]=NA

  probs$pageLabB = paste0(batchStrip(probs$pageLab),"st",probs$collarCount,"ri",probs$collarInterval)
  pagesB=unique(probs$pageLabB)

  probs$AnthroScn=as.factor(probs$Anthro2023)
  levels(probs$AnthroScn) = c("low","low-med","med-high","high")

  for(pp in pagesB){
    #pp=pagesB[3]
    png(here::here(paste0("figs/",setName,"/bands",pp,".png")),
        height = 4, width = 7.48, units = "in",res=600)
    base=ggplot(subset(probs,pageLabB==pp),aes(x=obsYears,y=Mean,col=CorrectStatus))+geom_point(shape="-",size=3)+
      facet_grid(YearsOfProjection~AnthroScn,labeller="label_both")+
      theme_bw()+xlab("years of monitoring")+ylab("Estimated mean population growth rate")
    print(base)
    dev.off()
  }


  probs$pageLab = batchStrip(probs$pageLab)
  pagesCa = unique(probs$pageLab)
  probs$RenewalInterval=as.factor(probs$collarInterval)
  probs$NumCollars = as.factor(probs$collarCount)
  probs$grp = paste(probs$obsYears,probs$NumCollars)

  probsSum$CollarYrs = as.numeric(as.character(probsSum$NumCollars))*probsSum$obsYears
  for(pp in pagesCa){
    #pp=pagesCa[1]
    png(here::here(paste0("figs/",setName,"/diffs",pp,".png")),
        height = 4, width = 7.48, units = "in",res=600)
    base=ggplot(subset(probs,(pageLab==pp)&(RenewalInterval==1)),aes(x=as.factor(obsYears),y=LambdaDiff,col=NumCollars,fill=NumCollars,group=grp))+
      geom_violin(alpha=0.5)+ylim(-0.15,0.15)+
      facet_grid(YearsOfProjection~AnthroScn,labeller="label_both")+labs(color="Number of\n Collars",fill="Number of\n Collars")+
      theme_bw()+xlab("years of monitoring")+ylab("Difference between true growth rate and posterior mean")+
      scale_color_discrete(type=(pal4))+scale_fill_discrete(type=(pal4))
    print(base)
    dev.off()
  }

  ################
  #summarize outcome - proportion wrong
  groupVars = c("Anthro","AnthroScn","YearsOfProjection",setdiff(names(scns),c("rQuantile","sQuantile","rep","pageId","repBatch")))

  probsSum <- probs %>% group_by(across(groupVars)) %>% summarize(propWrong = mean(wrong))
  probsSum <- subset(probsSum,obsYears>0)
  probsSum$grp = paste(probsSum$collarCount,probsSum$collarInterval)

  table(probsSum$grp)
  probsSum$pageLabC = batchStrip(probsSum$pageLab)
  pagesC=unique(probsSum$pageLabC)

  probsSum$RenewalInterval=as.factor(probsSum$collarInterval)
  probsSum$NumCollars = as.factor(probsSum$collarCount)

  probsSum$CollarYrs = as.numeric(as.character(probsSum$NumCollars))*probsSum$obsYears
  for(pp in pagesC){
    #pp=pagesC[1]
    png(here::here(paste0("figs/",setName,"/power",pp,".png")),
        height = 4, width = 7.48, units = "in",res=600)
    base=ggplot(subset(probsSum,pageLabC==pp),aes(x=obsYears,y=1-propWrong,col=NumCollars,linetype=RenewalInterval,group=grp))+geom_line()+
      facet_grid(YearsOfProjection~AnthroScn,labeller="label_both")+labs(color="Number of\n Collars", type="Collar\nRenewal\nInterval")+
      theme_bw()+xlab("years of monitoring")+ylab("Probability of correct status assessment")+
      scale_color_discrete(type=(pal4))
    print(base)
    dev.off()

    png(here::here(paste0("figs/",setName,"/powerEffort",pp,".png")),
        height = 4, width = 7.48, units = "in",res=600)
    base=ggplot(subset(probsSum,pageLabC==pp),aes(x=CollarYrs,y=1-propWrong,linetype=RenewalInterval,col=NumCollars,group=grp))+geom_line()+
      facet_grid(YearsOfProjection~AnthroScn,labeller="label_both")+labs(color="Number of\n Collars", type="Collar\nRenewal\nInterval")+
      theme_bw()+xlab("years of monitoring * NumCollars")+ylab("Probability of correct status assessment")+
      scale_color_discrete(type=(pal4))
    print(base)
    dev.off()

  }
}


