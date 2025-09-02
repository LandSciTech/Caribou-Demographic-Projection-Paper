#devtools::load_all(here::here())
#devtools::load_all(path = "../caribouMetrics/")
library(tidyverse)
library(ggplot2)
library(caribouMetrics)
library(RColorBrewer)
library(scales)

pal2 = brewer.pal(7,"RdBu")[c(2,6)]
pal4 = brewer.pal(5,"RdBu")[c(2,1,5,4)]
pal3 = pal4[2:4]
pal4b = brewer.pal(5,"Purples")[c(2,3,4,5)]

scn_defaults <- eval(formals(getScenarioDefaults))

########################
#sensitivity
setName = "s2" #need to do s13, s14, and s15
dir.create(paste0("figs/",setName),recursive=T)
dir.create(paste0("tabs/",setName),recursive=T)

scns = read.csv(here::here(paste0("tabs/",setName,".csv")))

scns = scns[order(scns$pageId),]

numBatches= max(scns$repBatch)
table(scns$pageId)

1942/(nrow(scns)/60)
pagesa=unique(scns$pageId)

pages = pagesa

setLTYVar <-function(scns){
  if(is.element("interannualVar",names(scns))&&(length(unique(scns$interannualVar))>1)){
    scns$ltyVariable = "none"
    scns$ltyVariable[grepl("0.23",scns$interannualVar,fixed=T)]="low"
    scns$ltyVariable[grepl("0.46",scns$interannualVar,fixed=T)]="high"
  }else{
    scns$ltyVariable=scns$collarInterval
  }
  return(scns)
}

scns = setLTYVar(scns)
if(is.element("interannualVar",names(scns))&&(length(unique(scns$interannualVar))>1)){
  ltyLabel = "Interannual\nvariation"
  ltySel="low"
}else{
  ltyLabel = "Collar\nrenewal\ninterval"
  ltySel=1
}


for (p in pagesa){
  #p=pagesa[1]
  if(!file.exists(paste0("results/",setName,"/rTest",p,"all",p,".Rds"))){pages=pages[pages!=p]}
}


batchStrip<-function(l,batches=c(seq(10,numBatches),seq(1:9))){
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
  #combine=F;i=20
  cpageId=pages[i]

  if(i==length(pages)){
    nextP = "TrepBatch1"
  }else{
    nextP = unique(scns$pageLab[scns$pageId==pages[i+1]])
  }

  p = unique(scns$pageLab[scns$pageId==cpageId])

  print(paste(i,p))

  if(combine&(cpageId>1)){
    scNew = readRDS(paste0("results/",setName,"/rTest",cpageId,"all",cpageId,".Rds"))
    for(n in names(scNew)){
      if(n=="errorLog"){
        scResults[[n]]=c(scResults[[n]],scNew[[n]])
      }else{
        scResults[[n]]=rbind(scResults[[n]],scNew[[n]])
      }
    }
  }else{
    scResults = readRDS(paste0("results/",setName,"/rTest",cpageId,"all",cpageId,".Rds"))
  }

  if((as.numeric(strsplit(nextP,"repBatch")[[1]][2])==1)){#<=as.numeric(strsplit(p,"repBatch")[[1]][2]))){
    #print(p,nextP)
    combine=F
  }else{combine=T;next}

  scResults$rr.summary.all = setLTYVar(scResults$rr.summary.all)

  scResults$obs.all = setLTYVar(scResults$obs.all)
  #figure out how to count out errors.
  head(scResults$rr.summary.all)
  unique(scResults$rr.summary.all$collarInterval)
  #show examples projections
  exResults = subset(scResults$rr.summary.all,(collarCount==20)&(ltyVariable==ltySel)&(Parameter=="Expected growth rate"))

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

  #unique(scResults$obs.all$collarCount)
  #unique(scResults$obs.all$ltyVariable)
  pars = unique(scResults$obs.all$Parameter)
  unique(scResults$obs.all$ltyVariable)
  obs = subset(scResults$obs.all,(collarCount==60)&(ltyVariable==ltySel)&(Parameter=="Expected growth rate"))
  obs$startYear = obs$startYear+obs$preYears
  obs = merge(obs,unique(subset(exResults,select=c(tA,obsYears,rQuantile,sQuantile,quantile,grp,Anthro2023))))
  obs$type = "true"

  png(here::here(paste0("figs/",setName,"/examples",batchStrip(p),".png")),
      height = 6, width = 10.56, units = "in",res=600)
  base=ggplot(exResults,aes(x=Year,y=Mean,col=quantile,group=grp,linetype=type))+geom_line(show.legend=T)+
    geom_ribbon(aes(ymin = lower, ymax = upper,fill=quantile),
                show.legend = FALSE, alpha = 0.25,col=NA)+
    geom_line(data=obs,aes(x=Year,y=Mean, col=quantile,group=grp,linetype=type),show.legend=T)+
    theme_bw()+facet_grid(obsYears~Anthro2023,labeller = "label_both")+ylab("Expected growth rate")+
    theme(axis.text.x = element_text(angle=90,vjust=0.5,hjust=1,size=8))+
    geom_hline(yintercept=1, color = "black",size=0.7)+scale_fill_discrete(type=pal2)+scale_color_discrete(type=pal2)
  print(base)
  dev.off()

  scResults$rr.summary.all$MetricTypeID = NULL
  probs <- scResults$rr.summary.all%>%
    pivot_wider(names_from = Parameter, values_from = c("Mean","lower","upper","probViable"))
  names(probs)= gsub("_Expected growth rate","",names(probs),fixed=T)
  probs$startYear = probs$startYear+probs$preYears
  probs$projectionTime = probs$Year-2023
  probs$Anthro2023 = probs$tA
  probs$YearsOfProjection = probs$projectionTime

  #See disturbance scenarios
  #distScns = subset(probs,is.element(projectionTime,c(0,5,20))|(Year==iYr))

  if(!is.element(20,probs$projectionTime)){
    lastTime=19
  }else{lastTime=20}
  distScns = unique(subset(probs,is.element(projectionTime,c(0,5,lastTime))|(Year<2023),select=c(startYear,projectionTime,Year,obsYears,Anthro2023,Anthro,projAnthroSlope)))
  distScns$grp = paste0(distScns$Anthro2023,distScns$projAnthroSlope)

  distScns$Timeline = NA
  distScns$Timeline[distScns$Year==2023]="Finish monitoring"
  distScns$Timeline[distScns$projectionTime==5]="End 5 yr projection (2028)"
  distScns$Timeline[distScns$projectionTime==lastTime]="End 20 yr projection (2042)"

  Ps = unique(distScns$obsYears)
  for(s in Ps){
    distScns$Timeline[distScns$Year==(2023-s+1)]= paste("Start",s,"yrs monitoring")
  }

  startLevels = unique(distScns$Timeline[distScns$Year<2023])
  #distScns$Timeline = factor(distScns$Timeline,levels=c(startLevels[length(startLevels):1],"Finish monitoring","Assessment 2028","Assessment 2043"))
  distScns$Anthro2023=as.factor(distScns$Anthro2023)
  if(is.element(0,levels(distScns$Anthro2023))){
    levels(distScns$Anthro2023) = rev(c("high","med-high","low-med","low"))
  }else{
    levels(distScns$Anthro2023) = rev(c("high","med-high","low-med"))
  }
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

  #make wide obs table
  scResults$obs.all$MetricTypeID= NULL
  scResults$obs.all = subset(scResults$obs.all,!is.na(Parameter))
  obsWide <- subset(scResults$obs.all,(Type=="true"))%>%
    pivot_wider(names_from = Parameter, values_from = Mean)
  obsWide$startYear=obsWide$startYear+obsWide$preYears
  #obsWide$c = obsWide[["Adjusted recruitment"]]*2/obsWide[["Recruitment"]]

  #now combine and make small
  names(obsWide)[names(obsWide)=="Expected growth rate"]= "trueMean"
  names(obsWide)[names(obsWide)=="Female population size"]= "trueSize"

  probs = subset(probs,is.element(projectionTime,c(0,5,lastTime)))
  obsWide=subset(obsWide, is.element(Year,2023+c(0,5,lastTime))&(Type=="true"))

  nrow(probs)
  intersect(names(probs),names(obsWide))
  probs = merge(probs,obsWide)
  nrow(probs)
  obsWide=NULL

  probs$trueChange = probs$trueMean#probs$trueSize/probs$N0
  probs$predChange = probs$Mean#probs$projSize/probs$N0

  probs$viableTrue = (probs$trueChange>0.99)#&(probs$trueSize>10)
  probs$viablePred = (probs$predChange>0.99)#&(probs$projSize>10)

  probs$viableTrueB = cut_width(pmax(0.98,pmin(1.02,probs$trueChange)), width=0.02,center=1)
  probs$viablePredB = cut_width(pmax(0.98,pmin(1.02,probs$predChange)), width=0.02,center=1)

  probs$wrong = probs$viableTrue != probs$viablePred

  probs$CorrectStatus[probs$wrong]="no"
  probs$CorrectStatus[!probs$wrong]="yes"
  probs$LambdaDiff = probs$trueChange-probs$predChange

  probs$pageLabB = paste0(batchStrip(probs$pageLab),"st",probs$collarCount,"ri",probs$ltyVariable)
  pagesB=unique(probs$pageLabB)

  probs$AnthroScn=as.factor(probs$Anthro2023)

  if(is.element(0,levels(probs$AnthroScn))){
    levels(probs$AnthroScn) = c("low","low-med","med-high","high")
  }else{
    levels(probs$AnthroScn) = c("low-med","med-high","high")
  }

  if(0){
    for(pp in pagesB){
      #pp=pagesB[3]
      png(here::here(paste0("figs/",setName,"/bands",pp,".png")),
          height = 4, width = 7.48, units = "in",res=600)
      base=ggplot(subset(probs,pageLabB==pp),aes(x=obsYears,y=Mean,col=CorrectStatus))+geom_point(shape="-",size=3,alpha=0.5)+
        facet_grid(YearsOfProjection~AnthroScn,labeller="label_both")+
        theme_bw()+xlab("Years of monitoring")+ylab("Estimated expected growth rate")
      print(base)
      dev.off()

      png(here::here(paste0("figs/",setName,"/bandsTrue",pp,".png")),
          height = 4, width = 7.48, units = "in",res=600)
      base=ggplot(subset(probs,pageLabB==pp),aes(x=obsYears,y=trueMean,col=CorrectStatus))+geom_point(shape="-",size=3,alpha=0.5)+
        facet_grid(YearsOfProjection~AnthroScn,labeller="label_both")+
        theme_bw()+xlab("Years of monitoring")+ylab("True mean growth rate")
      print(base)
      dev.off()

    }
  }

  probs$pageLab = batchStrip(probs$pageLab)
  pagesCa = unique(probs$pageLab)
  probs$RenewalInterval=as.factor(probs$ltyVariable)
  probs$NumCollars = as.factor(probs$collarCount)
  probs$grp = paste(probs$obsYears,probs$NumCollars)

  probs$CollarYrs = as.numeric(as.character(probs$NumCollars))*probs$obsYears
  for(pp in pagesCa){
    #pp=pagesCa[1]
    probs$Sdiff = probs[["Adult female survival"]]-probs[["Mean_Adult female survival"]]
    probs$Rdiff = probs[["Adjusted recruitment"]]-probs[["Mean_Adjusted recruitment"]]

    for (ltyS in unique(probs$ltyVariable)){
      #ltyS="low"
      base=ggplot(subset(probs,(pageLab==pp)&(ltyVariable==ltyS)),aes(x=as.factor(obsYears),y=LambdaDiff,col=NumCollars,fill=NumCollars,group=grp))+
        geom_violin(alpha=0.5)+ylim(-0.15,0.15)+
        #stat_summary(fun = "mean",
        #             geom = "crossbar",
        #             size = 0.3,
        #             position = position_dodge(width = 0.9))+
        facet_grid(YearsOfProjection~AnthroScn,labeller="label_both")+labs(color="Number of\ncollars",fill="Number of\ncollars")+
        theme_bw()+xlab("Years of monitoring")+ylab("Difference between true growth rate and posterior mean")+
        scale_color_discrete(type=(pal4))+scale_fill_discrete(type=(pal4))

      png(here::here(paste0("figs/",setName,"/diffs",ltyS,pp,".png")),
          height = 5, width = 7.48, units = "in",res=600)
      print(base)
      dev.off()

      pdf(here::here(paste0("figs/",setName,"/diffs",ltyS,pp,".pdf")),
          height = 5, width = 7.48)
      print(base)
      dev.off()

      base=ggplot(subset(probs,(pageLab==pp)&(ltyVariable==ltyS)),aes(x=as.factor(obsYears),y=Sdiff,col=NumCollars,fill=NumCollars,group=grp))+
        geom_violin(alpha=0.5)+ylim(-0.15,0.15)+
        #stat_summary(fun = "mean",
        #             geom = "crossbar",
        #             size = 0.3,
        #             position = position_dodge(width = 0.9))+
        facet_grid(YearsOfProjection~AnthroScn,labeller="label_both")+labs(color="Number of\ncollars",fill="Number of\ncollars")+
        theme_bw()+xlab("Years of monitoring")+ylab("Difference between true survival and posterior mean")+
        scale_color_discrete(type=(pal4))+scale_fill_discrete(type=(pal4))

      png(here::here(paste0("figs/",setName,"/diffsS",ltyS,pp,".png")),
          height = 5, width = 7.48, units = "in",res=600)
      print(base)
      dev.off()

      pdf(here::here(paste0("figs/",setName,"/diffsS",ltyS,pp,".pdf")),
          height = 5, width = 7.48)
      print(base)
      dev.off()

      base=ggplot(subset(probs,(pageLab==pp)&(ltyVariable==ltyS)),aes(x=as.factor(obsYears),y=Rdiff,col=NumCollars,fill=NumCollars,group=grp))+
        geom_violin(alpha=0.5)+ylim(-0.15,0.15)+
        #stat_summary(fun = "mean",
        #             geom = "crossbar",
        #             size = 0.3,
        #             position = position_dodge(width = 0.9))+
        facet_grid(YearsOfProjection~AnthroScn,labeller="label_both")+labs(color="Number of\ncollars",fill="Number of\ncollars")+
        theme_bw()+xlab("Years of monitoring")+ylab("Difference between true survival and posterior mean")+
        scale_color_discrete(type=(pal4))+scale_fill_discrete(type=(pal4))

      png(here::here(paste0("figs/",setName,"/diffsR",ltyS,pp,".png")),
          height = 5, width = 7.48, units = "in",res=600)
      print(base)
      dev.off()

      pdf(here::here(paste0("figs/",setName,"/diffsR",ltyS,pp,".pdf")),
          height = 5, width = 7.48)
      print(base)
      dev.off()

    }

    #lots of collars, lots of monitoring, 5 yrs projection
    probf = subset(probs,(pageLab==pp)&(NumCollars==60)&(obsYears==24)&(YearsOfProjection==0))
    probf$Interannual = probf$ltyVariable

    probf$Sdot = probf[["Adult female survival"]]
    probf$S = probf[["Mean_Adult female survival"]]
    #probf = subset(probf,Sdot>0.85)

    base=ggplot(probf,aes(x=Sdot,y=S,col=c))+
      geom_point(alpha=0.3)+scale_colour_gradient2(
        low = muted("red"),
        mid = "grey",
        high = muted("blue"),
        midpoint = 1,
      )+geom_smooth(col="black")+
      facet_wrap(~Interannual,labeller=label_both)+geom_abline(slope=1,linetype=2)+
      xlab("True survival")+ylab("Posterior mean survival")+theme_bw()

    png(here::here(paste0("figs/",setName,"/diffsSFocus",pp,".png")),
        height = 3, width = 5.51, units = "in",res=600)
    print(base)
    dev.off()
    pdf(here::here(paste0("figs/",setName,"/diffsSFocus",pp,".pdf")),
        height = 3, width = 5.51)
    print(base)
    dev.off()

    probf$Rdot = probf[["Adjusted recruitment"]]
    probf$R = probf[["Mean_Adjusted recruitment"]]
    base=ggplot(probf,aes(x=Rdot,y=R,col=c))+
      geom_point(alpha=0.3)+scale_colour_gradient2(
        low = muted("red"),
        mid = "grey",
        high = muted("blue"),
        midpoint = 1,
      )+geom_smooth(col="black")+
      facet_wrap(~Interannual,labeller=label_both)+geom_abline(slope=1,linetype=2)+
      xlab("True adjusted recruitment")+ylab("Posterior mean adjusted recruitment")+theme_bw()

    png(here::here(paste0("figs/",setName,"/diffsRFocus",pp,".png")),
        height = 3, width = 5.51, units = "in",res=600)
    print(base)
    dev.off()

    pdf(here::here(paste0("figs/",setName,"/diffsRFocus",pp,".pdf")),
        height = 3, width = 5.51)
    print(base)
    dev.off()

    base=ggplot(probf,aes(x=trueMean,y=Mean,col=c))+
      geom_point(alpha=0.3)+scale_colour_gradient2(
        low = muted("red"),
        mid = "grey",
        high = muted("blue"),
        midpoint = 1,
      )+geom_smooth(col="black")+
      facet_wrap(~Interannual,labeller=label_both)+geom_abline(slope=1,linetype=2)+
    xlab("True mean growth rate")+ylab("Posterior mean growth rate")+theme_bw()

    png(here::here(paste0("figs/",setName,"/diffsFocus",pp,".png")),
        height = 3, width = 5.51, units = "in",res=600)
    print(base)
    dev.off()

    pdf(here::here(paste0("figs/",setName,"/diffsFocus",pp,".pdf")),
        height = 3, width = 5.51)
    print(base)
    dev.off()
  }

  #######################
  #EVSI calculations. See Dunham et al and references therein, and EVPIFromNationalSims.R

  #probs$cBin = cut_width(pmax(0.94,pmin(1.06,probs$c)), width=0.1,center=1)
  #table(probs$cBin)
  #levels(probs$cBin)=c("<0.95","mid",">1.05")

  hist(probs$Mean)

  #all results together
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
    summarize(propWrong = mean(wrong)*100,propViableTrue=mean(viableTrue)*100,propViablePred = mean(viablePred)*100,
              propViablePosterior=mean(probViable)*100)

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
    base=ggplot(subset(probsSum,pageLabC==pp),aes(x=obsYears,y=100-propWrong,col=NumCollars,linetype=RenewalInterval,group=grp))+geom_line()+
      facet_grid(YearsOfProjection~AnthroScn,labeller="label_both")+labs(color="Number of\ncollars", linetype=ltyLabel)+
      theme_bw()+xlab("Years of monitoring")+ylab("Probability of correct status assessment")+
      scale_color_discrete(type=(pal3)) + scale_x_continuous(breaks=c(0,5,10,15,20))

    png(here::here(paste0("figs/",setName,"/power",pp,".png")),
        height = 5, width = 7.48, units = "in",res=600)
    print(base)
    dev.off()

    pdf(here::here(paste0("figs/",setName,"/power",pp,".pdf")),
        height = 5, width = 7.48)
    print(base)
    dev.off()

    png(here::here(paste0("figs/",setName,"/EVsample",pp,".png")),
        height = 5, width = 7.48, units = "in",res=600)
    base=ggplot(subset(probsSum,pageLabC==pp),aes(x=obsYears,y=EVsample,col=NumCollars,linetype=RenewalInterval,group=grp))+geom_line()+
      facet_grid(YearsOfProjection~AnthroScn,labeller="label_both")+labs(color="Number of\ncollars", linetype=ltyLabel)+
      theme_bw()+xlab("Years of monitoring")+ylab("EVsample")+
      scale_color_discrete(type=(pal3))+ scale_x_continuous(breaks=c(0,5,10,15,20))
    print(base)
    dev.off()

    png(here::here(paste0("figs/",setName,"/probChecks",pp,".png")),
        height = 5, width = 7.48, units = "in",res=600)
    base=ggplot(subset(probsSum,pageLabC==pp),aes(x=obsYears,y=propViableTrue,col=NumCollars,linetype=RenewalInterval,group=grp))+geom_line()+
      facet_grid(YearsOfProjection~AnthroScn,labeller="label_both")+labs(color="Number of\ncollars", linetype=ltyLabel)+
      theme_bw()+xlab("Years of monitoring")+ylab("propViableTrue")+
      scale_color_discrete(type=(pal3))+ scale_x_continuous(breaks=c(0,5,10,15,20))
    print(base)
    dev.off()


    png(here::here(paste0("figs/",setName,"/powerEffort",pp,".png")),
        height = 5, width = 7.48, units = "in",res=600)
    base=ggplot(subset(probsSum,pageLabC==pp),aes(x=CollarYrs,y=100-propWrong,linetype=RenewalInterval,col=NumCollars,group=grp))+geom_line()+
      facet_grid(YearsOfProjection~AnthroScn,labeller="label_both")+labs(color="Number of\ncollars", linetype=ltyLabel)+
      theme_bw()+xlab("Years of monitoring * NumCollars")+ylab("Probability of correct status assessment")+
      scale_color_discrete(type=(pal3))
    print(base)
    dev.off()

  }

  ####################################
  #risk of error in cases where predictive interval does not include 1?
  probs$BandWidth1 = (probs$lower<1)&(probs$upper>=1)
  errorsByBand <- probs %>% group_by(BandWidth1) %>%
    summarize(probWrong = 100*mean(wrong))

  write.csv(errorsByBand,here::here(paste0("tabs/",setName,"/ErrorsByBand",batchStrip(p),".csv")),row.names=F)

  hist(probs$c)
  probs$cClass = cut_width(pmax(0.86,pmin(1.3,probs$c)), width=0.1,center=1)
  table(probs$cClass)
  levels(probs$cClass)[1]="<=0.95"

  probErrorPlot = ggplot(probs,aes(x=probViable*100,y=100*(1-as.numeric(wrong)),color=cClass,fill=cClass,group=cClass))+
    geom_smooth()+
    theme_bw()+xlab("Posterior probability of viability")+ylab("Probability of correct status assessment")+
    geom_abline(intercept=95,slope=0,linetype=2)+labs(color="True \ncomposition \nbias",fill="True \ncomposition \nbias")+
    scale_color_discrete(type=(pal4b))+scale_fill_discrete(type=(pal4b))+
    scale_x_continuous(breaks=seq(0,90,by=10))

  png(here::here(paste0("figs/",setName,"/probViability",batchStrip(p),".png")),
      height = 3.5, width =3.543, units = "in",res=600)
  print(probErrorPlot)
  dev.off()

  pdf(here::here(paste0("figs/",setName,"/probViability",batchStrip(p),".pdf")),
      height = 3.5, width = 3.543)
  print(probErrorPlot)
  dev.off()

  ####################################
  #grouped by lambda - omit voi, focus on low scenario
  probsLow = subset(probs,AnthroScn=="low")

  if(nrow(probsLow)>0){
    hist(probsLow$trueMean)
    probsLow$GrowthRate = cut_width(pmax(0.96,pmin(1.03,probsLow$trueMean)), width=0.02,center=1)
    table(probsLow$GrowthRate)
    levels(probsLow$GrowthRate)[1]="<=0.97"
    levels(probsLow$GrowthRate)[length(levels(probsLow$GrowthRate))]=">1.01"
    levels(probsLow$GrowthRate)

    groupVars = c("Anthro","AnthroScn","YearsOfProjection","GrowthRate",setdiff(names(scns),c("rQuantile","sQuantile","rep","pageId","repBatch")))

    probsSum <- probsLow %>% group_by(across(groupVars)) %>%
      summarize(propWrong = mean(wrong),propViableTrue=mean(viableTrue),propViablePred = mean(viablePred),
                propViablePosterior=mean(probViable))

    probsSum$grp = paste(probsSum$collarCount,probsSum$ltyVariable)
    probsSum$pageLabC = batchStrip(probsSum$pageLab)
    pagesC=unique(probsSum$pageLabC)
    probsSum$RenewalInterval=as.factor(probsSum$ltyVariable)
    probsSum$NumCollars = as.factor(probsSum$collarCount)
    probsSum$CollarYrs = as.numeric(as.character(probsSum$NumCollars))*probsSum$obsYears
    probsSum <- subset(probsSum,collarCount>0)

    for(pp in pagesC){
      #pp=pagesC[1]
      probsSum$Growth=probsSum$GrowthRate
      base=ggplot(subset(probsSum,pageLabC==pp),aes(x=obsYears,y=1-propWrong,col=NumCollars,linetype=RenewalInterval,group=grp))+geom_line()+
        facet_grid(YearsOfProjection~Growth,labeller="label_both")+labs(color="Number of\ncollars", linetype=ltyLabel)+
        theme_bw()+xlab("Years of monitoring")+ylab("Probability of correct status assessment")+
        scale_color_discrete(type=(pal3)) + scale_x_continuous(breaks=c(0,5,10,15,20))

      png(here::here(paste0("figs/",setName,"/powerLam",pp,".png")),
          height = 5, width = 7.48, units = "in",res=600)
      print(base)
      dev.off()

      pdf(here::here(paste0("figs/",setName,"/powerLam",pp,".pdf")),
          height = 5, width = 7.48)
      print(base)
      dev.off()

    }
  }
}

write.csv(EVout,here::here(paste0("tabs/EVsample",setName,".csv")),row.names=F)

write.csv(ProbsOut,here::here(paste0("tabs/ProbsOut",setName,".csv")),row.names=F)

str(subset(EVout,collarCount==0))
