########################
#EVSI: see Dunham et al. 2023 and references therein.
#Run after viewSensitivityResults.R

library(tidyverse)
library(ggplot2)
library(caribouMetrics)
library(RColorBrewer)

pal2 = brewer.pal(7,"RdBu")[c(2,6)]
pal4 = brewer.pal(5,"RdBu")[c(1,2,4,5)]

pal4b = brewer.pal(5,"Purples")[c(2,3,4,5)]

scn_defaults <- eval(formals(getScenarioDefaults))

batchStrip<-function(l,batches=c(10,seq(1:9))){
  for(b in batches){
    l=gsub(paste0("repBatch",b),"",l,fixed=T)
  }
  return(l)
}

setName = "s16"
probsSum = read.csv(here::here(paste0("tabs/EVsample",setName,".csv")),stringsAsFactors = F)

probsSum$pageLabC = batchStrip(probsSum$pageLab)
pagesC=unique(probsSum$pageLabC)

unique(probsSum$AnthroScn)
probsSum$AnthroScn = factor(probsSum$AnthroScn, levels = c("low","low-med","med-high","high"))


if(is.element("interannualVar",names(probsSum))){
  ltyLabel = "Interannual\nvariation"
}else{
  ltyLabel = "Collar\nrenewal\ninterval"
}


EVuncertainty = subset(probsSum,collarCount==0)
#i.e. obsYears == 0 results
str(EVuncertainty)
EVuncertainty=subset(EVuncertainty,select=c(EVsample,EVvar,YearsOfProjection,AnthroScn,ltyVariable,pageLab,pageLabC))
names(EVuncertainty)[names(EVuncertainty)=="EVsample"]="EVuncertainty"
hist(EVuncertainty$EVvar)#expect low values here. Method used to calculate EVuncertainty assumes priors probs do not differ among replicates.

EVuncertainty$EVPI = 1-EVuncertainty$EVuncertainty

#EVPI across more scenarios

str(EVuncertainty)

EVuncertainty$cmult=substr(EVuncertainty$pageLabC,6,6)

EVuncertainty$trend=substr(EVuncertainty$pageLabC,13,13)
EVuncertainty$trend[EVuncertainty$trend=="-"]= "decreasing"
EVuncertainty$trend[EVuncertainty$trend=="0"]= "stable"
EVuncertainty$trend[EVuncertainty$trend=="1"]= "increasing"
EVuncertainty$YearsOfProjection =as.factor(EVuncertainty$YearsOfProjection)

subset(EVuncertainty, (AnthroScn=="high")&(trend=="decreasing"))

png(here::here(paste0("figs/",setName,"/EVPIall.png")),
    height = 3.8, width = 7.48, units = "in",res=600)
base=ggplot(EVuncertainty,aes(x=YearsOfProjection,y=EVPI,col=cmult,group=pageLabC))+geom_point()+
  facet_grid(trend~AnthroScn,labeller="label_both")+labs(color="cow multiplier")+
  theme_bw()+xlab("years of projection")+ylab("Expected Value of Perfect Information EVPI")+
  scale_color_discrete(type=rev(pal4)[2:4])
print(base)
dev.off()


##########
#EVSI
probsSum <- subset(probsSum,collarCount>0)

probsSum= merge(probsSum,subset(EVuncertainty,select=-EVvar))
probsSum$EVSI = probsSum$EVsample-probsSum$EVuncertainty

names(probsSum)
hist(probsSum$EVvar)
probsSum$grp = paste(probsSum$collarCount,probsSum$ltyVariable)

probsSum$RenewalInterval=as.factor(probsSum$ltyVariable)
probsSum$NumCollars = as.factor(probsSum$collarCount)


probsSum$CollarYrs = as.numeric(as.character(probsSum$NumCollars))*probsSum$obsYears
EVuncertainty$YearsOfProjection=as.factor(EVuncertainty$YearsOfProjection)
for(pp in pagesC){
  #pp=pagesC[1]
  base=ggplot(subset(probsSum,pageLabC==pp),aes(x=obsYears,y=EVSI,col=NumCollars,linetype=RenewalInterval,group=grp))+geom_line()+
    facet_grid(YearsOfProjection~AnthroScn,labeller="label_both")+labs(color="Number of\n collars", linetype=ltyLabel)+
    theme_bw()+xlab("Years of monitoring")+ylab("Expected value of sample information EVSI")+
    scale_color_discrete(type=(pal4))

  png(here::here(paste0("figs/",setName,"/EVSI",pp,".png")),
      height = 3.7, width = 7.48, units = "in",res=600)
  print(base)
  dev.off()

  pdf(here::here(paste0("figs/",setName,"/EVSI",pp,".pdf")),
      height = 3.7, width = 7.48)
  print(base)
  dev.off()

}
