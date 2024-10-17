
library(caribouMetrics)

rawScns<-read.csv(here::here("tabs/s10.csv"))
nrow(rawScns)
rawScns = subset(rawScns,collarCount!=1)

monitoringPars = list(obsYears=list(symbol= "d", description = "Monitoring duration.",unit="years"),
                      collarCount=list(symbol="n",description="Target # of collars.", unit="cows"),
                      collarInterval = list(symbol = "o",description="Years between collar deployments.", unit="years"),
                      cowMult=list(symbol="w",description="Cows per collared animal in composition survey.",unit = "ratio")
                      )

#assessmentYrs = list(symbol="y",description="Assessment period for population growth rate.",unit="years"),
start=T
for(par in names(monitoringPars)){
  #par="st"
  crow = data.frame(name=paste("*",monitoringPars[[par]]$symbol,"*, ",par,sep=""),description=monitoringPars[[par]]$description,
                    units=monitoringPars[[par]]$unit,
                    values = paste(unique(rawScns[,par]),collapse=","))

  if(start){
    tbl = crow
    start=F
  }else{
    tbl=rbind(tbl,crow)
  }
}

