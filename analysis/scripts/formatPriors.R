
#app_path <- "C:/Users/HughesJo/Documents/gitprojects/BayesianCaribouDemographicProjection"
library(caribouMetrics)

rawPriors<-getPriors(returnValues=F)

#tbl <- data.frame()
"Parameter|Description|Mean|Standard Deviation
"

nameSet <- gsub(".Prior1","",names(rawPriors)[grepl("Prior1",names(rawPriors))],fixed=T)

start=T
for(n in nameSet){
  crow = data.frame(name=n,Mean=rawPriors[[paste0(n,".Prior1")]],SE=rawPriors[[paste0(n,".Prior2")]])

  if(start){
    tbl = crow
    start=F
  }else{
    tbl=rbind(tbl,crow)
  }
}

pars <- data.frame(name="l.R",Parameter="$\\beta^R_0$",Description="R intercept")
pars<-rbind(pars,data.frame(name="beta.Rec.anthro",Parameter="$\\beta^R_a$",Description="R anthro slope"))
pars<-rbind(pars,data.frame(name="beta.Rec.fire",Parameter="$\\beta^R_f$",Description="R fire slope"))
pars<-rbind(pars,data.frame(name="sig.R",Parameter="$\\sigma^2_{R}$",Description="R random effect"))
pars <- rbind(pars,data.frame(name="l.Saf",Parameter="$\\beta^S_0$",Description="S intercept"))
pars<-rbind(pars,data.frame(name="beta.Saf",Parameter="$\\beta^S_a$",Description="S anthro slope"))
pars<-rbind(pars,data.frame(name="sig.Saf",Parameter="$\\sigma^2_{S}$",Description="S random effect"))

tbl<-merge(pars,tbl);tbl$name=NULL

