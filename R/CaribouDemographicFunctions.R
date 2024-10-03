#############################################################################

#################################################################################


# functions for caribou demographics paper

# run scenario - temporary wrapper
runScenario<-function(scns,quants=NULL,Anthro=NULL,survAnalysisMethod="KaplanMeier",getKSDists=T){
  #quants=NULL;Anthro=NULL;survAnalysisMethod="Exponential";getKSDists=F
  retdir <-getwd()

  dir.create("figs")
  dir.create("tabs")
  dir.create("results")

  library(caribouMetrics)
  library(tidyr)
  library(dplyr)

  scns<-getScenarioDefaults(scns)

  eParsIn = list()
  eParsIn$cowCounts <- data.frame(Year = 1981:2023,
                                  Count = 100,
                                  Class = "cow")
  eParsIn$freqStartsByYear <- data.frame(Year = 1981:2023,
                                         numStarts = 30)
  eParsIn$collarOnTime=1
  eParsIn$collarOffTime=12
  eParsIn$collarNumYears=6

  ##########
  #Get full set of sims for comparison

  if(is.null(quants)){
    simBig<-getSimsNational()#If called with default parameters, use saved object to speed things up.
  }else{
    simBig<-getSimsNational(useQuantiles = quants,Anthro=Anthro)#If called with default parameters, use saved object to speed things up.
  }

  scResults = caribouMetrics:::runScnSet(scns,eParsIn,simBig,survAnalysisMethod,getKSDists=getKSDists,printProgress=T)
  return(scResults)
}

makeInterceptPlots <- function(scResults, addBit = "", facetVars = NULL,
                               loopVars = NULL,
                               whichPlots = c("Adult female survival",
                                              "Population growth rate",
                                              "Recruitment",
                                              "Female population size"),
                               survLow = 0.6, type = "png", useNational = T,lowLine=NULL,highLine=NULL) {
  # facetVars=NULL;loopVars="sNuMax";scResults=scResults
  #useNational=T;type="png"; survLow=0.6
  if (!is.null(loopVars)) {
    loopSet <- unique(subset(scResults$rr.summary.all, select = loopVars))
    loopSet$dummy <- 1
  } else {
    loopSet <- data.frame(dummy = 1)
  }


  for (l in 1:nrow(loopSet)) {
    # l = 1
    crow <- loopSet[l, ]

    aa <- ""
    for (n in names(crow)) {
      if (n == "dummy") {
        next
      }
      aa <- paste0(aa, n, crow[[n]])
    }

    addBitO <- paste0(addBit, aa)

    scResCur = scResults
    scResCur$sim.all = merge(scResCur$sim.all,crow)
    scResCur$rr.summary.all = merge(scResCur$rr.summary.all, crow)
    if(is.element("obs.all",names(scResCur))){
      scResCur$obs.all = merge(scResCur$obs.all, crow)
    }
    if (!(useNational)) {
      scResCur$sim.all = NULL
    }

    if (is.element("Adult female survival", whichPlots)) {
      if (type == "png") {
        png(here::here(paste0("figs/Surv", addBitO, ".png")),
            height = 6, width = 7.48, units = "in", res = 600
        )
      } else {
        pdf(paste0("figs/Surv", addBitO, ".pdf"), width = 10, height = 7)
      }
      print(caribouMetrics:::plotRes(scResCur, "Adult female survival",
                    lowBound = survLow, facetVars = facetVars
      ))
      dev.off()
    }

    if (is.element("Population growth rate", whichPlots)) {
      if (type == "png") {
        png(here::here(paste0("figs/Lambda", addBitO, ".png")),
            height = 6, width = 7.48, units = "in", res = 600
        )
      } else {
        pdf(paste0("figs/Lambda", addBitO, ".pdf"), width = 10, height = 7)
      }
      print(caribouMetrics:::plotRes(scResCur, "Population growth rate",
                    lowBound = 0, facetVars = facetVars
      ))
      dev.off()
    }

    if (is.element("Recruitment", whichPlots)) {
      if (type == "png") {
        png(here::here(paste0("figs/Rec", addBitO, ".png")),
            height = 6, width = 7.48, units = "in", res = 600
        )
      } else {
        pdf(paste0("figs/Rec", addBitO, ".pdf"), width = 10, height = 7)
      }
      print(caribouMetrics:::plotRes(scResCur, "Recruitment",
                    lowBound = 0, facetVars = facetVars
      ))
      dev.off()
    }

    if (is.element("Female population size", whichPlots)) {
      if (type == "png") {
        png(here::here(paste0("figs/FPOP", addBitO, ".png")),
            height = 6, width = 7.48, units = "in", res = 600
        )
      } else {
        pdf(paste0("figs/FPOP", addBitO, ".pdf"), width = 10, height = 7)
      }
      print(caribouMetrics:::plotRes(scResCur, "Female population size",
                    lowBound = 0, facetVars = facetVars
      ))
      dev.off()
    }
  }
}









