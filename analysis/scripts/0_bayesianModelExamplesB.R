# Prior and posterior predictions from Bayesian model
library(caribouMetrics)
library(tidyverse)
library(ggpubr)
theme_set(theme_bw())
library(RColorBrewer)

baseDir <- "."

monitoringScns = data.frame(obsYears=c(24),collarCount=c(60),cowMult=c(6),collarInterval=c(1),
                            assessmentYrs=c(1))
stateScns = data.frame(obsAnthroSlope=c(1),projAnthroSlope=c(1))
stateScns = merge(stateScns,data.frame(rep=seq(1:1)))
stateScns = merge(stateScns,data.frame(interannualVar=c(NA,"list(R_CV=0.46,S_CV=0.087)","list(R_CV=0.23,S_CV=0.0435)")))
stateScns = merge(stateScns,data.frame(sQuantile = c(0.1,0.5,0.9),rQuantile=c(0.1,0.5,0.9)))
stateScns$qMax = 0;stateScns$uMax=0;stateScns$zMax=0
scns=merge(monitoringScns,stateScns)

scns$iAnthro = 0
scns$tA = scns$iAnthro+(scns$obsYears)*scns$obsAnthroSlope
scns$projYears = 50-scns$obsYears
scns$N0 = 2000

scns
#scns$assessmentYrs=3

####################
eParsIn = list()
eParsIn$cowCounts <- data.frame(Year = 1981:2023,
                                Count = 100,
                                Class = "cow")
eParsIn$freqStartsByYear <- data.frame(Year = 1981:2023,
                                       numStarts = 30)
eParsIn$collarOnTime=1
eParsIn$collarOffTime=12
eParsIn$collarNumYears=1
labFontSize = 10; breakInterval=5

###############################
#With bias adjustment
simBig<-getSimsNational(replicates=3000,forceUpdate=T)

results = caribouMetrics:::runScnSet(scns,eParsIn,simBig,getKSDists=F,printProgress=F)

localPlotFn<-function(modTables,parameter,lowBound,highBound,facetVars,legendPosition,breakInterval){
  allRes <- modTables$rr.summary.all
  obs <- modTables$obs.all
  simRange <- NULL#modTables$sim.all

  pal2 = c("#EF8A62","#67A9CF")#brewer.pal(7,"RdBu")[c(2,6)]

    titleFontSize <- 11
    labFontSize <- 10
    if(breakInterval==1){
      breakInterval <- 2
    }


  df <- subset(allRes, allRes$Parameter == parameter)

  pr <- parameter
  obs <- subset(obs, parameter == pr)

  if(!is.null(simRange)){
    pr <- parameter
    simRange <- subset(simRange, parameter == pr)
    if(nrow(simRange) == 0){
      simRange <- NULL
    }
  }

  if ( !is.null(simRange)) {

    df$Type <- "Bayesian"
    simRange$Type <- "national"
    nameSel <- c(c("Year", "Mean", "Lower 95% CRI", "Upper 95% CRI", "Type"), facetVars)
    df <- rbind(subset(df, select = nameSel), subset(simRange, select = nameSel))
    df$grp <- df$Type
    if (!is.null(facetVars)) {
      for (i in facetVars) {
        df$grp <- paste0(df$grp, df[[i]])
      }
    }
    x1 <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["Year"]], y = .data[["Mean"]],
                                           fill = .data[["Type"]], col = .data[["Type"]]))
  } else {
    x1 <- ggplot2::ggplot(df, ggplot2::aes(x = .data[["Year"]], y = .data[["Mean"]],colour=factor(collarCount),group=collarCount))
  }
  x2 <- x1 + ggplot2::theme_classic() + ggplot2::xlab("Year") +
    ggplot2::ylab(parameter) +
    ggplot2::geom_line(ggplot2::aes(x = .data[["Year"]], y = .data[["Mean"]],colour=factor(collarCount),group=collarCount),
                       linewidth = 1.75) +
    ggplot2::scale_color_discrete(type=pal2, name = NULL)+
    ggplot2::theme(
      legend.position = legendPosition,
      axis.text.y = ggplot2::element_text(size = labFontSize),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, size = labFontSize),
      axis.title.x = ggplot2::element_text(size = titleFontSize, face = "bold"),
      axis.title.y = ggplot2::element_text(size = titleFontSize, face = "bold")
    ) +
    ggplot2::scale_x_continuous(breaks = seq(
      min(df$Year, na.rm = TRUE),
      max(df$Year, na.rm = TRUE), breakInterval
    ))

    x2 <- x2 + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[["Lower 95% CRI"]],
                                                 ymax = .data[["Upper 95% CRI"]],fill=factor(collarCount),group=collarCount),
                                    show.legend = FALSE, alpha = 0.25, colour = NA
    ) +ggplot2::scale_fill_discrete(type=pal2, name = NULL)+
      ggplot2::scale_y_continuous(limits = c(
        ifelse(any(df$`Lower 95% CRI` < lowBound), NA, lowBound),
        ifelse(any(df$`Upper 95% CRI` > 1), NA, highBound)
      ))

  if (!is.null(obs)) {
    if(nrow(obs) > 0){
      obs$Type <- "Bayesian"
      obs$obsError <- FALSE
      obs$obsError[obs$type == "observed"] <- TRUE
      obs <- filter(obs, !is.na(.data$Mean))
      x2 <- x2 + ggplot2::geom_point(data = obs,
                                     ggplot2::aes(x = .data[["Year"]], y = .data[["Mean"]],
                                                  shape = .data[["obsError"]]),
                                     col = "black", fill = "black", inherit.aes = FALSE,
                                     show.legend = TRUE) +
        ggplot2::scale_shape_manual(values = c(16, 2),
                                    labels = c(`TRUE` = "observed",
                                               `FALSE` = "true\nsimulated"),
                                    name = NULL)
    }
  }

  if (!is.null(facetVars)) {
    if (length(facetVars) == 2) {
      x2 <- x2 + ggplot2::facet_grid(as.formula(paste(facetVars[1], "~", facetVars[2])),
                                     labeller = "label_both")
    } else {
      x2 <- x2 + ggplot2::facet_wrap(as.formula(paste0("~", facetVars[1])),
                                     labeller = "label_both")
    }
  }
  if ( (parameter == "Population growth rate")) {
    x2 <- x2 + ggplot2::geom_hline(yintercept = 1, color = "black")+ggplot2::ylab("Expected population growth rate")
  }

  #plot(x2)
  return(x2)
}


  modTables=results; parameter="Population growth rate";lowBound=0.7; highBound=1.2;facetVars=c("interannualVar","sQuantile");legendPosition="right";breakInterval=1

  x2 = localPlotFn(modTables,parameter,lowBound,highBound,facetVars,legendPosition,breakInterval)
  plot(x2)

  ggsave(paste0(baseDir,"/analysis/paper/figs/bayesianModelExamplesB.png"),
         width = 9.6*0.779, height = 9.2, units = "in",
         dpi = 1200)

  modTables=results; parameter="Adult female survival";lowBound=0; highBound=1;facetVars=c("interannualVar","sQuantile");legendPosition="right";breakInterval=1
  x2 = localPlotFn(modTables,parameter,lowBound,highBound,facetVars,legendPosition,breakInterval)
  plot(x2)

  ggsave(paste0(baseDir,"/analysis/paper/figs/bayesianModelExamplesBSurv.png"),
         width = 9.6*0.779, height = 9.2, units = "in",
         dpi = 1200)

  modTables=results; parameter="Recruitment";lowBound=0; highBound=1;facetVars=c("interannualVar","sQuantile");legendPosition="right";breakInterval=1
  x2 = localPlotFn(modTables,parameter,lowBound,highBound,facetVars,legendPosition,breakInterval)
  plot(x2)

  ggsave(paste0(baseDir,"/analysis/paper/figs/bayesianModelExamplesBRec.png"),
         width = 9.6*0.779, height = 9.2, units = "in",
         dpi = 1200)

