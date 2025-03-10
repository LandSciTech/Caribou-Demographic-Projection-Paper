# Example trajectories from national model
library(caribouMetrics)
library(tidyverse)
library(ggpubr)
theme_set(theme_bw())
library(RColorBrewer)

dia_shp <- 23

err_col <- "grey50"

baseDir <- "."

#numbers from Johnson et al for validation
johnsonCompare <- read.csv(paste0(baseDir,"/data/Johnson et al. figures5_6.csv"))

# get outputs from demographics
covTableSim <- data.frame(Anthro = c(0, 10, 20, 30, 40, 50,
                                     60, 70, 80, 90),
                          Fire = 0,
                          fire_excl_anthro = 0)
covTableSim$polygon <- paste0("Missisa_", covTableSim$Anthro + 1)
covTableSim$area <- "FarNorth"
covTableSim$Total_dist <- covTableSim$Anthro + covTableSim$fire_excl_anthro

popGrowthPars <- demographicCoefficients(500,
  modelVersion = "Johnson",
  survivalModelNumber = "M1",
  recruitmentModelNumber = "M4"
)

popGrowthParsSmall <- demographicCoefficients(35,
  modelVersion = "Johnson",
  survivalModelNumber = "M1",
  recruitmentModelNumber = "M4"
)

rateSamples <- demographicRates(
  covTable = covTableSim,
  popGrowthPars = popGrowthParsSmall,
  ignorePrecision = F, returnSample = T, useQuantiles = T
)

rateSamplesLarge <- demographicRates(
  covTable = covTableSim,
  popGrowthPars = popGrowthPars,
  ignorePrecision = F, returnSample = T, useQuantiles = T
)

rateSummaries <- demographicRates(
  covTable = covTableSim, popGrowthPars = popGrowthPars,
  ignorePrecision = F, returnSample = F, useQuantiles = F
)

#for each sample, select bias correction term from distributions

str(popGrowthPars)

cr = data.frame(replicate=unique(rateSamples$replicate))
nr= nrow(cr)
cr$c = compositionBiasCorrection(q=runif(nr,0,0.6),w=runif(nr,3,9),u=runif(nr,0,0.2),z=runif(nr,0,0.2))
rateSamples$c=NULL;rateSamples=merge(rateSamples,cr)

str(rateSamplesLarge)
cr = data.frame(replicate=unique(rateSamplesLarge$replicate))
nr= nrow(cr)
cr$c = compositionBiasCorrection(q=runif(nr,0,0.6),w=runif(nr,3,9),u=runif(nr,0,0.2),z=runif(nr,0,0.2))
rateSamplesLarge$c=NULL;rateSamplesLarge=merge(rateSamplesLarge,cr)

str(rateSamplesLarge)
rateSummaries$c = mean(rateSamplesLarge$c)

rateSamples$S_PIlow <- 1
rateSamples$S_PIhigh <- 1
rateSamples$rep <- as.factor(rateSamples$replicate)
levels(rateSamples$rep) <- sample(unique(rateSamples$replicate), replace = F)
rateSamples$rep <- as.character(rateSamples$rep)
rateSamplesLarge$S_PIlow <- 1
rateSamplesLarge$S_PIhigh <- 1

rateSamples$R_PIlow <- 1
rateSamples$R_PIhigh <- 1
rateSamples$fullGrp <- paste(rateSamples$rep) # ,rateSamples$Fire)
rateSamplesLarge$R_PIlow <- 1
rateSamplesLarge$R_PIhigh <- 1
rateSamplesLarge$fullGrp <- paste(rateSamplesLarge$rep) # ,rateSamplesLarge$Fire)

johnsonCompare$Anthro=johnsonCompare$anthro
johnsonCompare$S_bar=johnsonCompare$Sresp
johnsonCompare$S_PIlow = johnsonCompare$Slow_95_pred
johnsonCompare$S_PIhigh = johnsonCompare$Shigh_95_pred
johnsonCompare$R_bar=johnsonCompare$Rresp/100
johnsonCompare$R_PIlow = johnsonCompare$Rlow_95_pre/100
johnsonCompare$R_PIhigh = johnsonCompare$Rhigh_95_pred/100
johnsonCompare$c = 1

pal = colorRampPalette(brewer.pal(7, "Blues"))(35)

# demography
pars <- data.frame(N0 = 5000)
# increase to get a better sample size, or set interannualVar to NA
pars <- merge(pars, data.frame(rrp = 1))
pars <- merge(pars, rateSamplesLarge)
numSteps <- 3
pars1 <- cbind(pars, caribouPopGrowth(pars$N0,
  numSteps = numSteps, R_bar = pars$R_bar,
  S_bar = pars$S_bar, c=pars$c, probOption = "binomial",
  interannualVar = list(R_CV=0.46*0.25,S_CV=0.08696*0.25)
))

pars <- data.frame(N0 = 5000)
# increase to get a better sample size, or set interannualVar to NA
pars <- merge(pars, data.frame(rrp = 1))
pars <- merge(pars, rateSamples)
numSteps <- 3
pars2 <- cbind(pars, caribouPopGrowth(pars$N0,
  numSteps = numSteps, R_bar = pars$R_bar,
  S_bar = pars$S_bar,c=pars$c, probOption = "binomial",
  interannualVar = list(R_CV=0.46*0.25,S_CV=0.08696*0.25)
))

str(pars)
oo <- pars2 %>%
  select(Anthro, lambdaTrue, fullGrp, rrp) %>%
  group_by(fullGrp, Anthro) %>%
  summarise(lambdaTrue = median(lambdaTrue))

ooT <- pars1 %>%
  select(Anthro, lambdaTrue, fullGrp, rrp) %>%
  group_by(fullGrp, Anthro) %>%
  summarise(lambdaTrue = median(lambdaTrue))

oo$lambdaH <- oo$lambdaTrue
oo$lambdaL <- oo$lambdaTrue

plot_lambda <- ggplot(oo,
                      aes(x = Anthro, y = lambdaTrue, ymin = lambdaH, ymax = lambdaL)) +
  geom_line(size = 0.5,
            aes(x = Anthro, y = lambdaTrue, group = fullGrp, color = fullGrp),
            alpha = 1) + scale_color_manual(values=pal)+
  scale_x_continuous(limits = c(-1, 90), breaks = c(0, 20, 40, 60, 80)) +
  xlab("Anthropogenic disturbance") +
  ylab(expression("Population growth rate " * dot(lambda)[t])) +
  theme(axis.title=element_text(size=rel(0.95)),legend.position = "none", plot.margin = margin(l = 0.6, unit = "cm"))

str(pars1)

plot_recruitment3 <- ggplot(data = rateSummaries,
                            aes(x = Anthro, y = c*R_bar/2,
                                ymin = c*R_PIlow/2, ymax = c*R_PIhigh/2)) +
  geom_ribbon(fill="#67A9CF",colour=NA,data=johnsonCompare,alpha=0.25)+
  geom_line(data = subset(pars2,select=c(Anthro,R_bar,R_PIlow,R_PIhigh,fullGrp,R_t,c)), size = 0.5,
            aes(x = Anthro, y = 0.5*R_t, group = fullGrp, color = fullGrp),
            alpha = 1) +scale_color_manual(values=pal)+
  geom_line(data=johnsonCompare,colour = "black", size = 1.5) +
  geom_line(colour = "#ef8a62", size = 2, linetype = "dotted") +
  scale_x_continuous(limits = c(-1, 90), breaks = c(0, 20, 40, 60, 80)) +
  scale_y_continuous(limits = c(0, 0.5), breaks = c(0, 0.1, 0.2, 0.3, 0.4,0.5)) +
  xlab("Anthropogenic disturbance") +
  ylab(expression("Adjusted recruitment " * dot(X)[t])) +
  theme(axis.title=element_text(size=rel(0.95)),legend.position = "none", plot.margin = margin(l = 0.6, unit = "cm"))
plot(plot_recruitment3)

plot_recruitment1 <- ggplot(data = rateSummaries,
                            aes(x = Anthro, y = R_bar,
                                ymin = R_PIlow, ymax = R_PIhigh)) +
  geom_ribbon(fill="#67A9CF",colour=NA,data=johnsonCompare,alpha=0.25)+
  geom_line(data = subset(pars2,select=c(Anthro,R_bar,R_PIlow,R_PIhigh,fullGrp,R_t,c)), size = 0.5,
            aes(x = Anthro, y = R_t, group = fullGrp, color = fullGrp),
            alpha = 1) +scale_color_manual(values=pal)+
  geom_line(data=johnsonCompare,colour = "black", size = 1.5) +
  geom_line(colour = "#ef8a62", size = 2, linetype = "dotted") +
  scale_x_continuous(limits = c(-1, 90), breaks = c(0, 20, 40, 60, 80)) +
  scale_y_continuous(limits = c(0, 0.7), breaks = c(0, 0.1, 0.2, 0.3, 0.4,0.5,0.6,0.7)) +
  xlab("Anthropogenic disturbance") +
  ylab(expression("Recruitment " * dot(R)[t])) +
  theme(axis.title=element_text(size=rel(0.95)),legend.position = "none", plot.margin = margin(l = 0.6, unit = "cm"))
plot(plot_recruitment1)

base1 <- ggplot(data = rateSummaries,
                aes(x = Anthro, y = S_bar, ymin = S_PIlow, ymax = S_PIhigh)) +
  geom_ribbon(fill="#67A9CF",colour=NA,data=johnsonCompare,alpha=0.25)+
  geom_line(data=subset(pars2,select=c(Anthro,S_bar,S_PIlow,S_PIhigh,rep,S_t)), size = 0.5, alpha = 1,
            aes(x = Anthro, y = S_t, group = rep, colour = rep)) +
  scale_color_manual(values=pal)+
  geom_line(data=johnsonCompare, colour = "black", size = 1.5) +
  geom_line(colour = "#ef8a62", size = 2, linetype = "dotted") +
  xlab("Anthropogenic disturbance") +
  ylab(expression("Adult female survival " * dot(S)[t])) +
  scale_x_continuous(limits = c(-1, 90), breaks = c(0, 20, 40, 60, 80)) +
  scale_y_continuous(limits = c(0.5, 1)) +
  theme(axis.title=element_text(size=rel(0.95)),legend.position = "none", plot.margin = margin(l = 0.6, unit = "cm"))

plot(base1)

# combine ggplots to one figure
ggpubr::ggarrange(plot_recruitment3,base1,plot_lambda,labels = "",
                  ncol = 3, vjust = 1)

ggsave(paste0(baseDir,"/analysis/paper/figs/DemographicRates.png"), width = 9.6*0.779, height = 2.9*0.779, units = "in",
       dpi = 1200)
ggsave(paste0(baseDir,"/analysis/paper/figs_submit/DemographicRates.pdf"), width = 9.6*0.779, height = 2.9*0.779, units = "in",
       dpi = 1200)

ggpubr::ggarrange(plot_recruitment1,base1,plot_lambda,labels = "",
                  ncol = 3, vjust = 1)

ggsave(paste0(baseDir,"/analysis/paper/figs/DemographicRatesNotAdjusted.png"), width = 9.6*0.779, height = 2.9*0.779,  units = "in",
       dpi = 1200)
