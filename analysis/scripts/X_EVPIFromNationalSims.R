
#Calculation of EVPI (expected value of perfect information) with prior probabilities from national model
#See Dunham et al. 2023 and references therein.
library(caribouMetrics)
library(tidyverse)

#Step 1: get prior belief that true state is s p0_sd
simsBig = getSimsNational(replicates=5000)
lambdas = subset(simsBig$samples,Parameter=="Population growth rate")
lambdas$s1 = (lambdas$Value>0.99)

p0 = lambdas %>%
  group_by(Anthro) %>%
  summarise(p0_sd = mean(s1))
p0$s = 1

p0_s0 = p0; p0_s0$s = 0; p0_s0$p0_sd=1-p0_s0$p0_sd
p0= rbind(p0,p0_s0)

#step 2: value matrix v_as
v = expand.grid(s = c(0,1),a=c(0,1))
v$v_as = as.numeric(v$s==v$a)

#step 3: merge
vals = merge(v,p0)

#step 4: calculate
#Note expected value of certainty in this case is 1
vals$innerProd = vals$p0_sd*vals$v_as

Es = vals %>%
  group_by(Anthro,a) %>%
  summarise(Es = sum(innerProd))

EVPI = Es %>%
  group_by(Anthro) %>%
  summarise(EVPI = 1-max(Es),EVuncertainty=max(Es))

write.csv(EVPI,paste0("tabs/EVPI.csv"),row.names=F)

plot(EVPI~Anthro,data=EVPI)
