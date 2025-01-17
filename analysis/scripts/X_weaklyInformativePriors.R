#weakly informative priors priors
#https://rdrr.io/github/poissonconsulting/bboutools/f/vignettes/articles/priors.Rmd

#survival
#Intercept (log-odds scale) $$b0 \sim Normal(3, sd = 10)$$
#  Year fixed effect $$bAnnual[i] \sim Normal(0, sd = 10)$$


bbSInt = rnorm(10000,3,10)
bbISInt = 1/(1+exp(-bbSInt))

hist(bbISInt)

sIntMean = mean(bbISInt)

#recruitment
#ntercept (log-odds scale) $$b0 \sim Normal(-1, sd = 5)$$
bbRInt = rnorm(10000,-1,5)
bbIRInt = 1/(1+exp(-bbRInt))

hist(bbIRInt)

rIntMean = mean(bbIRInt)
