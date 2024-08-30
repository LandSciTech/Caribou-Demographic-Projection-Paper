library(ggplot2)
library(tidyverse)
library(caribouMetrics)
#Notes
#q: assume more females than males in calf:cow group, so q<1

p=expand.grid(delay=c(T,F),q=c(0,0.3,0.6),w=c(1,3,6),
              u=c(0,0.1,0.2),z=seq(0,0.2,length.out=3),R=seq(0.15,0.6,length.out=10))

getX<-function(R,delay,q,w,u,z){
  X = 0.5*R*w*(q*u+1-u)/((q*u+w-u)*(1-z))
  X=X*(1-delay)+delay*X/(1+X)
  return(X)
}

p$X = getX(p$R,p$delay,p$q,p$w,p$u,p$z)

p$grp = paste(p$u,p$z)
names(p)
p$u=as.factor(p$u)
p$z=as.factor(p$z)

base = ggplot(subset(p,!delay),aes(x=R,y=2*X/R,col=z,linetype=u,group=grp))+
  geom_hline(yintercept=1)+geom_line()+facet_grid(q~w,labeller=label_both)+theme_bw()
plot(base)

base2 = ggplot(subset(p,delay),aes(x=R,y=2*X/R,col=z,linetype=u,group=grp))+
  geom_hline(yintercept=1)+geom_line()+facet_grid(q~w,labeller=label_both)+theme_bw()
plot(base2)

#############
#approximate distribution for Bayesian model - lognormal
cr=expand.grid(w = c(2,3,4,5,6,7,8,9),rep=seq(1,10000))
nr=nrow(cr)
cr$c = compositionBiasCorrection(q=runif(nr,0,0.6),w=cr$w,u=runif(nr,0,0.2),z=runif(nr,0,0.2))

cs = compositionBiasCorrection(q=runif(nr,0,0.6),w=cr$w,u=runif(nr,0,0.2),z=runif(nr,0,0.2),approx=T)

cr = merge(cr,cs)
cr$ca= exp(rnorm(nrow(cr),mean=cr$mu,sd=cr$sig2^0.5))

base = ggplot(cr,aes(x=c))+geom_histogram(aes(y=..density..))+
  facet_wrap(~w,nrow=3,labeller=label_both)+theme_bw()+
  geom_density(aes(x=ca),alpha=.2, fill="#FF6666",col = "#FF6666")
plot(base)

mns <- cr %>%
  group_by(w) %>%
  summarise(c_mean = mean(c), c_sd = sd(c),ca_mean=mean(ca),ca_sd=sd(ca))

base2 = ggplot(mns,aes(x=c_mean,y=ca_mean))+geom_point()+geom_abline(slope=1,intercept=0)
plot(base2)
#consider high and low bounds. When q and u = 1, c= 1/(1-z)
z = seq(0,0.99,length.out=100)
c = 1/(1-z)
plot(log(c)~z)

