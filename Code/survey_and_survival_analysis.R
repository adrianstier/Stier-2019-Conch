
############
###observed density
############

x=read.csv("data/obs_density.csv")
density=as.numeric(x$density)
habitat=factor(x$habitat)

t.test(asin(sqrt(density))~habitat)
boxplot(density~habitat)


############
###Survival Analysis
############

x=read.csv("data/conch.csv",na.strings="NA")

library(survival)
library(ggplot2)


ttt=as.factor(x$ttt)
ID=as.numeric(x$ID)
habitat=factor(x$habitat)
habitat2=as.factor(x$habitat2)
size=as.numeric(x$initial_size)
Lf=as.numeric(x$Lf)
tethered.density=as.numeric(x$tethered.density)
density=as.numeric(x$overall.density)
date.mortality.recorded=as.Date(x$date.mortality.recorded,format="%m/%d/%Y")
date.last.alive=as.Date(x$date.last.alive,format="%m/%d/%Y")
start.date=as.Date(x$start.date,format="%m/%d/%Y")
time_alive=as.numeric(x$time_alive)
length.experiment=as.numeric(x$length.experiment)
mortality=as.numeric(x$Mortality)
mort2=as.numeric(x$mort2)#inverse of mort


#### Cox Proportional hazard with right censoring (interval no worky on cox)  

#time alive is the midpoint between last seen and observed dead

mod1_int=coxph(Surv(time_alive,mort2)~habitat*density)
mod1=coxph(Surv(time_alive,mort2)~habitat+density)

#LRT to test interaction (note these are partial likelihoods)
anova(mod1_int,mod1)

#Test for Proprotionality
cox.zph(mod1)

#Test for Influential observations
dfbeta=residuals(mod1,type='dfbeta')
plot(dfbeta)


#plot the model with just habitat
plot(survfit(coxph(Surv(time_alive,mortality)~strata(habitat))),lty=c(3,1),log=T)

#legend
legend(5,.2,c("Seagrass","Hardbottom"),lty = c(1,3))

#I want to plot density (x-axis) and survival (y-axis), but I'm having a hard time to get it to predict the response I want. 

##predict values for final time period from cox model the density range and habitats should work here but I'm having a hard time figuring out how to specify that I want predictions for the end of the study. I thought doing it in the first pp row here made sense but it's not working too well and is giving me negative numbers. 

newdata=expand.grid(density=seq(.001,.25,.001),habitat=factor(c("HB","SG")))


pp = predict(mod1,data.frame(newdata),time_alive=45) #don't think referencing time_alive here is correct 

pp2 = data.frame(newdata,surv=pp)


############
###Effects of Density of Survival at time t 
############

x=read.csv("data/density.csv")
density=as.numeric(x$density)
habitat=factor(x$habitat)
surv=as.numeric(x$surv)
surv2=surv/100
x$tsurv=asin(sqrt(surv2))


plot(density,surv,pch=as.numeric(habitat))

mod1=lm(surv~density*habitat,data=x)
summary(mod1)

m2 = with(subset(x,habitat=="h"),lm(tsurv~density))
m3 = with(subset(x,habitat=="s"),lm(tsurv~density))

sin(coef(m2))^2
sin(coef(m3))^2

ggplot(x,aes(x=density,surv2,pch=habitat))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()+
  ylab("Survival")+
  xlab("Density m2")


ggplot(x,aes(x=density,y=surv2))+
  facet_grid(habitat~.)+
  stat_smooth(method=lm)+
  geom_point(size=3)+labs(x="Density m2",y="Survival")+
  theme_bw()