# Rachel Zhu Project 3

# given info
library(tidyverse) 
library(ape)
library(nlme)
library(geiger)
library(caper)
library(phytools)
library(viridis)
library(MuMIn)
library(ggplot2)


anole <- read_csv("anole.dat.csv")
anole.eco <- read_csv("anole.eco.csv")

anole2 <- anole%>%
  left_join(anole.eco)%>%
  filter(!Ecomorph%in%c("U","CH"))%>%
  na.omit()%>%
  print()

anole.log <- anole2%>%
  mutate_at(c("SVL","HTotal","PH","ArbPD"),log)

#plot of hind limb length v size in anoles
anole2%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_smooth(method="lm")

anole.lm <- lm(HTotal~SVL,anole2)
coef(anole.lm)

#plot of hind limb v size in anoles w/ a best fit line
anole2%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_abline(slope=coef(anole.lm)[2],intercept=coef(anole.lm)[1],col="blue")

SVL2 <- seq(min(anole2$SVL),max(anole2$SVL),0.1)

pred.lm <-tibble(
  SVL=SVL2,
  H.pred=predict(anole.lm,newdata = data.frame(SVL=SVL2))
)

#HTotal prediction for range of snout vent lengths
anole2%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_point(data=pred.lm,aes(SVL,H.pred),col="blue")

#gives slope and r-squared value
summary(anole.lm)

#fitting allometric model with nls
anole.allo <- nls(HTotal~a*SVL^b, start=list(b=1, a=1),data = anole2)

summary(anole.allo)

#AICc from MuMin 
anole.aic <- AICc(anole.lm,anole.allo)

#AICw from geiger
anole.aicw <- aicw(anole.aic$AICc)

#log transformed data
anole.log%>%
  ggplot(aes(HTotal,SVL,col=Ecomorph2))+geom_point()+geom_smooth(method="lm")

#model including ecomorph variable
anole.log.eco.lm <- lm(HTotal~SVL*Ecomorph2,anole.log)
summary(anole.log.eco.lm)

#assessing effect of Ecomorph2 in the context of how HTotal covaries w/ SVL
anova(anole.log.eco.lm)


anole.log.lm  <- lm(HTotal~SVL,anole.log)
anova(anole.log.lm)

anole.log <- anole.log %>%
  mutate(res=residuals(anole.log.lm))

anole.log%>%
  ggplot(aes(Ecomorph2,res))+geom_point()

p.eco <- anole.log%>%
  ggplot(aes(x=Ecomorph2,y=res)) +geom_boxplot()
print(p.eco)

p.eco+ geom_boxplot() +stat_summary(fun=mean, geom="point", size=3)

anole.tree <- read.tree("anole.tre")
plot(anole.tree,cex=0.4)

pgls.BM1 <- gls(HTotal ~SVL, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")
pgls.BM2 <- gls(HTotal ~SVL * Ecomorph2, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")
pgls.OU1 <- gls(HTotal ~SVL, correlation = corMartins(0,phy = anole.tree,form=~Species),data = anole.log, method = "ML")
pgls.OU2 <- gls(HTotal ~SVL * Ecomorph2, correlation = corMartins(0,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

anole.phylo.aic <- AICc(pgls.BM1,pgls.BM2,pgls.OU1,pgls.OU2)
aicw(anole.phylo.aic$AICc)
anova(pgls.BM2)

anole.log <- anole.log%>%
  mutate(phylo.res=residuals(pgls.BM2))

p.eco.phylo <- anole.log%>%
  ggplot(aes(x=Ecomorph2,y=phylo.res)) +geom_boxplot() +stat_summary(fun=mean, geom="point", size=3)

print(p.eco.phylo)

##Question 1

anole.log%>%
  dplyr::select(Ecomorph2,res,phylo.res)%>%
  pivot_longer(cols=c("res","phylo.res"))%>%
  print%>%
  ggplot(aes(x=Ecomorph2,y=value)) +geom_boxplot() +stat_summary(fun=mean, geom="point", size=3)+facet_grid(name~.,scales = "free_y")+ylab("residual")

##Question 2

anole.PD <- lm(HTotal~SVL+ArbPD,anole.log)
anole.PH <- lm(HTotal~SVL+PH,anole.log)


##Question 3

anole.log <- anole.log %>%
  mutate(resPD=residuals(anole.PD))

anole.log%>%
  ggplot(aes(Ecomorph2,resPD))+geom_point()

anole.log <- anole.log %>%
  mutate(resPH=residuals(anole.PH))

anole.log%>%
  ggplot(aes(Ecomorph2,resPH))+geom_point()


##Question 4

#PGLS Brownian w/ecomorph for PD
pgls.BMPD <- gls(HTotal ~SVL+ArbPD, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#PGLS Brownian w/ecomorph for PH
pgls.BMPH <- gls(HTotal ~SVL+PH, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#PGLS Brownian w/ecomorph for both PH and PD

pgls.BMPHPD <- gls(HTotal ~SVL+PH+ArbPD, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")



##Question 5

anole.phylo.aic.total <- AICc(pgls.BMPD,pgls.BMPH,pgls.BMPHPD)
aicw(anole.phylo.aic.total$AICc)

#Best Fit is pgls.BMPD, delta is 0 for this model

anova(pgls.BMPHPD)


##Question 6

anole.log <- anole.log%>%
  mutate(phylo.res.PHPD=residuals(pgls.BMPHPD))

anole.log%>%
  dplyr::select(SVL,resPD,resPH)%>%
  pivot_longer(cols=c("resPD","resPH"))%>%
  print%>%
  ggplot(aes(x=SVL,y=value)) +geom_point() +stat_summary(fun=mean, geom="point", size=3)+facet_grid(name~.,scales = "free_y")+ylab("residual")



p.eco.phylo.PHPD <- anole.log%>%
  ggplot(aes(x=SVL,y=phylo.res.PHPD)) +geom_point() +stat_summary(fun=mean, geom="point", size=3)

print(p.eco.phylo.PHPD)













