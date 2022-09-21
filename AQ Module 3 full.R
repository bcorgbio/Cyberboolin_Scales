#Adam Qu - Module 3 report

library(tidyverse) # Rember to load your libraries!
library(ape)
library(nlme)
library(geiger)
library(caper)
library(phytools)
library(viridis)
library(MuMIn)

#Loading data
anole <- read_csv("anole.dat.csv")
anole.eco <- read_csv("anole.eco.csv")

#merge anole data tibble with anole.eco
anole2 <- anole%>%
  left_join(anole.eco)%>%
  filter(!Ecomorph%in%c("U","CH"))%>% #exclude U, CH values in Ecomorph
  na.omit()%>% 
  print()
anole.log <- anole2%>% #change size, ecological data to log transformations
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)

#Plot hind limb length vs size in anoles
anole2%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_smooth(method="lm")
#evaluate fit of linear model 
anole.lm <- lm(HTotal~SVL, anole2)
coef(anole.lm)
#linear model with HTotal predicted by SVL
anole2%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_abline(slope=coef(anole.lm)[2],intercept=coef(anole.lm)[1],col="blue")

#Tibble with predictions using linear model for HTotal using SVLs 
SVL2 <- seq(min(anole2$SVL),max(anole2$SVL),0.1)

pred.lm <-tibble(
  SVL=SVL2,
  H.pred=predict(anole.lm,newdata = data.frame(SVL=SVL2))
)

anole2%>%
  ggplot(aes(SVL,HTotal))+geom_point()+geom_point(data=pred.lm,aes(SVL,H.pred),col="blue")
summary(anole.lm)

#use nonlinear least squares to assess how HTotal relates to size
#Parameters a and b are significant, are explanatory parameters
anole.allo <- nls(HTotal~a*SVL^b, start=list(b=1, a=1),data = anole2)
summary(anole.allo)

#use AIC to compare models
#AICc from the MuMIn package
anole.aic <- AICc(anole.lm,anole.allo)
#aicw from the geiger package
anole.aicw <- aicw(anole.aic$AICc)

print(anole.aicw) #second model (anole.allo) has lower AIC score = better fit

#Visualize hindlimb-SVL relationships to ecomorph
anole.log%>%
  ggplot(aes(HTotal,SVL,col=Ecomorph2))+geom_point()+geom_smooth(method="lm")
anole.log.eco.lm <- lm(HTotal~SVL*Ecomorph2,anole.log)
summary(anole.log.eco.lm)
# ANOVA indicates that reject null hypothesis that ecomorph groups do not have separate hindlimb-SVL relationships
anova(anole.log.eco.lm) 
#ecomorph has significant effect on hindlimb-SVL relationship

anole.log.lm  <- lm(HTotal~SVL,anole.log)
anova(anole.log.lm)

anole.log.aic <- AICc(anole.log.lm,anole.log.eco.lm)
aicw(anole.log.aic$AICc) #model with Ecomorph2 added parameter is better fit

#compute residuals for anole.log.lm
anole.log <- anole.log %>%
  mutate(res=residuals(anole.log.lm))
#plot residuals against ecomorph2
anole.log%>%
  ggplot(aes(Ecomorph2,res))+geom_point()
#Plot residuals with median residuals for each ecomorph
p.eco <- anole.log%>%
  ggplot(aes(x=Ecomorph2,y=res)) +geom_boxplot()
print(p.eco)
#Add geometry that includes representation of mean residual for ecomorph
p.eco+ geom_boxplot() +stat_summary(fun=mean, geom="point", size=3)

#Anole phylogeny tree
anole.tree <- read.tree("anole.tre")
plot(anole.tree,cex=0.4)

#PGLS under BM, no ecomorph
pgls.BM1 <- gls(HTotal ~SVL, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#PGLS under BM, w ecomorph
pgls.BM2 <- gls(HTotal ~SVL * Ecomorph2, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")


#PGLS under OU, no ecomorph
pgls.OU1 <- gls(HTotal ~SVL, correlation = corMartins(0,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#PGLS under OU, w, ecomorph
pgls.OU2 <- gls(HTotal ~SVL * Ecomorph2, correlation = corMartins(0,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#AIC to see which model fits best: pgls.BM2 is best
#OU models specifying pull to global optimum are rejected--traits evolved randomly within each lineage
anole.phylo.aic <- AICc(pgls.BM1,pgls.BM2,pgls.OU1,pgls.OU2)
aicw(anole.phylo.aic$AICc)
#anova on pgls.BM2 to see if Ecomorph is significant factor in predicting HL-SVL relationship
anova(pgls.BM2)

anole.log <- anole.log%>%
  mutate(phylo.res=residuals(pgls.BM2))

p.eco.phylo <- anole.log%>%
  ggplot(aes(x=Ecomorph2,y=phylo.res)) +geom_boxplot() +stat_summary(fun=mean, geom="point", size=3)

print(p.eco.phylo)

#Pivot longer + facet, plot phylogentically corrected, uncorrected residuals against ecomorph
anole.log%>%
  dplyr::select(Ecomorph2,res,phylo.res)%>%
  pivot_longer(cols=c("res","phylo.res"))%>%
  print%>%
  ggplot(aes(x=Ecomorph2,y=value)) +geom_boxplot() +stat_summary(fun=mean, geom="point", size=3)+facet_grid(name~.,scales = "free_y")+ylab("residual")

#1. Combine the code above so that you can establish the anole.log data tibble.
anole2 <- anole%>%
  left_join(anole.eco)%>%
  filter(!Ecomorph%in%c("U","CH"))%>% #exclude U, CH values in Ecomorph
  na.omit()%>% 
  print()
anole.log <- anole2%>% #change size, ecological data to log transformations
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)




