#CyberBoolin - Module 3 report

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

anole.tree <- read.tree("anole.tre")
plot(anole.tree,cex=0.4)

#1. Establish the anole.log data tibble.
anole2 <- anole%>%
  left_join(anole.eco)%>%
  filter(!Ecomorph%in%c("U","CH"))%>% #exclude U, CH values in Ecomorph
  na.omit()%>% 
  print()
anole.log <- anole2%>% #change size, ecological data to log transformations
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)

#2. Construct two simple linear models that assess the effect of perch diameter and height
anole.PH <- lm(HTotal~SVL+PH, anole.log)
anole.PD <- lm(HTotal~SVL+ArbPD, anole.log)

#3. Plot residuals of linear models against Ecomorph.
anole.log <- anole.log %>% 
  mutate(resPH=residuals(anole.PH))
p.ecoPH <- anole.log %>% 
  ggplot(aes(Ecomorph2,resPH)) + geom_boxplot()
p.ecoPH + geom_boxplot() + stat_summary(fun=mean, geom="point", size=3)

anole.log <- anole.log %>% 
  mutate(resPD=residuals(anole.PD))
p.ecoPD <- anole.log %>% 
  ggplot(aes(Ecomorph2,resPD)) + geom_boxplot()
p.ecoPD + geom_boxplot()+stat_summary(fun=mean, geom="point", size=3)

#4. BM, PGLS models
#PGLS model with hindlimb-SVL relationship + perch height:
pgls.BM.PH <- gls(HTotal ~SVL + PH, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#PGLS model with hindlimb-SVL relationship + perch diameter:
pgls.BM.PD <- gls(HTotal ~SVL + ArbPD, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#PGLS model with hindlimb-SVL relationship + PH + PD:
pgls.BM.PHPD <- gls(HTotal ~SVL + PH + ArbPD, correlation = corBrownian(1,phy = anole.tree,form=~Species),data = anole.log, method = "ML")

#5. Assess fit of models using AICc, AICw
anole.log.aic <- AICc(pgls.BM.PH, pgls.BM.PD, pgls.BM.PHPD)
aicw(anole.log.aic$AICc)
#The PGLS model with both covariates is the best fit; it has the lowest AIC score and delta equal to 0.
anova(pgls.BM.PHPD)
#Both PH and PD are significant predictors of hindlimb length in a phylogenetic context (p-value<0.05).

#6. Plot showing effect of covariates on residuals of the pgls.BM.PHPD model
anole.log <- anole.log %>% 
  mutate(phylo.res.PHPD=residuals(pgls.BM.PHPD))

anole.log%>%
  dplyr::select(PH,Ecomorph2, resPH,resPD)%>%
  pivot_longer(cols=c("resPH","resPD"))%>%
  print%>%
  ggplot(aes(x=PH,y=value,col=Ecomorph2)) +geom_point() + stat_summary(fun=mean, geom="point", size=3) + facet_grid(name~.,scales = "free_y")+ylab("residual")


#CPK: An exceptional job!!! Great work putting the code togeter and answering the questons with some sophisticated analysis. (15/15)

