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

