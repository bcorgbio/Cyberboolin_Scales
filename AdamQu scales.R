#Adam Qu, Rachel Zhu - Module 1 Project

library(ggplot2)
library(tidyverse)

dat <- read.csv("scales.csv")
dim(dat)
head(dat)
class(dat$N)
class(dat$quadrant)
class(dat$species)
class(dat$specimen)
sapply(dat,class)
dat$species <- as.factor(dat$species)
species <- levels(dat$species)
species
length(species)
#^How many unique values there are in "species"
dat$species==species[1]
#^Do the values in dat$species equal the first value in the "species" variable

