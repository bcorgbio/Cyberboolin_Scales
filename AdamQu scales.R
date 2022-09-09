#Adam Qu, Rachel Zhu - Module 1 Project

library(ggplot2)
library(tidyverse)

#A dat variable containing the scales dataset.
dat <- read.csv("scales.csv")

#A line of code which reports the class of each column in the dataset.
sapply(dat,class)

#A line of code which reports the dimensions of the dataset.
dim(dat)

#Code that produces a summary of the number of scales punctured for each species.
species.n<- dat %>%
  group_by(species) %>%
  summarise(n = n())
species.n

#Code that produces a summary of the number of specimens sampled for each species.
dat %>% 
  count(species,specimen) %>%
  print() %>%
  count(species,name = "n.specimens")

#Code that produces a PDF file containing 6 figures, one for each species that includes a boxplot of puncture force verus quadrant.
pdf("species.quadrant.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()



