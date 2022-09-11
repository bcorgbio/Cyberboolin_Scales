#Adam Qu, Rachel Zhu - Module 1 Project

library(ggplot2)
library(tidyverse)

#Load the "scales" dataset into a variable
dat <- read.csv("scales.csv")

#Report the class of each column in the dataset
sapply(dat,class) 

#Report the dimensions of the dataset (number of rows, columns)
dim(dat)

#Summarize the number of punctures for each species using the pipe function and save to a variable
species.n <- dat %>%
  group_by(species) %>%
  summarise(n = n())
species.n

#Returns data frame with the number of specimens sampled for each species
dat %>% 
  count(species,specimen) %>%
  print() %>%
  count(species,name = "n.specimens")

#For loop to produce boxplots of puncture force verus quadrant for each species and saving to a PDF
pdf("Adam.species.quadrant.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()
list.files(pattern=".pdf")